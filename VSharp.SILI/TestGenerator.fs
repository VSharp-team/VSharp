namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core

module TestGenerator =

    let private obj2test eval (indices : Dictionary<concreteHeapAddress, int>) (encodeMock : ITypeMock -> obj) (test : UnitTest) addr typ =
        let index = ref 0
        if indices.TryGetValue(addr, index) then
            let referenceRepr : referenceRepr = {index = index.Value}
            referenceRepr :> obj
        else
            match typ with
            | ConcreteType typ ->
                let cha = ConcreteHeapAddress addr
                match typ with
                | TypeUtils.ArrayType(elemType, dim) ->
                    let index = test.MemoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let arrayType, (lengths : int array), (lowerBounds : int array) =
                        match dim with
                        | Vector ->
                            let arrayType = (elemType, 1, true)
                            arrayType, [| ArrayLength(cha, MakeNumber 0, arrayType) |> eval |> unbox |], null
                        | ConcreteDimension rank ->
                            let arrayType = (elemType, rank, false)
                            arrayType,
                            Array.init rank (fun i -> ArrayLength(cha, MakeNumber i, arrayType) |> eval |> unbox),
                            Array.init rank (fun i -> ArrayLowerBound(cha, MakeNumber i, arrayType) |> eval |> unbox)
                        | SymbolicDimension -> __notImplemented__()
                    let length = Array.reduce ( * ) lengths
                    // TODO: normalize model (for example, try to minimize lengths of generated arrays)
                    if length > 128 then raise <| InsufficientInformationException "Test generation for too large buffers disabled for now"
                    let contents = Array.init length (fun i ->
                        let indices = Seq.delinearizeArrayIndex i lengths lowerBounds
                        let indexTerms = indices |> Seq.map (fun i -> Concrete i Types.IndexType) |> List.ofSeq
                        ArrayIndex(cha, indexTerms, arrayType) |> eval)
                    let repr = test.MemoryGraph.AddArray typ contents lengths lowerBounds index
                    repr :> obj
                | _ when typ.IsValueType -> BoxedLocation(addr, typ) |> eval
                | _ when typ = typeof<string> ->
                    let length : int = ClassField(cha, Reflection.stringLengthField) |> eval |> unbox
                    let contents : char array = Array.init length (fun i -> ArrayIndex(cha, [MakeNumber i], (typeof<char>, 1, true)) |> eval |> unbox)
                    String(contents) :> obj
                | _ ->
                    let index = test.MemoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let fields = typ |> Reflection.fieldsOf false |> Array.map (fun (field, _) ->
                        ClassField(cha, field) |> eval)
                    let repr = test.MemoryGraph.AddClass typ fields index
                    repr :> obj
            | MockType mock -> encodeMock mock

    let rec private term2obj (model : model) state indices mockCache (test : UnitTest) = function
        | {term = Concrete(_, TypeUtils.AddressType)} -> __unreachable__()
        | {term = Concrete(v, t)} when t.IsEnum -> test.MemoryGraph.RepresentEnum v
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | {term = Constant _ } as c -> model.Eval c |> term2obj model state indices mockCache test
        | {term = Struct(fields, t)} when Types.IsNullable t ->
            let valueField, hasValueField = Reflection.fieldsOfNullable t
            let hasValue : bool = fields.[hasValueField] |> term2obj model state indices mockCache test |> unbox
            if hasValue then
                fields.[valueField] |> term2obj model state indices mockCache test
            else null
        | {term = Struct(fields, t)} ->
            let fieldReprs =
                t |> Reflection.fieldsOf false |> Array.map (fun (field, _) -> model.Complete fields.[field] |> term2obj model state indices mockCache test)
            test.MemoryGraph.RepresentStruct t fieldReprs
        | NullRef _
        | NullPtr -> null
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} when VectorTime.less addr VectorTime.zero ->
            let eval address =
                address |> Ref |> Memory.Read model.state |> model.Complete |> term2obj model state indices mockCache test
            let typ = model.state.allocatedTypes.[addr]
            obj2test eval indices (encodeTypeMock model state indices mockCache test >> test.AllocateMockObject) test addr typ
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} ->
            let eval address =
                address |> Ref |> Memory.Read state |> model.Eval |> term2obj model state indices mockCache test
            let typ = state.allocatedTypes.[addr]
            obj2test eval indices (encodeTypeMock model state indices mockCache test >> test.AllocateMockObject) test addr typ
        | Combined(terms, t) ->
            let slices = List.map model.Eval terms
            ReinterpretConcretes slices t
        | term -> internalfailf "creating object from term: unexpected term %O" term

    and encodeTypeMock (model : model) state indices (mockCache : Dictionary<ITypeMock, Mocking.Type>) (test : UnitTest) mock =
        Dict.getValueOrUpdate mockCache mock (fun () ->
            let freshMock = test.DefineTypeMock(mock.Name)
            mock.SuperTypes |> Seq.iter freshMock.AddSuperType
            mock.MethodMocks |> Seq.iter (fun m ->
                let clauses = m.GetImplementationClauses() |> Array.map (term2obj model state indices mockCache test)
                freshMock.AddMethod(m.BaseMethod, clauses))
            freshMock)


    let model2test (test : UnitTest) isError hasException indices mockCache (m : Method) model cmdArgs (cilState : cilState) =
        match SolveTypes model cilState.state with
        | None -> None
        | Some(classParams, methodParams) ->
            let concreteClassParams = Array.zeroCreate classParams.Length
            let mockedClassParams = Array.zeroCreate classParams.Length
            let concreteMethodParams = Array.zeroCreate methodParams.Length
            let mockedMethodParams = Array.zeroCreate methodParams.Length
            let processSymbolicType (concreteArr : Type array) (mockArr : Mocking.Type option array) i = function
                | ConcreteType t -> concreteArr.[i] <- t
                | MockType m -> mockArr.[i] <- Some (encodeTypeMock model cilState.state indices mockCache test m)
            classParams |> Seq.iteri (processSymbolicType concreteClassParams mockedClassParams)
            methodParams |> Seq.iteri (processSymbolicType concreteMethodParams mockedMethodParams)
            test.SetTypeGenericParameters concreteClassParams mockedClassParams
            test.SetMethodGenericParameters concreteMethodParams mockedMethodParams

            let parametersInfo = m.Parameters
            match cmdArgs with
            | Some args ->
                // NOTE: entry point with specified args case
                assert(Array.length parametersInfo = 1)
                test.AddArg (Array.head parametersInfo) args
            | None ->
                parametersInfo |> Seq.iter (fun pi ->
                    let value = Memory.ReadArgument model.state pi |> model.Complete
                    let concreteValue : obj = term2obj model cilState.state indices mockCache test value
                    test.AddArg pi concreteValue)

            if m.HasThis then
                let value = Memory.ReadThis model.state m |> model.Complete
                let concreteValue : obj = term2obj model cilState.state indices mockCache test value
                test.ThisArg <- concreteValue

            if not isError && not hasException then
                let retVal = model.Eval cilState.Result
                test.Expected <- term2obj model cilState.state indices mockCache test retVal
            Some test

    let state2test isError (m : Method) cmdArgs (cilState : cilState) =
        let indices = Dictionary<concreteHeapAddress, int>()
        let mockCache = Dictionary<ITypeMock, Mocking.Type>()
        let test = UnitTest (m :> IMethod).MethodBase
        let hasException =
            match cilState.state.exceptionsRegister with
            | Unhandled e ->
                let t = MostConcreteTypeOfHeapRef cilState.state e
                test.Exception <- t
                true
            | _ -> false
        test.IsError <- isError

        match TryGetModel cilState.state with
        | Some model ->
            model2test test isError hasException indices mockCache m model cmdArgs cilState
        | None -> None
