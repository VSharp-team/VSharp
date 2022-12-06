namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Core

module TestGenerator =

    let mutable private maxBufferSize = 128
    let internal setMaxBufferSize size = maxBufferSize <- size

    let private addMockToMemoryGraph (indices : Dictionary<concreteHeapAddress, int>) (encodeMock : ITypeMock -> obj) (test : UnitTest) addr mock =
        let index = test.MemoryGraph.ReserveRepresentation()
        indices.Add(addr, index)
        let repr = test.MemoryGraph.AddMockedClass (encodeMock mock) index
        repr :> obj

    let private obj2test eval encodeArr (indices : Dictionary<concreteHeapAddress, int>) (encodeMock : ITypeMock -> obj) (test : UnitTest) addr typ =
        let index = ref 0
        if indices.TryGetValue(addr, index) then
            let referenceRepr : referenceRepr = {index = index.Value}
            referenceRepr :> obj
        else
            match typ with
            | ConcreteType typ when TypeUtils.isDelegate typ ->
                // Obj is a delegate which mock hasn't been created yet
                let mock = TypeMock(Seq.singleton typ)
                addMockToMemoryGraph indices encodeMock test addr mock
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
                    if maxBufferSize > 0 && length > maxBufferSize then
                        raise <| InsufficientInformationException "Test generation for too large buffers disabled for now"
                    let repr = encodeArr test arrayType addr typ lengths lowerBounds index
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
            | MockType mock when mock.IsValueType -> encodeMock mock
            | MockType mock -> addMockToMemoryGraph indices encodeMock test addr mock

    let private encodeArrayCompactly (state : state) (model : model) (encode : term -> obj) (test : UnitTest) arrayType cha typ lengths lowerBounds index =
        if state.concreteMemory.Contains cha then
            test.MemoryGraph.AddArray typ (state.concreteMemory.VirtToPhys cha :?> Array |> Array.mapToOneDArray test.MemoryGraph.Encode) lengths lowerBounds index
        else
            let arrays =
                if VectorTime.less cha VectorTime.zero then
                    match model with
                    | StateModel(modelState, _) -> modelState.arrays
                    | _ -> __unreachable__()
                else
                    state.arrays
            let defaultValue, indices, values =
                match PersistentDict.tryFind arrays arrayType with
                | Some region ->
                    let defaultValue =
                        match region.defaultValue with
                        | Some defaultValue -> encode defaultValue
                        | None -> null
                    let updates = region.updates
                    let indices = List<int array>()
                    let values = List<obj>()
                    updates |> RegionTree.foldr (fun _ k () ->
                        let heapAddress = model.Eval k.key.address
                        match heapAddress with
                        | {term = ConcreteHeapAddress(cha')} when cha' = cha ->
                            let i : int array = k.key.indices |> List.map (encode >> unbox) |> Array.ofList
                            let v = encode k.value
                            indices.Add(i)
                            values.Add(v)
                        | _ -> ()) ()
                    defaultValue, indices.ToArray(), values.ToArray()
                | None -> null, Array.empty, Array.empty
            // TODO: if addr is managed by concrete memory, then just serialize it normally (by test.MemoryGraph.AddArray)
            test.MemoryGraph.AddCompactArrayRepresentation typ defaultValue indices values lengths lowerBounds index

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
            match model with
            | StateModel(modelState, _) ->
                let eval address =
                    address |> Ref |> Memory.Read modelState |> model.Complete |> term2obj model state indices mockCache test
                let arr2Obj = encodeArrayCompactly state model (term2obj model state indices mockCache test)
                let typ = modelState.allocatedTypes.[addr]
                obj2test eval arr2Obj indices (encodeTypeMock model state indices mockCache test >> test.AllocateMockObject) test addr typ
            | PrimitiveModel _ -> __unreachable__()
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} ->
            let eval address =
                address |> Ref |> Memory.Read state |> model.Eval |> term2obj model state indices mockCache test
            let arr2Obj = encodeArrayCompactly state model (term2obj model state indices mockCache test)
            let typ = state.allocatedTypes.[addr]
            obj2test eval arr2Obj indices (encodeTypeMock model state indices mockCache test >> test.AllocateMockObject) test addr typ
        | Combined(terms, t) ->
            let slices = List.map model.Eval terms
            ReinterpretConcretes slices t
        | term -> internalfailf "creating object from term: unexpected term %O" term

    and private encodeTypeMock (model : model) state indices (mockCache : Dictionary<ITypeMock, Mocking.Type>) (test : UnitTest) mock =
        let createMock() =
            let freshMock = test.DefineTypeMock(mock.Name)
            for t in mock.SuperTypes do
                freshMock.AddSuperType t
            for m in mock.MethodMocks do
                let eval = model.Eval >> term2obj model state indices mockCache test
                let clauses = m.GetImplementationClauses() |> Array.map eval
                freshMock.AddMethod(m.BaseMethod, clauses)
            freshMock
        Dict.getValueOrUpdate mockCache mock createMock

    let private model2test (test : UnitTest) isError indices mockCache (m : Method) model cmdArgs (cilState : cilState) message =
        let suitableState cilState =
            let methodHasByRefParameter (m : Method) = m.Parameters |> Seq.exists (fun pi -> pi.ParameterType.IsByRef)
            match () with
            | _ when m.DeclaringType.IsValueType || methodHasByRefParameter m ->
                Memory.CallStackSize cilState.state = 2
            | _ -> Memory.CallStackSize cilState.state = 1
        if not <| suitableState cilState
            then internalfail "Finished state has many frames on stack! (possibly unhandled exception)"

        match model with
        | StateModel(modelState, typeModel) ->
            match SolveGenericMethodParameters typeModel m with
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
                        let value =
                            if pi.ParameterType.IsByRef then
                                let key = ParameterKey pi
                                let stackRef = Memory.ReadLocalVariable cilState.state key
                                Memory.Read modelState stackRef
                            else
                                Memory.ReadArgument modelState pi |> model.Complete
                        let concreteValue : obj = term2obj model cilState.state indices mockCache test value
                        test.AddArg pi concreteValue)

                if m.HasThis then
                    let thisTerm =
                        if m.DeclaringType.IsValueType then
                            let stackRef = Memory.ReadThis cilState.state m
                            Memory.Read modelState stackRef
                        else
                            Memory.ReadThis modelState m |> model.Complete
                    let concreteThis = term2obj model cilState.state indices mockCache test thisTerm
                    test.ThisArg <- concreteThis

                let hasException, message =
                    match cilState.state.exceptionsRegister with
                    | Unhandled(e, _) ->
                        let t = MostConcreteTypeOfHeapRef cilState.state e
                        test.Exception <- t
                        let message =
                            if isError && String.IsNullOrEmpty message then
                                let messageReference = Memory.ReadField cilState.state e Reflection.exceptionMessageField |> model.Eval
                                term2obj model cilState.state indices mockCache test messageReference :?> string
                            else message
                        true, message
                    | _ -> false, message
                test.IsError <- isError
                test.ErrorMessage <- message

                if not isError && not hasException then
                    let retVal = model.Eval cilState.Result
                    test.Expected <- term2obj model cilState.state indices mockCache test retVal
                Some test
        | _ -> __unreachable__()

    let internal state2test isError (m : Method) cmdArgs (cilState : cilState) message =
        let indices = Dictionary<concreteHeapAddress, int>()
        let mockCache = Dictionary<ITypeMock, Mocking.Type>()
        let test = UnitTest((m :> IMethod).MethodBase)

        model2test test isError indices mockCache m cilState.state.model cmdArgs cilState message
