namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Reflection
open FSharpx.Collections
open VSharp
open VSharp.Core

module TestGenerator =
    let rec term2obj model indices (test : UnitTest) = function
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | {term = Struct(fields, t)} ->
            let t = Types.ToDotNetType t
            let fieldReprs =
                t |> Reflection.fieldsOf false |> Array.map (fun (field, _) -> term2obj model indices test fields.[field])
            test.MemoryGraph.RepresentStruct t fieldReprs
        | NullRef -> null
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} -> obj2test model indices test addr
        | _ -> __notImplemented__()

    and obj2test model (indices : Dictionary<concreteHeapAddress, int>) test addr =
        let eval address =
            address |> Ref |> Memory.Read model.state |> model.Complete |> term2obj model indices test
        let index = ref 0
        if indices.TryGetValue(addr, index) then
            let referenceRepr : referenceRepr = {index = !index}
            referenceRepr :> obj
        else
            let typ = model.state.allocatedTypes.[addr]
            let cha = ConcreteHeapAddress addr
            match typ with
            | ArrayType(elemType, dim) ->
                let dnt = Types.ToDotNetType typ
                let arrayType, (lengths : int array), (lowerBounds : int array) =
                    match dim with
                    | Vector ->
                        let arrayType = (elemType, 1, true)
                        arrayType, [| ArrayLength(cha, MakeNumber 0, arrayType) |> eval |> unbox |], null
                    | ConcreteDimension rank ->
                        let arrayType = (elemType, rank, true)
                        arrayType,
                        Array.init rank (fun i -> ArrayLength(cha, MakeNumber i, arrayType) |> eval |> unbox),
                        Array.init rank (fun i -> ArrayLowerBound(cha, MakeNumber i, arrayType) |> eval |> unbox)
                    | SymbolicDimension -> __notImplemented__()
                let contents = Array.init (Array.reduce ( * ) lengths) (fun i ->
                    let indices = Seq.delinearizeArrayIndex i lengths lowerBounds
                    let indexTerms = indices |> Seq.map (fun i -> Concrete i Types.IndexType) |> List.ofSeq
                    ArrayIndex(cha, indexTerms, arrayType) |> eval)
                let repr = test.MemoryGraph.AddArray dnt contents lengths lowerBounds
                indices.Add(addr, repr.index)
                repr :> obj
            | _ ->
                let typ = Types.ToDotNetType typ
                let fields = typ |> Reflection.fieldsOf false |> Array.map (fun (field, _) ->
                    ClassField(cha, field) |> eval)
                let repr = test.MemoryGraph.AddClass typ fields
                indices.Add(addr, repr.index)
                repr :> obj

    let state2test (m : MethodBase) (cilState : cilState) =
        let indices = Dictionary<concreteHeapAddress, int>()
        let test = UnitTest m

        match cilState.state.model with
        | Some model ->
            m.GetParameters() |> Seq.iter (fun pi ->
                let value = Memory.ReadArgument model.state pi |> model.Complete
//                let value = model.Eval symbolicArg
                let concreteValue : obj = term2obj model indices test value
                test.AddArg pi concreteValue)

            if Reflection.hasThis m then
                let value = Memory.ReadThis model.state m |> model.Complete
//                let value = model.Eval symbolicArg
                let concreteValue : obj = term2obj model indices test value
                test.ThisArg <- concreteValue

            let retVal = model.Eval cilState.Result
            test.Expected <- term2obj model indices test retVal
        | None ->
            let emptyState = Memory.EmptyState()
            emptyState.allocatedTypes <- cilState.state.allocatedTypes
            let parameters = m.GetParameters() |> Seq.map (fun param ->
                (ParameterKey param, None, Types.FromDotNetType param.ParameterType)) |> List.ofSeq
            let parametersAndThis =
                if Reflection.hasThis m then
                    let this = System.Runtime.Serialization.FormatterServices.GetUninitializedObject(m.DeclaringType)
                    test.ThisArg <- this
                    let t = Types.FromDotNetType m.DeclaringType
                    let addr = [-1]
                    let thisRef = HeapRef (ConcreteHeapAddress addr) t
                    emptyState.allocatedTypes <- PersistentDict.add addr t emptyState.allocatedTypes
                    emptyState.startingTime <- [-2]
                    (ThisKey m, Some thisRef, t) :: parameters // TODO: incorrect type when ``this'' is Ref to stack
                else parameters
            Memory.NewStackFrame emptyState m parametersAndThis
            m.GetParameters() |> Seq.iter (fun pi ->
                let defaultValue = TypeUtils.defaultOf pi.ParameterType
                test.AddArg pi defaultValue)
            let emptyModel = {subst = Dictionary<_,_>(); state = emptyState; complete = true}
            let retVal = emptyModel.Eval cilState.Result
            test.Expected <- term2obj emptyModel indices test retVal
        test
