namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open FSharpx.Collections
open VSharp
open VSharp.Core

module TestGenerator =

    let private obj2test eval (indices : Dictionary<concreteHeapAddress, int>) (test : UnitTest) addr typ =
        let index = ref 0
        if indices.TryGetValue(addr, index) then
            let referenceRepr : referenceRepr = {index = !index}
            referenceRepr :> obj
        else
            let cha = ConcreteHeapAddress addr
            let dnt = Types.ToDotNetType typ
            match typ with
            | ArrayType(elemType, dim) ->
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
                let repr = test.MemoryGraph.AddArray dnt contents lengths lowerBounds index
                repr :> obj
            | _ when dnt.IsValueType -> BoxedLocation(addr, typ) |> eval
            | _ when dnt = typeof<string> ->
                let length : int = ClassField(cha, Reflection.stringLengthField) |> eval |> unbox
                let contents : char array = Array.init length (fun i -> ArrayIndex(cha, [MakeNumber i], (Types.Char, 1, true)) |> eval |> unbox)
                String(contents) :> obj
            | _ ->
                let index = test.MemoryGraph.ReserveRepresentation()
                indices.Add(addr, index)
                let typ = Types.ToDotNetType typ
                let fields = typ |> Reflection.fieldsOf false |> Array.map (fun (field, _) ->
                    ClassField(cha, field) |> eval)
                let repr = test.MemoryGraph.AddClass typ fields index
                repr :> obj

    let rec private term2obj (model : model) state indices (test : UnitTest) = function
        | {term = Concrete(_, AddressType)} -> __unreachable__()
        | {term = Concrete(v, t)} when Types.IsEnum t -> test.MemoryGraph.RepresentEnum v
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | {term = Constant _ } as c -> model.Eval c |> term2obj model state indices test
        | {term = Struct(fields, t)} when Types.IsNullable t ->
            let t = Types.ToDotNetType t
            let valueField, hasValueField = Reflection.fieldsOfNullable t
            let hasValue : bool = fields.[hasValueField] |> term2obj model state indices test |> unbox
            if hasValue then
                fields.[valueField] |> term2obj model state indices test
            else null
        | {term = Struct(fields, t)} ->
            let t = Types.ToDotNetType t
            let fieldReprs =
                t |> Reflection.fieldsOf false |> Array.map (fun (field, _) -> model.Complete fields.[field] |> term2obj model state indices test)
            test.MemoryGraph.RepresentStruct t fieldReprs
        | NullRef
        | NullPtr -> null
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} when VectorTime.less addr VectorTime.zero ->
            let eval address =
                address |> Ref |> Memory.Read model.state |> model.Complete |> term2obj model state indices test
            let typ = model.state.allocatedTypes.[addr]
            obj2test eval indices test addr typ
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} ->
            let eval address =
                address |> Ref |> Memory.Read state |> model.Eval |> term2obj model state indices test
            let typ = state.allocatedTypes.[addr]
            obj2test eval indices test addr typ
        | Combined(terms, t) ->
            let slices = List.map model.Eval terms
            ReinterpretConcretes slices t
        | term -> internalfailf "creating object from term: unexpected term %O" term

    let private solveTypes (m : MethodBase) (model : model) (cilState : cilState) =
        let typeOfAddress addr =
            if VectorTime.less addr VectorTime.zero then model.state.allocatedTypes.[addr]
            else cilState.state.allocatedTypes.[addr]
        let supertypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let subtypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let notSupertypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let notSubtypeConstraints = Dictionary<concreteHeapAddress, List<Type>>()
        let addresses = HashSet<concreteHeapAddress>()
        let add dict address typ =
            match model.Eval address with
            | {term = ConcreteHeapAddress addr} when addr <> VectorTime.zero ->
                addresses.Add addr |> ignore
                let list = Dict.getValueOrUpdate dict addr (fun () -> List<Type>())
                let typ = Types.ToDotNetType typ
                if not <| list.Contains typ then
                    list.Add typ
                Dict.getValueOrUpdate supertypeConstraints addr (fun () ->
                    let list = List<Type>()
                    addr |> typeOfAddress |> Types.ToDotNetType |> list.Add
                    list)
                |> ignore
            | {term = ConcreteHeapAddress _} -> ()
            | term -> internalfailf "Unexpected address %O in subtyping constraint!" term

        PathConditionToSeq cilState.state.pc |> Seq.iter (function
            | {term = Constant(_, TypeSubtypeTypeSource _, _)} -> __notImplemented__()
            | {term = Constant(_, RefSubtypeTypeSource(address, typ), _)} -> add supertypeConstraints address typ
            | {term = Constant(_, TypeSubtypeRefSource(typ, address), _)} -> add subtypeConstraints address typ
            | {term = Constant(_, RefSubtypeRefSource _, _)} -> __notImplemented__()
            | Negation({term = Constant(_, TypeSubtypeTypeSource _, _)}) -> __notImplemented__()
            | Negation({term = Constant(_, RefSubtypeTypeSource(address, typ), _)}) -> add notSupertypeConstraints address typ
            | Negation({term = Constant(_, TypeSubtypeRefSource(typ, address), _)}) -> add notSubtypeConstraints address typ
            | Negation({term = Constant(_, RefSubtypeRefSource _, _)}) -> __notImplemented__()
            | _ ->())
        let toList (d : Dictionary<concreteHeapAddress, List<Type>>) addr =
            let l = Dict.tryGetValue d addr null
            if l = null then [] else List.ofSeq l
        let addresses = List.ofSeq addresses
        let inputConstraints =
            addresses
            |> Seq.map (fun addr -> {supertypes = toList supertypeConstraints addr; subtypes = toList subtypeConstraints addr
                                     notSupertypes = toList notSupertypeConstraints addr; notSubtypes = toList notSubtypeConstraints addr})
            |> List.ofSeq
        let typeGenericParameters = m.DeclaringType.GetGenericArguments()
        let methodGenericParameters = m.GetGenericArguments()
        let solverResult = TypeSolver.solve inputConstraints (Array.append typeGenericParameters methodGenericParameters |> List.ofArray)
        match solverResult with
        | TypeSat(refsTypes, typeParams) ->
            let classParams, methodParams = List.splitAt typeGenericParameters.Length typeParams
            Some(List.zip addresses refsTypes, Array.ofList classParams, Array.ofList methodParams)
        | TypeUnsat -> None
        | TypeVariablesUnknown -> raise (InsufficientInformationException "Could not detect appropriate substitution of generic parameters")
        | TypesOfInputsUnknown -> raise (InsufficientInformationException "Could not detect appropriate types of inputs")

    let tryGetModel (state : state) =
        match IsValid state with
        | SolverInteraction.SmtSat model -> Some model.mdl
        | SolverInteraction.SmtUnknown _ -> None
        // NOTE: irrelevant case, because exploring branch must be valid
        | SolverInteraction.SmtUnsat _ -> __unreachable__()

    let model2test (test : UnitTest) isError hasException indices m model cmdArgs cilState =
        match solveTypes m model cilState with
        | None -> None
        | Some(typesOfAddresses, classParams, methodParams) ->
            test.SetTypeGenericParameters classParams
            test.SetMethodGenericParameters methodParams
            typesOfAddresses |> Seq.iter (fun (addr, t) ->
                model.state.allocatedTypes <- PersistentDict.add addr (Types.FromDotNetType t) model.state.allocatedTypes
                if t.IsValueType then
                    model.state.boxedLocations <- PersistentDict.add addr (t |> Types.FromDotNetType |> Memory.DefaultOf) model.state.boxedLocations)

            let parametersInfo = m.GetParameters()
            match cmdArgs with
            | Some args ->
                // NOTE: entry point with specified args case
                assert(Array.length parametersInfo = 1)
                test.AddArg (Array.head parametersInfo) args
            | None ->
                parametersInfo |> Seq.iter (fun pi ->
                    let value = Memory.ReadArgument model.state pi |> model.Complete
                    let concreteValue : obj = term2obj model cilState.state indices test value
                    test.AddArg pi concreteValue)

            if Reflection.hasThis m then
                let value = Memory.ReadThis model.state m |> model.Complete
                let concreteValue : obj = term2obj model cilState.state indices test value
                test.ThisArg <- concreteValue

            if not isError && not hasException then
                let retVal = model.Eval cilState.Result
                test.Expected <- term2obj model cilState.state indices test retVal
            Some test

    let state2test isError (m : MethodBase) cmdArgs (cilState : cilState) =
        let indices = Dictionary<concreteHeapAddress, int>()
        let test = UnitTest m
        let hasException =
            match cilState.state.exceptionsRegister with
            | Unhandled e ->
                let t = MostConcreteTypeOfHeapRef cilState.state e |> Types.ToDotNetType
                test.Exception <- t
                true
            | _ -> false
        test.IsError <- isError

        match cilState.state.model with
        | Some model ->
            model2test test isError hasException indices m model cmdArgs cilState
        | None when cilState.state.pc = EmptyPathCondition ->
            // NOTE: case when no branches occured
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
            let parametersInfo = m.GetParameters()
            match cmdArgs with
            | Some args ->
                // NOTE: entry point with specified args case
                assert(Array.length parametersInfo = 1)
                test.AddArg (Array.head parametersInfo) args
            | None ->
                parametersInfo |> Seq.iter (fun pi -> test.AddArg pi (TypeUtils.defaultOf pi.ParameterType))
            let emptyModel = {subst = Dictionary<_,_>(); state = emptyState; complete = true}
            match solveTypes m emptyModel cilState with
            | None -> None
            | Some(typesOfAddresses, classParams, methodParams) ->
                test.SetTypeGenericParameters classParams
                test.SetMethodGenericParameters methodParams
                typesOfAddresses |> Seq.iter (fun (addr, t) ->
                    emptyModel.state.allocatedTypes <- PersistentDict.add addr (Types.FromDotNetType t) emptyModel.state.allocatedTypes
                    if t.IsValueType then
                        emptyModel.state.boxedLocations <- PersistentDict.add addr (t |> Types.FromDotNetType |> Memory.DefaultOf) emptyModel.state.boxedLocations)

                if not isError && not hasException then
                    let retVal = emptyModel.Eval cilState.Result
                    test.Expected <- term2obj emptyModel cilState.state indices test retVal
                Some test
        | None ->
            // NOTE: case when branch occured, but we have no model, so trying to get it
            match tryGetModel cilState.state with
            | Some model ->
                model2test test isError hasException indices m model cmdArgs cilState
            | None -> None
