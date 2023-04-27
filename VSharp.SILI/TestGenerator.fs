namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Reflection
open FSharpx.Collections
open VSharp
open VSharp.Core
open System.Linq;

module TestGenerator =

    let mutable private maxBufferSize = 128
    let internal setMaxBufferSize size = maxBufferSize <- size

    let private addMockToMemoryGraph (indices : Dictionary<concreteHeapAddress, int>) encodeMock evalField (test : UnitTest) addr (mock : ITypeMock) =
        let index = test.MemoryGraph.ReserveRepresentation()
        indices.Add(addr, index)
        let mock : Mocking.Type = encodeMock mock
        let baseClass = mock.BaseClass
        let fields =
            match evalField with
            | Some evalField when baseClass <> null && not (TypeUtils.isDelegate baseClass) ->
                Reflection.fieldsOf false baseClass
                |> Array.map (fst >> evalField)
            | _ -> Array.empty
        test.MemoryGraph.AddMockedClass mock fields index :> obj

    let private obj2test eval encodeArr (indices : Dictionary<concreteHeapAddress, int>) encodeMock (test : UnitTest) addr typ =
        let index = ref 0
        if indices.TryGetValue(addr, index) then
            let referenceRepr : referenceRepr = {index = index.Value}
            referenceRepr :> obj
        else
            let memoryGraph = test.MemoryGraph
            let cha = ConcreteHeapAddress addr
            match typ with
            | ConcreteType typ when TypeUtils.isDelegate typ ->
                // Obj is a delegate which mock hasn't been created yet
                let mock = TypeMock(Seq.singleton typ)
                addMockToMemoryGraph indices encodeMock None test addr mock
            | ConcreteType typ ->
                match typ with
                | TypeUtils.ArrayType(elemType, dim) ->
                    let index = memoryGraph.ReserveRepresentation()
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
                    String(contents) |> memoryGraph.RepresentString
                | _ ->
                    let index = memoryGraph.ReserveRepresentation()
                    indices.Add(addr, index)
                    let fields = typ |> Reflection.fieldsOf false |> Array.map (fun (field, _) ->
                        ClassField(cha, field) |> eval)
                    let repr = memoryGraph.AddClass typ fields index
                    repr :> obj
            | MockType mock when mock.IsValueType -> memoryGraph.RepresentMockedStruct (encodeMock mock) Array.empty
            | MockType mock ->
                let evalField field = ClassField(cha, field) |> eval
                addMockToMemoryGraph indices encodeMock (Some evalField) test addr mock

    let private encodeArrayCompactly (state : state) (model : model) (encode : term -> obj) (test : UnitTest) arrayType cha typ lengths lowerBounds index =
        if state.concreteMemory.Contains cha then
            // TODO: Use compact representation for big arrays
            let contents =
                state.concreteMemory.VirtToPhys cha :?> Array
                |> Array.mapToOneDArray test.MemoryGraph.Encode
            test.MemoryGraph.AddArray typ contents lengths lowerBounds index
        else
            let arrays =
                if VectorTime.less cha VectorTime.zero then
                    match model with
                    | StateModel modelState -> modelState.arrays
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
                    let indicesWithValues = SortedDictionary<int list, obj>()
                    let addOneKey _ (k : updateTreeKey<heapArrayKey, term>) () =
                        let value = k.value
                        match k.key with
                        | OneArrayIndexKey(address, keyIndices) ->
                            let heapAddress = model.Eval address
                            match heapAddress with
                            | {term = ConcreteHeapAddress(cha')} when cha' = cha ->
                                let i = keyIndices |> List.map (encode >> unbox)
                                let v = encode value
                                indicesWithValues[i] <- v
                            | _ -> ()
                        | RangeArrayIndexKey(address, fromIndices, toIndices) ->
                            let heapAddress = model.Eval address
                            match heapAddress with
                            | {term = ConcreteHeapAddress(cha')} when cha' = cha ->
                                let fromIndices : int list = fromIndices |> List.map (encode >> unbox)
                                let toIndices : int list = toIndices |> List.map (encode >> unbox)
                                let allIndices = Array.allIndicesViaBound fromIndices toIndices
                                match value.term with
                                | Constant(_, ArrayRangeReading, _) ->
                                    for i in allIndices do
                                        let index = List.map MakeNumber i
                                        let key = OneArrayIndexKey(heapAddress, index)
                                        let v = SpecializeWithKey value key k.key |> encode
                                        indicesWithValues[i] <- v
                                | _ ->
                                    for i in allIndices do
                                        let v = encode value
                                        indicesWithValues[i] <- v
                            | _ -> ()
                    updates |> RegionTree.foldr addOneKey ()
                    let indices = indicesWithValues.Keys.ToArray()
                    let values = indicesWithValues.Values.ToArray()
                    defaultValue, indices.ToArray(), values.ToArray()
                | None -> null, Array.empty, Array.empty
            let indices = Array.map Array.ofList indices
            test.MemoryGraph.AddCompactArrayRepresentation typ defaultValue indices values lengths lowerBounds index

    let rec private term2obj (model : model) state indices mockCache (implementations : IDictionary<MethodInfo, term[]>) (test : UnitTest) = function
        | {term = Concrete(_, TypeUtils.AddressType)} -> __unreachable__()
        | {term = Concrete(v, t)} when t.IsEnum -> test.MemoryGraph.RepresentEnum v
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | {term = Constant _ } as c -> model.Eval c |> term2obj model state indices mockCache implementations test
        | {term = Struct(fields, t)} when Types.IsNullable t ->
            let valueField, hasValueField = Reflection.fieldsOfNullable t
            let hasValue : bool = fields.[hasValueField] |> term2obj model state indices mockCache implementations test |> unbox
            if hasValue then
                fields.[valueField] |> term2obj model state indices mockCache implementations test
            else null
        | {term = Struct(fields, t)} ->
            let fieldReprs =
                t |> Reflection.fieldsOf false |> Array.map (fun (field, _) -> model.Complete fields.[field] |> term2obj model state indices mockCache implementations test)
            test.MemoryGraph.RepresentStruct t fieldReprs
        | NullRef _
        | NullPtr -> null
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} when VectorTime.less addr VectorTime.zero ->
            match model with
            | StateModel modelState ->
                match PersistentDict.tryFind modelState.allocatedTypes addr with
                | Some typ ->
                    let eval address =
                        address |> Ref |> Memory.Read modelState |> model.Complete |> term2obj model state indices mockCache implementations test
                    let arr2Obj = encodeArrayCompactly state model (term2obj model state indices mockCache implementations test)
                    let encodeMock = encodeTypeMock model state indices mockCache implementations test
                    obj2test eval arr2Obj indices encodeMock test addr typ
                // If address is not in the 'allocatedTypes', it should not be allocated, so result is 'null'
                | None -> null
            | PrimitiveModel _ -> __unreachable__()
        | {term = HeapRef({term = ConcreteHeapAddress(addr)}, _)} ->
            let term2Obj = model.Eval >> term2obj model state indices mockCache implementations test
            let eval address =
                address |> Ref |> Memory.Read state |> term2Obj
            let arr2Obj = encodeArrayCompactly state model term2Obj
            let typ = state.allocatedTypes[addr]
            let encodeMock = encodeTypeMock model state indices mockCache implementations test
            obj2test eval arr2Obj indices encodeMock test addr typ
        | Combined(terms, t) ->
            let slices = List.map model.Eval terms
            ReinterpretConcretes slices t
        | term -> internalfailf "creating object from term: unexpected term %O" term

    and private encodeTypeMock (model : model) state indices (mockCache : Dictionary<ITypeMock, Mocking.Type>) (implementations : IDictionary<MethodInfo, term[]>) (test : UnitTest) mock : Mocking.Type =
        let mockedType = ref Mocking.Type.Empty
        if mockCache.TryGetValue(mock, mockedType) then mockedType.Value
        else
            let eval = model.Eval >> term2obj model state indices mockCache implementations test
            let freshMock = Mocking.Type(mock.Name)
            mockCache.Add(mock, freshMock)
            for t in mock.SuperTypes do
                freshMock.AddSuperType t
                for methodMock in implementations do
                    let method = methodMock.Key
                    let values = methodMock.Value
                    let methodType = method.ReflectedType
                    let mockedBaseInterface() =
                        t.IsInterface && Seq.contains methodType (TypeUtils.getBaseInterfaces t)
                    if methodType = t || mockedBaseInterface() then
                        freshMock.AddMethod(method, Array.map eval values)
            freshMock

    let encodeExternMock (model : model) state indices (mockCache : Dictionary<ITypeMock, Mocking.Type>) (implementations : IDictionary<MethodInfo, term[]>) (test : UnitTest) (methodMock : IMethodMock) =
        let eval = model.Eval >> term2obj model state indices mockCache implementations test
        let clauses = methodMock.GetImplementationClauses() |> Array.map eval
        let methodRepr = methodRepr.Encode methodMock.BaseMethod
        let isExtern = methodMock.IsExtern
        test.AllocateExternMock methodRepr isExtern clauses

    let private model2test (test : UnitTest) isError indices mockCache (m : Method) model (cilState : cilState) message =
        let state = cilState.state
        let suitableState state =
            let methodHasByRefParameter = m.Parameters |> Seq.exists (fun pi -> pi.ParameterType.IsByRef)
            if m.DeclaringType.IsValueType && not m.IsStatic || methodHasByRefParameter then
                Memory.CallStackSize state = 2
            else Memory.CallStackSize state = 1

        if not <| suitableState state
            then internalfail "Finished state has many frames on stack! (possibly unhandled exception)"

        match model with
        | StateModel modelState ->
            match SolveGenericMethodParameters state.typeStorage m with
            | None -> None
            | Some(classParams, methodParams) ->
                let implementations = Dictionary<MethodInfo, term[]>()
                for entry in state.methodMocks do
                    let mock = entry.Value
                    let values = mock.GetImplementationClauses()
                    implementations.Add(mock.BaseMethod, values)

                let concreteClassParams = Array.zeroCreate classParams.Length
                let mockedClassParams = Array.zeroCreate classParams.Length
                let concreteMethodParams = Array.zeroCreate methodParams.Length
                let mockedMethodParams = Array.zeroCreate methodParams.Length
                let encodeMock = encodeTypeMock model state indices mockCache implementations test
                let processSymbolicType (concreteArr : Type array) (mockArr : Mocking.Type option array) i = function
                    | ConcreteType t -> concreteArr[i] <- t
                    | MockType m -> mockArr[i] <- Some (encodeMock m)
                classParams |> Seq.iteri (processSymbolicType concreteClassParams mockedClassParams)
                methodParams |> Seq.iteri (processSymbolicType concreteMethodParams mockedMethodParams)
                test.SetTypeGenericParameters concreteClassParams mockedClassParams
                test.SetMethodGenericParameters concreteMethodParams mockedMethodParams

                let extMocks = cilState.state.externMocks.Values
                extMocks |> Seq.iter (encodeExternMock cilState.state.model cilState.state indices mockCache implementations test)
                let parametersInfo = m.Parameters
                if state.complete then
                    for pi in parametersInfo do
                        let arg = Memory.ReadArgument state pi
                        let concreteArg = term2obj model state indices mockCache implementations test arg
                        test.AddArg (Array.head parametersInfo) concreteArg
                else
                    for pi in parametersInfo do
                        let value =
                            if pi.ParameterType.IsByRef then
                                let key = ParameterKey pi
                                let stackRef = Memory.ReadLocalVariable state key
                                Memory.Read modelState stackRef
                            else
                                Memory.ReadArgument modelState pi |> model.Complete
                        let concreteValue : obj = term2obj model state indices mockCache implementations test value
                        test.AddArg pi concreteValue

                if m.HasThis then
                    let thisTerm =
                        if m.DeclaringType.IsValueType then
                            let stackRef = Memory.ReadThis state m
                            Memory.Read modelState stackRef
                        else
                            Memory.ReadThis modelState m |> model.Complete
                    let concreteThis = term2obj model state indices mockCache implementations test thisTerm
                    test.ThisArg <- concreteThis

                let hasException, message =
                    match state.exceptionsRegister with
                    | Unhandled(e, _) ->
                        let t = MostConcreteTypeOfHeapRef state e
                        test.Exception <- t
                        let message =
                            if isError && String.IsNullOrEmpty message then
                                let messageReference = Memory.ReadField state e Reflection.exceptionMessageField |> model.Eval
                                let encoded = term2obj model state indices mockCache implementations test messageReference :?> stringRepr
                                encoded.Decode()
                            else message
                        true, message
                    | _ -> false, message
                test.IsError <- isError
                test.ErrorMessage <- message

                if not isError && not hasException then
                    let retVal = model.Eval cilState.Result
                    test.Expected <- term2obj model state indices mockCache implementations test retVal
                Some test
        | _ -> __unreachable__()

    let internal state2test isError (m : Method) (cilState : cilState) message =
        let indices = Dictionary<concreteHeapAddress, int>()
        let mockCache = Dictionary<ITypeMock, Mocking.Type>()
        let test = UnitTest((m :> IMethod).MethodBase)

        model2test test isError indices mockCache m cilState.state.model cilState message
