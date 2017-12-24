namespace VSharp

open VSharp.State
open Types
open Types.Constructor
open System.Collections.Immutable

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let private pointer = Persistent<int>(always 0, id)
    let private timestamp = Persistent<Timestamp>(always Timestamp.zero, id)
    let internal freshAddress () =
        pointer.Mutate(pointer.Read() + 1)
        pointer.Read()
    let internal tick() =
        timestamp.Mutate(timestamp.Read() |> Timestamp.inc)
        timestamp.Read()
    let public reset() =
        pointer.Reset()
        timestamp.Reset()
    let public saveConfiguration() =
        pointer.Save()
        timestamp.Save()
    let public restore() =
        pointer.Restore()
        timestamp.Restore()

    type private StaticsInitializedSource(key : Term) =
        inherit SymbolicConstantSource()
        override x.SubTerms = Seq.singleton key
        member x.Key = key

    type private LazyInstantiation(location : Term, isTopLevelHeapAddress : bool, state : State.state) =
        inherit SymbolicConstantSource()
        override x.SubTerms = Seq.singleton location
        member x.Location = location
        member x.IsTopLevelHeapAddress = isTopLevelHeapAddress
        member x.State = state

    let private (|StaticsInitializedSource|_|) (src : SymbolicConstantSource) =
        match src with
        | :? StaticsInitializedSource as si -> Some(si.Key)
        | _ -> None

    let internal (|LazyInstantiation|_|) (src : SymbolicConstantSource) =
        match src with
        | :? LazyInstantiation as li -> Some(li.Location, li.IsTopLevelHeapAddress)
        | _ -> None

    type private ArrayElementLazyInstantiation(location : Term, isTopLevelHeapAddress : bool, state : State.state, array : Term, index : Term) =
        inherit LazyInstantiation(location, isTopLevelHeapAddress, state)

    type private ArrayLengthLazyInstantiation(location : Term, isTopLevelHeapAddress : bool, state : State.state, array : Term, dim : Term) =
        inherit LazyInstantiation(location, isTopLevelHeapAddress, state)

    type private ArrayLowerBoundLazyInstantiation(location : Term, isTopLevelHeapAddress : bool, state : State.state, array : Term, dim : Term) =
        inherit LazyInstantiation(location, isTopLevelHeapAddress, state)

    let private isStaticLocation = function
        | StaticRef _ -> true
        | _ -> false

    let rec internal defaultOf time metadata typ =
        match typ with
        | Bool -> MakeFalse metadata
        | Numeric t when t.IsEnum -> CastConcrete (System.Activator.CreateInstance t) t metadata
        | Numeric t -> CastConcrete 0 t metadata
        | PointerType t -> Terms.MakeNullRef t metadata
        | String
        | ClassType _
        | ArrayType _ -> Terms.MakeNullRef typ metadata
        | SubType(dotNetType, _, _,  _) when dotNetType.IsValueType -> Struct metadata Heap.empty typ
        | SubType _ -> Terms.MakeNullRef typ metadata
        | Func _ -> Terms.MakeNullRef (FromGlobalSymbolicDotNetType typedefof<System.Delegate>) metadata
        | StructType(dotNetType, _, _) ->
            let contents =
                Types.GetFieldsOf dotNetType false
                |> Map.fold (fun acc k v -> Heap.add (Terms.MakeConcreteString k metadata) (defaultOf time metadata v, time, time) acc) Heap.empty
            in
            Struct metadata contents typ
        | _ -> __notImplemented__()

    let internal mkDefault metadata typ =
        defaultOf (tick()) metadata typ

    let internal mkDefaultStatic metadata qualifiedTypeName =
        let t = qualifiedTypeName |> System.Type.GetType |> FromConcreteDotNetType  in
        let time = tick()
        let fields = DecompilerServices.getDefaultFieldValuesOf true false qualifiedTypeName in
        let contents =
            fields
            |> List.fold (fun acc (n, (t, _)) ->
                let key = Terms.MakeConcreteString n metadata in
                let value = mkDefault metadata (FromConcreteMetadataType t) in
                Heap.add key (value, time, time) acc) Heap.empty
        in
        fields, t, Struct metadata contents t

    //let rec internal makeSymbolicStruct metadata time source t dotNetType =
    //    let fields = Types.GetFieldsOf dotNetType false
    //                    |> Map.toSeq
    //                    |> Seq.map (fun (name, typ) ->
    //                                    let key = MakeStringKey name in
    //                                    (key, makeSymbolicInstance metadata time (source key) name typ))
    //                    |> Heap.ofSeq
    //    in Struct fields t metadata

    let rec makeSymbolicInstance metadata time (source : SymbolicConstantSource) name = function
        | PointerType t ->
            let source' =
                match source with
                | :? LazyInstantiation as li -> LazyInstantiation(li.Location, true, State.empty) :> SymbolicConstantSource
                | _ -> source
            in
            let constant = Constant metadata name source' pointerType in
            HeapRef metadata ((constant, t), []) {time=time}
        | t when Types.IsPrimitive t || Types.IsFunction t -> Constant metadata name source t
        | StructType _
        | SubType _
        | String _
        | ClassType _ as t -> Struct metadata Heap.empty t
        | ArrayType(e, d) as t -> Arrays.makeSymbolic metadata source d e t name
        | _ -> __notImplemented__()

    let internal genericLazyInstantiator =
        let instantiator metadata time fullyQualifiedLocation typ () =
            makeSymbolicInstance metadata time (LazyInstantiation(fullyQualifiedLocation, false, State.empty)) (nameOfLocation fullyQualifiedLocation) typ
        in
        State.genericLazyInstantiator <- instantiator
        instantiator

    let rec private referenceSubLocation location term =
        match term.term with
        | Error _ -> term
        | StackRef(var, path) -> StackRef term.metadata var (List.append path [location])
        | StaticRef(var, path) -> StaticRef term.metadata var (List.append path [location])
        | HeapRef((addr, path), t) -> HeapRef term.metadata (addr, List.append path [location]) t
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (referenceSubLocation location) |> List.zip gs |> Union term.metadata
        | _ -> internalfailf "expected reference, but got %O" term

    let private mkStaticKeyGuard mtd key =
        Constant mtd (IdGenerator.startingWith "hasKey#") (StaticsInitializedSource key) Bool

    let rec private staticGuardOfHeap (exploredRecursiveIds : ImmutableHashSet<FunctionIdentifier>) mtd key = function
        | Defined(r, h) ->
            if h.ContainsKey key then Merging.guardOf (fst3 h.[key])
            elif r then False
            else mkStaticKeyGuard mtd key
        | Merged ghs -> Merging.guardedMap (staticGuardOfHeap exploredRecursiveIds mtd key) ghs
        | Composition(s, h) -> staticGuardOfHeap exploredRecursiveIds mtd key s.statics ||| staticGuardOfHeap exploredRecursiveIds mtd (* TODO: state o *)key h
        | RecursiveApplication(f, _, _) when exploredRecursiveIds.Contains f -> False
        | RecursiveApplication(f, _, _) ->
            match Database.queryState f with
            | Some body ->
                staticGuardOfHeap (exploredRecursiveIds.Add f) mtd key body.statics
            | None -> True
        | HigherOrderApplication _ ->
            mkStaticKeyGuard mtd key

    let private staticKeyInitialized mtd key state =
        staticGuardOfHeap ImmutableHashSet<FunctionIdentifier>.Empty mtd key state.statics

    let internal typeNameInitialized mtd typeName state =
        staticKeyInitialized mtd (MakeStringKey typeName) state

// ------------------------------- Comparison -------------------------------

    let npe mtd state = State.activator.CreateInstance mtd typeof<System.NullReferenceException> [] state

    let private canPoint mtd pointerAddr pointerType pointerTime locationAddr locationValue locationTime =
        // TODO: what if locationType is Null?
        if locationTime > pointerTime then MakeFalse mtd
        else
            let addrEqual = Pointers.locationEqual mtd locationAddr pointerAddr in
            let typeSuits v =
                let locationType = TypeOf v in
                Common.is mtd locationType pointerType &&& Common.is mtd pointerType locationType
            in
            let typeEqual =
                match locationValue.term with
                | Union gvs ->
                    gvs |> List.map (fun (g, v) -> (g, typeSuits v)) |> Merging.merge
                | _ -> typeSuits locationValue
            in
            if IsConcrete addrEqual then addrEqual else addrEqual &&& typeEqual

// ------------------------------- Dereferencing/mutation -------------------------------

    let private stackDeref time instantiateLazy state location =
        if isAllocatedOnStack state location then
            (readStackLocation state location, state)
        else
            let lazyInstance = instantiateLazy(), time, time in
            (lazyInstance, writeStackLocation state location lazyInstance)

    let private findSuitableLocations mtd h ptr ptrType ptrTime =
        let filterMapKey (k, ((v, created, modified) as cell)) =
            let guard = canPoint mtd ptr ptrType ptrTime k v created in
            match guard with
            | False -> None
            | _ -> Some(guard, k, cell)
        in
        let gvs = h |> Heap.toSeq |> List.ofSeq |> List.filterMap filterMapKey in
        let baseGvs, restGvs = gvs |> List.partition (fst3 >> IsTrue) in
        assert(List.length baseGvs <= 1)
        List.tryHead baseGvs, restGvs

    let rec private heapDeref restricted metadata time instantiateLazy h ptr ptrType ptrTime =
        let exists = Heap.contains ptr h in
        if IsConcrete ptr && exists then
            [(MakeTrue metadata, ptr, Heap.find ptr h)], h
        else
            let baseGav, restGavs = findSuitableLocations metadata h ptr ptrType ptrTime in
            let baseGuard, baseAddr, baseValue, h' =
                match baseGav with
                | None when restricted ->
                    // TODO: undefined behaviour detected!
                    __notImplemented__()
                | None ->
                    let baseGuard = restGavs |> List.map (fst3 >> (!!)) |> conjunction metadata in
                    let lazyValue = instantiateLazy() in
                    let baseCell = lazyValue, time, time in
                    let h' = h.Add(ptr, baseCell) in
                    baseGuard, ptr, baseCell, h'
                | Some(g, a, v) -> g, a, v, h
            (baseGuard, baseAddr, baseValue)::restGavs, h'

    let private writeHeap time guard h addr newValue =
        assert(Heap.contains addr h)
        let (oldValue, created, modified) as oldCell = Heap.find addr h in
        let cell = Merging.merge2Cells guard !!guard (newValue, created, time) oldCell in
        Heap.add addr cell h

    let private structLazyInstantiator metadata fullyQualifiedLocation field fieldType () =
        makeSymbolicInstance metadata Timestamp.zero (LazyInstantiation(fullyQualifiedLocation, false, State.empty)) (toString field) fieldType

    let private arrayElementLazyInstantiator metadata time location array idx = function
        | DefaultInstantiator concreteType -> fun () -> defaultOf time metadata concreteType
        | LazyInstantiator(constant, concreteType) -> fun () ->
            let id = sprintf "%s[%s]" (toString constant) (toString idx) |> IdGenerator.startingWith in
            makeSymbolicInstance metadata time (ArrayElementLazyInstantiation(location, false, State.empty, array, idx)) id concreteType

    let private arrayLowerBoundLazyInstantiator metadata time location array idx = function
        | DefaultInstantiator concreteType -> fun () -> defaultOf time metadata Arrays.lengthTermType
        | LazyInstantiator(constant, _) -> fun () ->
            match Options.ExplorationMode() with
            | Options.TrustConventions -> defaultOf time metadata Arrays.lengthTermType
            | Options.CompleteExploration ->
                let id = sprintf "%s.GetLowerBound(%s)" (toString constant) (toString idx) in
                makeSymbolicInstance metadata time (ArrayLowerBoundLazyInstantiation (location, false, State.empty, array, idx)) id Arrays.lengthTermType

    let private arrayLengthLazyInstantiator metadata time location array idx = function
        | DefaultInstantiator concreteType -> fun () -> MakeNumber 1 metadata
        | LazyInstantiator(constant, _) -> fun () ->
            let id = sprintf "%s.GetLength(%s)" (toString constant) (toString idx) in
            (makeSymbolicInstance metadata time (ArrayLengthLazyInstantiation(location, false, State.empty, array, idx)) id Arrays.lengthTermType)

    let private staticMemoryLazyInstantiator metadata t location () =
        Struct metadata Heap.empty (FromConcreteDotNetType t)

    let rec private accessTerm metadata guard update created modified ptrTime ctx path term =
        match path with
        | [] ->
            let newTerm, newTime = update term modified in
            newTerm, newTerm, newTime
        | ((key, typ) as location)::path' ->
            match term.term with
            | Error _ -> term, term, modified
            | Struct(fields, t) ->
                let ctx' = referenceSubLocation location ctx in
                let instantiator = structLazyInstantiator term.metadata ctx' key typ in
                let result, newFields, newTime =
                    accessHeap false metadata guard update fields created (fun loc -> referenceSubLocation (loc, typ) ctx) instantiator key typ ptrTime path'
                in result, Struct term.metadata newFields t, newTime
            | Array(dimension, length, lower, constant, contents, lengths, arrTyp) ->
                let ctx' = referenceSubLocation location ctx in
                let makeInstantiator key instantiator = always <| Merging.guardedMap (fun c -> instantiator term.metadata modified ctx' term key c ()) constant in
                let newHeap heap key instantiator = accessHeap false metadata guard update heap created (fun loc -> referenceSubLocation (loc, typ) ctx) instantiator key typ ptrTime path' in
                match key with
                | _ when key.metadata.misc.Contains Arrays.ArrayIndicesType.LowerBounds ->
                    let instantiator = makeInstantiator key arrayLowerBoundLazyInstantiator
                    let result, newLower, newTime = newHeap lower key instantiator
                    in result, Array term.metadata dimension length newLower constant contents lengths arrTyp, newTime
                | _ when key.metadata.misc.Contains Arrays.ArrayIndicesType.Lengths ->
                    let instantiator = makeInstantiator key arrayLengthLazyInstantiator
                    let result, newLengths, newTime = newHeap lengths key instantiator
                    in result, Array term.metadata dimension length lower constant contents newLengths arrTyp, newTime
                | _ when key.metadata.misc.Contains Arrays.ArrayIndicesType.Contents ->
                    let instantiator = makeInstantiator key arrayElementLazyInstantiator
                    let result, newContents, newTime = newHeap contents key instantiator
                    in result, Array term.metadata dimension length lower constant newContents lengths arrTyp, newTime
                | _ -> __notImplemented__()
            | Union gvs ->
                internalfail "unexpected union of complex types! Probably merge function implemented incorrectly."
            | t ->
                internalfailf "expected complex type, but got %O" t

    and private accessHeap restricted metadata guard update h time mkCtx lazyInstantiator ptr ptrType ptrTime path =
        let gvas, h = heapDeref restricted metadata time lazyInstantiator h ptr ptrType ptrTime in
        let gvs, (h', newTime) = gvas |> ((h, Timestamp.zero) |> List.mapFold (fun (h, maxTime) (guard', addr, (baseValue, created, modified)) ->
            let ctx = mkCtx addr in
            let guard'' = guard &&& guard' in
            let accessedValue, newBaseValue, newTime = accessTerm metadata guard update created modified ptrTime ctx path baseValue in
            let h' = if baseValue = newBaseValue then h else writeHeap newTime guard'' h addr newBaseValue
            ((guard, accessedValue), (h', max maxTime newTime))))
        in (Merging.merge gvs, h', newTime)

    let rec private accessGeneralizedHeap defined = function
        | Defined(r, h) ->
            let result, heap, _ = defined r h in
            result, Defined r heap
        | _ -> __notImplemented__()

    let private commonHierarchicalStackAccess update metadata state location path =
        let firstLocation = stackLocationToReference state location in
        let time = frameTime state location in
        let (baseValue, created, modified), h' = stackDeref time (fun () -> stackLazyInstantiator state time location |> fst3) state location in
        let accessedValue, newBaseValue, newTime = accessTerm metadata (Terms.MakeTrue metadata) update created modified time firstLocation path baseValue in
        let newState = if baseValue = newBaseValue then state else writeStackLocation state location (newBaseValue, created, newTime) in
        accessedValue, newState

    let private commonHierarchicalHeapAccess restricted update metadata heap ((addr, t) as location) path time =
        let mkFirstLocation location = HeapRef metadata ((location, t), []) time in
        let firstLocation = HeapRef metadata (location, []) time in
        accessHeap restricted metadata (MakeTrue metadata) update heap Timestamp.zero mkFirstLocation (genericLazyInstantiator Metadata.empty time.time firstLocation t) addr t time.time path

    let private commonHierarchicalStaticsAccess restricted update metadata statics location path =
        let firstLocation = Terms.term >> function
            | Concrete(location, String) -> StaticRef metadata (location :?> string) []
            | _ -> __notImplemented__()
        in
        let addr = Terms.MakeStringKey location in
        let dnt = System.Type.GetType(location) in
        let t = FromConcreteDotNetType dnt in
        accessHeap restricted metadata (MakeTrue metadata) update statics Timestamp.zero firstLocation (staticMemoryLazyInstantiator Metadata.empty dnt location) addr t Timestamp.infinity path in

    let rec private commonHierarchicalAccess actionNull update metadata state term =
        match term.term with
        | Error _ -> (term, state)
        | StackRef(location, path) ->
            commonHierarchicalStackAccess update metadata state location path
        | HeapRef(((addr, t) as location, path), time) ->
            Common.reduceConditionalExecution state
                (fun state k -> k (Arithmetics.simplifyEqual metadata addr (Concrete metadata [0] pointerType) id, state))
                (fun state k -> k (actionNull metadata state t))
                (fun state k ->
                    let result, h' = accessGeneralizedHeap (fun r h -> commonHierarchicalHeapAccess r update metadata h location path time) (heapOf state) in
                    k (result, withHeap state h'))
                Merging.merge Merging.merge2Terms id id
        | StaticRef(location, path) ->
            let result, m' = accessGeneralizedHeap (fun r h -> commonHierarchicalStaticsAccess r update metadata h location path) (staticsOf state) in
            result, withStatics state m'
        | Union gvs -> Merging.guardedStateMap (commonHierarchicalAccess actionNull update metadata) gvs state
        | t -> internalfailf "expected reference, but got %O" t

    let private hierarchicalAccess = commonHierarchicalAccess (fun m s _ ->
        let res, state = npe m s
        Error m res, state)

    let internal deref = hierarchicalAccess makePair

    let internal derefWith actionNull = commonHierarchicalAccess actionNull makePair

    let private mutateStack metadata state location path time value =
        commonHierarchicalStackAccess (fun _ _ -> (value, time)) metadata state location path

    let private mutateHeap restricted metadata h location path time value =
        commonHierarchicalHeapAccess restricted (fun _  _ -> (value, time)) metadata h location path {time=time}

    let private mutateStatics restricted metadata statics location path time value =
        commonHierarchicalStaticsAccess restricted (fun _ _ -> (value, time)) metadata statics location path

    let internal mutate metadata state reference value =
        assert(value <> Nop)
        let time = tick() in
        hierarchicalAccess (fun _ _ -> (value, time)) metadata state reference

// ------------------------------- Referencing -------------------------------

    let rec private referenceTerm state name followHeapRefs term =
        match term.term with
        | Error _
        | StackRef _
        | StaticRef _
        | HeapRef _ when followHeapRefs -> term
        | Union gvs -> Merging.guardedMap (referenceTerm state name followHeapRefs) gvs
        | _ -> StackRef term.metadata name []

    let internal referenceLocalVariable metadata state location followHeapRefs =
        let reference = StackRef metadata location [] in
        let term, state = deref metadata state reference in
        referenceTerm state location followHeapRefs term

    let rec private referenceFieldOf state field parentRef reference =
        match reference with
        | ErrorT _ -> reference, state
        | { term = HeapRef((addr, path), t) } ->
            assert(List.isEmpty path)
            HeapRef reference.metadata (addr, [field]) t, state
        | Null ->
            let term, state = State.activator.CreateInstance reference.metadata typeof<System.NullReferenceException> [] state in
            Error reference.metadata term, state
        | { term = Struct _ } -> referenceSubLocation field parentRef, state
        | UnionT gvs -> Merging.guardedStateMap (fun state term -> referenceFieldOf state field parentRef term) gvs state
        | t -> internalfailf "expected reference or struct, but got %O" t, state

    let rec private followOrReturnReference metadata state reference =
        let term, state = deref metadata state reference in
        match term.term with
        | Error _
        | StackRef _
        | StaticRef _
        | HeapRef _ -> term, state
        | Union gvs when List.forall (fun (_, t) -> IsError t || IsRef t) gvs ->
            Merging.guardedStateMap (followOrReturnReference metadata) gvs state
        | _ -> reference, state

    let internal referenceField metadata state followHeapRefs name typ parentRef =
        let typ = Types.PointerFromReferenceType typ in
        let term, state = deref metadata state parentRef in
        let reference, newState = referenceFieldOf state (Terms.MakeStringKey name, typ) parentRef term in
        if followHeapRefs then followOrReturnReference metadata newState reference
        else (reference, newState)

    let internal referenceStaticField metadata state followHeapRefs fieldName typ typeName =
        let typ = Types.PointerFromReferenceType typ in
        let reference = StaticRef metadata typeName [(Terms.MakeStringKey fieldName, typ)] in
        if followHeapRefs then followOrReturnReference metadata state reference
        else (reference, state)

    let internal referenceArrayLowerBound metadata arrayRef (indices : Term) =
        let newIndices = { indices with metadata = Metadata.clone indices.metadata} in
        Metadata.addMisc newIndices Arrays.ArrayIndicesType.LowerBounds
        referenceSubLocation (newIndices, Arrays.lengthTermType) arrayRef

    let internal referenceArrayLength metadata arrayRef (indices : Term) =
        let newIndices = { indices with metadata = Metadata.clone indices.metadata} in
        Metadata.addMisc newIndices Arrays.ArrayIndicesType.Lengths
        referenceSubLocation (newIndices, Arrays.lengthTermType) arrayRef

    let internal checkIndices mtd state arrayRef dimension (indices : Term list) k =
        let intToTerm i = MakeNumber i mtd in
        let idOfDimensionsForLowerBounds = Seq.init indices.Length (intToTerm >> referenceArrayLowerBound mtd arrayRef) in
        let idOfDimensionsForLengths = Seq.init indices.Length (intToTerm >> referenceArrayLength mtd arrayRef) in
        Cps.Seq.mapFold (deref mtd) state idOfDimensionsForLowerBounds (fun (lowerBoundsList, state') ->
        Cps.Seq.mapFold (deref mtd) state' idOfDimensionsForLengths (fun (lengthsList, state'') ->
        let bounds =
            Seq.map3
                (fun idx low len ->
                    let up = add mtd low len in
                    Arithmetics.simplifyGreaterOrEqual mtd idx low (fun bigEnough ->
                    Arithmetics.simplifyLess mtd idx up (fun smallEnough ->
                    bigEnough &&& smallEnough)))
                indices lowerBoundsList lengthsList
            |> List.ofSeq
        in k (conjunction mtd bounds |> Merging.unguard |> Merging.merge , state'')))

    let internal referenceArrayIndex metadata state arrayRef (indices : Term list) =
        let array, state = deref metadata state arrayRef in
        // TODO: what about followHeapRefs?
        let rec reference state term =
            match term.term with
            | Error _ -> (term, state)
            | Array(dimension, _, _, _, _, _, ArrayType(elementType, _)) ->
                Common.reduceConditionalExecution state
                    (fun state k -> checkIndices metadata state arrayRef dimension indices k)
                    (fun state k ->
                        let location = Arrays.makeIntegerArray metadata (fun i -> indices.[i]) indices.Length in
                        let newLocation = { location with metadata = Metadata.clone location.metadata}
                        Metadata.addMisc newLocation Arrays.ArrayIndicesType.Contents
                        k (referenceSubLocation (newLocation, elementType) arrayRef, state))
                    (fun state k ->
                        let exn, state = State.activator.CreateInstance metadata typeof<System.IndexOutOfRangeException> [] state
                        in k (Error metadata exn, state))
                    Merging.merge Merging.merge2Terms id id
            | Union gvs -> Merging.guardedStateMap reference gvs state
            | t -> internalfail ("accessing index of non-array term " + toString t)
        in
        reference state array

    let internal derefLocalVariable metadata state id =
        referenceLocalVariable metadata state id false |> deref metadata state

// ------------------------------- Allocation -------------------------------

    let internal newStackFrame state metadata funcId frame = State.newStackFrame (tick()) metadata state funcId frame
    let internal newScope state frame = State.newScope (tick()) state frame

    let internal freshHeapLocation metadata =
        Concrete metadata ([freshAddress()]) pointerType

    let internal allocateOnStack metadata (s : state) key term : state =
        let time = tick() in
        let { func = frameMetadata; entries = oldFrame; time = frameTime } = Stack.peek s.frames.f in
        let newStack = pushToCurrentStackFrame s key (term, time, time) in
        let newEntries = { key = key; mtd = metadata; typ = None } in
        let stackFrames = Stack.updateHead s.frames.f { func = frameMetadata; entries = newEntries :: oldFrame; time = frameTime } in
        { s with stack = newStack; frames = { s.frames with f = stackFrames } }

    let private allocateInGeneralizedHeap address term time = function
        | Defined(r, h) -> h.Add(address, (term, time, time)) |> Defined r
        | _ -> __notImplemented__()

    let internal allocateInHeap metadata (s : state) term : Term * state =
        let address = freshHeapLocation metadata in
        let time = tick() in
        let pointer = HeapRef metadata ((address, Terms.TypeOf term), []) {time=time} in
        (pointer, { s with heap = allocateInGeneralizedHeap address term time s.heap } )

    let internal allocateInStaticMemory metadata (s : state) typeName term =
        let time = tick()
        let address = Terms.MakeConcreteString typeName metadata in
        { s with  statics = allocateInGeneralizedHeap address term time s.statics }

    let internal allocateSymbolicInstance metadata state t =
        match t with
        | TermType.ClassType(tp, arg, interfaces) ->
            let contents = makeSymbolicInstance metadata Timestamp.zero (LazyInstantiation(Nop, false, State.empty)) "" (StructType(tp, arg, interfaces)) in
            allocateInHeap metadata state contents
        | StructType _ ->
            makeSymbolicInstance metadata Timestamp.zero (LazyInstantiation(Nop, false, State.empty)) "" t, state
        | _ -> __notImplemented__()

// ------------------------------- Traversal -------------------------------

    let rec private foldHeapLocationsRec folder acc loc path heap =
        Heap.fold (fun acc subloc cell -> foldSubLocations folder acc loc (List.append path [(subloc, cell |> fst3 |> TypeOf)]) cell) acc heap

    and private foldSubLocations folder acc loc path cell =
        match (fst3 cell).term with
        | Struct(contents, _)
        | Array(_, _, _, _, contents, _, _) ->
            foldHeapLocationsRec folder acc loc path contents
        | _ -> folder acc loc path cell

    and private foldHeapLocations folder acc heap =
        Heap.fold (fun acc loc cell -> foldSubLocations folder acc (loc, TypeOf (fst3 cell)) [] cell) acc heap

    and private foldStackLocations folder acc stack =
        stackFold (fun acc loc cell -> foldSubLocations folder acc loc [] cell) acc stack

//// ------------------------------- Comparison -------------------------------
//
//    type internal StateDiff =
//        | Mutation of Term * Term
//        | Allocation of Term * Term
//
//    let private compareHeaps h1 h2 =
//        assert(Heap.size h1 <= Heap.size h2)
//        let oldValues, newValues = Heap.partition (fun (k, _) -> Heap.contains k h1) h2 in
//        let changedValues = List.filter (fun (k, v) -> (fst3 h1.[k]) <> v) oldValues in
//        changedValues, newValues
//
//    let rec private addrLess addr1 addr2 =
//        match addr1.term, addr2.term with
//        | Concrete(a1, t1), Concrete(a2, t2) ->
//            match t1, t2 with
//            | _ when t1 = t2 && t1 = pointerType -> compare (a1 :?> int) (a2 :?> int)
//            | String, String -> compare (a1 :?> string) (a2 :?> string)
//            | _, String when t1 = pointerType -> -1
//            | String, _ when t2 = pointerType -> 1
//            | _ -> __notImplemented__()
//        | Constant(name1, _, _), Constant(name2, _, _) -> compare name1 name2
//        | Concrete _, _ -> -1
//        | _, Concrete _ -> 1
//        | _ -> __notImplemented__()
//
//    let private sortMap mapper = List.sortWith (fun (k1, _) (k2, _) -> addrLess k1 k2) >> List.map mapper
//
//    let rec private isDefaultValue srcFilter startTime reference (term, c, time) =
//        time <= startTime ||
//        match term.term with
//        | Constant(_, src, _) -> srcFilter reference src
//        | Struct(contents, _)
//        | Array(_, _, _, _, contents, _, _) ->
//            contents |> Heap.toSeq |> Seq.forall (fun (k, cell) -> isDefaultValue srcFilter startTime (referenceSubLocation (k, Terms.TypeOf (fst3 cell)) reference) cell)
//        | Union gvs ->
//            gvs |> List.forall (fun (_, v) -> isDefaultValue srcFilter startTime reference (v, c, time))
//        | _ -> false
//
//    let rec private isFreshLocation startTime (_, time, _) =
//        time > startTime
//
//    let private inspectLocation srcFilter startTime (fresh, mutated) k cell =
//        if isFreshLocation startTime cell then ((k, cell)::fresh, mutated)
//        elif isDefaultValue srcFilter startTime k cell then (fresh, mutated)
//        else (fresh, (k, cell)::mutated)
//
//    let internal affectedLocations srcFilter startTime (state : state) =
//        fold (inspectLocation srcFilter startTime) ([], []) state

// ------------------------------- Composition -------------------------------

    let private composeAddresses (a1 : ConcreteHeapAddress) (a2 : ConcreteHeapAddress) =
        List.append a1 a2

    let private composeTime (t1 : Timestamp) (t2 : Timestamp) =
        // TODO
        t1

    let rec private fillHole mtd addr time state term =
        match term.term with
        | Constant(_, source, _) ->
            match source with
            | LazyInstantiation(loc, isTop) ->
                let result, state = deref mtd state loc in
                if isTop then Pointers.topLevelLocation result else result
            | StaticsInitializedSource key ->
                staticKeyInitialized term.metadata key state
            | _ -> term
        | Concrete(:? ConcreteHeapAddress as addr', t) ->
            Concrete mtd (composeAddresses addr addr') t
        | _ -> term

    and internal fillHoles mtd addr time state term =
        Common.substitute (fillHole mtd addr time state) term

    let private fillAndMutateStack mtd addrPrefix timePrefix source target loc path cell =
        let time = composeTime timePrefix (thd3 cell) in
        let path = path |> List.map (fun (x, t) -> (fillHoles mtd addrPrefix timePrefix source x, t)) in
        let v = fillHoles mtd addrPrefix timePrefix source (fst3 cell) in
        mutateStack mtd target loc path time v |> snd

    let private fillAndMutateHeap mtd addrPrefix timePrefix restricted source target (addr, t) path cell =
        let time = composeTime timePrefix (thd3 cell) in
        let addr = fillHoles mtd addrPrefix timePrefix source addr in
        let loc = (addr, t) in
        let path = path |> List.map (fun (x, t) -> (fillHoles mtd addrPrefix timePrefix source x, t)) in
        let v = fillHoles mtd addrPrefix timePrefix source (fst3 cell) in
        mutateHeap restricted mtd target loc path time v |> snd3

    let private fillAndMutateStatics mtd addrPrefix timePrefix restricted source target (addr, _) path cell =
        let time = composeTime timePrefix (thd3 cell) in
        let addr = fillHoles mtd addrPrefix timePrefix source addr in
        let path = path |> List.map (fun (x, t) -> (fillHoles mtd addrPrefix timePrefix source x, t)) in
        let v = fillHoles mtd addrPrefix timePrefix source (fst3 cell) in
        let loc =
            match addr.term with
            | Concrete(s, String) -> string s
            | _ -> __notImplemented__()
        in
        mutateStatics restricted mtd target loc path time v |> snd3

    let private composeDefinedHeaps writer restricted s h h' =
        foldHeapLocations (writer restricted s) h h' |> Defined restricted

    let rec private composeHeaps writer mtd addr time getter setter s h' =
        match getter s, h' with
        | Defined(r, h), Defined(r', h') ->
            assert(not r')
            composeDefinedHeaps writer r s h h'
        | Merged ghs, _ ->
            let gs, hs = List.unzip ghs in
            hs |> List.map (fun h -> composeHeaps writer mtd addr time getter setter (setter s h) h') |> Merging.mergeGeneralizedHeaps gs
        | _, Merged ghs' ->
            let gs, hs' = List.unzip ghs' in
            let gs' = List.map (fillHoles mtd addr time s) gs in
            hs' |> List.map (composeHeaps writer mtd addr time getter setter s) |> Merging.mergeGeneralizedHeaps gs
        | Defined _, Composition(s', h'') ->
            let s = composeStates mtd addr time s s' in
            composeHeaps writer mtd addr time getter setter s h'
        | Defined _, HigherOrderApplication _
        | Defined _, RecursiveApplication _
        | Composition _, HigherOrderApplication _
        | Composition _, RecursiveApplication _ ->
            Composition(s, h')
        | Composition(s', h), Defined(r'', h'') ->
            assert(not r'')
            match h with
            | Defined(r, h) ->
                let h = composeDefinedHeaps writer r s h h''
                composeHeaps writer mtd addr time getter setter s' h
            | _ -> Composition(s, h')
        | HigherOrderApplication(f, a, t), Defined(r', h') -> __notImplemented__()
        | HigherOrderApplication(f, a, t), HigherOrderApplication(f', a', t') -> __notImplemented__()
        | HigherOrderApplication(f, a, t), RecursiveApplication(f', a', t') -> __notImplemented__()
        | HigherOrderApplication(f, a, t), Composition(h', h'') -> __notImplemented__()
        | RecursiveApplication(f, a, t), Defined(r', h') -> __notImplemented__()
        | RecursiveApplication(f, a, t), HigherOrderApplication(f', a', t') -> __notImplemented__()
        | RecursiveApplication(f, a, t), RecursiveApplication(f', a', t') -> __notImplemented__()
        | RecursiveApplication(f, a, t), Composition(h', h'') -> __notImplemented__()
        | Composition(h1, h2), Composition(h3, h4) -> __notImplemented__()

    and composeStates mtd addr time state state' =
        let stack = (foldStackLocations (fillAndMutateStack mtd addr time state) state state'.stack).stack in
        let heap = composeHeaps (fillAndMutateHeap mtd addr time) mtd addr time heapOf (fun s h -> {s with heap = h}) state state'.heap in
        let statics = composeHeaps (fillAndMutateStatics mtd addr time) mtd addr time staticsOf (fun s h -> {s with statics = h}) state state'.statics in
        { stack = stack; heap = heap; statics = statics; frames = state.frames; pc = state.pc }
