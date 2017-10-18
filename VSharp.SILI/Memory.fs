namespace VSharp

open VSharp.State
open Types
open Types.Constructor

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let private pointer = Persistent<int>(always 0, id)
    let internal ZeroTime = State.zeroTime
    let private infiniteTime : Timestamp = System.UInt32.MaxValue
    let private timestamp = Persistent<Timestamp>(always ZeroTime, id)
    let internal freshAddress () =
        pointer.Mutate(pointer.Read() + 1)
        pointer.Read()
    let internal tick() =
        timestamp.Mutate(timestamp.Read() + 1u)
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

    type private LazyInstantiation(location : Term, isTopLevelHeapAddress : bool) =
        inherit SymbolicConstantSource()
        override x.SubTerms = Seq.singleton location
        member x.Location = location
        member x.IsTopLevelHeapAddress = isTopLevelHeapAddress

    let internal (|LazyInstantiation|_|) (src : SymbolicConstantSource) =
        match src with
        | :? LazyInstantiation as li -> Some(LazyInstantiation(li.Location, li.IsTopLevelHeapAddress))
        | _ -> None

    type private ArrayElementLazyInstantiation(location : Term, isTopLevelHeapAddress : bool, array : Term, index : Term) =
        inherit LazyInstantiation(location, isTopLevelHeapAddress)

    type private ArrayLengthLazyInstantiation(location : Term, isTopLevelHeapAddress : bool, array : Term, dim : Term) =
        inherit LazyInstantiation(location, isTopLevelHeapAddress)

    type private ArrayLowerBoundLazyInstantiation(location : Term, isTopLevelHeapAddress : bool, array : Term, dim : Term) =
        inherit LazyInstantiation(location, isTopLevelHeapAddress)

    let private isStaticLocation = function
        | StaticRef _ -> true
        | _ -> false

    let rec internal defaultOf time metadata typ =
        match typ with
        | Bool -> MakeFalse metadata
        | Numeric t when t.IsEnum -> CastConcrete (System.Activator.CreateInstance(t)) t metadata
        | Numeric t -> CastConcrete 0 t metadata
        | PointerType t -> Terms.MakeNullRef t metadata ZeroTime
        | String
        | ClassType _
        | ArrayType _ -> Terms.MakeNullRef typ metadata ZeroTime
        | SubType(dotNetType, _, _,  _) when dotNetType.IsValueType -> Struct metadata Heap.empty typ
        | SubType _ -> Terms.MakeNullRef typ metadata ZeroTime
        | Func _ -> Terms.MakeNullRef (FromGlobalSymbolicDotNetType typedefof<System.Delegate>) metadata ZeroTime
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
        let time = tick() in
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
                | :? LazyInstantiation as li -> LazyInstantiation(li.Location, true) :> SymbolicConstantSource
                | _ -> source
            in
            let constant = Constant metadata name source' pointerType in
            HeapRef metadata ((constant, t), []) time
        | t when Types.IsPrimitive t || Types.IsFunction t -> Constant metadata name source t
        | StructType _
        | SubType _
        | String _
        | ClassType _ as t -> Struct metadata Heap.empty t
        | ArrayType(e, d) as t -> Arrays.makeSymbolic metadata source d e t name
        | _ -> __notImplemented__()

    let internal genericLazyInstantiator =
        let instantiator metadata time fullyQualifiedLocation typ () =
            makeSymbolicInstance metadata time (LazyInstantiation(fullyQualifiedLocation, false)) (nameOfLocation fullyQualifiedLocation) typ
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

// ------------------------------- Comparison -------------------------------

    let npe mtd state = State.activator.CreateInstance mtd typeof<System.NullReferenceException> [] state

    let private canPoint mtd pointerAddr pointerType pointerTime locationAddr locationValue locationTime =
        // TODO: what if locationType is Null?
        if locationTime > pointerTime then Terms.MakeFalse mtd
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
            addrEqual &&& typeEqual

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

    let rec private heapDeref metadata time instantiateLazy h ptr ptrType ptrTime =
        let exists = Heap.contains ptr h in
        if IsConcrete ptr && exists then
            [(MakeTrue metadata, ptr, Heap.find ptr h)], h
        else
            let baseGav, restGavs = findSuitableLocations metadata h ptr ptrType ptrTime in
            let baseGuard = restGavs |> List.map (fst3 >> (!!)) |> conjunction metadata in
            let baseAddr, baseValue, h' =
                match baseGav with
                | None ->
                    let lazyValue = instantiateLazy() in
                    let baseCell = lazyValue, time, time in
                    let h' = h.Add(ptr, baseCell) in
                    ptr, baseCell, h'
                | Some(_, a, v) -> a, v, h
            (baseGuard, baseAddr, baseValue)::restGavs, h'

    let private mutateHeap metadata time guard h addr newValue =
        assert(Heap.contains addr h)
        let (oldValue, created, modified) as oldCell = Heap.find addr h in
        let cell = Merging.merge2Cells guard !!guard (newValue, created, time) oldCell in
        Heap.add addr cell h

    let private structLazyInstantiator metadata fullyQualifiedLocation field fieldType () =
        makeSymbolicInstance metadata ZeroTime (LazyInstantiation(fullyQualifiedLocation, false)) (toString field) fieldType

    let private arrayElementLazyInstantiator metadata time location array idx = function
        | DefaultInstantiator concreteType -> fun () -> defaultOf time metadata concreteType
        | LazyInstantiator(constant, concreteType) -> fun () ->
            let id = sprintf "%s[%s]" (toString constant) (toString idx) |> IdGenerator.startingWith in
            makeSymbolicInstance metadata time (ArrayElementLazyInstantiation(location, false, array, idx)) id concreteType

    let private arrayLowerBoundLazyInstantiator metadata time location array idx = function
        | DefaultInstantiator concreteType -> fun () -> defaultOf time metadata Arrays.lengthTermType
        | LazyInstantiator(constant, _) -> fun () ->
            match Options.SymbolicArrayLowerBoundStrategy() with
            | Options.AlwaysZero -> defaultOf time metadata Arrays.lengthTermType
            | Options.AlwaysSymbolic ->
                let id = sprintf "%s.GetLowerBound(%s)" (toString constant) (toString idx) in
                makeSymbolicInstance metadata time (ArrayLowerBoundLazyInstantiation (location, false, array, idx)) id Arrays.lengthTermType

    let private arrayLengthLazyInstantiator metadata time location array idx = function
        | DefaultInstantiator concreteType -> fun () -> MakeNumber 1 metadata
        | LazyInstantiator(constant, _) -> fun () ->
            let id = sprintf "%s.GetLength(%s)" (toString constant) (toString idx) in
            (makeSymbolicInstance metadata time (ArrayLengthLazyInstantiation(location, false, array, idx)) id Arrays.lengthTermType)

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
                    accessHeap metadata guard update fields created (fun loc -> referenceSubLocation (loc, typ) ctx) instantiator key typ ptrTime path'
                in result, Struct term.metadata newFields t, newTime
            | Array(dimension, length, lower, constant, contents, lengths, arrTyp) ->
                let ctx' = referenceSubLocation location ctx in
                let makeInstantiator key instantiator = always <| Merging.guardedMap (fun c -> instantiator term.metadata modified ctx' term key c ()) constant in
                let newHeap heap key instantiator = accessHeap metadata guard update heap created (fun loc -> referenceSubLocation (loc, typ) ctx) instantiator key typ ptrTime path' in
                match key with
                | _ when key.metadata.misc.Contains Arrays.ArrayIndicesType.LowerBounds ->
//                    key.metadata.misc.Remove(Arrays.ArrayIndicesType.LowerBounds) |> ignore
                    let instantiator = makeInstantiator key arrayLowerBoundLazyInstantiator
                    let result, newLower, newTime = newHeap lower key instantiator
                    in result, Array term.metadata dimension length newLower constant contents lengths arrTyp, newTime
                | _ when key.metadata.misc.Contains Arrays.ArrayIndicesType.Lengths ->
//                    key.metadata.misc.Remove(Arrays.ArrayIndicesType.Lengths) |> ignore
                    let instantiator = makeInstantiator key arrayLengthLazyInstantiator
                    let result, newLengths, newTime = newHeap lengths key instantiator
                    in result, Array term.metadata dimension length lower constant contents newLengths arrTyp, newTime
                | _ when key.metadata.misc.Contains Arrays.ArrayIndicesType.Contents ->
//                    key.metadata.misc.Remove(Arrays.ArrayIndicesType.Contents) |> ignore
                    let instantiator = makeInstantiator key arrayElementLazyInstantiator
                    let result, newContents, newTime = newHeap contents key instantiator
                    in result, Array term.metadata dimension length lower constant newContents lengths arrTyp, newTime
                | _ -> __notImplemented__()
            | Union gvs ->
                internalfail "unexpected union of complex types! Probably merge function implemented incorrectly."
            | t ->
                internalfailf "expected complex type, but got %O" t

    and private accessHeap metadata guard update h time mkCtx lazyInstantiator ptr ptrType ptrTime path =
        let gvas, h = heapDeref metadata time lazyInstantiator h ptr ptrType ptrTime in
        let gvs, (h', newTime) = gvas |> ((h, ZeroTime) |> List.mapFold (fun (h, maxTime) (guard', addr, (baseValue, created, modified)) ->
            let ctx = mkCtx addr in
            let guard'' = guard &&& guard' in
            let accessedValue, newBaseValue, newTime = accessTerm metadata guard update created modified ptrTime ctx path baseValue in
            let h' = if baseValue = newBaseValue then h else mutateHeap metadata newTime guard'' h addr newBaseValue
            ((guard, accessedValue), (h', max maxTime newTime))))
        in (Merging.merge gvs, h', newTime)

    let rec private commonHierarchicalAccess actionNull update metadata state term =
        match term.term with
        | Error _ -> (term, state)
        | StackRef(location, path) ->
            let firstLocation = stackLocationToReference state location in
            let time = frameTime state location in
            let (baseValue, created, modified), h' = stackDeref time (fun () -> stackLazyInstantiator state time location |> fst3) state location in
            let accessedValue, newBaseValue, newTime = accessTerm metadata (Terms.MakeTrue metadata) update created modified time firstLocation path baseValue in
            let newState = if baseValue = newBaseValue then state else writeStackLocation state location (newBaseValue, created, newTime) in
            accessedValue, newState
        | StaticRef(location, path) ->
            let firstLocation = Terms.term >> function
                | Concrete(location, String) -> StaticRef term.metadata (location :?> string) []
                | _ -> __notImplemented__()
            in
            let addr = Terms.MakeStringKey location in
            let dnt = System.Type.GetType(location) in
            let t = FromConcreteDotNetType dnt in
            let result, m', _ = accessHeap metadata (MakeTrue metadata) update (staticsOf state) ZeroTime firstLocation (staticMemoryLazyInstantiator Metadata.empty dnt location) addr t infiniteTime path in
            result, withStatics state m'
        | HeapRef(((addr, t) as location, path), time) ->
            let mkFirstLocation location = HeapRef term.metadata ((location, t), []) time in
            let firstLocation = HeapRef term.metadata (location, []) time in
            Common.reduceConditionalExecution state
                (fun state k -> k (Arithmetics.simplifyEqual metadata addr (Concrete metadata [0] pointerType) id, state))
                (fun state k -> k (actionNull metadata state t))
                (fun state k ->
                    let result, h', _ = accessHeap metadata (Terms.MakeTrue metadata) update (heapOf state) ZeroTime mkFirstLocation (genericLazyInstantiator Metadata.empty time firstLocation t) addr t time path in
                    k (result, withHeap state h'))
                Merging.merge Merging.merge2Terms id id
        | Union gvs -> Merging.guardedStateMap (commonHierarchicalAccess actionNull update metadata) gvs state
        | t -> internalfailf "expected reference, but got %O" t

    let private hierarchicalAccess = commonHierarchicalAccess (fun m s _ ->
        let res, state = npe m s
        Error m res, state)

    let internal deref = hierarchicalAccess makePair

    let internal derefWith actionNull = commonHierarchicalAccess actionNull makePair

    let internal mutate metadata state reference value =
        assert(value <> Nop)
        let time = tick() in
        hierarchicalAccess (fun _ _ -> (value, time)) metadata state reference

    let rec private derefPathIfInstantiated term = function
        | [] -> Some term
        | (loc, _)::path' ->
            match term.term with
            | Struct(contents, _)
            | Array(_, _, _, _, contents, _, _) ->
                if Heap.contains loc contents then derefPathIfInstantiated (fst3 contents.[loc]) path' else None
            | _ -> internalfailf "expected complex type, but got %O" term

    let internal derefIfInstantiated state = term >> function
        | StackRef(addr, path) ->
            if isAllocatedOnStack state addr then
                derefPathIfInstantiated (readStackLocation state addr |> fst3) path
            else None
        | HeapRef(((addr, _), path), _) ->
            if isAllocatedInHeap state addr then
                derefPathIfInstantiated (readHeapLocation state addr) path
            else None
        | StaticRef(addr, path) ->
            if staticMembersInitialized state addr then
                derefPathIfInstantiated (readStaticLocation state (MakeStringKey addr)) path
            else None
        | term -> internalfailf "expected reference, but %O got" term

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

    let internal referenceArrayLowerBound metadata arrayRef indices =
        let newIndices = { indices with metadata = Metadata.clone indices.metadata} in
        Metadata.addMisc newIndices Arrays.ArrayIndicesType.LowerBounds
        referenceSubLocation (newIndices, Arrays.lengthTermType) arrayRef

    let internal referenceArrayLength metadata arrayRef indices =
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

    let internal allocateInHeap metadata (s : state) term : Term * state =
        let address = freshHeapLocation metadata in
        let time = tick() in
        let pointer = HeapRef metadata ((address, Terms.TypeOf term), []) time in
        (pointer, { s with heap = s.heap.Add(address, (term, time, time)) } )

    let internal allocateInStaticMemory metadata (s : state) typeName term =
        let time = tick() in
        let address = Terms.MakeConcreteString typeName metadata in
        { s with  statics = s.statics.Add(address, (term, time, time)) }

    let internal allocateSymbolicInstance metadata state t =
        match t with
        | TermType.ClassType(tp, arg, interfaces) ->
            let contents = makeSymbolicInstance metadata ZeroTime (LazyInstantiation(Nop, false)) "" (StructType(tp, arg, interfaces)) in
            allocateInHeap metadata state contents
        | StructType _ ->
            makeSymbolicInstance metadata ZeroTime (LazyInstantiation(Nop, false)) "" t, state
        | _ -> __notImplemented__()

// ------------------------------- Comparison -------------------------------

    type internal StateDiff =
        | Mutation of Term * Term
        | Allocation of Term * Term

    let private compareHeaps h1 h2 =
        assert(Heap.size h1 <= Heap.size h2)
        let oldValues, newValues = Heap.partition (fun (k, _) -> Heap.contains k h1) h2 in
        let changedValues = List.filter (fun (k, v) -> (fst3 h1.[k]) <> v) oldValues in
        changedValues, newValues

    let rec private addrLess addr1 addr2 =
        match addr1.term, addr2.term with
        | Concrete(a1, t1), Concrete(a2, t2) ->
            match t1, t2 with
            | _ when t1 = t2 && t1 = pointerType -> compare (a1 :?> int) (a2 :?> int)
            | String, String -> compare (a1 :?> string) (a2 :?> string)
            | _, String when t1 = pointerType -> -1
            | String, _ when t2 = pointerType -> 1
            | _ -> __notImplemented__()
        | Constant(name1, _, _), Constant(name2, _, _) -> compare name1 name2
        | Concrete _, _ -> -1
        | _, Concrete _ -> 1
        | _ -> __notImplemented__()

    let private comparePaths = List.compareWith (fun (a1, _) (a2, _) -> addrLess a1 a2)

    let rec internal compareRefs ref1 ref2 =
        match ref1, ref2 with
        | _ when ref1 = ref2 -> 0
        | HeapRef(((addr1, _), path1), _), HeapRef(((addr2, _), path2), _) ->
            let h = addrLess addr1 addr2 in
            if h = 0 then comparePaths path1 path2 else h
        | StackRef(name1, path1), StackRef(name2, path2) ->
            let h = compare name1 name2 in
            if h = 0 then comparePaths path1 path2 else h
        | StaticRef(name1, path1), StaticRef(name2, path2) ->
            let h = compare name1 name2 in
            if h = 0 then comparePaths path1 path2 else h
        | StackRef _, _ -> -1
        | _, StackRef _ -> 1
        | HeapRef _, _ -> -1
        | _, HeapRef _ -> 1
        | _ -> internalfail "compareRefs called with non-reference terms"

    let private sortMap mapper = List.sortWith (fun (k1, _) (k2, _) -> addrLess k1 k2) >> List.map mapper

    let rec private isDefaultValue reference cell =
        match (fst3 cell).term with
        | Constant(_, LazyInstantiation(reference', _), _) -> reference = reference'
        | Struct(contents, _)
        | Array(_, _, _, _, contents, _, _) ->
            contents |> Heap.toSeq |> Seq.forall (fun (k, cell) -> isDefaultValue (referenceSubLocation (k, Terms.TypeOf (fst3 cell)) reference) cell)
        | _ -> false

    let rec private isFreshLocation startTime (_, time, _) =
        time > startTime

    let rec private inspectLocation startTime ctx goDeep (fresh, mutated) k cell =
        let reference = ctx (k, cell) in
        if isFreshLocation startTime cell then ((reference, cell)::fresh, mutated)
        elif isDefaultValue reference cell then (fresh, mutated)
        else
            let subFresh, subMutated = goDeep startTime reference cell in
            (List.append subFresh fresh, List.append subMutated mutated)

    let rec private affectedStackLocations startTime ctx s =
        s |> stackFold (inspectLocation startTime ctx affectedSubLocations) ([], [])

    and private affectedHeapLocations startTime ctx h =
        h |> Heap.fold (inspectLocation startTime ctx affectedSubLocations) ([], [])

    and private affectedSubLocations startTime ctx cell =
        match (fst3 cell).term with
        | Struct(contents, _)
        | Array(_, _, _, _, contents, _, _) ->
            affectedHeapLocations startTime (fun (loc, (v, _, _)) -> referenceSubLocation (loc, TypeOf v) ctx) contents
        | _ -> ([], [(ctx, cell)])

    let rec internal affectedLocations startTime (state : state) =
        let freshStack,   mutatedStack    = affectedStackLocations startTime (fst >> stackLocationToReference state) state.stack in
        let freshHeap,    mutatedHeap     = affectedHeapLocations startTime (fun (loc, (v, t, _)) -> HeapRef loc.metadata ((loc, TypeOf v), []) t) state.heap in
        let freshStatics, mutatedStatics  = affectedHeapLocations startTime (fst >> staticLocationToReference) state.statics in
        List.append3 freshStack freshHeap freshStatics, List.append3 mutatedStack mutatedHeap mutatedStatics
