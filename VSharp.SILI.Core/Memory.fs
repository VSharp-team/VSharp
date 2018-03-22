namespace VSharp.Core

open VSharp
open VSharp.Core.State
open VSharp.Core.Types
open VSharp.Core.Types.Constructor
open System.Collections.Immutable

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let private pointer = persistent<int>(always 0, id)
    let private timestamp = persistent<timestamp>(always Timestamp.zero, id)
    let freshAddress() =
        pointer.Mutate(pointer.Value + 1)
        pointer.Value
    let tick() =
        timestamp.Mutate(timestamp.Value |> Timestamp.inc)
        timestamp.Value
    let reset() =
        pointer.Reset()
        timestamp.Reset()
    let saveConfiguration() =
        pointer.Save()
        timestamp.Save()
    let restore() =
        pointer.Restore()
        timestamp.Restore()

    let npe mtd state = State.createInstance mtd typeof<System.NullReferenceException> [] state

    let private npeTerm mtd state _ =
        let exn, state = npe mtd state
        Error mtd exn, state

// ------------------------------- Traversal -------------------------------

    let rec private foldHeapLocationsRec arrayTarget folder acc loc path heap =
        Heap.fold (fun acc subloc cell -> foldSubLocations arrayTarget folder acc loc (List.append path [(subloc, typeOf cell.value)]) cell) acc heap

    and private foldSubLocations arrayTarget folder acc loc path cell =
        match cell.value.term with
        | Struct(contents, _) ->
            foldHeapLocationsRec arrayTarget folder acc loc path contents
        | Array(_, _, lower, _, contents, lengths, _) ->
            let acc = foldHeapLocationsRec ArrayLowerBounds folder acc loc path lower
            let acc = foldHeapLocationsRec ArrayLengths folder acc loc path lengths
            foldHeapLocationsRec ArrayContents folder acc loc path contents
        | _ -> folder acc loc path arrayTarget cell

    and private foldHeapLocations folder acc heap =
        Heap.fold (fun acc loc cell -> foldSubLocations ArrayContents folder acc (loc, typeOf cell.value) [] cell) acc heap

    and private foldStackLocations folder acc stack =
        stackFold (fun acc loc cell -> foldSubLocations ArrayContents folder acc loc [] cell) acc stack

// ------------------------------- Lazy Instantiation -------------------------------

    type private lazyInstantiation =
        {location : term; isTopLevelHeapAddress : bool; heap : generalizedHeap option}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.singleton x.location

    let private (|LazyInstantiation|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? lazyInstantiation as li -> Some(li.location, li.isTopLevelHeapAddress, li.heap)
        | _ -> None

    let private isStaticLocation = function
        | StaticRef _ -> true
        | _ -> false

    let private mkStruct metadata time isStatic mkField dotNetType typ =
        let fields = Types.fieldsOf dotNetType isStatic
        let contents =
            fields
            |> FSharp.Collections.Array.fold (fun acc (name, typ) ->
                let key = Terms.makeConcreteString name metadata
                let value = mkField metadata name typ
                Heap.add key { value = value; created = time; modified = time } acc) Heap.empty
        Struct metadata contents typ

    let rec private defaultOf time metadata typ =
        match typ with
        | Bool -> makeFalse metadata
        | Numeric t when t.IsEnum -> CastConcrete (System.Activator.CreateInstance t) t metadata
        | Numeric t -> CastConcrete 0 t metadata
        | Reference t -> Terms.makeNullRef t metadata
        | ClassType _
        | ArrayType _
        | InterfaceType _ -> Terms.makeNullRef typ metadata
        | TypeVariable _ ->
            Common.statelessConditionalExecution
                (fun k -> k <| Common.isValueType metadata typ)
                (fun k -> k <| Struct metadata Heap.empty typ)
                (fun k -> k <| Terms.makeNullRef typ metadata)
                Merging.merge Merging.merge2Terms id
        | Func _ -> Terms.makeNullRef (fromDotNetType typedefof<System.Delegate>) metadata
        | StructType(dotNetType, _, _) ->
            mkStruct metadata time false (fun m _ t -> defaultOf time m t) dotNetType typ
        | Pointer typ -> Terms.makeNullPtr typ metadata
        | _ -> __notImplemented__()

    let mkDefault metadata typ =
        defaultOf (tick()) metadata typ

    let mkDefaultStruct metadata isStatic qualifiedTypeName =
        let dnt = System.Type.GetType qualifiedTypeName
        let time = tick()
        mkStruct metadata time isStatic (fun m _ t -> defaultOf time m t) dnt (fromDotNetType dnt)

    let private makeSymbolicHeapReference metadata time (source : ISymbolicConstantSource) name typ construct =
        let source' =
            match source with
            | :? lazyInstantiation as li -> {location = li.location; isTopLevelHeapAddress = true; heap = None} :> ISymbolicConstantSource
            | _ -> source
        let constant = Constant metadata name source' pointerType
        construct metadata ((constant, typ), []) ArrayContents {v=time}

    let internal makeSymbolicInstance metadata time source name = function
        | Pointer typ -> makeSymbolicHeapReference metadata time source name typ <| fun mtd path _ time -> HeapPtr mtd path time typ
        | Reference typ -> makeSymbolicHeapReference metadata time source name typ HeapRef
        | t when Types.isPrimitive t || Types.isFunction t -> Constant metadata name source t
        | StructType _ // TODO: initialize all fields of struct symbolicly (via mkStruct). Warning: `source` should be updated!
        | TypeVariable _
        | ClassType _ as t -> Struct metadata Heap.empty t
        | ArrayType(e, d) as t -> Arrays.makeSymbolic metadata source d e t name
        | _ -> __notImplemented__()


    let private genericLazyInstantiator =
        let instantiator metadata heap time fullyQualifiedLocation typ () =
            makeSymbolicInstance metadata time {location = fullyQualifiedLocation; isTopLevelHeapAddress = false; heap = heap} (nameOfLocation fullyQualifiedLocation) typ
        State.genericLazyInstantiator <- instantiator
        instantiator

    let instantiateArrayLowerBound metadata time name location heap =
        match Options.ExplorationMode() with
        | TrustConventions -> defaultOf time metadata Arrays.lengthTermType
        | CompleteExploration ->
            makeSymbolicInstance metadata time {location = location; isTopLevelHeapAddress = false; heap = heap} name Arrays.lengthTermType

    let instantiateArrayLength metadata time name location heap =
        makeSymbolicInstance metadata time {location = location; isTopLevelHeapAddress = false; heap = heap} name Arrays.lengthTermType

    let private arrayElementLazyInstantiator metadata instantiator typ heap time location idx = function
        | DefaultInstantiator(_, concreteType) -> fun () -> defaultOf time metadata (typ |?? concreteType)
        | LazyInstantiator(array, concreteType) -> instantiator |?? fun () ->
            let id = sprintf "%s[%s]" (toString array) (idx.term.IndicesToString()) |> IdGenerator.startingWith
            makeSymbolicInstance metadata time {location = location; isTopLevelHeapAddress = false; heap = heap} id (Types.Variable.fromTermType concreteType)

    let private arrayLowerBoundLazyInstantiator metadata instantiator _ heap time location (idx : term) = function
        | DefaultInstantiator(_, _) -> fun () -> defaultOf time metadata Arrays.lengthTermType
        | LazyInstantiator(array, _) -> instantiator |?? fun () ->
            let name = sprintf "%O.%s_LowerBound" array (idx.term.IndicesToString())
            instantiateArrayLowerBound metadata time name location heap

    let private arrayLengthLazyInstantiator metadata instantiator _ heap time location (idx : term) = function
        | DefaultInstantiator(_, _) -> fun () ->
            // In case when array was allocated during the interpretation (and thus has default instantiator) lengths by
            // all dimensions are known (they can be symbolic, but still defined). If this code triggers then we have
            // requested length by a greater dimension than our array has. That can happen in case of comparison of array
            // lengths when arrays have different ranks. In that case we consider lengths in all other dimensions equal to 1.
            makeNumber 1 metadata
        | LazyInstantiator(array, _) -> instantiator |?? fun () ->
            let name = sprintf "%O.%s_Length" array (idx.term.IndicesToString())
            instantiateArrayLength metadata time name location heap

    let private staticMemoryLazyInstantiator metadata t () =
        Struct metadata Heap.empty (fromDotNetType t)

    let private selectLazyInstantiator metadata heap time arrayTarget fullyQualifiedLocation typ =
        match arrayTarget with
        | ArrayLengths -> fun () -> instantiateArrayLength metadata time (nameOfLocation fullyQualifiedLocation + "_Length") fullyQualifiedLocation heap
        | ArrayLowerBounds -> fun () -> instantiateArrayLowerBound metadata time (nameOfLocation fullyQualifiedLocation + "_LowerBound") fullyQualifiedLocation heap
        | _ -> genericLazyInstantiator metadata heap time fullyQualifiedLocation typ

// ------------------------------- Locations comparison -------------------------------

    type private pointerInfo = { location : term; fullyQualifiedLocation : term; typ : termType; time : timestamp; path : (term * termType) list; isTopLevel : bool; arrayTarget : arrayReferenceTarget }

    let private canPoint mtd pointerAddr pointerType pointerTime locationAddr locationValue locationTime =
        // TODO: what if locationType is Null?
        if locationTime > pointerTime then makeFalse mtd
        else
            let addrEqual = Pointers.locationEqual mtd locationAddr pointerAddr
            let typeSuits v =
                let locationType = typeOf v
                Common.is mtd locationType pointerType &&& Common.is mtd pointerType locationType
            let typeEqual =
                match locationValue.term with
                | Union gvs ->
                    gvs |> List.map (fun (g, v) -> (g, typeSuits v)) |> Merging.merge
                | _ -> typeSuits locationValue
            if isConcrete addrEqual then addrEqual else addrEqual &&& typeEqual

    let private findSuitableLocations mtd h keyMapper valueMapper (ptr : pointerInfo) =
        let filterMapKey (k, cell) =
            let k = keyMapper k
            let cell = {cell with value = valueMapper cell.value}
            let guard = canPoint mtd ptr.location ptr.typ ptr.time k cell.value cell.created
            match guard with
            | False -> None
            | _ -> Some(guard, k, cell)
        let gvs = h |> Heap.toSeq |> List.ofSeq |> List.choose filterMapKey
        let baseGvs, restGvs = gvs |> List.partition (fst3 >> isTrue)
        assert(List.length baseGvs <= 1)
        List.tryHead baseGvs, restGvs

// ------------------------------- Primitive read/write -------------------------------

    let private stackDeref time instantiateLazy state location =
        if isAllocatedOnStack state location then
            (readStackLocation state location, state)
        else
            let lazyInstance = {value = instantiateLazy(); created = time; modified = time }
            (lazyInstance, writeStackLocation state location lazyInstance)

    let private writeHeap time guard h addr newValue =
        assert(Heap.contains addr h)
        let oldCell = Heap.find addr h
        let cell = Merging.merge2Cells guard !!guard { oldCell with value = newValue; modified = time } oldCell
        Heap.add addr cell h

    let rec private referenceSubLocation location arrayTarget term =
        match term.term with
        | Error _ -> term
        | StackRef(var, path, None) -> StackRef term.metadata var (List.append path [location])
        | StaticRef(var, path, None) -> StaticRef term.metadata var (List.append path [location])
        | HeapRef((addr, path), t, _, None) -> HeapRef term.metadata (addr, List.append path [location]) arrayTarget t
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (referenceSubLocation location arrayTarget) |> List.zip gs |> Union term.metadata
        | _ -> internalfailf "expected reference, but got %O" term

// ------------------------------- Core -------------------------------

    let rec private accessTerm read metadata groundHeap guard update keyMapper valueMapper lazyInstantiator created modified ptrTime ptrFql path arrayMode term =
        match path with
        | [] ->
            let newTerm, newTime = update term modified
            newTerm, newTerm, newTime
        | ((key, typ) as location)::path' ->
            match term.term with
            | Error _ -> term, term, modified
            | Struct(fields, t) ->
                let fql' = referenceSubLocation location arrayMode ptrFql
                let ptr' = {location = key; fullyQualifiedLocation = fql'; typ = typ; time = ptrTime; path = path'; isTopLevel = false; arrayTarget = arrayMode}
                let instantiator = if read then lazyInstantiator else genericLazyInstantiator term.metadata groundHeap modified fql' typ
                let result, newFields, newTime =
                    accessHeap read false metadata groundHeap guard update fields created keyMapper valueMapper instantiator ptr'
                result, Struct term.metadata newFields t, newTime
            | Array(dimension, length, lower, constant, contents, lengths, arrTyp) ->
                let fql' = referenceSubLocation location arrayMode ptrFql
                let ptr' = {location = key; fullyQualifiedLocation = fql'; typ = typ; time = ptrTime; path = path'; isTopLevel = false; arrayTarget = arrayMode}
                let makeInstantiator key instantiator =
                    let realInstantiator = if read then Some lazyInstantiator else None
                    let targetType = if read then Some(path |> List.last |> snd) else None
                    let doJob = lazy(Merging.guardedMap (fun c -> instantiator term.metadata realInstantiator targetType groundHeap modified fql' key c ()) constant)
                    doJob.Force
                let newHeap heap instantiator = accessHeap read false metadata groundHeap guard update heap created keyMapper valueMapper instantiator ptr'
                match arrayMode with
                | _ when arrayMode = ArrayContents || not (List.isEmpty path') ->
                    let instantiator = makeInstantiator key arrayElementLazyInstantiator
                    let result, newContents, newTime = newHeap contents instantiator
                    result, Array term.metadata dimension length lower constant newContents lengths arrTyp, newTime
                | ArrayLengths ->
                    let instantiator = makeInstantiator key arrayLengthLazyInstantiator
                    let result, newLengths, newTime = newHeap lengths instantiator
                    result, Array term.metadata dimension length lower constant contents newLengths arrTyp, newTime
                | ArrayLowerBounds ->
                    let instantiator = makeInstantiator key arrayLowerBoundLazyInstantiator
                    let result, newLower, newTime = newHeap lower instantiator
                    result, Array term.metadata dimension length newLower constant contents lengths arrTyp, newTime
                | _ -> __unreachable__()
            | Union _ ->
                internalfail "unexpected union of complex types! Probably merge function implemented incorrectly."
            | t ->
                internalfailf "expected complex type, but got %O" t

    and private accessHeap read restricted metadata groundHeap guard update h time keyMapper valueMapper lazyInstantiator (ptr : pointerInfo) =
        let accessRec gvas lazyValue h =
            let gvs, (h', newTime) = gvas |> ((h, Timestamp.zero) |> List.mapFold (fun (h, maxTime) (guard', addr, cell) ->
                let guard'' = guard &&& guard'
                let accessedValue, newBaseValue, newTime = accessTerm read metadata groundHeap guard update keyMapper valueMapper lazyInstantiator cell.created cell.modified ptr.time ptr.fullyQualifiedLocation ptr.path ptr.arrayTarget cell.value
                let h' = if read || cell.value = newBaseValue then h else writeHeap newTime guard'' h addr newBaseValue
                ((guard, accessedValue), (h', max maxTime newTime))))
            (Merging.merge (optCons gvs lazyValue), h', newTime)
        if isConcrete ptr.location && Heap.contains ptr.location h then
            accessRec [(makeTrue metadata, ptr.location, Heap.find ptr.location h)] None h
        else
            let baseGav, restGavs = findSuitableLocations metadata h keyMapper valueMapper ptr
            match baseGav with
            | None when read && restricted ->
                // TODO: undefined behaviour detected!
                __notImplemented__()
            | None ->
                let baseGuard = restGavs |> List.map (fst3 >> (!!)) |> conjunction metadata
                let shouldLazyInstantiate = read && not ptr.isTopLevel
                let lazyValue =
                    if ptr.isTopLevel then genericLazyInstantiator metadata groundHeap time ptr.fullyQualifiedLocation ptr.typ ()
                    else lazyInstantiator()
                let baseCell = { value = lazyValue; created = time; modified = time}
                let gavs = if shouldLazyInstantiate then restGavs else (baseGuard, ptr.location, baseCell)::restGavs
                let lv = if shouldLazyInstantiate then Some(baseGuard, lazyValue) else None
                let h = if shouldLazyInstantiate then h else h.Add(ptr.location, baseCell)
                accessRec gavs lv h
            | Some(g, a, v) -> accessRec ((g, a, v)::restGavs) None h

    let private commonHierarchicalStackAccess read update metadata state location path =
        let firstLocation = stackLocationToReference state location
        let time = frameTime state location
        let cell, _ = stackDeref time (fun () -> (stackLazyInstantiator state time location).value) state location
        let termLazyInstantiator = if read && not (List.isEmpty path) then genericLazyInstantiator metadata None time (StackRef metadata location path) (path |> List.last |> snd) else __unreachable__
        let accessedValue, newBaseValue, newTime = accessTerm read metadata None (makeTrue metadata) update id id termLazyInstantiator cell.created cell.modified time firstLocation path ArrayContents cell.value
        let newState = if read || cell.value = newBaseValue then state else writeStackLocation state location { cell with value = newBaseValue; modified = newTime }
        accessedValue, newState

    let private commonHierarchicalHeapAccess read restricted update metadata groundHeap heap keyMapper valueMapper lazyInstantiator ((addr, t) as location) path time arrayMode =
        let firstLocation = HeapRef metadata (location, []) arrayMode time
        let lazyInstantiator = lazyInstantiator |?? selectLazyInstantiator metadata groundHeap time.v arrayMode (HeapRef metadata (location, path) arrayMode time) (if path.IsEmpty then t else path |> List.last |> snd)
        let ptr = {location = addr; fullyQualifiedLocation = firstLocation; typ = t; time = time.v; path = path; isTopLevel = true; arrayTarget = arrayMode}
        accessHeap read restricted metadata groundHeap (makeTrue metadata) update heap Timestamp.zero keyMapper valueMapper lazyInstantiator ptr

    let private commonHierarchicalStaticsAccess read restricted update metadata groundHeap statics keyMapper valueMapper lazyInstantiator location path =
        let addr = Terms.makeStringKey location
        let dnt = System.Type.GetType(location)
        let t = fromDotNetType dnt
        let lazyInstantiator = lazyInstantiator |?? genericLazyInstantiator metadata groundHeap Timestamp.zero (StaticRef metadata location path) (if path.IsEmpty then t else path |> List.last |> snd)
        let ptr = {location = addr; fullyQualifiedLocation = StaticRef metadata location []; typ = t; time = Timestamp.infinity; path = path; isTopLevel = true; arrayTarget = ArrayContents}
        accessHeap read restricted metadata groundHeap (makeTrue metadata) update statics Timestamp.zero keyMapper valueMapper lazyInstantiator ptr

    let rec private independent (exploredRecursiveIds : ImmutableHashSet<IFunctionIdentifier>) (exploredLocations : ImmutableHashSet<term>) read funcId location : bool =
        exploredLocations.Contains(location) ||
        let exploredRecursiveIds = exploredRecursiveIds.Add funcId
        match Database.queryState funcId with
        | Some body ->
            let t, _ = read exploredRecursiveIds body
            match t.term with
            | Constant(_, LazyInstantiation(location', false, None), _) when location = location' -> true
            |_ -> false
        | None -> false

    and private accessGeneralizedHeapRec (exploredIds : ImmutableHashSet<IFunctionIdentifier>) keyMapper valueMapper lazyInstantiator read getter location accessDefined = function
        | Defined(r, h) ->
            let result, heap, _ = accessDefined keyMapper valueMapper lazyInstantiator None r h
            result, Defined r heap
        | Merged ghs ->
            let gs, hs = List.unzip ghs
            let rs, hs' = hs |> List.map (accessGeneralizedHeapRec exploredIds keyMapper valueMapper lazyInstantiator read getter location accessDefined) |> List.unzip
            let grs = List.zip gs rs
            Merging.merge grs, Merging.mergeGeneralizedHeaps gs hs'
        | Mutation(h, h') as m ->
            let result, h'', _ = accessDefined keyMapper valueMapper lazyInstantiator (Some h) false h'
            if read then
                let accessH = lazy(accessGeneralizedHeapRec exploredIds keyMapper valueMapper lazyInstantiator read getter location accessDefined h |> fst)
                let simplifyInstantiated term =
                    match term.term with
                    | Constant(_, LazyInstantiation(loc, _, Some heap), _) when loc = location && heap = h ->
                        accessH.Force()
                    | _ -> term
                Substitution.substitute simplifyInstantiated result, m
            else
                result, Mutation(h, h'')
        | Composition(_, _, Defined _) ->
            internalfail "composition with the defined heap should not be met, it must be simplified to a simple mutation!"
        | Composition(s, ctx, h) when read ->
            let keyMapper' = keyMapper >> fillHoles ctx s
            let valueMapper' = valueMapper >> fillHoles ctx s
            let lazyInstantiator' = lazy(accessGeneralizedHeapRec exploredIds keyMapper valueMapper lazyInstantiator read getter location accessDefined (getter s) |> fst)
            accessGeneralizedHeapRec exploredIds keyMapper' valueMapper' (Some lazyInstantiator'.Force) read getter location accessDefined h
        | RecursiveApplication(f, _, _) as h
                when not <| exploredIds.Contains(f) && read &&
                     independent exploredIds ImmutableHashSet.Empty
                                 (fun ids s -> accessGeneralizedHeapRec ids keyMapper valueMapper lazyInstantiator read getter location accessDefined (getter s)) f location ->
            let r, _, _ = accessDefined keyMapper valueMapper lazyInstantiator None false Heap.empty
            r, h
        | RecursiveApplication _
        | HigherOrderApplication _ as h ->
            let r, e, _ = accessDefined keyMapper valueMapper lazyInstantiator (Some h) false Heap.empty
            r, if read then h else Mutation(h, e)
        | _ -> __unreachable__()

    and private accessGeneralizedHeap = accessGeneralizedHeapRec ImmutableHashSet.Empty id id None

    and private hierarchicalAccess read actionNull updateDefined metadata state term =
        match term.term with
        | Error _ -> (term, state)
        | StackRef(location, path, None) ->
            commonHierarchicalStackAccess read updateDefined metadata state location path
        | HeapRef(((addr, t) as location, path), time, arrayMode, None) ->
            Common.statedConditionalExecution state
                (fun state k -> k (Arithmetics.simplifyEqual metadata addr (Concrete metadata [0] pointerType) id, state))
                (fun state k -> k (actionNull metadata state t))
                (fun state k ->
                    let accessDefined keyMapper valueMapper lazyInstantiator groundHeap r h =
                        commonHierarchicalHeapAccess read r updateDefined metadata groundHeap h keyMapper valueMapper lazyInstantiator location path time arrayMode
                    let result, h' = accessGeneralizedHeap read heapOf term accessDefined (heapOf state)
                    k (result, withHeap state h'))
                Merging.merge Merging.merge2Terms id id
        | StaticRef(location, path, None) ->
            let accessDefined keyMapper valueMapper lazyInstantiator groundHeap r h =
                commonHierarchicalStaticsAccess read r updateDefined metadata groundHeap h keyMapper valueMapper lazyInstantiator location path
            let result, m' = accessGeneralizedHeap read staticsOf term accessDefined (staticsOf state)
            result, withStatics state m'
        | Union gvs -> Merging.guardedStateMap (hierarchicalAccess read actionNull updateDefined metadata) gvs state
        | PointerTo viewType ->
            let ref = getReferenceFromPointer metadata term
            let term, state = hierarchicalAccess read actionNull updateDefined metadata state ref
            if typeOf term = viewType
            then term, state
            else __notImplemented__() // TODO: [columpio] [Reinterpretation]
        | t -> internalfailf "expected reference or pointer, but got %O" t

// ------------------------------- Composition -------------------------------

    and private fillHole (ctx : compositionContext) state term =
        match term.term with
        | Constant(_, source, _) ->
            match source with
            | LazyInstantiation(loc, isTop, heap') ->
                let state' =
                    match heap' with
                    | Some heap' ->
                        // TODO: make it more effective (use lower-level functions to access heap directly instead of creating fresh state)
                        match loc.term with
                        | HeapRef _ -> { State.empty with heap = composeHeapsOf ctx state heap' }
                        | StaticRef _ -> { State.empty with statics = composeStaticsOf ctx state heap' }
                        | _ -> __notImplemented__()
                    | None -> state
                let loc = fillHoles ctx state loc
                let result, _ = deref ctx.mtd state' loc
                if isTop then Pointers.topLevelLocation result else result
 // TODO: composition should be a method of ISymbolicConstantSource
//            | StaticsInitializedSource key ->
//                staticKeyInitialized term.metadata key state
            | _ -> term
        | Concrete(:? concreteHeapAddress as addr', t) ->
            Concrete ctx.mtd (composeAddresses ctx.addr addr') t
        | _ -> term

    and fillHoles ctx state term =
        Substitution.substitute (fillHole ctx state) term

    and fillHolesInHeap ctx state heap =
        Heap.map (fun k cell -> (fillHoles ctx state k, {cell with value = fillHoles ctx state cell.value})) heap

    and private fillAndMutateStack (ctx : compositionContext) source target loc path _ cell =
        let time = Timestamp.compose ctx.time cell.modified
        let path = path |> List.map (mapfst <| fillHoles ctx source)
        let v = fillHoles ctx source cell.value
        mutateStack ctx.mtd target loc path time v |> snd

    and private fillAndMutateHeap (ctx : compositionContext) restricted source target (addr, t) path arrayTarget cell =
        let time = Timestamp.compose ctx.time cell.modified
        let addr = fillHoles ctx source addr
        let loc = (addr, t)
        let path = path |> List.map (mapfst <| fillHoles ctx source)
        let v = fillHoles ctx source cell.value
        mutateHeap restricted ctx.mtd target loc path time arrayTarget v |> snd3

    and private fillAndMutateStatics (ctx : compositionContext) restricted source target (addr, _) path _ cell =
        let time = Timestamp.compose ctx.time cell.modified
        let addr = fillHoles ctx source addr
        let path = path |> List.map (mapfst <| fillHoles ctx source)
        let v = fillHoles ctx source cell.value
        let loc =
            match addr.term with
            | Concrete(s, StringType) -> string s
            | _ -> __notImplemented__()
        mutateStatics restricted ctx.mtd target loc path time v |> snd3

    and private composeDefinedHeaps writer restricted s h h' =
        foldHeapLocations (writer restricted s) h h'

    and private composeGeneralizedHeaps writer ctx getter setter s h' =
        match getter s, h' with
        | Defined(r, h), Defined(r', h') ->
            assert(not r')
            composeDefinedHeaps (writer ctx) r s h h' |> Defined r
        | Merged ghs, _ ->
            let gs, hs = List.unzip ghs
            hs |> List.map (fun h -> composeGeneralizedHeaps writer ctx getter setter (setter s h) h') |> Merging.mergeGeneralizedHeaps gs
        | _, Merged ghs' ->
            let gs, hs' = List.unzip ghs'
            let gs' = List.map (fillHoles ctx s) gs
            hs' |> List.map (composeGeneralizedHeaps writer ctx getter setter s) |> Merging.mergeGeneralizedHeaps gs'
        | Defined _, Composition(s', ctx', h'') ->
            let s = composeStates ctx s s'
            composeGeneralizedHeaps writer ctx' getter setter s h''
        | Defined _, Mutation(h', h'') ->
            let res = composeGeneralizedHeaps writer ctx getter setter s h'
            let res' = fillHolesInHeap ctx s h''
            Mutation(res, res')
        | Defined _, HigherOrderApplication _
        | Defined _, RecursiveApplication _
        | Composition _, HigherOrderApplication _
        | Composition _, RecursiveApplication _
        | RecursiveApplication _, RecursiveApplication _
        | Mutation _, Mutation _ ->
            Composition(s, ctx, h')
        | Composition(s', ctx', h') as h, Defined(r'', h'') ->
            assert(not r'')
            match h' with
            | Defined(r, h') ->
                let ctx'' = decomposeContexts ctx ctx'
                let h = composeDefinedHeaps (writer ctx'') r s h' h'' |> Defined r
                composeGeneralizedHeaps writer ctx' getter setter s' h
            | _ ->
                let h'' = fillHolesInHeap ctx s h''
                Mutation(h, h'')
        | Mutation(h, h'), Defined(r, h'') ->
            Mutation(h, composeDefinedHeaps (writer ctx) r s h' h'')
        | HigherOrderApplication _, Defined _ -> __notImplemented__()
        | HigherOrderApplication _, HigherOrderApplication _ -> __notImplemented__()
        | HigherOrderApplication _, RecursiveApplication _ -> __notImplemented__()
        | HigherOrderApplication _, Composition _ -> __notImplemented__()
        | RecursiveApplication _, Defined _ -> __notImplemented__()
        | RecursiveApplication _, HigherOrderApplication _ -> __notImplemented__()
        | RecursiveApplication _, Composition _ -> __notImplemented__()
        | Composition _, Composition _ -> __notImplemented__()
        | _, Mutation _
        | Mutation _, _ -> __notImplemented__()

    and composeStacksOf ctx state state' =
        (foldStackLocations (fillAndMutateStack ctx state) state state'.stack).stack

    and composeHeapsOf ctx state heap =
        composeGeneralizedHeaps fillAndMutateHeap ctx heapOf withHeap state heap

    and composeStaticsOf ctx state statics =
        composeGeneralizedHeaps fillAndMutateStatics ctx staticsOf withStatics state statics

    and composeStates ctx state state' =
        let stack = composeStacksOf ctx state state'
        let heap = composeHeapsOf ctx state state'.heap
        let statics = composeStaticsOf ctx state state'.statics
        { stack = stack; heap = heap; statics = statics; frames = state.frames; pc = state.pc }

// ------------------------------- High-level read/write -------------------------------

    and deref metadata state location =
        hierarchicalAccess true npeTerm makePair metadata state location

    and private mutateStack metadata state location path time value =
        commonHierarchicalStackAccess false (fun _ _ -> (value, time)) metadata state location path

    and private mutateHeap restricted metadata h location path time arrayTarget value =
        commonHierarchicalHeapAccess false restricted (fun _  _ -> (value, time)) metadata None h id id None location path {v=time} arrayTarget

    and private mutateStatics restricted metadata statics location path time value =
        commonHierarchicalStaticsAccess false restricted (fun _ _ -> (value, time)) metadata None statics id id None location path

    and derefWith actionNull metadata state location = hierarchicalAccess true actionNull makePair metadata state location

    let mutate metadata state reference value =
        assert(value <> Nop)
        let time = tick()
        hierarchicalAccess false npeTerm (fun _ _ -> (value, time)) metadata state reference

// ------------------------------- Referencing -------------------------------

    let rec private referenceTerm state name followHeapRefs term =
        match term.term with
        | Error _
        | PointerTo _ -> StackRef term.metadata name []
        | StackRef _
        | StaticRef _
        | HeapRef _ when followHeapRefs -> term
        | Union gvs -> Merging.guardedMap (referenceTerm state name followHeapRefs) gvs
        | _ -> StackRef term.metadata name []

    let referenceLocalVariable metadata state location followHeapRefs =
        let reference = StackRef metadata location []
        let term, state = deref metadata state reference
        referenceTerm state location followHeapRefs term

    let rec private referenceFieldOf state field parentRef reference =
        match reference with
        | ErrorT _ -> reference, state
        | { term = HeapRef((addr, path), t, ArrayContents, None) } ->
            assert(List.isEmpty path)
            HeapRef reference.metadata (addr, [field]) ArrayContents t, state
        | Null ->
            let term, state = State.createInstance reference.metadata typeof<System.NullReferenceException> [] state
            Error reference.metadata term, state
        | { term = Struct _ } -> referenceSubLocation field ArrayContents parentRef, state
        | UnionT gvs -> Merging.guardedStateMap (fun state term -> referenceFieldOf state field parentRef term) gvs state
        | t -> internalfailf "expected reference or struct, but got %O" t, state

    let rec private followOrReturnReference metadata state reference =
        let term, state = deref metadata state reference
        match term.term with
        | Error _
        | StackRef _
        | StaticRef _
        | HeapRef _ -> term, state
        | Union gvs when List.forall (fun (_, t) -> isError t || isRef t) gvs ->
            Merging.guardedStateMap (followOrReturnReference metadata) gvs state
        | _ -> reference, state

    let referenceField metadata state followHeapRefs name typ parentRef =
        let typ = Types.wrapReferenceType typ
        let term, state = deref metadata state parentRef
        let reference, newState = referenceFieldOf state (makeStringKey name, typ) parentRef term
        if followHeapRefs then followOrReturnReference metadata newState reference
        else (reference, newState)

    let referenceStaticField metadata state followHeapRefs fieldName fieldType typeName =
        let fieldType = Types.wrapReferenceType fieldType
        let reference = StaticRef metadata typeName [(Terms.makeStringKey fieldName, fieldType)]
        if followHeapRefs then followOrReturnReference metadata state reference
        else (reference, state)

    let referenceArrayLowerBound arrayRef (indices : term) =
        let newIndices = { indices with metadata = Metadata.clone indices.metadata }
        referenceSubLocation (newIndices, Arrays.lengthTermType) ArrayLowerBounds arrayRef

    let referenceArrayLength arrayRef (indices : term) =
        let newIndices = { indices with metadata = Metadata.clone indices.metadata }
        referenceSubLocation (newIndices, Arrays.lengthTermType) ArrayLengths arrayRef

    let private checkIndices mtd state arrayRef (indices : term list) k =
        let intToTerm i = makeNumber i mtd
        let idOfDimensionsForLowerBounds = Seq.init indices.Length (intToTerm >> referenceArrayLowerBound arrayRef)
        let idOfDimensionsForLengths = Seq.init indices.Length (intToTerm >> referenceArrayLength arrayRef)
        Cps.Seq.mapFold (deref mtd) state idOfDimensionsForLowerBounds (fun (lowerBoundsList, state') ->
        Cps.Seq.mapFold (deref mtd) state' idOfDimensionsForLengths (fun (lengthsList, state'') ->
        let bounds =
            Seq.map3
                (fun idx low len ->
                    let up = add mtd low len
                    Arithmetics.simplifyGreaterOrEqual mtd idx low (fun bigEnough ->
                    Arithmetics.simplifyLess mtd idx up (fun smallEnough ->
                    bigEnough &&& smallEnough)))
                indices lowerBoundsList lengthsList
            |> List.ofSeq
        k (conjunction mtd bounds |> Merging.unguard |> Merging.merge , state'')))

    let referenceArrayIndex metadata state arrayRef (indices : term list) =
        let array, state = deref metadata state arrayRef
        // TODO: what about followHeapRefs?
        let rec reference state term =
            match term.term with
            | Error _ -> (term, state)
            | Array(_, _, _, _, _, _, ArrayType(elementType, _)) ->
                Common.statedConditionalExecution state
                    (fun state k -> checkIndices metadata state arrayRef indices k)
                    (fun state k ->
                        let location = Arrays.makeIntegerArray metadata (fun i -> indices.[i]) indices.Length
                        let result = referenceSubLocation (location, elementType) ArrayContents arrayRef
                        k (result, state))
                    (fun state k ->
                        let exn, state = State.createInstance metadata typeof<System.IndexOutOfRangeException> [] state
                        k (Error metadata exn, state))
                    Merging.merge Merging.merge2Terms id id
            | Union gvs -> Merging.guardedStateMap reference gvs state
            | t -> internalfailf "accessing index of non-array term %O" t
        reference state array

// ------------------------------- Allocation -------------------------------

    let newStackFrame state metadata funcId frame = State.newStackFrame (tick()) metadata state funcId frame
    let newScope metadata state frame = State.newScope (tick()) metadata state frame

    let freshHeapLocation metadata =
        Concrete metadata ([freshAddress()]) pointerType

    let allocateOnStack metadata s key term =
        let time = tick()
        let { func = frameMetadata; entries = oldFrame; time = frameTime } = Stack.peek s.frames.f
        let newStack = pushToCurrentStackFrame s key { value = term; created = time; modified = time }
        let newEntries = { key = key; mtd = metadata; typ = None }
        let stackFrames = Stack.updateHead s.frames.f { func = frameMetadata; entries = newEntries :: oldFrame; time = frameTime }
        { s with stack = newStack; frames = { s.frames with f = stackFrames } }

    let private allocateInGeneralizedHeap address term time = function
        | Defined(r, h) -> h.Add(address, {value = term; created = time; modified = time }) |> Defined r
        | _ -> __notImplemented__()

    let allocateInHeap metadata s term : term * state =
        let address = freshHeapLocation metadata
        let time = tick()
        let pointer = HeapRef metadata ((address, Terms.typeOf term), []) ArrayContents {v=time}
        (pointer, { s with heap = allocateInGeneralizedHeap address term time s.heap } )

    let allocateInStaticMemory metadata (s : state) typeName term =
        let time = tick()
        let address = makeConcreteString typeName metadata
        { s with statics = allocateInGeneralizedHeap address term time s.statics }

    let allocateSymbolicInstance metadata state = function
        | termType.ClassType(tp, arg, interfaces) ->
            let contents = makeSymbolicInstance metadata Timestamp.zero {location = Nop; isTopLevelHeapAddress = false; heap = None} "" (StructType(tp, arg, interfaces))
            allocateInHeap metadata state contents
        | StructType _ as t ->
            makeSymbolicInstance metadata Timestamp.zero {location = Nop; isTopLevelHeapAddress = false; heap = None} "" t, state
        | _ -> __notImplemented__()

// ------------------------------- Static Memory Initialization -------------------------------

    [<StructuralEquality;NoComparison>]
    type private StaticsInitializedSource =
        {heap : generalizedHeap; key : term}
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.singleton x.key

    let private (|StaticsInitializedSource|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? StaticsInitializedSource as si -> Some(si.key)
        | _ -> None

    let private mkStaticKeyGuard mtd heap key =
        Constant mtd (IdGenerator.startingWith "hasKey#") ({heap = heap; key = key} : StaticsInitializedSource) Bool

    let private staticGuardOfDefinedHeap mtd key r (h : symbolicHeap) =
            if h.ContainsKey key then Merging.guardOf h.[key].value
            elif r then False
            else mkStaticKeyGuard mtd (Defined r h) key

    let rec private staticGuardOfHeap (exploredRecursiveIds : ImmutableHashSet<IFunctionIdentifier>) mtd key = function
        | Defined(r, h) -> staticGuardOfDefinedHeap mtd key r h
        | Merged ghs -> Merging.guardedMap (staticGuardOfHeap exploredRecursiveIds mtd key) ghs
        | Mutation(h, h') ->
            staticGuardOfHeap exploredRecursiveIds mtd key h ||| staticGuardOfDefinedHeap mtd key false h'
        | Composition(s, ctx, h) ->
            staticGuardOfHeap exploredRecursiveIds mtd key s.statics ||| staticGuardOfHeap exploredRecursiveIds mtd (fillHoles ctx s key) h
        | RecursiveApplication(f, _, _) when exploredRecursiveIds.Contains f -> False
        | RecursiveApplication(f, _, _) ->
            match Database.queryState f with
            | Some body ->
                staticGuardOfHeap (exploredRecursiveIds.Add f) mtd key body.statics
            | None -> True
        | HigherOrderApplication _ as h ->
            mkStaticKeyGuard mtd h key

    let private staticKeyInitialized mtd key state =
        staticGuardOfHeap ImmutableHashSet<IFunctionIdentifier>.Empty mtd key state.statics

    let internal typeNameInitialized mtd typeName state =
        staticKeyInitialized mtd (makeStringKey typeName) state
