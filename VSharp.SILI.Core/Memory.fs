namespace VSharp.Core

#nowarn "69"

open VSharp
open VSharp.Core.State
open VSharp.Core.Types
open VSharp.Core.Types.Constructor
open System.Collections.Immutable

module internal Memory =
    open Terms

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

    let rec private referenceSubLocations locations term =
        match term.term with
        | Error _ -> term
        | Ref(tl, path) -> Ref term.metadata tl (List.append path locations)
        | Ptr(tl, path, typ, shift) -> AnyPtr term.metadata tl (List.append path locations) typ shift
        | GuardedValues(gs, vs) -> vs |> List.map (referenceSubLocations locations) |> List.zip gs |> Union term.metadata
        | _ -> internalfailf "expected reference, but got %O" term

    let referenceArrayLowerBound arrayRef (indices : term) =
        referenceSubLocations [ArrayLowerBound indices] arrayRef

    let referenceArrayLength arrayRef (indices : term) =
        referenceSubLocations [ArrayLength indices] arrayRef

// ------------------------------- Traversal -------------------------------

    // TODO: path should NOT be accumulated, but taken from key metainfo
    let inline private foldHeapLocationsRec folder acc loc typ path ctor heap =
        Heap.fold (fun acc subloc cell -> folder acc loc typ (List.append path [ctor (subloc, cell.value)]) cell) acc heap

    let rec private foldSubLocations folder acc loc typ path cell = // TODO: get rid of typ
        match cell.value.term with
        | Struct(contents, _) ->
            foldHeapLocationsRec (foldSubLocations folder) acc loc typ path (mapsnd typeOf >> StructField) contents
        | Array(_, _, lower, _, contents, lengths, _) ->
            let acc = foldHeapLocationsRec (foldSubLocations folder) acc loc typ path (fst >> ArrayLowerBound) lower
            let acc = foldHeapLocationsRec (foldSubLocations folder) acc loc typ path (fst >> ArrayLength) lengths
            foldHeapLocationsRec (foldSubLocations folder) acc loc typ path (mapsnd typeOf >> ArrayIndex) contents
        | _ -> folder acc loc typ path cell

    let private foldHeapLocations folder acc heap =
        Heap.fold (fun acc loc cell -> foldSubLocations folder acc loc (typeOf cell.value) [] cell) acc heap

    let private foldStackLocations folder acc stack =
        stackFold (fun acc loc cell -> foldSubLocations folder acc loc (typeOf cell.value) [] cell) acc stack

// ------------------------------- Instantiation (lazy & default) -------------------------------

    [<StructuralEquality;NoComparison>]
    type private lazyInstantiation<'a when 'a : equality> =
        {location : term; heap : 'a generalizedHeap option; extractor : TermExtractor; typeExtractor : TypeExtractor}
        interface IExtractingSymbolicConstantSource with
            override x.SubTerms = Seq.singleton x.location
            override x.WithExtractor e = {x with extractor = e} :> IExtractingSymbolicConstantSource
        interface IExtractingSymbolicTypeSource with
            override x.WithTypeExtractor e = {x with typeExtractor = e} :> IExtractingSymbolicTypeSource
            override x.TypeCompose ctx state =
                (x :> IStatedSymbolicConstantSource).Compose ctx state |> typeOf |> x.typeExtractor.TypeExtract
            override x.TypeEquals other =
                match other with
                | :? lazyInstantiation<'a> as li -> x.location = li.location
                | _ -> false
    let (|LazyInstantiation|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? lazyInstantiation<_> as li -> Some(li.location, li.heap, li.extractor :? IdTermExtractor)
        | _ -> None

    let private mkStruct metadata time isStatic mkField dotNetType typ =
        let fields = Types.fieldsOf dotNetType isStatic
        let contents =
            fields
            |> FSharp.Collections.Array.fold (fun acc (name, typ) ->
                let value = mkField metadata name typ
                Heap.add name { value = value; created = time; modified = time } acc) Heap.empty
        Struct metadata contents typ

    let rec private defaultOf time metadata typ =
        match typ with
        | Bool -> makeFalse metadata
        | Numeric t when t.IsEnum -> CastConcrete (System.Activator.CreateInstance t) t metadata
        | Numeric t -> CastConcrete 0 t metadata
        | Func _
        | Reference _
        | ClassType _
        | ArrayType _
        | InterfaceType _ -> makeNullRef metadata
        | TypeVariable _ ->
            Common.statelessConditionalExecution
                (fun k -> k <| Common.isValueType metadata typ)
                (fun k -> k <| Struct metadata Heap.empty typ)
                (fun k -> k <| makeNullRef metadata)
                Merging.merge Merging.merge2Terms id
        | StructType(dotNetType, _) ->
            mkStruct metadata time false (fun m _ t -> defaultOf time m t) dotNetType typ
        | Pointer typ -> makeNullPtr metadata typ
        | _ -> __notImplemented__()

    let mkDefault metadata typ =
        defaultOf (tick()) metadata typ

    let mkDefaultStruct metadata isStatic targetType =
        let dnt = toDotNetType targetType
        let time = tick()
        mkStruct metadata time isStatic (fun m _ t -> defaultOf time m t) dnt targetType

    let private makeSymbolicHeapReference metadata (source : IExtractingSymbolicConstantSource) name typ construct =
        let source' = source.WithExtractor(Pointers.HeapAddressExtractor())
        let constant = Constant metadata name source' pointerType
        construct metadata constant typ typ []

    let private makeSymbolicOveralArrayLength metadata (source : IExtractingSymbolicConstantSource) arrayName =
        Constant metadata (sprintf "|%s|" arrayName) (source.WithExtractor(Arrays.LengthExtractor())) Arrays.lengthTermType

    let private makeSymbolicArrayRank metadata (source : IExtractingSymbolicConstantSource) arrayName =
        Constant metadata ("RankOf_%s" + arrayName) (source.WithExtractor(Arrays.RankExtractor())) Arrays.lengthTermType

    let private makeSymbolicArrayLowerBound metadata time name location heap =
        match Options.ExplorationMode() with
        | TrustConventions -> defaultOf time metadata Arrays.lengthTermType
        | CompleteExploration ->
            Constant metadata name {location = location; heap = heap; extractor = IdTermExtractor(); typeExtractor = IdTypeExtractor()} Arrays.lengthTermType

    let private makeSymbolicArrayLength metadata name location heap =
        Constant metadata name {location = location; heap = heap; extractor = IdTermExtractor(); typeExtractor = IdTypeExtractor()} Arrays.lengthTermType

    let private makeSymbolicArrayLowerBounds metadata (source : IExtractingSymbolicConstantSource) arrayName dimension =
        match source with
        | :? lazyInstantiation<term> as liSource ->
            match Options.ExplorationMode() with
            | TrustConventions -> Arrays.zeroLowerBound metadata dimension
            | CompleteExploration ->
                let idOfBound i = sprintf "%s.%i_LowerBound" arrayName i
                let mkLowerBound i = Constant metadata (idOfBound i) {liSource with location = referenceArrayLowerBound liSource.location (makeNumber i metadata)} Arrays.lengthTermType
                Seq.foldi (fun h i l -> Heap.add (makeNumber i metadata) { value = l; created = Timestamp.zero; modified = Timestamp.zero } h) Heap.empty (Seq.init dimension mkLowerBound)
        | _ -> __notImplemented__()

    let private makeSymbolicArrayLengths metadata (source : IExtractingSymbolicConstantSource) arrayName dimension =
        match source with
        | :? lazyInstantiation<term> as liSource ->
            let idOfLength i = sprintf "%s.%i_Length" arrayName i
            let mkLength i = Constant metadata (idOfLength i) {liSource with location = referenceArrayLength liSource.location (makeNumber i metadata)} Arrays.lengthTermType
            let lengths = Seq.init dimension mkLength
            let length = Seq.reduce (mul metadata) lengths
            Seq.foldi (fun h i l -> Heap.add (makeNumber i metadata) { value = l; created = Timestamp.zero; modified = Timestamp.zero } h) Heap.empty lengths, length
        | _ -> __notImplemented__()

    let private makeSymbolicArray metadata (source : IExtractingSymbolicConstantSource) dimension elemTyp typ arrayName =
        let arrayConstant = Constant metadata arrayName source typ
        let instantiator = [True, LazyInstantiator(arrayConstant, elemTyp)]
        let lowerBound, arrayLengths, arrayLength, dim =
            let makeConcrete d =
                let lb = makeSymbolicArrayLowerBounds metadata source arrayName d
                let al, length = makeSymbolicArrayLengths metadata source arrayName d
                lb, al, length, makeNumber d metadata
            match dimension with
            | Vector -> makeConcrete 1
            | ConcreteDimension d -> makeConcrete d
            | SymbolicDimension _ ->
                let length = makeSymbolicOveralArrayLength metadata source arrayName
                Heap.empty, Heap.empty, length, makeSymbolicArrayRank metadata source arrayName
        Array metadata dim arrayLength lowerBound instantiator Heap.empty arrayLengths typ

    let makeSymbolicInstance metadata (source : IExtractingSymbolicConstantSource) (typeSource: IExtractingSymbolicTypeSource) name = function
        | Pointer typ -> makeSymbolicHeapReference metadata source name typ (fun mtd tl bTyp sTyp path -> HeapPtr mtd tl bTyp sTyp path sTyp)
        | Reference typ -> makeSymbolicHeapReference metadata source name typ HeapRef
        | t when Types.isPrimitive t || Types.isFunction t -> Constant metadata name source t
        | StructType _ // TODO: initialize all fields of struct symbolicly (via mkStruct). Warning: `source` should be updated!
        | InterfaceType _
        | TypeVariable _
        | ClassType _ as t ->
            let t = Types.Variable.fromTermType name typeSource t
            Struct metadata Heap.empty t
        | ArrayType(_, d) as t ->
            let t = Types.Variable.fromTermType name typeSource t
            let e = ArrayTypeExtractor().TypeExtract t
            makeSymbolicArray metadata source d e t name
        | Void -> Nop
        | _ -> __notImplemented__()

    let private genericLazyInstantiator<'a when 'a : equality> metadata heap fullyQualifiedLocation typ () =
        let source : 'a lazyInstantiation = {location = fullyQualifiedLocation; heap = heap; extractor = IdTermExtractor(); typeExtractor = IdTypeExtractor()}
        makeSymbolicInstance metadata source source (nameOfLocation fullyQualifiedLocation) typ

    let () =
        State.genericLazyInstantiator <- fun mtd -> genericLazyInstantiator mtd None

    let private arrayElementLazyInstantiator metadata instantiator typ heap time location idx = function
        | DefaultInstantiator(_, concreteType) -> fun () -> defaultOf time metadata (typ |?? concreteType)
        | LazyInstantiator(array, concreteType) -> instantiator |?? fun () ->
            let id = sprintf "%s[%s]" (toString array) (idx.term.IndicesToString())
            let source = {location = location; heap = heap; extractor = IdTermExtractor(); typeExtractor = IdTypeExtractor()}
            makeSymbolicInstance metadata source source id concreteType
    let private arrayLowerBoundLazyInstantiator metadata instantiator _ heap time location (idx : term) = function
        | DefaultInstantiator(_, _) -> fun () -> defaultOf time metadata Arrays.lengthTermType
        | LazyInstantiator(array, _) -> instantiator |?? fun () ->
            let name = sprintf "%O.%s_LowerBound" array (idx.term.IndicesToString())
            makeSymbolicArrayLowerBound metadata time name location heap

    let private arrayLengthLazyInstantiator metadata instantiator _ heap _ location (idx : term) = function
        | DefaultInstantiator(_, _) -> fun () ->
            // In case when array was allocated during the interpretation (and thus has default instantiator) lengths by
            // all dimensions are known (they can be symbolic, but still defined). If this code triggers then we have
            // requested length by a greater dimension than our array has. That can happen in case of comparison of array
            // lengths when arrays have different ranks. In that case we consider lengths in all other dimensions equal to 1.
            makeNumber 1 metadata
        | LazyInstantiator(array, _) -> instantiator |?? fun () ->
            let name = sprintf "%O.%s_Length" array (idx.term.IndicesToString())
            makeSymbolicArrayLength metadata name location heap

    let private staticMemoryLazyInstantiator metadata t () =
        Struct metadata Heap.empty (fromDotNetType t)

    let private selectLazyInstantiator<'a when 'a : equality> metadata (heap : 'a generalizedHeap option) time fullyQualifiedLocation typ =
        match fullyQualifiedLocation.term with
        | Ref(_, (_::_ as path)) when isArrayLengthSeg <| List.last path -> fun () -> makeSymbolicArrayLength metadata (nameOfLocation fullyQualifiedLocation + "_Length") fullyQualifiedLocation heap
        | Ref(_, (_::_ as path)) when isArrayLowerBoundSeg <| List.last path -> fun () -> makeSymbolicArrayLowerBound metadata time (nameOfLocation fullyQualifiedLocation + "_LowerBound") fullyQualifiedLocation heap
        | _ -> genericLazyInstantiator<'a> metadata heap fullyQualifiedLocation typ

// ------------------------------- Locations comparison -------------------------------

    type private 'key pointerInfo = { location : 'key; fullyQualifiedLocation : term; typ : termType; time : timestamp; path : pathSegment list; isTopLevel : bool }

    let private canPoint mtd keyCompare pointerAddr pointerType locationAddr locationValue =
        // TODO: what if locationType is Null?
        let addrEqual = keyCompare mtd pointerAddr locationAddr
        let typeSuits v = Common.typesEqual mtd (typeOf v) pointerType
        let typeEqual =
            match locationValue.term with
            | Union gvs ->
                gvs |> List.map (fun (g, v) -> (g, typeSuits v)) |> Merging.merge
            | _ -> typeSuits locationValue
        if isConcrete addrEqual then addrEqual else addrEqual &&& typeEqual

    let private findSuitableLocations<'key when 'key : equality> mtd h keyCompare contextList mapper (ptr : 'key pointerInfo) =
        let filterMapKey (k : 'key, cell) =
            let k, v = List.fold mapper (k, cell.value) contextList
            let cell = {cell with value = v}
            let guard = canPoint mtd keyCompare ptr.location ptr.typ k v
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

// ------------------------------- Core -------------------------------

    let rec private accessHeap<'a, 'key when 'a : equality and 'key : equality> read restricted metadata (groundHeap:'a generalizedHeap option) guard update (h : heap<'key, term>) time keyCompare contextList mapper lazyInstantiator ptr =
        let accessRec gvas lazyValue h =
            let gvs, (h', newTime) = gvas |> ((h, Timestamp.zero) |> List.mapFold (fun (h, maxTime) (guard', addr, cell) ->
                let guard'' = guard &&& guard'
                let accessedValue, newBaseValue, newTime = accessTerm read metadata groundHeap guard update contextList lazyInstantiator cell.created cell.modified ptr.time ptr.fullyQualifiedLocation ptr.path cell.value
                let h' = if read || cell.value = newBaseValue then h else writeHeap newTime guard'' h addr newBaseValue
                ((guard, accessedValue), (h', max maxTime newTime))))
            (Merging.merge (optCons gvs lazyValue), h', newTime)
        if Heap.contains ptr.location h then
            accessRec [(makeTrue metadata, ptr.location, Heap.find ptr.location h)] None h
        else
            let baseGav, restGavs = findSuitableLocations metadata h keyCompare contextList mapper ptr
            match baseGav with
            | None when read && restricted ->
                // TODO: undefined behaviour detected!
                __notImplemented__()
            | None ->
                let baseGuard = restGavs |> List.map (fst3 >> (!!)) |> conjunction metadata
                let shouldLazyInstantiate = read && not ptr.isTopLevel
                let lazyValue =
                    if ptr.isTopLevel then genericLazyInstantiator<'a> metadata groundHeap ptr.fullyQualifiedLocation ptr.typ ()
                    else lazyInstantiator()
                let baseCell = { value = lazyValue; created = time; modified = time}
                let gavs = if shouldLazyInstantiate then restGavs else (baseGuard, ptr.location, baseCell)::restGavs
                let lv = if shouldLazyInstantiate then Some(baseGuard, lazyValue) else None
                let h = if shouldLazyInstantiate then h else h.Add(ptr.location, baseCell)
                accessRec gavs lv h
            | Some(g, a, v) -> accessRec ((g, a, v)::restGavs) None h

    and private accessTerm read metadata (groundHeap: 'a generalizedHeap option) guard (update : term -> timestamp -> term * timestamp) contextList lazyInstantiator created modified ptrTime ptrFql path term =
        match path with
        | [] ->
            let newTerm, newTime = update term modified
            newTerm, newTerm, newTime
        | location :: path' ->
            match term.term with
            | Error _ -> term, term, modified
            | Struct(fields, t) ->
                let fql' = referenceSubLocations [location] ptrFql
                match location with
                | StructField(name, typ) ->
                    let instantiator = if read then lazyInstantiator else genericLazyInstantiator<'a> term.metadata groundHeap fql' typ
                    let ptr' = {location = name; fullyQualifiedLocation = fql'; typ = typ; time = ptrTime; path = path'; isTopLevel = false }
                    let mapper (k, v) (ctx, s) = k, fillHoles ctx s v
                    let result, newFields, newTime = accessHeap<'a, string> read false metadata groundHeap guard update fields created (fun mtd loc key -> makeBool (loc = key) mtd) contextList mapper instantiator ptr'
                    result, Struct term.metadata newFields t, newTime
                | _ -> __unreachable__()
            | Array(dimension, length, lower, constant, contents, lengths, arrTyp) ->
                let fql' = referenceSubLocations [location] ptrFql
                let newHeap heap instor keyCompare ptr = accessHeap<'a, term> read false metadata groundHeap guard update heap created keyCompare contextList termKeyMapper instor ptr
                let makePtr key typ = {location = key; fullyQualifiedLocation = fql'; typ = typ; time = ptrTime; path = path'; isTopLevel = false }
                let makeInstantiator key instantiator =
                    let realInstantiator = if read then Some lazyInstantiator else None
                    let targetType = if read then Some(path |> List.last |> typeOfPathSegment) else None
                    let doJob = lazy(Merging.guardedMap (fun c -> instantiator term.metadata realInstantiator targetType groundHeap modified fql' key c ()) constant)
                    doJob.Force
                match location with
                | ArrayIndex(key, typ) ->
                    let instantiator = makeInstantiator key arrayElementLazyInstantiator
                    let result, newContents, newTime = newHeap contents instantiator Arrays.equalsArrayIndices <| makePtr key typ
                    result, Array term.metadata dimension length lower constant newContents lengths arrTyp, newTime
                | ArrayLength key ->
                    let instantiator = makeInstantiator key arrayLengthLazyInstantiator
                    let result, newLengths, newTime = newHeap lengths instantiator fastNumericCompare <| makePtr key indexType
                    result, Array term.metadata dimension length lower constant contents newLengths arrTyp, newTime
                | ArrayLowerBound key ->
                    let instantiator = makeInstantiator key arrayLowerBoundLazyInstantiator
                    let result, newLower, newTime = newHeap lower instantiator fastNumericCompare <| makePtr key indexType
                    result, Array term.metadata dimension length newLower constant contents lengths arrTyp, newTime
                | _ -> __unreachable__()
            | Union _ ->
                internalfail "unexpected union of complex types! Probably merge function implemented incorrectly."
            | t ->
                internalfailf "expected complex type, but got %O" t

    and private commonHierarchicalStackAccess read update metadata state location path =
        let firstLocation = stackLocationToReference state location
        let time = frameTime state location
        let cell, _ = stackDeref time (fun () -> (stackLazyInstantiator state time location).value) state location
        let termLazyInstantiator = if read && not (List.isEmpty path) then genericLazyInstantiator metadata None (StackRef metadata location path) (path |> List.last |> typeOfPathSegment) else __unreachable__
        let accessedValue, newBaseValue, newTime = accessTerm read metadata None (makeTrue metadata) update [] termLazyInstantiator cell.created cell.modified time firstLocation path cell.value
        let newState = if read || cell.value = newBaseValue then state else writeStackLocation state location { cell with value = newBaseValue; modified = newTime }
        accessedValue, newState

    and private termKeyMapper (k, v) (ctx, s) = fillHoles ctx s k, fillHoles ctx s v

    and private commonHierarchicalHeapAccess read restricted update metadata groundHeap heap contextList lazyInstantiator addr typ path time =
        let firstLocation = HeapRef metadata addr typ typ []
        let typ' = if List.isEmpty path then typ else path |> List.last |> typeOfPathSegment
        let lazyInstantiator = lazyInstantiator |?? selectLazyInstantiator<term> metadata groundHeap time.v (HeapRef metadata addr typ typ' path) typ'
        let ptr = {location = addr; fullyQualifiedLocation = firstLocation; typ = typ; time = time.v; path = path; isTopLevel = true}
        accessHeap<term, term> read restricted metadata groundHeap (makeTrue metadata) update heap Timestamp.zero Pointers.simplifyReferenceEquality contextList termKeyMapper lazyInstantiator ptr

    and private commonHierarchicalStaticsAccess read restricted update metadata groundHeap statics contextList lazyInstantiator typ path =
        let lazyInstantiator = lazyInstantiator |?? genericLazyInstantiator<termType> metadata groundHeap (StaticRef metadata typ path) (if path.IsEmpty then typ else path |> List.last |> typeOfPathSegment)
        let ptr = {location = typ; fullyQualifiedLocation = StaticRef metadata typ []; typ = typ; time = Timestamp.infinity; path = path; isTopLevel = true }
        let mapper (k, v) (ctx, s) = substituteTypeVariables ctx s k, fillHoles ctx s v
        accessHeap<termType, termType> read restricted metadata groundHeap (makeTrue metadata) update statics Timestamp.zero Common.typesEqual contextList mapper lazyInstantiator ptr

    and mutateStack metadata state location path time value =
        snd <| commonHierarchicalStackAccess false (fun _ _ -> (value, time)) metadata state location path

    and private mutateHeap restricted metadata h loc typ path time value =
        snd3 <| commonHierarchicalHeapAccess false restricted (fun _  _ -> (value, time)) metadata None h [] None loc typ path {v=time}

    and private mutateStatics restricted metadata statics location _ path time value =
        snd3 <| commonHierarchicalStaticsAccess false restricted (fun _ _ -> (value, time)) metadata None statics [] None location path

    and private independent (exploredRecursiveIds : ImmutableHashSet<IFunctionIdentifier>) (exploredLocations : ImmutableHashSet<term>) read funcId location : bool =
        exploredLocations.Contains(location) ||
        let exploredRecursiveIds = exploredRecursiveIds.Add funcId
        match Database.querySummary funcId with
        | Some summary ->
            let t, _ = read exploredRecursiveIds summary.state
            match t.term with
            | Constant(_, LazyInstantiation(location', None, true), _) when location = location' -> true
            |_ -> false
        | None -> false

    and private accessGeneralizedHeapRec<'a when 'a : equality> (exploredIds : ImmutableHashSet<IFunctionIdentifier>) contextList lazyInstantiator read (getter : state -> 'a generalizedHeap) location accessDefined = function
        | Defined(r, h) ->
            let result, heap, _ = accessDefined contextList lazyInstantiator None r h
            result, Defined r heap
        | Merged ghs ->
            let gs, hs = List.unzip ghs
            let rs, hs' = hs |> List.map (accessGeneralizedHeapRec exploredIds contextList lazyInstantiator read getter location accessDefined) |> List.unzip
            let grs = List.zip gs rs
            Merging.merge grs, Merging.mergeGeneralizedHeaps gs hs'
        | Mutation(h, h') as m ->
            let result, h'', _ = accessDefined contextList lazyInstantiator (Some h) false h'
            if read then
                let accessH = lazy(accessGeneralizedHeapRec exploredIds contextList lazyInstantiator read getter location accessDefined h |> fst)
                let simplifyInstantiated term =
                    match term.term with
                    | Constant(_, LazyInstantiation(loc, Some heap, _), _) when loc = location && heap = h ->
                        accessH.Force()
                    | _ -> term
                Substitution.substitute simplifyInstantiated id result, m
            else
                result, Mutation(h, h'')
        | Composition(_, _, Defined _) ->
            internalfail "composition with the defined heap should not be met, it must be simplified to a simple mutation!"
        | Composition(s, ctx, h) when read ->
            let lazyInstantiator' = lazy(accessGeneralizedHeapRec exploredIds contextList lazyInstantiator read getter location accessDefined (getter s) |> fst)
            accessGeneralizedHeapRec exploredIds ((ctx, s) :: contextList) (Some lazyInstantiator'.Force) read getter location accessDefined h
        | RecursiveApplication(f, _, _) as h
                when not <| exploredIds.Contains(f) && read &&
                     independent exploredIds ImmutableHashSet.Empty
                                 (fun ids s -> accessGeneralizedHeapRec ids contextList lazyInstantiator read getter location accessDefined (getter s)) f location ->
            let r, _, _ = accessDefined contextList lazyInstantiator None false Heap.empty
            r, h
        | RecursiveApplication _
        | HigherOrderApplication _ as h ->
            let r, e, _ = accessDefined contextList lazyInstantiator (Some h) false Heap.empty
            r, if read then h else Mutation(h, e)
        | _ -> __unreachable__()

    and private accessGeneralizedHeap<'a when 'a : equality> = accessGeneralizedHeapRec<'a> ImmutableHashSet.Empty [] None

    and private hierarchicalAccess read actionNull updateDefined metadata state term =
        match term.term with
        | Error _ -> (term, state)
        | Ref(NullAddress, _) -> actionNull metadata state Null
        | Ref(TopLevelStack location, path) ->
            commonHierarchicalStackAccess read updateDefined metadata state location path
        | Ref(TopLevelHeap(addr, bT, _), path) ->
            Common.statedConditionalExecution state
                (fun state k -> k (Pointers.isZeroAddress metadata addr, state))
                (fun state k -> k (actionNull metadata state bT))
                (fun state k ->
                    let accessDefined contextList lazyInstantiator groundHeap r h = //TODO: get rid of time
                        commonHierarchicalHeapAccess read r updateDefined metadata groundHeap h contextList lazyInstantiator addr bT path {v = Timestamp.zero}
                    let result, h' = accessGeneralizedHeap read heapOf term accessDefined (heapOf state)
                    k (result, withHeap state h'))
                Merging.merge Merging.merge2Terms id id
        | Ref(TopLevelStatics location, path) ->
            let accessDefined contextList lazyInstantiator groundHeap r h =
                commonHierarchicalStaticsAccess read r updateDefined metadata groundHeap h contextList lazyInstantiator location path
            let result, m' = accessGeneralizedHeap read staticsOf term accessDefined (staticsOf state)
            result, withStatics state m'
        | Union gvs -> Merging.guardedStateMap (hierarchicalAccess read actionNull updateDefined metadata) gvs state
        | Ptr(_, _, viewType, _) ->
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
            | :? IStatedSymbolicConstantSource as source -> source.Compose ctx state
            | :? INonComposableSymbolicConstantSource -> term
            | _ -> __notImplemented__()
        | Concrete(:? concreteHeapAddress as addr', t) ->
            Concrete ctx.mtd (composeAddresses ctx.addr addr') t
        | Pointers.SymbolicThisOnStack(token, path) ->
            let id = ("this", token)
            let reference = referenceLocalVariable term.metadata state id false |> deref term.metadata state |> fst
            referenceSubLocations path reference
        | _ -> term

    and fillHoles ctx state term =
        Substitution.substitute (fillHole ctx state) (substituteTypeVariables ctx state) term

    and fillHolesInHeap fillHolesInKey ctx state heap =
        Heap.map (fun k cell -> (fillHolesInKey ctx state k, {cell with value = fillHoles ctx state cell.value})) heap

    and fillHolesInPathSegment ctx source = function
        | StructField(addr, typ) -> StructField(addr, substituteTypeVariables ctx source typ)
        | ArrayIndex(addr, typ) -> ArrayIndex(fillHoles ctx source addr, substituteTypeVariables ctx source typ)
        | ArrayLowerBound addr -> ArrayLowerBound(fillHoles ctx source addr)
        | ArrayLength addr -> ArrayLength(fillHoles ctx source addr)

    and private fillAndMutateStack (ctx : compositionContext) source target addr _ path cell =
        let time = Timestamp.compose ctx.time cell.modified
        let path = List.map (fillHolesInPathSegment ctx source) path
        let v = fillHoles ctx source cell.value
        mutateStack ctx.mtd target addr path time v

    and private fillAndMutateCommon<'a when 'a : equality> mutateHeap (fillKey : compositionContext -> state -> 'a -> 'a) (ctx : compositionContext) restricted source (target : heap<'a, term>) addr typ path cell : heap<'a, term> =
        let time = Timestamp.compose ctx.time cell.modified
        let addr = fillKey ctx source addr
        let path = List.map (fillHolesInPathSegment ctx source) path
        let v = fillHoles ctx source cell.value
        mutateHeap restricted ctx.mtd target addr typ path time v

    and private composeDefinedHeaps writer restricted s h h' =
        foldHeapLocations (writer restricted s) h h'

    and private composeGeneralizedHeaps<'key when 'key : equality> writer fillHolesInKey ctx getter setter s (h' : 'key generalizedHeap) =
        match getter s, h' with
        | Defined(r, h), Defined(r', h') ->
            assert(not r')
            composeDefinedHeaps (writer fillHolesInKey ctx) r s h h' |> Defined r
        | Merged ghs, _ ->
            let gs, hs = List.unzip ghs
            hs |> List.map (fun h -> composeGeneralizedHeaps writer fillHolesInKey ctx getter setter (setter s h) h') |> Merging.mergeGeneralizedHeaps gs
        | _, Merged ghs' ->
            let gs, hs' = List.unzip ghs'
            let gs' = List.map (fillHoles ctx s) gs
            hs' |> List.map (composeGeneralizedHeaps writer fillHolesInKey ctx getter setter s) |> Merging.mergeGeneralizedHeaps gs'
        | Defined _, Composition(s', ctx', h')
        | Mutation _, Composition(s', ctx', h')
        | Composition _, Composition(s', ctx', h') ->
            let s = composeStates ctx s s'
            composeGeneralizedHeaps writer fillHolesInKey ctx' getter setter s h'
        | Defined _, Mutation(h', h'')
        | RecursiveApplication _, Mutation(h', h'')
        | HigherOrderApplication _, Mutation(h', h'')
        | Composition _, Mutation(h', h'')
        | Mutation _, Mutation(h', h'') ->
            let res = composeGeneralizedHeaps writer fillHolesInKey ctx getter setter s h'
            let res' = fillHolesInHeap fillHolesInKey ctx s h''
            Mutation(res, res')
        | Defined _, HigherOrderApplication _
        | Defined _, RecursiveApplication _
        | Composition _, HigherOrderApplication _
        | Composition _, RecursiveApplication _
        | RecursiveApplication _, RecursiveApplication _
        | HigherOrderApplication _, HigherOrderApplication _ ->
            Composition(s, ctx, h')
        | Composition(s', ctx', h') as h, Defined(r'', h'') ->
            assert(not r'')
            match h' with
            | Defined(r, h') ->
                let ctx'' = decomposeContexts ctx ctx'
                let h = composeDefinedHeaps (writer fillHolesInKey ctx'') r s h' h'' |> Defined r
                composeGeneralizedHeaps writer fillHolesInKey ctx' getter setter s' h
            | _ ->
                let h'' = fillHolesInHeap fillHolesInKey ctx s h''
                Mutation(h, h'')
        | (HigherOrderApplication _ as h), Defined(r, h')
        | (RecursiveApplication _ as h), Defined(r, h') ->
            assert(not r)
            let h' = fillHolesInHeap fillHolesInKey ctx s h'
            Mutation(h, h')
        | Mutation(h, h'), Defined(r, h'') ->
            // TODO: this is probably wrong!
            assert(not r)
            Mutation(h, composeDefinedHeaps (writer fillHolesInKey ctx) false s h' h'')
        | RecursiveApplication _, Composition _ -> __notImplemented__()
        | HigherOrderApplication _, Composition _ -> __notImplemented__()
        | RecursiveApplication _, HigherOrderApplication _ -> __notImplemented__()
        | HigherOrderApplication _, RecursiveApplication _ -> __notImplemented__()
        | Mutation _, RecursiveApplication _ -> __notImplemented__()
        | Mutation _, HigherOrderApplication _ -> __notImplemented__()

    and composeStacksOf ctx state state' =
        (foldStackLocations (fillAndMutateStack ctx state) state state'.stack).stack

    and composeHeapsOf ctx state heap =
        composeGeneralizedHeaps (fillAndMutateCommon mutateHeap) fillHoles ctx heapOf withHeap state heap

    and composeStaticsOf ctx state statics =
        composeGeneralizedHeaps (fillAndMutateCommon mutateStatics) substituteTypeVariables ctx staticsOf withStatics state statics

    and composeStates ctx state state' =
        let stack = composeStacksOf ctx state state'
        let heap = composeHeapsOf ctx state state'.heap
        let statics = composeStaticsOf ctx state state'.statics
        assert(state'.typeVariables |> snd |> Stack.isEmpty)
        { stack = stack; heap = heap; statics = statics; frames = state.frames; pc = state.pc; typeVariables = state.typeVariables }

// ------------------------------- High-level read/write -------------------------------

    and deref metadata state location =
        hierarchicalAccess true npeTerm makePair metadata state location

    and derefWith actionNull metadata state location = hierarchicalAccess true actionNull makePair metadata state location

    and mutate metadata state reference value =
        assert(value <> Nop)
        let time = tick()
        hierarchicalAccess false npeTerm (fun _ _ -> (value, time)) metadata state reference

// ------------------------------- Referencing -------------------------------

    and private referenceTerm name followHeapRefs term =
        match term.term with
        | Ref _ when followHeapRefs -> term
        | Union gvs -> Merging.guardedMap (referenceTerm name followHeapRefs) gvs
        | _ -> StackRef term.metadata name []

    and referenceLocalVariable metadata state location followHeapRefs =
        let reference = StackRef metadata location []
        let term, _ = deref metadata state reference
        referenceTerm location followHeapRefs term

    let rec private referenceFieldOf state field parentRef reference =
        match reference.term with
        | Error _ -> reference, state
        | Ref(TopLevelHeap(addr, bT, sT), path) ->
            assert(List.isEmpty path)
            HeapRef reference.metadata addr bT sT [field], state
        | Null ->
            let term, state = State.createInstance reference.metadata typeof<System.NullReferenceException> [] state
            Error reference.metadata term, state
        | Struct _ -> referenceSubLocations [field] parentRef, state
        | Union gvs -> Merging.guardedStateMap (fun state term -> referenceFieldOf state field parentRef term) gvs state
        | t -> internalfailf "expected reference or struct, but got %O" t, state

    let rec private followOrReturnReference metadata state reference =
        let term, state = deref metadata state reference
        match term.term with
        | Error _
        | Ref _
        | Ptr _ -> term, state
        | Union gvs when List.forall (fun (_, t) -> isError t || isRefOrPtr t) gvs ->
            Merging.guardedStateMap (followOrReturnReference metadata) gvs state
        | _ -> reference, state

    let referenceField metadata state followHeapRefs name typ parentRef =
        let typ = Types.wrapReferenceType typ
        let term, state = deref metadata state parentRef
        let reference, newState = referenceFieldOf state (StructField(name, typ)) parentRef term
        if followHeapRefs then followOrReturnReference metadata newState reference
        else (reference, newState)

    let referenceStaticField metadata state followHeapRefs fieldName fieldType targetType =
        let fieldType = Types.wrapReferenceType fieldType
        let reference = StaticRef metadata targetType [StructField(fieldName, fieldType)]
        if followHeapRefs then followOrReturnReference metadata state reference
        else (reference, state)

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
                        let result = referenceSubLocations [ArrayIndex(location, elementType)] arrayRef
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
        let newEntries = { key = key; mtd = metadata; typ = typeOf term }
        let stackFrames = Stack.updateHead s.frames.f { func = frameMetadata; entries = newEntries :: oldFrame; time = frameTime }
        { s with stack = newStack; frames = { s.frames with f = stackFrames } }

    let private allocateInDefinedHeap (h : heap<'a, term>) address term time =
        h.Add(address, {value = term; created = time; modified = time })

    let rec private allocateInGeneralizedHeap address term time = function
        | Defined(r, h) -> allocateInDefinedHeap h address term time |> Defined r
        | Composition _
        | RecursiveApplication _
        | HigherOrderApplication _ as h ->
            let mutatedHeap = allocateInDefinedHeap Heap.empty address term time
            Mutation(h, mutatedHeap)
        | Mutation(gh, h) -> Mutation(gh, allocateInDefinedHeap h address term time)
        | Merged gvh ->
            Merging.commonGuardedMapk (fun h k -> k <| allocateInGeneralizedHeap address term time h) gvh
                (fun gvh ->
                    let g, h = List.unzip gvh
                    Merging.mergeGeneralizedHeaps g h) id

    let allocateInHeap metadata s term : term * state =
        let address = freshHeapLocation metadata
        let time = tick()
        let typ = typeOf term
        let ref = HeapRef metadata address typ typ []
        (ref, { s with heap = allocateInGeneralizedHeap address term time s.heap } )

    let allocateInStaticMemory (s : state) address term =
        let time = tick()
        { s with statics = allocateInGeneralizedHeap address term time s.statics }

    let makeSymbolicThis metadata state token typ =
        let isRef = isReferenceType typ
        let thisKey = ((if isRef then "this" else Pointers.symbolicThisStackKey), token)
        let thisStackRef = StackRef metadata thisKey []
        let source = {location = thisStackRef; heap = None; extractor = IdTermExtractor(); typeExtractor = IdTypeExtractor()}
        let instance = makeSymbolicInstance metadata source source "this" (wrapReferenceType typ)
        if isRef
            then instance, state, false
            else
                let key = (Pointers.symbolicThisStackKey, token)
                let state = newStackFrame state metadata (EmptyIdentifier()) [(key, Specified instance, typ)]
                referenceLocalVariable metadata state key true, state, true

// ------------------------------- Static Memory Initialization -------------------------------

    [<StructuralEquality;NoComparison>]
    type internal keyInitializedSource<'a when 'a : equality> =
        {heap : 'a generalizedHeap; key : 'a; getter : (state -> 'a generalizedHeap) transparent; fillHolesInKey : (compositionContext -> state -> 'a -> 'a) transparent }
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = seq []

    let mutable counter = 0
    let private mkKeyGuard mtd fillHolesInKey getter heap (key : 'a) =
        Constant mtd (IdGenerator.startingWith "hasKey#") ({ heap = heap; key = key; getter = {v=getter}; fillHolesInKey = {v=fillHolesInKey} } : 'a keyInitializedSource) Bool

    let private guardOfDefinedHeap mtd fillHolesInKey getter key r (h : heap<'key, term>) =
        if h.ContainsKey key then Merging.guardOf h.[key].value
        elif r then False
        else mkKeyGuard mtd fillHolesInKey getter (Defined r h) key

    let rec private guardOfHeap (exploredRecursiveIds : ImmutableHashSet<IFunctionIdentifier>) mtd fillHolesInKey getter key = function
        | Defined(r, h) -> guardOfDefinedHeap mtd fillHolesInKey getter key r h
        | Merged ghs -> Merging.guardedMap (guardOfHeap exploredRecursiveIds mtd fillHolesInKey getter key) ghs
        | Mutation(h, h') ->
            guardOfHeap exploredRecursiveIds mtd fillHolesInKey getter key h ||| guardOfDefinedHeap mtd fillHolesInKey getter key false h'
        | Composition(s, ctx, h) ->
            guardOfHeap exploredRecursiveIds mtd fillHolesInKey getter key (getter s) ||| guardOfHeap exploredRecursiveIds mtd fillHolesInKey getter (fillHolesInKey ctx s key) h
        | RecursiveApplication(f, _, _) when exploredRecursiveIds.Contains f -> False
        | RecursiveApplication(f, _, _) ->
            match Database.querySummary f with
            | Some summary ->
                guardOfHeap (exploredRecursiveIds.Add f) mtd fillHolesInKey getter key <| getter summary.state
            | None -> True
        | HigherOrderApplication _ as h ->
            mkKeyGuard mtd fillHolesInKey getter h key

    let private keyInitialized mtd key fillHolesInKey getter heap =
        guardOfHeap ImmutableHashSet<IFunctionIdentifier>.Empty mtd fillHolesInKey getter key heap

    let internal termTypeInitialized mtd termType state =
        keyInitialized mtd termType substituteTypeVariables staticsOf state.statics

    let internal termLocInitialized mtd loc state =
        keyInitialized mtd loc fillHoles heapOf state.heap

// ------------------------------- Compositions of constants -------------------------------

    type lazyInstantiation<'a when 'a : equality> with
        interface IExtractingSymbolicConstantSource with
            override x.Compose ctx state =
                let state' =
                    match x.heap with
                    | Some heap ->
                        // TODO: make it more effective (use lower-level functions to access heap directly instead of creating fresh state)
                        match x.location.term with // TODO: get rid of box someday
                        | Ref(TopLevelHeap _, _) -> { State.empty with heap = composeHeapsOf ctx state (box heap :?> term generalizedHeap) }
                        | Ref(TopLevelStatics _, _) -> { State.empty with statics = composeStaticsOf ctx state (box heap :?> termType generalizedHeap) }
                        | _ -> __notImplemented__()
                    | None -> state
                let loc = fillHoles ctx state x.location
                deref ctx.mtd state' loc |> fst |> x.extractor.Extract

    type keyInitializedSource<'a when 'a : equality> with
        interface IStatedSymbolicConstantSource with
            override x.Compose ctx state =
                keyInitialized ctx.mtd x.key x.fillHolesInKey.v x.getter.v (x.getter.v state)
