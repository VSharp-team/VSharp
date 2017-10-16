namespace VSharp

open System.Collections.Generic

module Effects =

//    type Effect =
//        | FreshAddress
//        | Sketch of Term

//    type private SymbolicEffectSource(address : Effect, state : State.state) =
//        inherit SymbolicConstantSource()
//        override x.SubTerms = Seq.empty
//        member x.Address = address
//        member x.State = state

    type ConcreteHeapAddress = int list

    let private composeStates (s1 : State.state) (s2 : State.state) =
        s1

    let private composeAddresses (a1 : ConcreteHeapAddress) (a2 : ConcreteHeapAddress) =
        List.append a1 a2

    let private composeTime (t1 : Timestamp) (t2 : Timestamp) =
        // TODO
        t1

    type internal SymbolicEffectContext = { state : State.state; address : ConcreteHeapAddress; time : Timestamp } with
        member x.Compose(prefix : SymbolicEffectContext) =
            { state = composeStates prefix.state x.state; address = composeAddresses prefix.address x.address; time = composeTime prefix.time x.time }

    type private SymbolicEffectSource(id : string, ctx : SymbolicEffectContext) =
        inherit SymbolicConstantSource()
        override x.SubTerms = Seq.empty
        member x.Id = id
        member x.Context = ctx

    let private (|SymbolicEffectApplication|_|) src =
        match src.term with
        | Constant(_, (:? SymbolicEffectSource as li), _) -> Some(SymbolicEffectApplication(li))
        | _ -> None

    type private FreshAddressMarker() = class end
    let private freshAddressMarker = FreshAddressMarker()

    let private convergedEffects = new HashSet<FunctionIdentifier>()
    let private returnValues = new Dictionary<FunctionIdentifier, StatementResult>()
    let private mutations = new Dictionary<FunctionIdentifier, IDictionary<Term, MemoryCell<Term>>>()
    let private storage = new Dictionary<string, MemoryCell<Term>>()

    let private isFrozen = convergedEffects.Contains >> not

    let rec private fillInHole mtd ctx term =
        match term.term with
        | Constant(_, source, _) ->
            match source with
            | Memory.LazyInstantiation(loc, isTop) ->
                Memory.derefIfInstantiated ctx.state loc |?? term
            | :? SymbolicEffectSource as e ->
                apply e mtd ctx
            | _ -> term
        | Concrete(:? ConcreteHeapAddress as addr, t) when term.metadata.misc.Contains(freshAddressMarker) ->
            Concrete mtd (composeAddresses ctx.address addr) t
        | _ -> term

    and private fillInHoles mtd ctx term =
        substitute (fillInHole mtd ctx) term

    and private apply (src : SymbolicEffectSource) mtd prefix =
        let composedCtx = src.Context.Compose(prefix) in
        assert(storage.ContainsKey(src.Id))
        fillInHoles mtd composedCtx (fst3 storage.[src.Id])

    type private SymbolicEffectSource with
        member x.Apply mtd prefix = apply x mtd prefix

    let private produceFrozenReturnValue mtd id ctx =
        let effectName = toString id + "!!ret" in
        Constant mtd effectName (SymbolicEffectSource(effectName, ctx)) (Types.ReturnType id) |> Return mtd

    let private produceUnfrozenReturnValue mtd id ctx =
        assert(returnValues.ContainsKey(id))
        fillInHoles mtd ctx (ControlFlow.resultToTerm returnValues.[id]) |> ControlFlow.throwOrReturn

    let private produceReturnValue mtd id ctx =
        if isFrozen id then produceFrozenReturnValue mtd id ctx
        else produceUnfrozenReturnValue mtd id ctx

    let private produceFrozenEffect mtd id ctx ptr value =
        let effectName = sprintf "%O!!%s!!eff" id (State.nameOfLocation ptr) in
        Constant mtd effectName (SymbolicEffectSource(effectName, ctx)) (TypeOf value) in

    let private produceEffect mtd id ctx (kvp : KeyValuePair<Term, MemoryCell<Term>>) =
        let ptr = fillInHoles mtd ctx kvp.Key in
        let effect =
            if isFrozen id then produceFrozenEffect mtd id ctx ptr (fst3 kvp.Value)
            else fillInHoles mtd ctx (fst3 kvp.Value)
        in (ptr, effect)

    let internal invoke mtd id ctx k =
        let returnValue = produceReturnValue mtd id ctx in
        let effects =
            if mutations.ContainsKey(id) then
                mutations.[id] |> Seq.map (produceEffect mtd id ctx)
            else Seq.empty
        in
        let state = Seq.fold (fun state (ptr, value) -> Memory.mutate mtd state ptr value |> snd) ctx.state effects in
        k (returnValue, state)

    let private produceFreshAddressEffect = function
        | ConcreteT(:? ConcreteHeapAddress, _) as t -> Metadata.addMisc t freshAddressMarker
        | t -> ()

//    let private produceFreshAddressEffect metadata state loc =
//        match loc.term with
//        | HeapRef(((addr, typ), path), time) ->
//            (addr, Concrete addr Types.pointerType metadata) |> Some
//        | _ -> None

    //let internal initialize (id : FunctionIdentifier) =
    //    let mtd = Metadata.empty in
    //    let returnType = Types.ReturnType id in
        //let initialSymbolicResult =
        //    match returnType with
        //    | Void -> NoResult mtd
        //    | _ ->
        //        let resultName = IdGenerator.startingWith(toString id + "%%res") in
        //        Memory.makeSymbolicInstance mtd (Memory.tick()) (SymbolicEffectSource ) resultName returnType |> Return mtd
        //in resultsOfFunctions.[id] <- initialSymbolicResult

    let internal parseEffects mtd id startTime result state =
        let freshLocations, mutatedLocations = Memory.affectedLocations startTime state in
//        let freshSubst = List.filterMap (fst >> produceFreshAddressEffect mtd state) freshLocations |> Dict.ofSeq in
//        let subst term = Dict.tryGetValue freshSubst term term in
        // TODO: time!
//        let replaceFreshLocations = fun (loc, (value, _, _)) -> (substitute subst loc, substitute subst value)
//        let freshEffects = List.map replaceFreshLocations freshLocations in
//        let mutatedEffects = List.map replaceFreshLocations mutatedLocations in
//        let result = result |> ControlFlow.resultToTerm |> substitute subst |> ControlFlow.throwOrReturn in
        let markFresh = Terms.iter produceFreshAddressEffect in
        freshLocations |> Seq.iter (fun (k, (v, _, _)) -> markFresh k; markFresh v)
        mutatedLocations |> Seq.iter (fun (k, (v, _, _)) -> markFresh k; markFresh v)
        result |> ControlFlow.resultToTerm |> markFresh
        let effects = List.append freshLocations mutatedLocations |> Dict.ofSeq in
        let resultsConverged = returnValues.ContainsKey(id) && returnValues.[id] = result in
        let effectsConverged = mutations.ContainsKey(id) && Dict.equals mutations.[id] effects in
        returnValues.[id] <- result
        mutations.[id] <- effects
        resultsConverged && effectsConverged && convergedEffects.Add(id)

//        let private findReadDependencies terms =
//            let filterMapConstant deps = function
//                | Constant(_, LazyInstantiation location, _) as term when not (List.exists (fst >> ((=) location)) deps) ->
//                    Some (location, term)
//                | _ -> None
//            let unsorted = Terms.filterMapConstants filterMapConstant terms in
//            List.sortWith (fun (loc1, _) (loc2, _) -> Memory.compareRefs loc1 loc2) unsorted
//
//        let private overwriteReadWriteDependencies id readDeps writeDeps =
//            let currentWriteDeps = writeDependencies.[id] in
//            readDependencies.[id] <- readDeps
//            writeDependencies.[id] <- writeDeps
//            let result = List.length currentWriteDeps = List.length writeDeps
//            unboundedApproximationFinished.[id] <- result
//            result
//
//        let private exceptionsFirst (xG, xS) (yG, yS) =
//            match xS, yS with
//            | _ when xS = yS -> 0
//            | Throw _, _ -> -1
//            | _, _ -> 1
//
//        let private bubbleUpExceptions = function
//            | Guarded gvs -> List.sortWith exceptionsFirst gvs |> Guarded
//            | r -> r
//
//        let rec private makeMutuallyExclusiveGuards acc res xs =
//            match xs with
//            | [] -> Terms.MakeNAry OperationType.LogicalAnd (List.map (fun t -> Terms.Negate t) acc) false Bool |> fun g -> g::res
//            | x::xs -> Terms.MakeNAry OperationType.LogicalAnd (x::(List.map (fun t -> Terms.Negate t) acc)) false Bool |> fun g -> makeMutuallyExclusiveGuards (x::acc) (g::res) xs
//
//        let private mutuallyExclusiveGuards guards =
//            match guards with
//            | [] -> internalfail "empty guard"
//            | [x] -> guards
//            | x::xs -> makeMutuallyExclusiveGuards [] [] (List.rev xs)
//
//        let rec private symbolizeUnboundedResult source id = function
//            | NoResult
//            | Break
//            | Return Nop -> NoResult
//            | Return term ->
//                let resultName = IdGenerator.startingWith(toString id + "%%res") in
//                // TODO: time!
//                let result = State.makeSymbolicInstance 0u source resultName (Terms.TypeOf term) in
//                Return result
//            | Throw e ->
//                let resultName = IdGenerator.startingWith(toString id + "%%err") in
//                // TODO: time!
//                let error = State.makeSymbolicInstance 0u source resultName (Terms.TypeOf e) in
//                Throw error
//            | Guarded gvs ->
//                let guards, results = List.unzip gvs in
//                let symbolizedGuards = List.map (fun _ -> VSharp.Constant(IdGenerator.startingWith(toString id + "%%guard"), source, Bool)) guards in
//                let symbolizedGuards = mutuallyExclusiveGuards symbolizedGuards in
//                let symbolizedResults = List.map (symbolizeUnboundedResult source id) results in
//                Guarded(List.zip symbolizedGuards symbolizedResults)
//            | r -> internalfail ("unexpected result of the unbounded encoding " + toString r)
//
//        let internal invokeUnboundedRecursion state id k =
//            let sourceRef = ref Nop in
//            let readDepsLocations = readDependencies.[id] |> List.unzip |> fst in
//            let writeDepsLocations = writeDependencies.[id] in
//            let readDeps = readDepsLocations |> List.map (Memory.deref state) |> List.unzip |> fst in
////            let writeDeps, state' = writeDepsLocations |> Memory.symbolizeLocations state sourceRef in
//            let writeDeps, state' = [], state in
//
//            let result = symbolizeUnboundedResult (UnboundedRecursion (TermRef sourceRef)) id unboundedFunctionResult.[id] in
//            let isSymbolizedConstant _ = function
//                | Constant (_, UnboundedRecursion (TermRef r), _) as c when LanguagePrimitives.PhysicalEquality r sourceRef -> Some c
//                | _ -> None
//            in
//            let resultConstants = Terms.filterMapConstants isSymbolizedConstant [ControlFlow.resultToTerm result] in
//            let allResults = List.append resultConstants writeDeps in
//
//            let applicationTerm = Expression(Application id, List.append readDeps allResults, Bool) in
//            sourceRef := applicationTerm
//            k (result, state')
