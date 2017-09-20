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

    type private SymbolicEffectSource(apply : Lazy<(Term * State.state) option>) =
        inherit SymbolicConstantSource()
        override x.SubTerms = Seq.empty
        member x.Apply() = apply.Force()

    let private (|SymbolicEffectSource|_|) (src : SymbolicConstantSource) =
        match src with
        | :? SymbolicEffectSource as li -> Some(SymbolicEffectSource(li.Apply))
        | _ -> None

    let private convergedEffects = new HashSet<FunctionIdentifier>()
    let private returnValues = new Dictionary<FunctionIdentifier, StatementResult>()
    let private mutations = new Dictionary<FunctionIdentifier, IDictionary<Term, Term>>()

    let private isFrozen = convergedEffects.Contains >> not

    let private fillInHole mtd state term =
        match term.term with
        | Constant(_, source, _) ->
            match source with
            | Memory.LazyInstantiation(loc, isTop) ->
                Memory.derefIfInstantiated state loc |?? term
//            | SymbolicEffectSource(addr, state) when applyEffects ->
            | _ -> term
        | _ -> term

    let private fillInHoles mtd state term =
        substitute (fillInHole mtd state) term

    let private produceFrozenReturnValue mtd id state =
        let effectName = toString id + "!!ret" in
        Constant effectName (SymbolicEffectSource (lazy None)) (Types.ReturnType id) mtd |> Return mtd

    let private produceUnfrozenReturnValue mtd id state =
        assert(returnValues.ContainsKey(id))
        fillInHoles mtd state (ControlFlow.resultToTerm returnValues.[id]) |> ControlFlow.throwOrReturn

    let private produceReturnValue mtd id state =
        if isFrozen id then produceFrozenReturnValue mtd id state
        else produceUnfrozenReturnValue mtd id state

    let private produceFrozenEffect mtd id state ptr value =
        let effectName = sprintf "%O!!%s!!eff" id (State.nameOfLocation ptr) in
        Constant effectName (SymbolicEffectSource (lazy None)) (TypeOf value) mtd in

    let private produceEffect mtd id state (kvp : KeyValuePair<Term, Term>) =
        let ptr = fillInHoles mtd state kvp.Key in
        let effect =
            if isFrozen id then produceFrozenEffect mtd id state ptr kvp.Value
            else fillInHoles mtd state kvp.Value
        in (ptr, effect)

    let internal apply mtd id state k =
        let returnValue = produceReturnValue mtd id state in
        let effects =
            if mutations.ContainsKey(id) then
                mutations.[id] |> Seq.map (produceEffect mtd id state)
            else Seq.empty
        in
        let state = Seq.fold (fun state (ptr, value) -> Memory.mutate mtd state ptr value |> snd) state effects in
        k (returnValue, state)

    let private produceFreshAddressEffect metadata state loc =
        match loc.term with
        | HeapRef(((addr, typ), path), time) ->
            let apply = lazy(Some(Memory.freshHeapLocation metadata, state)) in
            (addr, Constant (IdGenerator.startingWith "fresh") (SymbolicEffectSource apply) Types.pointerType metadata) |> Some
        | _ -> None

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

    let internal parseEffects mtd id startTime result state  =
        let freshLocations, mutatedLocations = Memory.affectedLocations startTime state in
        let freshSubst = List.filterMap (fst >> produceFreshAddressEffect mtd state) freshLocations |> Dict.ofSeq in
        let subst term = Dict.tryGetValue freshSubst term term in
        // TODO: time!
        let replaceFreshLocations = fun (loc, (value, _, _)) -> (substitute subst loc, substitute subst value)
        let freshEffects = List.map replaceFreshLocations freshLocations in
        let mutatedEffects = List.map replaceFreshLocations mutatedLocations in
        let result = result |> ControlFlow.resultToTerm |> substitute subst |> ControlFlow.throwOrReturn in
        let effects = List.append freshEffects mutatedEffects |> Dict.ofSeq in
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
