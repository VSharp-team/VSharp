namespace VSharp

open System.Collections.Generic

module Effects =
    type private Address =
        | Mutation of Term
        | ReturnAddress of Term list
        | FreshAddress of int

    type private Pattern = Term

    let private frozenEffects = new HashSet<FunctionIdentifier>()
    let private effectsOfFunctions = new Dictionary<FunctionIdentifier, IDictionary<Address, Pattern>>()

    let private isFrozen = frozenEffects.Contains

    let internal reproduce id state =
        if isFrozen id then
            NoResult Metadata.empty, state
        else
            NoResult Metadata.empty, state

    let internal initialize (id : FunctionIdentifier) =
        let mtd = Metadata.empty in
        let returnType = id.ReturnType in
        let initialSymbolicResult =
            match returnType with
            | Void -> NoResult mtd
            | _ ->
                let resultName = IdGenerator.startingWith(toString id + "%%res") in
                State.makeSymbolicInstance mtd (Memory.tick()) (SymbolicEffect (applyReturn id)) resultName returnType |> Return mtd
        in resultsOfFunctions.[id] <- initialSymbolicResult

    let internal parseEffects id startTime result state  =
        let freshLocations, mutatedLocations = Memory.affectedLocations startTime state in
        true

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
