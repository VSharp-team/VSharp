namespace VSharp

open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API
open System.Collections.Generic

type WriteDependence =
    | UnboundedMutation of Term * Term
    | UnboundedAllocation of Term * Term

module Functions =
    type SymbolicLambda<'a> = State.state -> Term list -> (StatementResult * State.state -> 'a) -> 'a

    let public MakeLambda state (metadataMethod : IMetadataMethod) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromMetadataMethodSignature metadataMethod in
        let term = Concrete(lambda, typ) in
        Memory.allocateInHeap state term None

    let public MakeLambdaTerm (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromDecompiledSignature signature returnMetadataType in
        Concrete(lambda, typ)

    let public MakeLambda2 state (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let term = MakeLambdaTerm signature returnMetadataType lambda in
        if Transformations.isInlinedSignatureCall signature
            then Memory.allocateInHeap state term None
            else term, state

    let (|Lambda|_|) = function
        | Concrete(lambda, t) when Types.IsFunction t && (lambda :? SymbolicLambda<'a>) ->
            Some(Lambda(lambda :?> SymbolicLambda<'a>))
        | _ -> None

    module UnboundedRecursionCache =
        type UnboundedApproximationState = NotStarted | InProgress | Ready

        let private unboundedApproximationFinished = new Dictionary<FunctionIdentifier, bool>()
        let private unboundedApproximationAttempts = new Dictionary<FunctionIdentifier, int>()
        let private unboundedApproximationSymbolicState = new Dictionary<FunctionIdentifier, State.state>()
        let private unboundedFunctionResult = new Dictionary<FunctionIdentifier, StatementResult>()
        let private readDependencies = new Dictionary<FunctionIdentifier, (Term * Term) list>()
        let private writeDependencies = new  Dictionary<FunctionIdentifier, Memory.StateDiff list>()

        let public clear () =
            unboundedApproximationAttempts.Clear()
            unboundedApproximationSymbolicState.Clear()
            unboundedFunctionResult.Clear()
            readDependencies.Clear()
            writeDependencies.Clear()

        let private findReadDependencies terms =
            let filterMapConstant deps = function
                | Constant(_, Symbolization location, _) as term when not (List.exists (fst >> ((=) location)) deps) ->
                    Some (location, term)
                | _ -> None
            let unsorted = Terms.filterMapConstants filterMapConstant terms in
            List.sortWith (fun (loc1, _) (loc2, _) -> Memory.compareRefs loc1 loc2) unsorted

        let private overwriteReadWriteDependencies id readDeps writeDeps =
            let currentWriteDeps = writeDependencies.[id] in
            readDependencies.[id] <- readDeps
            writeDependencies.[id] <- writeDeps
            let result = List.length currentWriteDeps = List.length writeDeps
            unboundedApproximationFinished.[id] <- result
            result

        let private exceptionsFirst (xG, xS) (yG, yS) =
            match xS, yS with
            | _ when xS = yS -> 0
            | Throw _, _ -> -1
            | _, _ -> 1

        let private bubbleUpExceptions = function
            | Guarded gvs -> List.sortWith exceptionsFirst gvs |> Guarded
            | r -> r

        let rec private makeMutuallyExclusiveGuards acc res xs =
            match xs with
            | [] -> Terms.MakeNAry OperationType.LogicalAnd (List.map (fun t -> Terms.Negate t) acc) false Bool |> fun g -> g::res
            | x::xs -> Terms.MakeNAry OperationType.LogicalAnd (x::(List.map (fun t -> Terms.Negate t) acc)) false Bool |> fun g -> makeMutuallyExclusiveGuards (x::acc) (g::res) xs

        let private mutuallyExclusiveGuards guards =
            match guards with
            | [x] -> guards
            | x::xs -> makeMutuallyExclusiveGuards [] [] (List.rev xs)
            | _ -> internalfail "single guard in union"

        let rec private symbolizeUnboundedResult source id = function
            | NoResult
            | Break
            | Return Nop -> NoResult
            | Return term ->
                let resultName = IdGenerator.startingWith(toString id + "%%res") in
                let result = Memory.makeSymbolicInstance false source resultName (Terms.TypeOf term) in
                Return result
            | Throw e ->
                let resultName = IdGenerator.startingWith(toString id + "%%err") in
                let error = Memory.makeSymbolicInstance false  source resultName (Terms.TypeOf e) in
                Throw error
            | Guarded gvs ->
                let guards, results = List.unzip gvs in
                let symbolizedGuards = List.map (fun _ -> VSharp.Constant(IdGenerator.startingWith(toString id + "%%guard"), source, Bool)) guards in
                let symbolizedGuards = mutuallyExclusiveGuards symbolizedGuards in
                let symbolizedResults = List.map (symbolizeUnboundedResult source id) results in
                Guarded(List.zip symbolizedGuards symbolizedResults)
            | r -> internalfail ("unexpected result of the unbounded encoding " + toString r)

        let internal invokeUnboundedRecursion state id k =
            let sourceRef = ref Nop in
            let readDepsLocations = readDependencies.[id] |> List.unzip |> fst in
            let writeDepsLocations = writeDependencies.[id] in
            let readDeps = readDepsLocations |> List.map (Memory.deref state) in
            let writeDeps, state' = writeDepsLocations |> Memory.symbolizeLocations state sourceRef in

            let result = symbolizeUnboundedResult (UnboundedRecursion (TermRef sourceRef)) id unboundedFunctionResult.[id] in
            let isSymbolizedConstant _ = function
                | Constant (_, UnboundedRecursion (TermRef r), _) as c when LanguagePrimitives.PhysicalEquality r sourceRef -> Some c
                | _ -> None
            in
            let resultConstants = Terms.filterMapConstants isSymbolizedConstant [ControlFlow.resultToTerm result] in
            let allResults = List.append resultConstants writeDeps in

            let applicationTerm = Expression(Application id, List.append readDeps allResults, Bool) in
            sourceRef := applicationTerm
            k (result, state')

        let internal unboundedApproximationState id =
            if unboundedApproximationFinished.ContainsKey(id) then
                if (unboundedApproximationFinished.[id]) then Ready
                else InProgress
            else NotStarted

        let internal startUnboundedApproximation state id returnType =
            unboundedApproximationFinished.[id] <- false
            let symbolicState = Memory.symbolizeState state in
            unboundedApproximationAttempts.[id] <- 0
            unboundedApproximationSymbolicState.[id] <- symbolicState
            readDependencies.[id] <- []
            writeDependencies.[id] <- []
            let symbolicResult =
                match returnType with
                | Void -> NoResult
                | _ ->
                    let resultName = IdGenerator.startingWith(toString id + "%%initial-res") in
                    Memory.makeSymbolicInstance false (UnboundedRecursion (TermRef (ref Nop))) resultName returnType |> Return
            in unboundedFunctionResult.[id] <- symbolicResult
            symbolicState

        let internal approximate id result state =
            let attempt = unboundedApproximationAttempts.[id] + 1 in
            if attempt > Options.WriteDependenciesApproximationTreshold() then
                failwith "Approximating iterations limit exceeded! Either this is an iternal error or some function is really complex. Consider either increasing approximation treshold or reporing it."
            else
                unboundedApproximationAttempts.[id] <- attempt
                let symbolicState = unboundedApproximationSymbolicState.[id] in
                let writeDependencies = Memory.diff symbolicState state in
                let writeDependenciesTerms = writeDependencies |> List.map (function | Memory.Mutation (_, t) -> t | Memory.Allocation (_, t) -> t)
                let result = bubbleUpExceptions result in
                let readDependencies = findReadDependencies ((ControlFlow.resultToTerm result)::writeDependenciesTerms) in
                unboundedFunctionResult.[id] <- result
                overwriteReadWriteDependencies id readDependencies writeDependencies
