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

    let public MakeLambda2 state (signature : IFunctionSignature) (returnMetadataType : IMetadataType) (lambda : SymbolicLambda<'a>) =
        let typ = Types.FromDecompiledSignature signature returnMetadataType in
        let term = Concrete(lambda, typ) in
        Memory.allocateInHeap state term None

    let (|Lambda|_|) = function
        | Concrete(lambda, t) when Types.IsFunction t && (lambda :? SymbolicLambda<'a>) ->
            Some(Lambda(lambda :?> SymbolicLambda<'a>))
        | _ -> None

    module UnboundedRecursionCache =

        let private unboundedApproximationAttempts = new Dictionary<FunctionIdentifier, int>()
        let private unboundedApproximationSymbolicState = new Dictionary<FunctionIdentifier, State.state>()
        let private unboundedFunctionResult = new Dictionary<FunctionIdentifier, StatementResult>()
        let private readDependencies = new Dictionary<FunctionIdentifier, (Term * Term) list>()
        let private writeDependencies = new  Dictionary<FunctionIdentifier, Memory.StateDiff list>()
//        let private unboundedFunctions = new Dictionary<FunctionIdentifier, obj>()

        let public clear () =
            unboundedApproximationAttempts.Clear()
            unboundedApproximationSymbolicState.Clear()
            unboundedFunctionResult.Clear()
            readDependencies.Clear()
            writeDependencies.Clear()
//            unboundedFunctions.Clear()

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
            List.length currentWriteDeps = List.length writeDeps

        let rec private symbolizeUnboundedResult source id state = function
            | NoResult
            | Return Nop -> (NoResult, state)
            | Return term ->
                let resultName = IdGenerator.startingWith(toString id + "%%res") in
                let result, state = Memory.makeSymbolicInstance false (UnboundedRecursion source) resultName state (Terms.TypeOf term) in
                Return result, state
            | Throw e ->
                let resultName = IdGenerator.startingWith(toString id + "%%err") in
                let error, state = Memory.makeSymbolicInstance false  (UnboundedRecursion source) resultName state (Terms.TypeOf e) in
                Throw error, state
            | Guarded gvs ->
                let guards, results = List.unzip gvs in
                let symbolizedGuards = List.map (fun _ -> VSharp.Constant(IdGenerator.startingWith(toString id + "%%guard"), UnboundedRecursion source, Bool)) guards in
                let symbolizedResults, state = List.mapFold (symbolizeUnboundedResult source id) state results in
                Guarded(List.zip symbolizedGuards symbolizedResults), state
            | r -> internalfail ("unexpected result of the unbounded encoding " + toString r)

        let internal invokeUnboundedRecursion state id k =
            let readDepsLocations = readDependencies.[id] |> List.unzip |> fst in
            let writeDepsLocations = writeDependencies.[id] in
            let readDeps = readDepsLocations |> List.map (Memory.deref state) in
            let writeDeps, state = writeDepsLocations |> Memory.symbolizeLocations state in
            let sourceRef = ref Nop in

            let result, state = symbolizeUnboundedResult sourceRef id state unboundedFunctionResult.[id] in
            let isSymbolizedConstant _ = function
                | Constant (_, UnboundedRecursion r, _) as c when LanguagePrimitives.PhysicalEquality r sourceRef -> Some c
                | _ -> None
            in
            let resultConstants = Terms.filterMapConstants isSymbolizedConstant [ControlFlow.resultToTerm result] in
            let allResults = List.append resultConstants writeDeps in

            let applicationTerm = Expression(Application id, List.append readDeps allResults, Bool) in
            sourceRef := applicationTerm
            k (result, state)

        let internal isUnboundedApproximationStarted = unboundedFunctionResult.ContainsKey

        let internal symbolizedState funcId = unboundedApproximationSymbolicState.[funcId]

        let internal startUnboundedApproximation state id returnType =
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
                    Memory.makeSymbolicInstance false (UnboundedRecursion (ref Nop)) resultName state returnType |> fst |> Return
            in unboundedFunctionResult.[id] <- symbolicResult

        let public approximate id result state = //k =
            let attempt = unboundedApproximationAttempts.[id] + 1 in
            if attempt > Options.WriteDependenciesApproximationTreshold() then
                failwith "Approximating iterations limit exceeded! Either this is an iternal error or some function is really complex. Consider either increasing approximation treshold or reporing it."
            else
                unboundedApproximationAttempts.[id] <- attempt
                let symbolicState = unboundedApproximationSymbolicState.[id] in
                let writeDependencies = Memory.diff symbolicState state in
                let writeDependenciesTerms = writeDependencies |> List.map (function | Memory.Mutation (_, t) -> t | Memory.Allocation (_, t) -> t)
                let readDependencies = findReadDependencies ((ControlFlow.resultToTerm result)::writeDependenciesTerms) in
                let approximationDone = overwriteReadWriteDependencies id readDependencies writeDependencies in
                if approximationDone then
                    unboundedFunctionResult.[id] <- result
                approximationDone
