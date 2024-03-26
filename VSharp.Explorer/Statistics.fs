namespace VSharp.Explorer

open System
open System.Diagnostics
open System.Collections.Concurrent
open System.IO
open System.Text
open System.Collections.Generic

open System.Threading
open System.Timers
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Utils

open CilState
open IpOperations
open CodeLocation

type pob = {loc : codeLocation; lvl : uint; pc : pathCondition}
    with
    override x.ToString() = $"loc = {x.loc}; lvl = %d{x.lvl}; pc = %s{Print.PrintPC x.pc}"

type pobStatus =
    | Unknown
    | Witnessed of cilState
    | Unreachable

type statisticsDump =
    {
        time : TimeSpan
        solverTime : TimeSpan
        internalFails : Exception list
        iies : InsufficientInformationException list
        coveringStepsInsideZone : uint
        nonCoveringStepsInsideZone : uint
        coveringStepsOutsideZone : uint
        nonCoveringStepsOutsideZone : uint
        topVisitedLocationsInZone : (codeLocation * uint) list
        topVisitedLocationsOutOfZone : (codeLocation * uint) list
    }

type generatedTestInfo =
    {
        isError : bool
        executionTime : TimeSpan
        stepsCount : uint
        coverage : double
    }

// TODO: move statistics into (unique) instances of code location!
(*
    If generalizeGenericsCoverage is true, generic methods with the same definition are considered equivalent
    when tracking covered blocks.
*)
type public SVMStatistics(entryMethods : Method seq, generalizeGenericsCoverage : bool) =

    let entryMethods = List<Method>(entryMethods)

    let totalVisited = Dictionary<codeLocation, uint>()
    let visitedWithHistory = Dictionary<codeLocation, HashSet<codeLocation>>()
    let emittedErrors = HashSet<ipStack * string * bool>()
    let emittedExceptions = HashSet<Type * string * string * bool>()

    let generatedTestInfos = List<generatedTestInfo>()

    let mutable isVisitedBlocksNotCoveredByTestsRelevant = 1
    let visitedBlocksNotCoveredByTests = Dictionary<cilState, Set<codeLocation>>()
    let blocksCoveredByTests = ConcurrentDictionary<MethodWithBody, ConcurrentDictionary<offset, unit>>()

    let unansweredPobs = List<pob>()
    let stopwatch = Stopwatch()
    let internalFails = List<Exception>()
    let iies = List<cilState>()
    let solverStopwatch = Stopwatch()

    let mutable stepsCount = 0u

    let mutable getStatesCount : (unit -> int) = (fun _ -> 0)
    let mutable getStates : (unit -> cilState seq) = (fun _ -> Seq.empty)

    let dumpTimer = new Timer()

    let mutable coveringStepsInsideZone = 0u
    let mutable nonCoveringStepsInsideZone = 0u
    let mutable coveringStepsOutsideZone = 0u
    let mutable nonCoveringStepsOutsideZone = 0u

    let mutable testsCount = 0u
    let mutable branchesReleased = false

    let generalizeIfNeeded =
        if not generalizeGenericsCoverage then
            (fun (m : Method) -> m :> MethodWithBody)
        else
            (fun (m : Method) ->
                let isGeneric = m.IsGenericMethod || m.DeclaringType <> null && m.DeclaringType.IsGenericType
                if not isGeneric then
                    m :> MethodWithBody
                else
                    m.Generalize() |> fst3
            )

    let formatTimeSpan (span : TimeSpan) =
        String.Format("{0:00}:{1:00}:{2:00}.{3}", span.Hours, span.Minutes, span.Seconds, span.Milliseconds)

    let isHeadOfBasicBlock (codeLocation : codeLocation) =
        let method = codeLocation.method
        method.CFG.IsBasicBlockStart codeLocation.offset

    let printDict' placeHolder (d : Dictionary<codeLocation, uint>) sb (m : Method, locs) =
        let sb = PrettyPrinting.appendLine sb $"%s{placeHolder}Method = %s{m.FullName}: ["
        let sb = Seq.fold (fun sb (loc : codeLocation) ->
            PrettyPrinting.appendLine sb (sprintf "%s\t\t%s <- %d" placeHolder ((int loc.offset).ToString("X")) d[loc])) sb locs
        PrettyPrinting.appendLine sb $"%s{placeHolder}]"

    let printDict placeHolder sb (d : Dictionary<codeLocation, uint>) =
        let keys = d.Keys
        let sortedKeys = keys |> Seq.sort |> Seq.groupBy (fun location -> location.method)
        Seq.fold (printDict' placeHolder d) sb sortedKeys

    let printPart (sb : StringBuilder) i (k : KeyValuePair<codeLocation, Dictionary<codeLocation, uint>>) =
        let sb = PrettyPrinting.appendLine sb $"Part %d{i}; Start from {k.Key}"
//        let sb = PrettyPrinting.appendLine sb
        printDict "\t\t" sb k.Value

    let pickTotalUnvisitedInCFG (currentLoc : codeLocation) : codeLocation option =
        let infinity = UInt32.MaxValue
        let method = currentLoc.method
        let suitable offset distance =
            let loc = { offset = offset; method = method }
            let numberOfVisit = Dict.getValueOrUpdate totalVisited loc (fun () -> 0u)
            distance <> infinity && distance <> 0u && numberOfVisit = 0u

        method.CFG.DistancesFrom currentLoc.offset
        |> Seq.sortBy (fun offsetDistancePair -> offsetDistancePair.Value)
        |> Seq.filter (fun offsetDistancePair -> suitable offsetDistancePair.Key.Offset offsetDistancePair.Value)
        |> Seq.tryHead
        |> Option.map (fun offsetDistancePair -> { offset = offsetDistancePair.Key.Offset; method = method })

    let pickUnvisitedWithHistoryInCFG (currentLoc : codeLocation) (history : codeLocation seq) : codeLocation option =
        let infinity = UInt32.MaxValue
        let method = currentLoc.method
        let suitable offset distance =
            let loc = { offset = offset; method = method }
            let totalHistory = Dict.getValueOrUpdate visitedWithHistory loc (fun () -> HashSet<_>())
            let validDistance = distance <> infinity && (distance <> 0u || method.BasicBlocksCount = 1)
            let emptyHistory = totalHistory.Count = 0

            let nontrivialHistory = Seq.exists (fun loc -> hasSiblings loc && not <| totalHistory.Contains loc) history
            validDistance && (emptyHistory || nontrivialHistory)

        method.CFG.DistancesFrom currentLoc.offset
        |> Seq.sortBy (fun offsetDistancePair -> offsetDistancePair.Value)
        |> Seq.filter (fun offsetDistancePair -> suitable offsetDistancePair.Key.Offset offsetDistancePair.Value)
        |> Seq.tryHead
        |> Option.map (fun offsetDistancePair -> { offset = offsetDistancePair.Key.Offset; method = method })

    member x.TrackStepForward (s : cilState) (ip : instructionPointer) (stackSize : int) =
        stepsCount <- stepsCount + 1u
        Logger.traceWithTag Logger.stateTraceTag $"{stepsCount} FORWARD: {s.internalId}"

        let setCoveredIfNeeded (loc : codeLocation) =
            // 'Call' instructions are considered covered only after return, because 'call' can throw exception
            if s.StackSize <= stackSize && loc.offset = loc.BasicBlock.FinalOffset then
                s.AddLocationToHistory loc

        match s.ipStack with
            // Successfully exiting method, now its call can be considered covered
            | Exit _ :: callerIp :: _ ->
                match callerIp.ToCodeLocation() with
                | Some callerLoc -> setCoveredIfNeeded callerLoc
                | None -> __unreachable__()
            | _ -> ()

        let currentLoc = ip.ToCodeLocation()
        match currentLoc with
        | Some currentLoc when isHeadOfBasicBlock currentLoc ->
            let mutable totalRef = ref 0u
            if not <| totalVisited.TryGetValue(currentLoc, totalRef) then
                totalRef <- ref 0u
                totalVisited.Add(currentLoc, 0u)

            let currentMethod = currentLoc.method
            if totalRef.Value = 0u then
                if currentMethod.InCoverageZone then coveringStepsInsideZone <- coveringStepsInsideZone + 1u
                else coveringStepsOutsideZone <- coveringStepsOutsideZone + 1u
            elif currentMethod.InCoverageZone then nonCoveringStepsInsideZone <- nonCoveringStepsInsideZone + 1u
            else nonCoveringStepsOutsideZone <- nonCoveringStepsOutsideZone + 1u

            totalVisited[currentLoc] <- totalRef.Value + 1u

            let mutable historyRef = ref null
            if not <| visitedWithHistory.TryGetValue(currentLoc, historyRef) then
                historyRef <- ref <| HashSet<_>()
                visitedWithHistory.Add(currentLoc, historyRef.Value)
            for visitedLocation in s.history do
                if hasSiblings visitedLocation then historyRef.Value.Add visitedLocation |> ignore

            let isCovered = x.IsBasicBlockCoveredByTest currentLoc
            if currentMethod.InCoverageZone && not isCovered then
                visitedBlocksNotCoveredByTests.TryAdd(s, Set.empty) |> ignore
                Interlocked.Exchange(ref isVisitedBlocksNotCoveredByTestsRelevant, 0) |> ignore
            setCoveredIfNeeded currentLoc
        | Some currentLoc -> setCoveredIfNeeded currentLoc
        | None -> ()

    member x.IsCovered (loc : codeLocation) =
       Dict.getValueOrUpdate totalVisited loc (fun () -> 0u) > 0u

    member x.GetVisitedBlocksNotCoveredByTests (s : cilState) =
        // Statistics can be updated from another thread during the loop, so the resulting statistics
        // is not guaranteed to be exact (some returned blocks can be already covered)
        if Interlocked.Exchange(ref isVisitedBlocksNotCoveredByTestsRelevant, 1) = 0 then
            let currentCilStates = visitedBlocksNotCoveredByTests.Keys |> Seq.toList
            for cilState in currentCilStates do
                let history = Set.filter (not << x.IsBasicBlockCoveredByTest) cilState.history
                visitedBlocksNotCoveredByTests[cilState] <- history

        let blocks = ref Set.empty
        if visitedBlocksNotCoveredByTests.TryGetValue(s, blocks) then blocks.Value
        else Set.empty

    member x.SetBasicBlocksAsCoveredByTest (blocks : codeLocation seq) =
        let mutable coveredBlocks = ref null
        let mutable hasNewCoverage = false
        let blocks = Seq.distinct blocks
        for block in blocks do
            let generalizedMethod = generalizeIfNeeded block.method
            let method = block.method
            let mutable isNewBlock = false
            if blocksCoveredByTests.TryGetValue(generalizedMethod, coveredBlocks) then
                isNewBlock <- coveredBlocks.Value.TryAdd(block.offset, ())
            else
                let coveredBlocks = ConcurrentDictionary()
                isNewBlock <- true
                coveredBlocks.TryAdd(block.offset, ()) |> ignore
                blocksCoveredByTests[generalizedMethod] <- coveredBlocks
            if method.InCoverageZone then
                Interlocked.Exchange(ref isVisitedBlocksNotCoveredByTestsRelevant, 0) |> ignore
            hasNewCoverage <- hasNewCoverage || isNewBlock && method.InCoverageZone
        hasNewCoverage

    member x.IsBasicBlockCoveredByTest (blockEnd : codeLocation) =
        let mutable coveredBlocks = ref null
        if blocksCoveredByTests.TryGetValue(generalizeIfNeeded blockEnd.method, coveredBlocks) then
            coveredBlocks.Value.ContainsKey blockEnd.offset
        else false

    member x.GetCurrentCoverage (methods : Method seq) =
        let getCoveredInstructionsCount (m : Method) =
            let generalizedMethod = generalizeIfNeeded m
            let mutable coveredBlocksOffsets = ref null
            if blocksCoveredByTests.TryGetValue(generalizedMethod, coveredBlocksOffsets) then
                let cfg = m.CFG
                coveredBlocksOffsets.Value |> Seq.sumBy (fun o -> (cfg.ResolveBasicBlock o.Key).BlockSize)
            else 0
        let methodsInZone = List.ofSeq methods |> List.filter (fun m -> m.InCoverageZone)
        let totalInstructionsCount = methodsInZone |> List.sumBy (fun m -> m.CFG.MethodSize)
        let coveredInstructionsCount = methodsInZone |> List.sumBy getCoveredInstructionsCount
        if totalInstructionsCount <> 0 then
            double coveredInstructionsCount / double totalInstructionsCount * 100.0
        else 0.0

    member x.GetCurrentCoverage (method : Method) =
        x.GetCurrentCoverage(Seq.singleton method)

    member x.GetCurrentCoverage() = x.GetCurrentCoverage(entryMethods)

    member x.OnBranchesReleased() =
        branchesReleased <- true

    member x.TrackFinished (s : cilState, isError) =
        testsCount <- testsCount + 1u
        x.SetBasicBlocksAsCoveredByTest s.history |> ignore
        let generatedTestInfo =
            {
                isError = isError
                executionTime = x.CurrentExplorationTime
                stepsCount = x.StepsCount
                coverage = x.GetCurrentCoverage()
            }
        generatedTestInfos.Add generatedTestInfo
        Logger.traceWithTag Logger.stateTraceTag $"FINISH: {s.internalId}"

    member x.IsNewError (s : cilState) (errorMessage : string) isFatal =
        let state = s.state
        match state.exceptionsRegister.Peek with
        | Unhandled(term, _, stackTrace) ->
            let exceptionType = MostConcreteTypeOfRef state term
            emittedExceptions.Add(exceptionType, stackTrace, errorMessage, isFatal)
        | _ -> emittedErrors.Add(s.ipStack, errorMessage, isFatal)

    member x.TrackStepBackward (pob : pob) (cilState : cilState) =
        // TODO
        ()

    member x.TrackFork (parent : cilState) (children : cilState seq) =
        for child in children do
            Logger.traceWithTag Logger.stateTraceTag $"BRANCH: {parent.internalId} -> {child.internalId}"

        let blocks = ref Set.empty
        // TODO: check why 'parent' may not be in 'visitedBlocksNotCoveredByTests'
        if visitedBlocksNotCoveredByTests.TryGetValue(parent, blocks) then
            let parentBlocks = blocks.Value
            for child in children do
                visitedBlocksNotCoveredByTests[child] <- parentBlocks

    member x.AddUnansweredPob (p : pob) = unansweredPobs.Add(p)

    member x.Reset (newEntryMethods : Method seq) =
        entryMethods.Clear()
        entryMethods.AddRange newEntryMethods

        totalVisited.Clear()
        unansweredPobs.Clear()
        internalFails.Clear()
        iies.Clear()
        solverStopwatch.Reset()

        coveringStepsInsideZone <- 0u
        nonCoveringStepsInsideZone <- 0u
        coveringStepsOutsideZone <- 0u
        nonCoveringStepsOutsideZone <- 0u

        branchesReleased <- false
        testsCount <- 0u

    member x.SolverStarted() = solverStopwatch.Start()
    member x.SolverStopped() = solverStopwatch.Stop()

    member x.ExplorationStarted() = stopwatch.Start()
    member x.ExplorationFinished() = stopwatch.Stop()

    member x.SetStatesCountGetter(getter : unit -> int) =
        getStatesCount <- getter

    member x.SetStatesGetter(getter : unit -> cilState seq) =
        getStates <- getter

    member x.PickTotalUnvisitedInMethod loc = pickTotalUnvisitedInCFG loc

    member x.PickUnvisitedWithHistoryInCFG (loc, history) = pickUnvisitedWithHistoryInCFG loc history

    member x.CurrentExplorationTime with get() = stopwatch.Elapsed

    member x.IncompleteStates with get() = iies

    member x.InternalFails with get() = internalFails

    member x.StepsCount with get() = stepsCount

    member x.GeneratedTestInfos with get() : IReadOnlyCollection<generatedTestInfo> = generatedTestInfos

    member x.DumpStatistics() =
        let topN = 5
        let topVisitedByMethods =
            totalVisited
            |> Seq.groupBy (fun kvp -> kvp.Key.method)
            |> Seq.map (snd >> Seq.maxBy (fun kvp -> kvp.Value))
            |> Seq.sortByDescending (fun kvp -> kvp.Value)
        let topVisitedByMethodsInZone = topVisitedByMethods |> Seq.filter (fun kvp -> kvp.Key.method.InCoverageZone) |> Seq.truncate topN
        let topVisitedByMethodsOutOfZone = topVisitedByMethods |> Seq.filter (fun kvp -> not kvp.Key.method.InCoverageZone) |> Seq.truncate topN
        {
            time = stopwatch.Elapsed
            solverTime = solverStopwatch.Elapsed
            internalFails = internalFails |> List.ofSeq
            iies = iies |> Seq.map (fun s -> s.iie.Value) |> List.ofSeq
            coveringStepsInsideZone = coveringStepsInsideZone
            nonCoveringStepsInsideZone = nonCoveringStepsInsideZone
            coveringStepsOutsideZone = coveringStepsOutsideZone
            nonCoveringStepsOutsideZone = nonCoveringStepsOutsideZone
            topVisitedLocationsInZone = topVisitedByMethodsInZone |> Seq.map (|KeyValue|) |> List.ofSeq
            topVisitedLocationsOutOfZone = topVisitedByMethodsOutOfZone |> Seq.map (|KeyValue|) |> List.ofSeq
        }

    member x.CommonPrintStatistics (writer : TextWriter) (statisticsDump : statisticsDump) =
        writer.WriteLine($"Total time: {formatTimeSpan statisticsDump.time}.")
        writer.WriteLine($"Total coverage: {x.GetCurrentCoverage()}")
        if not <| List.isEmpty statisticsDump.internalFails then
            writer.WriteLine()
            writer.WriteLine()
            writer.WriteLine("{0} error(s) occured!", statisticsDump.internalFails.Length)
            let sortedInternalFails = Dictionary<string, int>()
            statisticsDump.internalFails |> List.iter (fun e ->
                let failMessage = e.Message
                let count = ref 1
                if sortedInternalFails.TryGetValue(failMessage, count) then
                   sortedInternalFails[failMessage] <- count.Value + 1
                else
                    sortedInternalFails.Add(failMessage, 1))
            sortedInternalFails |> Seq.sortByDescending (fun kvp -> kvp.Value) |> Seq.iter (fun kvp ->
                writer.WriteLine("---------------------- {0} time(s): ----------------------", kvp.Value)
                writer.WriteLine(kvp.Key))
        if not <| List.isEmpty statisticsDump.iies then
            writer.WriteLine()
            writer.WriteLine()
            writer.WriteLine("{0} branch(es) with insufficient input information!", iies.Count)
            statisticsDump.iies |> List.iter (fun iie -> writer.WriteLine iie.Message)

    member x.PrintStatistics (writer : TextWriter) =
        x.CommonPrintStatistics writer <| x.DumpStatistics()

    member x.PrintDebugStatistics (writer : TextWriter) =
        let dump = x.DumpStatistics()
        x.CommonPrintStatistics writer dump
        let solverTimePercent = float dump.solverTime.TotalMilliseconds / float dump.time.TotalMilliseconds * 100.0
        writer.WriteLine($"Solver time percent: {solverTimePercent:F1}%% ({formatTimeSpan dump.solverTime})")
        writer.WriteLine("Covering steps inside coverage zone: {0}", dump.coveringStepsInsideZone)
        writer.WriteLine("Revisiting steps inside coverage zone: {0}", dump.nonCoveringStepsInsideZone)
        writer.WriteLine("Covering steps outside coverage zone: {0}", dump.coveringStepsOutsideZone)
        writer.WriteLine("Revisiting steps outside coverage zone: {0}", dump.nonCoveringStepsOutsideZone)
        if not <| List.isEmpty dump.topVisitedLocationsInZone then
            writer.WriteLine("Top {0} visited locations (one per method) in zone:", Seq.length dump.topVisitedLocationsInZone)
        for loc, times in dump.topVisitedLocationsInZone do
            writer.WriteLine("  offset {0} of {1}: {2} time{3}",
                                (int loc.offset).ToString("X"), loc.method.FullName, times,
                                (if times = 1u then "" else "s"))
        if not <| List.isEmpty dump.topVisitedLocationsOutOfZone then
            writer.WriteLine("Top {0} visited locations (one per method) out of zone:", Seq.length dump.topVisitedLocationsOutOfZone)
        for loc, times in dump.topVisitedLocationsOutOfZone do
            writer.WriteLine("  offset {0} of {1}: {2} time{3}",
                                (int loc.offset).ToString("X"), loc.method.FullName, times,
                                (if times = 1u then "" else "s"))

    interface IDisposable with
        member x.Dispose() = dumpTimer.Dispose()
