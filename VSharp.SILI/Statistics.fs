namespace VSharp.Interpreter.IL

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Text
open System.Collections.Generic

open System.Timers
open FSharpx.Collections
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Utils

open CilStateOperations
open ipOperations
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

type continuousStatistics =
    {
        millis: int64
        coveringStepsInsideZone : uint
        nonCoveringStepsInsideZone : uint
        coveringStepsOutsideZone : uint
        nonCoveringStepsOutsideZone : uint
        testsCount : uint
        branchesReleased : bool
        internalFailsCount : uint
        statesCount : int
        coveringStatesCount : uint
    }

// TODO: move statistics into (unique) instances of code location!
type public SILIStatistics(statsDumpIntervalMs : int) as this =
    let startIp2currentIp = Dictionary<codeLocation, Dictionary<codeLocation, uint>>()
    let totalVisited = Dictionary<codeLocation, uint>()
    let visitedWithHistory = Dictionary<codeLocation, HashSet<codeLocation>>()
    let emittedErrors = HashSet<codeLocation * string>()

    let mutable isVisitedBlocksNotCoveredByTestsRelevant = true
    let visitedBlocksNotCoveredByTests = Dictionary<cilState, Set<codeLocation>>()

    let unansweredPobs = List<pob>()
    let stopwatch = Stopwatch()
    let internalFails = List<Exception>()
    let iies = List<cilState>()
    let solverStopwatch = Stopwatch()

    let mutable getStatesCount : (unit -> int) = (fun _ -> 0)
    let mutable getStates : (unit -> cilState seq) = (fun _ -> Seq.empty)

    let dumpTimer = new Timer()

    let mutable coveringStepsInsideZone = 0u
    let mutable nonCoveringStepsInsideZone = 0u
    let mutable coveringStepsOutsideZone = 0u
    let mutable nonCoveringStepsOutsideZone = 0u

    let mutable testsCount = 0u
    let mutable branchesReleased = false

    let collectContinuousStatistics = statsDumpIntervalMs > 0
    let continuousStatistics = List<continuousStatistics>()

    let formatTimeSpan (span : TimeSpan) =
        String.Format("{0:00}:{1:00}:{2:00}.{3}", span.Hours, span.Minutes, span.Seconds, span.Milliseconds)

    let isHeadOfBasicBlock (codeLocation : codeLocation) =
        let method = codeLocation.method
        if method.HasBody then
            method.CFG.SortedOffsets.BinarySearch(codeLocation.offset) >= 0
        else false

    let printDict' placeHolder (d : Dictionary<codeLocation, uint>) sb (m : Method, locs) =
        let sb = PrettyPrinting.appendLine sb $"%s{placeHolder}Method = %s{m.FullName}: ["
        let sb = Seq.fold (fun sb (loc : codeLocation) ->
            PrettyPrinting.appendLine sb (sprintf "%s\t\t%s <- %d" placeHolder ((int loc.offset).ToString("X")) d.[loc])) sb locs
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

        if method.HasBody then
            method.CFG.DistancesFrom currentLoc.offset
            |> Seq.sortBy (fun offsetDistancePair -> offsetDistancePair.Value)
            |> Seq.filter (fun offsetDistancePair -> suitable offsetDistancePair.Key offsetDistancePair.Value)
            |> Seq.tryHead
            |> Option.map (fun offsetDistancePair -> { offset = offsetDistancePair.Key; method = method })
        else None

    let pickUnvisitedWithHistoryInCFG (currentLoc : codeLocation) (history : codeLocation seq) : codeLocation option =
        let infinity = UInt32.MaxValue
        let method = currentLoc.method
        let suitable offset distance =
            let loc = { offset = offset; method = method }
            let totalHistory = Dict.getValueOrUpdate visitedWithHistory loc (fun () -> HashSet<_>())
            let validDistance = distance <> infinity && distance <> 0u
            let emptyHistory = totalHistory.Count = 0

            let nontrivialHistory = Seq.exists (fun loc -> hasSiblings loc && not <| totalHistory.Contains loc) history
            validDistance && (emptyHistory || nontrivialHistory)

        if method.HasBody then
            method.CFG.DistancesFrom currentLoc.offset
            |> Seq.sortBy (fun offsetDistancePair -> offsetDistancePair.Value)
            |> Seq.filter (fun offsetDistancePair -> suitable offsetDistancePair.Key offsetDistancePair.Value)
            |> Seq.tryHead
            |> Option.map (fun offsetDistancePair -> { offset = offsetDistancePair.Key; method = method })
        else None

    let printStatistics (writer : TextWriter) (statisticsDump : statisticsDump) =
        writer.WriteLine($"Total time: {formatTimeSpan statisticsDump.time}.")
        if not <| List.isEmpty statisticsDump.internalFails then
            writer.WriteLine()
            writer.WriteLine()
            writer.WriteLine("{0} error(s) occured!", statisticsDump.internalFails.Length)
            let sortedInternalFails = Dictionary<string, int>()
            statisticsDump.internalFails |> List.iter (fun e ->
                let failMessage = e.Message
                let count = ref 1
                if sortedInternalFails.TryGetValue(failMessage, count) then
                   sortedInternalFails.[failMessage] <- count.Value + 1
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

    let continuousDumpEventHandler = ElapsedEventHandler(fun _ _ -> this.CreateContinuousDump())

    member x.TrackStepForward (s : cilState) =
        let startLoc = ip2codeLocation s.startingIP
        let currentLoc = ip2codeLocation (currentIp s)
        match startLoc, currentLoc with
        | Some startLoc, Some currentLoc when isHeadOfBasicBlock currentLoc ->
            let mutable startRefDict = ref null
            if not <| startIp2currentIp.TryGetValue(startLoc, startRefDict) then
                startRefDict <- ref (Dictionary<codeLocation, uint>())
                startIp2currentIp.Add(startLoc, startRefDict.Value)
            let startDict = startRefDict.Value

            let mutable currentRef = ref 0u
            if not <| startDict.TryGetValue(currentLoc, currentRef) then
                currentRef <- ref 0u
                startDict.Add(currentLoc, 0u)
            startDict.[currentLoc] <- currentRef.Value + 1u

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

            totalVisited.[currentLoc] <- totalRef.Value + 1u

            let mutable historyRef = ref null
            if not <| visitedWithHistory.TryGetValue(currentLoc, historyRef) then
                historyRef <- ref <| HashSet<_>()
                visitedWithHistory.Add(currentLoc, historyRef.Value)
            for visitedState in s.history do
                if hasSiblings visitedState then historyRef.Value.Add visitedState |> ignore

            if currentMethod.InCoverageZone && not <| x.IsBasicBlockCoveredByTest coverageType.ByTest currentLoc then
                if visitedBlocksNotCoveredByTests.ContainsKey s |> not then
                    visitedBlocksNotCoveredByTests.[s] <- Set.empty
                isVisitedBlocksNotCoveredByTestsRelevant <- false

            setBasicBlockIsVisited s currentLoc
        | _ -> ()

    member x.IsCovered (loc : codeLocation) =
       Dict.getValueOrUpdate totalVisited loc (fun () -> 0u) > 0u

    member x.GetVisitedBlocksNotCoveredByTests (s : cilState) =
        if not isVisitedBlocksNotCoveredByTestsRelevant then
            for kvp in visitedBlocksNotCoveredByTests do
                visitedBlocksNotCoveredByTests.[kvp.Key] <- kvp.Key.history |> Set.filter (not << x.IsBasicBlockCoveredByTest coverageType.ByTest)
            isVisitedBlocksNotCoveredByTestsRelevant <- true

        if visitedBlocksNotCoveredByTests.ContainsKey s then visitedBlocksNotCoveredByTests.[s] else Set.empty

    member x.IsBasicBlockCoveredByTest (coverageType : coverageType) (blockStart : codeLocation) =
        match coverageType with
        | ByTest -> blockStart.method.BlocksCoveredByTests.Contains blockStart.offset
        | ByEntryPointTest -> blockStart.method.BlocksCoveredFromEntryPoint.Contains blockStart.offset

    member x.GetApproximateCoverage (methods : Method seq, coverageType : coverageType) =
        let getCoveredBlocksCount (m : Method) =
            match coverageType with
            | ByTest -> m.BlocksCoveredByTests.Count
            | ByEntryPointTest -> m.BlocksCoveredFromEntryPoint.Count
        let methodsInZone = methods |> Seq.filter (fun m -> m.InCoverageZone)
        let totalBlocksCount = methodsInZone |> Seq.sumBy (fun m -> m.BasicBlocksCount)
        let coveredBlocksCount = methodsInZone |> Seq.sumBy getCoveredBlocksCount
        if totalBlocksCount <> 0u then
            uint <| floor (double coveredBlocksCount / double totalBlocksCount * 100.0)
        else
            0u

    member x.GetApproximateCoverage (method : Method, coverageType : coverageType) =
        x.GetApproximateCoverage(Seq.singleton method, coverageType)

    member x.OnBranchesReleased() =
        branchesReleased <- true

    member x.CreateContinuousDump() =
        if collectContinuousStatistics then
            let states = getStates() |> Seq.toList
            let continuousStatisticsDump = {
                millis = stopwatch.ElapsedMilliseconds;
                coveringStepsInsideZone = coveringStepsInsideZone;
                nonCoveringStepsInsideZone = nonCoveringStepsInsideZone;
                coveringStepsOutsideZone = coveringStepsOutsideZone;
                nonCoveringStepsOutsideZone = nonCoveringStepsOutsideZone;
                testsCount = testsCount;
                branchesReleased = branchesReleased;
                internalFailsCount = uint internalFails.Count;
                statesCount = getStatesCount()
                coveringStatesCount = states |> Seq.filter (fun s -> (x.GetVisitedBlocksNotCoveredByTests(s) |> Seq.filter (fun b -> b.method.InCoverageZone)) |> Seq.length > 0) |> Seq.length |> uint
            }
            continuousStatistics.Add continuousStatisticsDump

    member x.TrackFinished (s : cilState) =
        testsCount <- testsCount + 1u
        x.CreateContinuousDump()
        for block in s.history do
            block.method.SetBlockIsCoveredByTest(block.offset, entryMethodOf s)

            if block.method.InCoverageZone then
                isVisitedBlocksNotCoveredByTestsRelevant <- false

        visitedBlocksNotCoveredByTests.Remove s |> ignore

    member x.EmitError (s : cilState) (errorMessage : string) =
        emittedErrors.Add(s.currentLoc, errorMessage)

    member x.TrackStepBackward (pob : pob) (cilState : cilState) =
        // TODO
        ()

    member x.TrackFork (parent : cilState) (children : cilState seq) =
        for child in children do
            visitedBlocksNotCoveredByTests.[child] <- visitedBlocksNotCoveredByTests.[parent]

    member x.AddUnansweredPob (p : pob) = unansweredPobs.Add(p)

    member x.Reset() =
        startIp2currentIp.Clear()
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

        dumpTimer.Enabled <- false
        dumpTimer.Elapsed.RemoveHandler continuousDumpEventHandler
        continuousStatistics.Clear()

    member x.SolverStarted() = solverStopwatch.Start()
    member x.SolverStopped() = solverStopwatch.Stop()

    member x.ExplorationStarted() =
        stopwatch.Start()
        if collectContinuousStatistics then
            dumpTimer.Interval <- float statsDumpIntervalMs
            dumpTimer.Elapsed.AddHandler continuousDumpEventHandler
            dumpTimer.Start()

    member x.ExplorationFinished() =
        stopwatch.Stop()
        if collectContinuousStatistics then
            dumpTimer.Stop()
            dumpTimer.Elapsed.RemoveHandler continuousDumpEventHandler

    member x.SetStatesCountGetter(getter : unit -> int) =
        getStatesCount <- getter

    member x.SetStatesGetter(getter : unit -> cilState seq) =
        getStates <- getter

    member x.PickTotalUnvisitedInMethod loc = pickTotalUnvisitedInCFG loc

    member x.PickUnvisitedWithHistoryInCFG (loc, history) = pickUnvisitedWithHistoryInCFG loc history

    member x.CurrentExplorationTime with get() = stopwatch.Elapsed

    member x.IncompleteStates with get() = iies

    member x.InternalFails with get() = internalFails

    member x.ContinuousStatistics with get() = continuousStatistics

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

    member x.PrintStatistics (writer : TextWriter) =
        printStatistics writer <| x.DumpStatistics()

    member x.PrintDebugStatistics (writer : TextWriter) =
        let dump = x.DumpStatistics()
        printStatistics writer dump
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
