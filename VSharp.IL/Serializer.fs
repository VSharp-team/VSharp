module VSharp.IL.Serializer

open System
open System.Collections.Generic
open System.Text.Json
open Microsoft.FSharp.Collections
open VSharp
open VSharp.Core
open VSharp.GraphUtils
open VSharp.ML.GameServer.Messages


[<Struct>]
type Statistics =
    val CoveredVerticesInZone: uint
    val CoveredVerticesOutOfZone: uint
    val VisitedVerticesInZone: uint
    val VisitedVerticesOutOfZone: uint
    val VisitedInstructionsInZone: uint
    val TouchedVerticesInZone: uint
    val TouchedVerticesOutOfZone: uint
    val TotalVisibleVerticesInZone: uint        
    new (coveredVerticesInZone, coveredVerticesOutOfZone, visitedVerticesInZone, visitedVerticesOutOfZone
         , visitedInstructionsInZone, touchedVerticesInZone, touchedVerticesOutOfZone, totalVisibleVerticesInZone) =
        {
         CoveredVerticesInZone = coveredVerticesInZone
         CoveredVerticesOutOfZone = coveredVerticesOutOfZone
         VisitedVerticesInZone = visitedVerticesInZone
         VisitedVerticesOutOfZone = visitedVerticesOutOfZone
         VisitedInstructionsInZone = visitedInstructionsInZone
         TouchedVerticesInZone = touchedVerticesInZone
         TouchedVerticesOutOfZone = touchedVerticesOutOfZone
         TotalVisibleVerticesInZone = totalVisibleVerticesInZone
         }
        
[<Struct>]
type StateMetrics =
    val StateId: uint
    val NextInstructionIsUncoveredInZone: float
    val ChildNumber: uint
    val VisitedVerticesInZone: uint
    val HistoryLength: uint
    val DistanceToNearestUncovered: uint
    val DistanceToNearestReturn: uint
    new
        (
            stateId,
            nextInstructionIsUncoveredInZone,
            childNumber,
            visitedVerticesInZone,
            historyLength,
            distanceToNearestUncovered,
            distanceTuNearestReturn
        ) =
        {
            StateId = stateId
            NextInstructionIsUncoveredInZone = nextInstructionIsUncoveredInZone
            ChildNumber = childNumber
            VisitedVerticesInZone = visitedVerticesInZone
            HistoryLength = historyLength
            DistanceToNearestUncovered = distanceToNearestUncovered
            DistanceToNearestReturn = distanceTuNearestReturn
        }

[<Struct>]
type StateInfoToDump =
    val StateId: uint
    val NextInstructionIsUncoveredInZone: float
    val ChildNumberNormalized: float
    val VisitedVerticesInZoneNormalized: float
    val ExpectedWeight: float
    new
        (
            stateId,
            nextInstructionIsUncoveredInZone,
            childNumber,
            visitedVerticesInZone
        ) =
        {
            StateId = stateId
            NextInstructionIsUncoveredInZone = nextInstructionIsUncoveredInZone
            ChildNumberNormalized = childNumber
            VisitedVerticesInZoneNormalized = visitedVerticesInZone
            ExpectedWeight = nextInstructionIsUncoveredInZone + childNumber + visitedVerticesInZone
        }

let mutable firstFreeEpisodeNumber = 0

let calculateStateMetrics (state:IGraphTrackableState) =
    let childCountStore = Dictionary<_,HashSet<_>>()
    let rec childCount (state:IGraphTrackableState) =
        if childCountStore.ContainsKey state
        then childCountStore[state]
        else             
            let cnt = Array.fold (fun (cnt:HashSet<_>) n -> cnt.UnionWith (childCount n); cnt) (HashSet<_>(state.Children)) state.Children
            childCountStore.Add (state,cnt)
            cnt 
    let childNumber = uint (childCount state).Count    
    let visitedVerticesInZone = state.History |> Seq.fold (fun cnt basicBlock -> if basicBlock.IsGoal then cnt + 1u else cnt) 0u
    let nextInstructionIsUncoveredInZone =
        let currentBasicBlock = state.CodeLocation.BasicBlock
        let notTouchedFollowingBlocs, notVisitedFollowingBlocs, notCoveredFollowingBlocs =
            let mutable notCoveredBasicBlocksInZone = 0
            let mutable notVisitedBasicBlocksInZone = 0
            let mutable notTouchedBasicBlocksInZone = 0
            let basicBlocks = HashSet<_>()
            currentBasicBlock.OutgoingEdges.Values
            |> Seq.iter basicBlocks.UnionWith
            basicBlocks
            |> Seq.iter (fun basicBlock -> if basicBlock.IsGoal
                                           then if not basicBlock.IsTouched
                                                then notTouchedBasicBlocksInZone <- notTouchedBasicBlocksInZone + 1
                                                elif not basicBlock.IsVisited
                                                then notVisitedBasicBlocksInZone <- notVisitedBasicBlocksInZone + 1
                                                elif not basicBlock.IsCovered
                                                then notCoveredBasicBlocksInZone <- notCoveredBasicBlocksInZone + 1)
            notTouchedBasicBlocksInZone, notVisitedBasicBlocksInZone, notCoveredBasicBlocksInZone
        if state.CodeLocation.offset <> currentBasicBlock.FinalOffset && currentBasicBlock.IsGoal
        then if not currentBasicBlock.IsVisited
             then 1.0
             elif not currentBasicBlock.IsCovered
             then 0.5
             else 0.0
        elif state.CodeLocation.offset = currentBasicBlock.FinalOffset
        then if notTouchedFollowingBlocs > 0
             then 1.0
             elif notVisitedFollowingBlocs > 0
             then 0.5
             elif notCoveredFollowingBlocs > 0
             then 0.3
             else 0.0
        else 0.0 
    
    let historyLength = 0u
    let distanceToNearestUncovered = 0u
    let distanceToNearestReturn = 0u
    
    StateMetrics(state.Id, nextInstructionIsUncoveredInZone, childNumber, visitedVerticesInZone, historyLength
                 , distanceToNearestUncovered, distanceToNearestReturn)
    
let getFolderToStoreSerializationResult suffix =    
    let folderName = "SerializedEpisodes_for_" + suffix
    if System.IO.Directory.Exists folderName
    then System.IO.Directory.Delete(folderName,true)
    let _ = System.IO.Directory.CreateDirectory folderName
    folderName
    
let getFileForExpectedResults folderToStoreSerializationResult =
    let path = System.IO.Path.Combine(folderToStoreSerializationResult,"expectedResults.txt")    
    path
 
let collectGameState (location:codeLocation) =
    let mutable coveredVerticesInZone = 0u
    let mutable coveredVerticesOutOfZone = 0u
    let mutable visitedVerticesInZone = 0u
    let mutable visitedVerticesOutOfZone = 0u
    let mutable visitedInstructionsInZone = 0u
    let mutable touchedVerticesInZone = 0u
    let mutable touchedVerticesOutOfZone = 0u
    let mutable totalVisibleVerticesInZone = 0u    
    let mutable firstFreeBasicBlockID = 0
    
    let vertices = Dictionary<_,_>()

    let basicBlocks = Dictionary<_,_>()
    let basicBlocksIds = Dictionary<_,_>()
    let visitedMethods = HashSet<_>()
    let rec collectFullGraph (method:Method) =
        if not <| visitedMethods.Contains method
        then            
            let added = visitedMethods.Add method
            assert added
            for basicBlock in method.CFG.SortedBasicBlocks do
                    basicBlock.IsGoal <- method.InCoverageZone
                    basicBlocks.Add(firstFreeBasicBlockID, basicBlock)
                    basicBlocksIds.Add(basicBlock, firstFreeBasicBlockID)
                    firstFreeBasicBlockID <- firstFreeBasicBlockID + 1
                    basicBlock.IncomingCallEdges
                    |> Seq.iter (fun x -> collectFullGraph (x.Method :?> Method))                     
            (method :> ICallGraphNode).OutgoingEdges
            |> Seq.iter (fun x -> collectFullGraph (x:?> Method))
            (method :> IReversedCallGraphNode).OutgoingEdges
            |> Seq.iter (fun x -> collectFullGraph (x:?> Method))
    collectFullGraph location.method
    
    let getBasicBlockId =
        let basicBlockToIdMap = Dictionary<_,_>()
        for kvp in basicBlocks do basicBlockToIdMap.Add(kvp.Value, uint kvp.Key)
        fun basicBlock ->
            let exists,id = basicBlockToIdMap.TryGetValue basicBlock
            if exists
            then id
            else
                let id = uint firstFreeBasicBlockID
                firstFreeBasicBlockID <- firstFreeBasicBlockID + 1
                basicBlockToIdMap.Add(basicBlock, id)
                id
    
    let statesMetrics = ResizeArray<_>()
        
    for kvp in basicBlocks do
        let currentBasicBlock = kvp.Value
        let isCovered = if currentBasicBlock.IsCovered then 1u else 0u
        if currentBasicBlock.IsGoal
        then coveredVerticesInZone <- coveredVerticesInZone + isCovered
        else coveredVerticesOutOfZone <- coveredVerticesOutOfZone + isCovered
        
        let isVisited = if currentBasicBlock.IsVisited then 1u else 0u
        if currentBasicBlock.IsGoal
        then visitedVerticesInZone <- visitedVerticesInZone + isVisited
        else visitedVerticesOutOfZone <- visitedVerticesOutOfZone + isVisited
        
        let isTouched = if currentBasicBlock.IsTouched then 1u else 0u
        if currentBasicBlock.IsGoal
        then touchedVerticesInZone <- touchedVerticesInZone + isTouched
        else touchedVerticesOutOfZone <- touchedVerticesOutOfZone + isTouched        
        
        if currentBasicBlock.IsGoal
        then totalVisibleVerticesInZone <- totalVisibleVerticesInZone + 1u
    
        if currentBasicBlock.IsVisited
        then
            visitedInstructionsInZone <- visitedInstructionsInZone + uint currentBasicBlock.Instructions.Length
        elif currentBasicBlock.IsTouched
        then
            visitedInstructionsInZone <- visitedInstructionsInZone + currentBasicBlock.VisitedInstructions
            
        let states =
            currentBasicBlock.AssociatedStates
            |> Seq.map (fun s ->
                statesMetrics.Add (calculateStateMetrics s)
                State(s.Id,
                      uint <| s.CodeLocation.offset - currentBasicBlock.StartOffset + 1<offsets>,
                      s.PredictedUsefulness,
                      s.PathConditionSize,
                      s.VisitedAgainVertices,
                      s.VisitedNotCoveredVerticesInZone,
                      s.VisitedNotCoveredVerticesOutOfZone,
                      s.History |> Seq.map getBasicBlockId |> Array.ofSeq,
                      s.Children |> Array.map (fun s -> s.Id)
                      ))
            |> Array.ofSeq
                
        GameMapVertex(
            0u,
            uint kvp.Key,
            currentBasicBlock.IsGoal,
            uint <| currentBasicBlock.FinalOffset - currentBasicBlock.StartOffset + 1<offsets>,
            currentBasicBlock.IsCovered,
            currentBasicBlock.IsVisited,
            currentBasicBlock.IsTouched,
            states)
        |> (fun x -> vertices.Add(x.Id,x))
        
    let statesInfoToDump =
        let mutable maxVisitedVertices = UInt32.MinValue
        let mutable maxChildNumber = UInt32.MinValue
                                 
        statesMetrics
        |> ResizeArray.iter (fun s ->
            if s.VisitedVerticesInZone > maxVisitedVertices
            then maxVisitedVertices <- s.VisitedVerticesInZone
            if s.ChildNumber > maxChildNumber
            then maxChildNumber <- s.ChildNumber
            )
        statesMetrics
        |> ResizeArray.map (fun m -> StateInfoToDump (m.StateId
                                                      , m.NextInstructionIsUncoveredInZone
                                                      , if maxChildNumber = 0u then 0.0 else  float m.ChildNumber / float maxChildNumber
                                                      , if maxVisitedVertices = 0u then 0.0 else float m.VisitedVerticesInZone / float maxVisitedVertices))
    
    let edges = ResizeArray<_>()
    
    for kvp in basicBlocks do
        for outgoingEdges in kvp.Value.OutgoingEdges do
            for targetBasicBlock in outgoingEdges.Value do
                GameMapEdge (vertices.[uint kvp.Key],
                             vertices[uint basicBlocksIds[targetBasicBlock]],
                             GameEdgeLabel (int outgoingEdges.Key))
                |> edges.Add
                
    GameState (edges.ToArray())
    , Statistics(coveredVerticesInZone,coveredVerticesOutOfZone,visitedVerticesInZone,visitedVerticesOutOfZone,visitedInstructionsInZone,touchedVerticesInZone,touchedVerticesOutOfZone, totalVisibleVerticesInZone)
    , statesInfoToDump

let dumpGameState (location:codeLocation) fileForResultWithoutExtension =
    let gameState, statistics, statesInfoToDump = collectGameState location
    let gameStateJson = JsonSerializer.Serialize gameState
    let statesInfoJson = JsonSerializer.Serialize statesInfoToDump
    System.IO.File.WriteAllText(fileForResultWithoutExtension + "_gameState",gameStateJson)
    System.IO.File.WriteAllText(fileForResultWithoutExtension + "_statesInfo",statesInfoJson)
    statistics
    
let computeReward (statisticsBeforeStep:Statistics) (statisticsAfterStep:Statistics) =
    let rewardForCoverage =
        (statisticsAfterStep.CoveredVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<coverageReward>
    let rewardForVisitedInstructions = 
        (statisticsAfterStep.VisitedInstructionsInZone - statisticsBeforeStep.VisitedInstructionsInZone) * 1u<visitedInstructionsReward>                
    let maxPossibleReward = (statisticsBeforeStep.TotalVisibleVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<maxPossibleReward>
    
    Reward (rewardForCoverage, rewardForVisitedInstructions, maxPossibleReward)

let saveExpectedResult fileForExpectedResults (movedStateId:uint) (statistics1:Statistics) (statistics2:Statistics) =
    let reward = computeReward statistics1 statistics2
    
    System.IO.File.AppendAllLines(fileForExpectedResults, [sprintf $"%d{firstFreeEpisodeNumber} %d{movedStateId} %d{reward.ForMove.ForCoverage} %d{reward.ForMove.ForVisitedInstructions} %d{reward.MaxPossibleReward}"])
    firstFreeEpisodeNumber <- firstFreeEpisodeNumber + 1