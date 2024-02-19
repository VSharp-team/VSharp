module VSharp.IL.Serializer

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.Json
open Microsoft.FSharp.Collections
open VSharp
open VSharp.GraphUtils
open VSharp.ML.GameServer.Messages
open FSharpx.Collections


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
    val StateId: uint<stateId>
    val NextInstructionIsUncoveredInZone: float
    val ChildNumber: uint
    val VisitedVerticesInZone: uint
    val HistoryLength: uint
    val DistanceToNearestUncovered: uint
    val DistanceToNearestNotVisited: uint
    val DistanceToNearestReturn: uint
    new
        (
            stateId,
            nextInstructionIsUncoveredInZone,
            childNumber,
            visitedVerticesInZone,
            historyLength,
            distanceToNearestUncovered,
            distanceToNearestNotVisited,
            distanceTuNearestReturn
        ) =
        {
            StateId = stateId
            NextInstructionIsUncoveredInZone = nextInstructionIsUncoveredInZone
            ChildNumber = childNumber
            VisitedVerticesInZone = visitedVerticesInZone
            HistoryLength = historyLength
            DistanceToNearestUncovered = distanceToNearestUncovered
            DistanceToNearestNotVisited = distanceToNearestNotVisited
            DistanceToNearestReturn = distanceTuNearestReturn
        }

[<Struct>]
type StateInfoToDump =
    val StateId: uint<stateId>
    val NextInstructionIsUncoveredInZone: float
    val ChildNumberNormalized: float
    val VisitedVerticesInZoneNormalized: float
    val Productivity: float
    val DistanceToReturnNormalized: float
    val DistanceToUncoveredNormalized: float
    val DistanceToNotVisitedNormalized: float
    val ExpectedWeight: float
    new
        (
            stateId,
            nextInstructionIsUncoveredInZone,
            childNumber,
            visitedVerticesInZone,
            productivity,
            distanceToReturn,
            distanceToUncovered,
            distanceToNotVisited            
        ) =
        {
            StateId = stateId
            NextInstructionIsUncoveredInZone = nextInstructionIsUncoveredInZone
            ChildNumberNormalized = childNumber
            VisitedVerticesInZoneNormalized = visitedVerticesInZone
            Productivity = productivity
            DistanceToReturnNormalized = distanceToReturn
            DistanceToUncoveredNormalized = distanceToUncovered
            DistanceToNotVisitedNormalized = distanceToNotVisited
            ExpectedWeight = nextInstructionIsUncoveredInZone + childNumber + visitedVerticesInZone + distanceToReturn + distanceToUncovered + distanceToNotVisited + productivity
        }

let mutable firstFreeEpisodeNumber = 0

let calculateStateMetrics interproceduralGraphDistanceFrom (state:IGraphTrackableState) =
    let currentBasicBlock = state.CodeLocation.BasicBlock
    let distances = 
        let assembly = currentBasicBlock.Method.Module.Assembly
        let callGraphDist = Dict.getValueOrUpdate interproceduralGraphDistanceFrom assembly (fun () -> Dictionary<_, _>())
        Dict.getValueOrUpdate callGraphDist (currentBasicBlock :> IInterproceduralCfgNode) (fun () ->        
        let dist = incrementalSourcedShortestDistanceBfs (currentBasicBlock :> IInterproceduralCfgNode) callGraphDist
        let distFromNode = Dictionary<IInterproceduralCfgNode, uint>()
        for i in dist do
            if i.Value <> infinity then
                distFromNode.Add(i.Key, i.Value)
        distFromNode)
            
    let childCountStore = Dictionary<_,HashSet<_>>()
    let rec childCount (state:IGraphTrackableState) =
        if childCountStore.ContainsKey state
        then childCountStore[state]
        else             
            let cnt = Array.fold (fun (cnt:HashSet<_>) n -> cnt.UnionWith (childCount n); cnt) (HashSet<_>(state.Children)) state.Children
            childCountStore.Add (state,cnt)
            cnt 
    let childNumber = uint (childCount state).Count    
    let visitedVerticesInZone = state.History |> Seq.fold (fun cnt kvp -> if kvp.Key.IsGoal && not kvp.Key.IsCovered then cnt + 1u else cnt) 0u
    let nextInstructionIsUncoveredInZone =        
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
    
    let historyLength = state.History |> Seq.fold (fun cnt kvp -> cnt + kvp.Value.NumOfVisits) 0u
    
    let getMinBy cond =
        let s = distances |> Seq.filter cond
        if Seq.isEmpty s
        then UInt32.MaxValue
        else s |> Seq.minBy (fun x -> x.Value) |> fun x -> x.Value
        
    let distanceToNearestUncovered = getMinBy (fun kvp -> kvp.Key.IsGoal && not kvp.Key.IsCovered)
    let distanceToNearestNotVisited = getMinBy (fun kvp -> kvp.Key.IsGoal && not kvp.Key.IsVisited)
    let distanceToNearestReturn = getMinBy (fun kvp -> kvp.Key.IsGoal && kvp.Key.IsSink)
    
    StateMetrics(state.Id, nextInstructionIsUncoveredInZone, childNumber, visitedVerticesInZone, historyLength
                 , distanceToNearestUncovered, distanceToNearestNotVisited, distanceToNearestReturn)
    
let getFolderToStoreSerializationResult (prefix:string) suffix =
    let folderName = Path.Combine(Path.Combine(prefix, "SerializedEpisodes"), suffix)
    folderName

let computeStatistics (gameState:GameState) =
    let mutable coveredVerticesInZone = 0u
    let mutable coveredVerticesOutOfZone = 0u
    let mutable visitedVerticesInZone = 0u
    let mutable visitedVerticesOutOfZone = 0u
    let mutable visitedInstructionsInZone = 0u
    let mutable touchedVerticesInZone = 0u
    let mutable touchedVerticesOutOfZone = 0u
    let mutable totalVisibleVerticesInZone = 0u
    for v in gameState.GraphVertices do
        if v.CoveredByTest && v.InCoverageZone
        then coveredVerticesInZone <- coveredVerticesInZone + 1u
        if v.CoveredByTest && not v.InCoverageZone
        then coveredVerticesOutOfZone <- coveredVerticesOutOfZone + 1u               
        
        if v.VisitedByState && v.InCoverageZone
        then visitedVerticesInZone <- visitedVerticesInZone + 1u
        if v.VisitedByState && not v.InCoverageZone
        then visitedVerticesOutOfZone <- visitedVerticesOutOfZone + 1u
        
        if v.InCoverageZone
        then
            totalVisibleVerticesInZone <- totalVisibleVerticesInZone + 1u
            visitedInstructionsInZone <- visitedInstructionsInZone + v.BasicBlockSize
        
        if v.TouchedByState && v.InCoverageZone
        then touchedVerticesInZone <- touchedVerticesInZone + 1u
        if v.TouchedByState && not v.InCoverageZone
        then touchedVerticesOutOfZone <- touchedVerticesOutOfZone + 1u
        
    Statistics(coveredVerticesInZone,coveredVerticesOutOfZone,visitedVerticesInZone,visitedVerticesOutOfZone,visitedInstructionsInZone,touchedVerticesInZone,touchedVerticesOutOfZone, totalVisibleVerticesInZone)
        
let collectStatesInfoToDump (basicBlocks:ResizeArray<BasicBlock>) =
    let statesMetrics = ResizeArray<_>()
    for currentBasicBlock in basicBlocks do
        let interproceduralGraphDistanceFrom = Dictionary<Assembly, distanceCache<IInterproceduralCfgNode>>()
        currentBasicBlock.AssociatedStates
        |> Seq.iter (fun s -> statesMetrics.Add (calculateStateMetrics interproceduralGraphDistanceFrom s))
    
    let statesInfoToDump =        
        let mutable maxVisitedVertices = UInt32.MinValue
        let mutable maxChildNumber = UInt32.MinValue
        let mutable minDistToUncovered = UInt32.MaxValue
        let mutable minDistToNotVisited = UInt32.MaxValue
        let mutable minDistToReturn = UInt32.MaxValue
                                 
        statesMetrics
        |> ResizeArray.iter (fun s ->
            if s.VisitedVerticesInZone > maxVisitedVertices
            then maxVisitedVertices <- s.VisitedVerticesInZone
            if s.ChildNumber > maxChildNumber
            then maxChildNumber <- s.ChildNumber
            if s.DistanceToNearestUncovered < minDistToUncovered
            then minDistToUncovered <- s.DistanceToNearestUncovered
            if s.DistanceToNearestNotVisited < minDistToNotVisited
            then minDistToNotVisited <- s.DistanceToNearestNotVisited
            if s.DistanceToNearestReturn < minDistToReturn
            then minDistToReturn <- s.DistanceToNearestReturn
            )
        let normalize minV v (sm:StateMetrics) =            
            if v = minV || (v = UInt32.MaxValue && sm.DistanceToNearestReturn = UInt32.MaxValue) 
            then 1.0
            elif v = UInt32.MaxValue
            then 0.0
            else float (1u + minV) / float (1u + v)
            
        statesMetrics
        |> ResizeArray.map (fun m -> StateInfoToDump (m.StateId
                                                      , m.NextInstructionIsUncoveredInZone
                                                      , if maxChildNumber = 0u then 0.0 else  float m.ChildNumber / float maxChildNumber
                                                      , if maxVisitedVertices = 0u then 0.0 else float m.VisitedVerticesInZone / float maxVisitedVertices
                                                      , float m.VisitedVerticesInZone / float m.HistoryLength
                                                      , normalize minDistToReturn m.DistanceToNearestReturn m 
                                                      , normalize minDistToUncovered m.DistanceToNearestUncovered m
                                                      , normalize minDistToUncovered m.DistanceToNearestNotVisited m))
    statesInfoToDump
    
let collectGameState (basicBlocks:ResizeArray<BasicBlock>) filterStates =
    
    let vertices = ResizeArray<_>()
    let allStates = HashSet<_>()
    
    let activeStates =
        basicBlocks
        |> Seq.collect (fun basicBlock -> basicBlock.AssociatedStates)
        |> Seq.map (fun s -> s.Id)
        |> fun x -> HashSet x
         
    for currentBasicBlock in basicBlocks do        
        let states =
            currentBasicBlock.AssociatedStates
            |> Seq.map (fun s ->
                State(s.Id,
                      (uint <| s.CodeLocation.offset - currentBasicBlock.StartOffset + 1<byte_offset>) * 1u<byte_offset>,
                      s.PathConditionSize,
                      s.VisitedAgainVertices,
                      s.VisitedNotCoveredVerticesInZone,
                      s.VisitedNotCoveredVerticesOutOfZone,                      
                      s.StepWhenMovedLastTime,
                      s.InstructionsVisitedInCurrentBlock,
                      s.History |> Seq.map (fun kvp -> kvp.Value) |> Array.ofSeq,
                      s.Children |> Array.map (fun s -> s.Id)
                                 |> (fun x -> if filterStates
                                              then Array.filter activeStates.Contains x
                                              else x)
                      )
                |> allStates.Add
                |> ignore
                s.Id
                )
            |> Array.ofSeq
                

        GameMapVertex(
            currentBasicBlock.Id,
            currentBasicBlock.IsGoal,
            uint <| currentBasicBlock.FinalOffset - currentBasicBlock.StartOffset + 1<byte_offset>,
            currentBasicBlock.IsCovered,
            currentBasicBlock.IsVisited,
            currentBasicBlock.IsTouched,
            currentBasicBlock.ContainsCall,
            currentBasicBlock.ContainsThrow,
            states)
        |> vertices.Add
                    
    let edges = ResizeArray<_>()
    
    for basicBlock in basicBlocks do
        for outgoingEdges in basicBlock.OutgoingEdges do
            for targetBasicBlock in outgoingEdges.Value do
                GameMapEdge (basicBlock.Id,
                             targetBasicBlock.Id,
                             GameEdgeLabel (int outgoingEdges.Key))
                |> edges.Add
                
    GameState (vertices.ToArray(), allStates |> Array.ofSeq, edges.ToArray())
    
let collectGameStateDelta () =
    let basicBlocks = HashSet<_>(Application.applicationGraphDelta.TouchedBasicBlocks)
    for method in Application.applicationGraphDelta.LoadedMethods do
        for basicBlock in method.BasicBlocks do
            basicBlock.IsGoal <- method.InCoverageZone
            let added = basicBlocks.Add(basicBlock)
            ()
    collectGameState (ResizeArray basicBlocks) false  

let dumpGameState fileForResultWithoutExtension (movedStateId:uint<stateId>) =
    let basicBlocks = ResizeArray<_>()
    for method in Application.loadedMethods do
        for basicBlock in method.Key.BasicBlocks do
            basicBlock.IsGoal <- method.Key.InCoverageZone
            basicBlocks.Add(basicBlock)
            
    let gameState = collectGameState basicBlocks true
    let statesInfoToDump = collectStatesInfoToDump basicBlocks
    let gameStateJson = JsonSerializer.Serialize gameState        
    let statesInfoJson = JsonSerializer.Serialize statesInfoToDump
    File.WriteAllText(fileForResultWithoutExtension + "_gameState", gameStateJson)
    File.WriteAllText(fileForResultWithoutExtension + "_statesInfo", statesInfoJson)
    File.WriteAllText(fileForResultWithoutExtension + "_movedState", string movedStateId)
    
let computeReward (statisticsBeforeStep:Statistics) (statisticsAfterStep:Statistics) =
    let rewardForCoverage =
        (statisticsAfterStep.CoveredVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<coverageReward>
    let rewardForVisitedInstructions = 
        (statisticsAfterStep.VisitedInstructionsInZone - statisticsBeforeStep.VisitedInstructionsInZone) * 1u<visitedInstructionsReward>                
    let maxPossibleReward = (statisticsBeforeStep.TotalVisibleVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<maxPossibleReward>
    
    Reward (rewardForCoverage, rewardForVisitedInstructions, maxPossibleReward)
    