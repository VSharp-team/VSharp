module VSharp.IL.Serializer

open System
open System.Collections.Generic
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
    let currentBasicBlock = state.CodeLocation.ForceBasicBlock
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
    
    let historyLength = state.History |> Seq.fold (fun cnt kvp -> cnt + kvp.Value) 0u
    
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
    let mutable firstFreeBasicBlockID = 0u<graphVertexId>
    
    let vertices = Dictionary<_,_>()
    let allStates = HashSet<_>()

    let basicBlocks = Dictionary<_,_>()
    let basicBlocksIds = Dictionary<_,_>()
    let visitedMethods = HashSet<_>()
    let rec collectFullGraph (method:Method) =
        if not <| visitedMethods.Contains method
        then            
            let added = visitedMethods.Add method
            assert added
            for basicBlock in method.ForceCFG.SortedBasicBlocks do
                    basicBlock.IsGoal <- method.InCoverageZone
                    basicBlocks.Add(firstFreeBasicBlockID, basicBlock)
                    basicBlocksIds.Add(basicBlock, firstFreeBasicBlockID)
                    firstFreeBasicBlockID <- firstFreeBasicBlockID + + 1u<graphVertexId>
                    for state in basicBlock.AssociatedStates do
                         state.History
                         |> Seq.iter (fun kvp -> collectFullGraph (kvp.Key.Method :?> Method))
                    basicBlock.IncomingCallEdges
                    |> Seq.iter (fun x -> collectFullGraph (x.Method :?> Method))
                    for edge in basicBlock.OutgoingEdges do
                        for bb in edge.Value do
                            if bb.Method <> basicBlock.Method
                            then collectFullGraph (bb.Method :?> Method)
                    
            //(method :> VSharp.ICallGraphNode).OutgoingEdges
            //|> Seq.iter (fun x -> collectFullGraph (x:?> Method))
            (method :> VSharp.IReversedCallGraphNode).OutgoingEdges
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
                firstFreeBasicBlockID <- firstFreeBasicBlockID + 1u<graphVertexId>
                basicBlockToIdMap.Add(basicBlock, id)
                id
    
    //let statesMetrics = ResizeArray<_>()

    let activeStates =
        basicBlocks
        |> Seq.collect (fun kvp -> kvp.Value.AssociatedStates)
        |> Seq.map (fun s -> s.Id)
        |> fun x -> HashSet x
        
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
            visitedInstructionsInZone <- visitedInstructionsInZone + uint (currentBasicBlock.BlockSize)
        elif currentBasicBlock.IsTouched
        then
            visitedInstructionsInZone <- visitedInstructionsInZone + currentBasicBlock.VisitedInstructions
        
        //let interproceduralGraphDistanceFrom = Dictionary<Assembly, GraphUtils.distanceCache<IInterproceduralCfgNode>>()
        
        let states =
            currentBasicBlock.AssociatedStates
            |> Seq.map (fun s ->
                //statesMetrics.Add (calculateStateMetrics interproceduralGraphDistanceFrom s)
                State(s.Id,
                      uint <| s.CodeLocation.offset - currentBasicBlock.StartOffset + 1<offsets>,
                      s.PredictedUsefulness,
                      s.PathConditionSize,
                      s.VisitedAgainVertices,
                      s.VisitedNotCoveredVerticesInZone,
                      s.VisitedNotCoveredVerticesOutOfZone,
                      s.History |> Seq.map (fun kvp -> StateHistoryElem(getBasicBlockId kvp.Key, kvp.Value)) |> Array.ofSeq,
                      s.Children |> Array.map (fun s -> s.Id) |> Array.filter activeStates.Contains
                      )
                |> allStates.Add
                |> ignore
                s.Id
                )
            |> Array.ofSeq
                

        GameMapVertex(
            0u,
            kvp.Key,
            currentBasicBlock.IsGoal,
            uint <| currentBasicBlock.FinalOffset - currentBasicBlock.StartOffset + 1<offsets>,
            currentBasicBlock.IsCovered,
            currentBasicBlock.IsVisited,
            currentBasicBlock.IsTouched,
            states)
        |> (fun x -> vertices.Add(x.Id,x))
        
        
    (*let statesInfoToDump =
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
    *)
    let edges = ResizeArray<_>()
    
    for kvp in basicBlocks do
        for outgoingEdges in kvp.Value.OutgoingEdges do
            for targetBasicBlock in outgoingEdges.Value do
                GameMapEdge (vertices.[kvp.Key].Id,
                             vertices[basicBlocksIds[targetBasicBlock]].Id,
                             GameEdgeLabel (int outgoingEdges.Key))
                |> edges.Add
                
    GameState (vertices.Values |> Array.ofSeq, allStates |> Array.ofSeq, edges.ToArray())
    , Statistics(coveredVerticesInZone,coveredVerticesOutOfZone,visitedVerticesInZone,visitedVerticesOutOfZone,visitedInstructionsInZone,touchedVerticesInZone,touchedVerticesOutOfZone, totalVisibleVerticesInZone)
    //, statesInfoToDump

let dumpGameState (location:codeLocation) fileForResultWithoutExtension =
    let gameState, statistics(*, statesInfoToDump*) = collectGameState location
    let gameStateJson = JsonSerializer.Serialize gameState
    //let statesInfoJson = JsonSerializer.Serialize statesInfoToDump
    System.IO.File.WriteAllText(fileForResultWithoutExtension + "_gameState",gameStateJson)
    //System.IO.File.WriteAllText(fileForResultWithoutExtension + "_statesInfo",statesInfoJson)
    statistics
    
let computeReward (statisticsBeforeStep:Statistics) (statisticsAfterStep:Statistics) =
    let rewardForCoverage =
        (statisticsAfterStep.CoveredVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<coverageReward>
    let rewardForVisitedInstructions = 
        (statisticsAfterStep.VisitedInstructionsInZone - statisticsBeforeStep.VisitedInstructionsInZone) * 1u<visitedInstructionsReward>                
    let maxPossibleReward = (statisticsBeforeStep.TotalVisibleVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<maxPossibleReward>
    
    Reward (rewardForCoverage, rewardForVisitedInstructions, maxPossibleReward)

let saveExpectedResult fileForExpectedResults (movedStateId:uint<stateId>) (statistics1:Statistics) (statistics2:Statistics) =
    let reward = computeReward statistics1 statistics2
    
    System.IO.File.AppendAllLines(fileForExpectedResults, [sprintf $"%d{firstFreeEpisodeNumber} %d{movedStateId} %d{reward.ForMove.ForCoverage} %d{reward.ForMove.ForVisitedInstructions} %d{reward.MaxPossibleReward}"])
    firstFreeEpisodeNumber <- firstFreeEpisodeNumber + 1