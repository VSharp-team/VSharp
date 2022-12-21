module VSharp.IL.Serializer

open System.Collections.Generic
open Microsoft.FSharp.Collections
open VSharp
open VSharp.GraphUtils
open VSharp.Interpreter.IL    

[<Struct>]
type Statistics =
    val CoveredVerticesInZone: uint
    val CoveredVerticesOutOfZone: uint
    val VisitedVerticesInZone: uint
    val VisitedVerticesOutOfZone: uint
    val TouchedVerticesInZone: uint
    val TouchedVerticesOutOfZone: uint
    val TotalVisibleVerticesInZone: uint
    new (coveredVerticesInZone,coveredVerticesOutOfZone,visitedVerticesInZone,visitedVerticesOutOfZone,touchedVerticesInZone,touchedVerticesOutOfZone, totalVisibleVerticesInZone) =
        {
         CoveredVerticesInZone = coveredVerticesInZone
         CoveredVerticesOutOfZone = coveredVerticesOutOfZone
         VisitedVerticesInZone = visitedVerticesInZone
         VisitedVerticesOutOfZone = visitedVerticesOutOfZone
         TouchedVerticesInZone = touchedVerticesInZone
         TouchedVerticesOutOfZone = touchedVerticesOutOfZone
         TotalVisibleVerticesInZone = totalVisibleVerticesInZone
         }

[<Struct>]
type State =
    val Id: uint
    val Position: uint
    val PredictedUsefulness: float
    val PathConditionSize: uint
    val VisitedAgainVertices: uint
    val VisitedNotCoveredVerticesInZone: uint
    val VisitedNotCoveredVerticesOutOfZone: uint
    new(id,
        position,
        predictedUsefulness,
        pathConditionSize,
        visitedAgainVertices,
        visitedNotCoveredVerticesInZone,
        visitedNotCoveredVerticesOutOfZone) =
        {
            Id = id
            Position = position
            PredictedUsefulness = predictedUsefulness
            PathConditionSize = pathConditionSize
            VisitedAgainVertices = visitedAgainVertices
            VisitedNotCoveredVerticesInZone = visitedNotCoveredVerticesInZone
            VisitedNotCoveredVerticesOutOfZone = visitedNotCoveredVerticesOutOfZone
        }

[<Struct>]    
type GameMapVertex =
    val Uid: uint
    val Id: uint
    val InCoverageZone: bool
    val BasicBlockSize: uint
    val CoveredByTest: bool
    val VisitedByState: bool
    val TouchedByState: bool
    val States: State[]
    new (uid,
         id,
         inCoverageZone,
         basicBlockSize,
         coveredByTest,
         visitedByState,
         touchedByState,
         states) =
        {
            Uid = uid
            Id = id
            InCoverageZone = inCoverageZone
            BasicBlockSize = basicBlockSize
            CoveredByTest = coveredByTest
            VisitedByState = visitedByState
            TouchedByState = touchedByState
            States = states
        }

[<Struct>]
type GameEdgeLabel =
    val Token: int
    new (token) = {Token = token}

[<Struct>]
type GameMapEdge =
    val VertexFrom: GameMapVertex
    val VertexTo: GameMapVertex
    val Label: GameEdgeLabel
    new (vFrom, vTo, label) = {VertexFrom = vFrom; VertexTo = vTo; Label = label}
    
[<Struct>]
type GameState =
    val Map: GameMapEdge[]
    new (map) = {Map = map}

let mutable firstFreeEpisodeNumber = 0
let folderToStoreSerializationResult = "SerializedEpisodes"
let fileForExpectedResults =
    let path = System.IO.Path.Combine(folderToStoreSerializationResult,"expectedResults.txt")
    System.IO.File.AppendAllLines(path, ["GraphID ExpectedStateNumber ExpectedRewardForStep TotalReachableRewardFromCurrentState"])
    path
 
let collectGameState (location:codeLocation) =
    let mutable coveredVerticesInZone = 0u
    let mutable coveredVerticesOutOfZone = 0u
    let mutable visitedVerticesInZone = 0u
    let mutable visitedVerticesOutOfZone = 0u
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
    
    for kvp in basicBlocks do        
        let isCovered = if kvp.Value.IsCovered then 1u else 0u
        if kvp.Value.IsGoal
        then coveredVerticesInZone <- coveredVerticesInZone + isCovered
        else coveredVerticesOutOfZone <- coveredVerticesOutOfZone + isCovered
        
        let isVisited = if kvp.Value.IsVisited then 1u else 0u
        if kvp.Value.IsGoal
        then visitedVerticesInZone <- visitedVerticesInZone + isVisited
        else visitedVerticesOutOfZone <- visitedVerticesOutOfZone + isVisited
        
        let isTouched = if kvp.Value.IsTouched then 1u else 0u
        if kvp.Value.IsGoal
        then touchedVerticesInZone <- touchedVerticesInZone + isTouched
        else touchedVerticesOutOfZone <- touchedVerticesOutOfZone + isTouched        
        
        if kvp.Value.IsGoal
        then totalVisibleVerticesInZone <- totalVisibleVerticesInZone + 1u
    
        let states =
            kvp.Value.AssociatedStates
            |> Seq.map (fun s ->
                State(s.Id,
                      uint <| s.CodeLocation.offset - kvp.Value.StartOffset + 1<offsets>,
                      s.PredictedUsefulness,
                      s.PathConditionSize,
                      s.VisitedAgainVertices,
                      s.VisitedNotCoveredVerticesInZone,
                      s.VisitedNotCoveredVerticesOutOfZone))
                
        GameMapVertex(
            0u,
            uint kvp.Key,
            kvp.Value.IsGoal,
            uint <| kvp.Value.FinalOffset - kvp.Value.StartOffset + 1<offsets>,
            kvp.Value.IsCovered,
            kvp.Value.IsVisited,
            kvp.Value.IsTouched,
            Array.ofSeq states)
        |> (fun x -> vertices.Add(x.Id,x))
    
    let edges = ResizeArray<_>()
    
    for kvp in basicBlocks do
        for outgoingEdges in kvp.Value.OutgoingEdges do
            for targetBasicBlock in outgoingEdges.Value do
                GameMapEdge (vertices.[uint kvp.Key],
                             vertices[uint basicBlocksIds[targetBasicBlock]],
                             GameEdgeLabel (int outgoingEdges.Key))
                |> edges.Add
                
    GameState (edges.ToArray())
    , Statistics(coveredVerticesInZone,coveredVerticesOutOfZone,visitedVerticesInZone,visitedVerticesOutOfZone,touchedVerticesInZone,touchedVerticesOutOfZone, totalVisibleVerticesInZone)

   
let DumpFullGraph (location:codeLocation) fileForResult =
    let mutable coveredVerticesInZone = 0u
    let mutable coveredVerticesOutOfZone = 0u
    let mutable visitedVerticesInZone = 0u
    let mutable visitedVerticesOutOfZone = 0u
    let mutable touchedVerticesInZone = 0u
    let mutable touchedVerticesOutOfZone = 0u
    let mutable totalVisibleVerticesInZone = 0u
    let dump,fileForResult =
        match fileForResult with
        | Some x -> true,x
        | None -> false, ""
    if dump then 
        System.IO.File.AppendAllLines(fileForResult, ["#Vertices"
                                                      "#VertexId InCoverageZone BasicBlockSize CoveredByTest VisitedByState TouchedByState (StateID StatePosition statePredictedUsefulness statePathConditionSize stateVisitedAgainVertices stateVisitedNotCoveredVerticesInZone stateVisitedNotCoveredVerticesOutOfZone)*"])
    let mutable firstFreeBasicBlockID = 0
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
    for kvp in basicBlocks do        
        let isCovered = if kvp.Value.IsCovered then 1u else 0u
        if kvp.Value.IsGoal
        then coveredVerticesInZone <- coveredVerticesInZone + isCovered
        else coveredVerticesOutOfZone <- coveredVerticesOutOfZone + isCovered
        
        let isVisited = if kvp.Value.IsVisited then 1u else 0u
        if kvp.Value.IsGoal
        then visitedVerticesInZone <- visitedVerticesInZone + isVisited
        else visitedVerticesOutOfZone <- visitedVerticesOutOfZone + isVisited
        
        let isTouched = if kvp.Value.IsTouched then 1u else 0u
        if kvp.Value.IsGoal
        then touchedVerticesInZone <- touchedVerticesInZone + isTouched
        else touchedVerticesOutOfZone <- touchedVerticesOutOfZone + isTouched        
        
        if kvp.Value.IsGoal
        then totalVisibleVerticesInZone <- totalVisibleVerticesInZone + 1u
        
        if dump then
            let states = String.concat " " (kvp.Value.AssociatedStates |> Seq.map (fun s -> sprintf $"%d{s.Id} %d{s.CodeLocation.offset - kvp.Value.StartOffset + 1<offsets>} %f{s.PredictedUsefulness} %d{s.PathConditionSize} %d{s.VisitedAgainVertices} %d{s.VisitedNotCoveredVerticesInZone} %d{s.VisitedNotCoveredVerticesOutOfZone}"))
            System.IO.File.AppendAllLines(fileForResult, [sprintf $"%d{kvp.Key} %d{if kvp.Value.IsGoal then 1 else 0} %d{kvp.Value.FinalOffset - kvp.Value.StartOffset + 1<offsets>} %d{isCovered} %d{isVisited} %d{isTouched} %s{states}"])
    
    if dump then 
        System.IO.File.AppendAllLines(fileForResult, ["#Edges"; "# VertexFrom VertexTo Terminal(0-CFG, 1-Call, 2-Return)"])
        for kvp in basicBlocks do
            for outgoingEdges in kvp.Value.OutgoingEdges do
                for targetBasicBlock in outgoingEdges.Value do
                    System.IO.File.AppendAllLines(fileForResult, [sprintf $"%d{kvp.Key} %d{basicBlocksIds[targetBasicBlock]} %d{outgoingEdges.Key}"])
    
    Statistics(coveredVerticesInZone,coveredVerticesOutOfZone,visitedVerticesInZone,visitedVerticesOutOfZone,touchedVerticesInZone,touchedVerticesOutOfZone, totalVisibleVerticesInZone)
    
let saveExpectedResult (movedStateId:uint) (statistics1:Statistics) (statistics2:Statistics) =
    let score =
        (statistics2.CoveredVerticesInZone - statistics1.CoveredVerticesInZone) * 30u
        + (statistics2.VisitedVerticesInZone - statistics1.VisitedVerticesInZone) * 10u
        + (statistics2.TouchedVerticesInZone - statistics1.TouchedVerticesInZone) * 5u
    
    System.IO.File.AppendAllLines(fileForExpectedResults, [sprintf $"%d{firstFreeEpisodeNumber} %d{movedStateId} %d{score} %d{(statistics1.TotalVisibleVerticesInZone - statistics1.CoveredVerticesInZone) * 30u}"])
    firstFreeEpisodeNumber <- firstFreeEpisodeNumber + 1