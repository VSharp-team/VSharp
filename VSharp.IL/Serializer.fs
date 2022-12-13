module VSharp.IL.Serializer

open System.Collections.Generic
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
    

let mutable firstFreeEpisodeNumber = 0
let folderToStoreSerializationResult = "SerializedEpisodes"
let fileForExpectedResults =
    let path = System.IO.Path.Combine(folderToStoreSerializationResult,"expectedResults.txt")
    System.IO.File.AppendAllLines(path, ["GraphID ExpectedStateNumber ExpectedRewardForStep TotalReachableRewardFromCurrentState"])
    path
   
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
                                                      "#VertexId InCoverageZone BasicBlockSize CoveredByTest VisitedByState TouchedByState (State_i_ID State_i_Position)*"])
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
            let states = String.concat " " (kvp.Value.AssociatedStates |> Seq.map (fun s -> sprintf $"%d{s.Id} %d{s.CodeLocation.offset - kvp.Value.StartOffset + 1<offsets>}"))
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