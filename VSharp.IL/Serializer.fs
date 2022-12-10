module VSharp.IL.Serializer

open System.Collections.Generic
open VSharp
open VSharp.GraphUtils

let DumpFullGraph (location:codeLocation) fileForResult =
    System.IO.File.AppendAllLines(fileForResult, ["#Vertices"
                                                  "#VertexId InCoverageZone BasicBlockSize CoveredByTest VisitedByState State_i_ID State_i_Position"])
    let mutable firstFreeBasicBlochID = 0
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
                    basicBlocks.Add(firstFreeBasicBlochID, basicBlock)
                    basicBlocksIds.Add(basicBlock, firstFreeBasicBlochID)
                    firstFreeBasicBlochID <- firstFreeBasicBlochID + 1
            (method :> ICallGraphNode).OutgoingEdges
            |> Seq.iter (fun x -> collectFullGraph (x:?> Method))
            (method :> IReversedCallGraphNode).OutgoingEdges
            |> Seq.iter (fun x -> collectFullGraph (x:?> Method))
    collectFullGraph location.method
    for kvp in basicBlocks do
        let states = String.concat " " (kvp.Value.AssociatedStates |> Seq.map (fun s -> sprintf $"%d{s.Id} %d{s.CodeLocation.offset - kvp.Value.StartOffset + 1<offsets>}"))
        System.IO.File.AppendAllLines(fileForResult, [sprintf $"%d{kvp.Key} %d{if kvp.Value.IsGoal then 1 else 0} %d{kvp.Value.FinalOffset - kvp.Value.StartOffset + 1<offsets>} %d{if kvp.Value.IsCovered then 1 else 0} %d{if kvp.Value.IsVisited then 1 else 0}  %s{states}"])
    
    System.IO.File.AppendAllLines(fileForResult, ["#Edges"; "# VertexFrom VertexTo Terminal(0-CFG, 1-Call, 2-Return)"])
    for kvp in basicBlocks do
        for outgoingEdges in kvp.Value.OutgoingEdges do
            for targetBasicBlock in outgoingEdges.Value do
                System.IO.File.AppendAllLines(fileForResult, [sprintf $"%d{kvp.Key} %d{basicBlocksIds[targetBasicBlock]} %d{outgoingEdges.Key}"])