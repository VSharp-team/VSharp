module VSharp.IL.Serializer

open System.Collections.Generic
open System.Runtime.Serialization
open System.Text.Json
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
type State =
    val Id: uint
    val Position: uint
    val PredictedUsefulness: float
    val PathConditionSize: uint
    val VisitedAgainVertices: uint
    val VisitedNotCoveredVerticesInZone: uint
    val VisitedNotCoveredVerticesOutOfZone: uint
    val History: array<uint>
    val Children: array<uint> 
    new(id,
        position,
        predictedUsefulness,
        pathConditionSize,
        visitedAgainVertices,
        visitedNotCoveredVerticesInZone,
        visitedNotCoveredVerticesOutOfZone,
        history,
        children) =
        {
            Id = id
            Position = position
            PredictedUsefulness = predictedUsefulness
            PathConditionSize = pathConditionSize
            VisitedAgainVertices = visitedAgainVertices
            VisitedNotCoveredVerticesInZone = visitedNotCoveredVerticesInZone
            VisitedNotCoveredVerticesOutOfZone = visitedNotCoveredVerticesOutOfZone
            History = history
            Children = children
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
    
type [<Measure>] coverageReward
type [<Measure>] visitedInstructionsReward
type [<Measure>] maxPossibleReward
    
[<Struct>]
type MoveReward =
    val ForCoverage: uint<coverageReward>
    val ForVisitedInstructions: uint<visitedInstructionsReward>
    new (forCoverage, forVisitedInstructions) = {ForCoverage = forCoverage; ForVisitedInstructions = forVisitedInstructions}
[<Struct>]    
type Reward =
    val ForMove: MoveReward
    val MaxPossibleReward: uint<maxPossibleReward>
    new (forMove, maxPossibleReward) = {ForMove = forMove; MaxPossibleReward = maxPossibleReward}
    new (forCoverage, forVisitedInstructions, maxPossibleReward) = {ForMove = MoveReward(forCoverage,forVisitedInstructions); MaxPossibleReward = maxPossibleReward}

type Feedback =
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint
    | ServerError of string

type CoverageZone =
    | Method = 0
    | Class = 1
    
[<Struct>]
type GameMap =
    val Id: uint
    val CoverageToStart: uint
    val AssemblyFullName: string
    val CoverageZone: CoverageZone
    val NameOfObjectToCover: string
    new (id, coverageToStart, assembly, coverageZone, objectToCover) =
        {
            Id = id
            CoverageToStart = coverageToStart
            AssemblyFullName = assembly
            CoverageZone = coverageZone
            NameOfObjectToCover = objectToCover
        }

type OutgoingMessage =
    | GameOver
    | Maps of seq<GameMap>
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint
    | ReadyForNextStep of GameState
    | ServerError of string

let mutable firstFreeEpisodeNumber = 0
let folderToStoreSerializationResult = "SerializedEpisodes"
let fileForExpectedResults =
    let path = System.IO.Path.Combine(folderToStoreSerializationResult,"expectedResults.txt")
    System.IO.File.AppendAllLines(path, ["GraphID ExpectedStateNumber ExpectedRewardForCoveredInStep ExpectedRewardForVisitedInstructionsInStep TotalReachableRewardFromCurrentState"])
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
            (*let maxStatePosition =
                (currentBasicBlock.AssociatedStates
                |> Seq.maxBy (fun s -> s.CodeLocation.offset)).CodeLocation.offset
            let lastVisitedInstructionNumber =
                currentBasicBlock.Instructions
                |> Array.findIndex (fun instr -> Offset.from (int instr.offset) = maxStatePosition)*)
            visitedInstructionsInZone <- visitedInstructionsInZone + currentBasicBlock.VisitedInstructions
            
        
        let states =
            currentBasicBlock.AssociatedStates
            |> Seq.map (fun s ->                
                State(s.Id,
                      uint <| s.CodeLocation.offset - currentBasicBlock.StartOffset + 1<offsets>,
                      s.PredictedUsefulness,
                      s.PathConditionSize,
                      s.VisitedAgainVertices,
                      s.VisitedNotCoveredVerticesInZone,
                      s.VisitedNotCoveredVerticesOutOfZone,
                      s.History |> Seq.map getBasicBlockId |> Array.ofSeq,
                      s.Children |> Array.ofList
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

let dumpGameState (location:codeLocation) fileForResult =
    let gameState, statistics = collectGameState location
    let gameStateJson = JsonSerializer.Serialize gameState
    System.IO.File.WriteAllText(fileForResult,gameStateJson)
    statistics
    
let computeReward (statisticsBeforeStep:Statistics) (statisticsAfterStep:Statistics) =
    let rewardForCoverage =
        (statisticsAfterStep.CoveredVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<coverageReward>
    let rewardForVisitedInstructions = 
        (statisticsAfterStep.VisitedInstructionsInZone - statisticsBeforeStep.VisitedInstructionsInZone) * 1u<visitedInstructionsReward>                
    let maxPossibleReward = (statisticsBeforeStep.TotalVisibleVerticesInZone - statisticsBeforeStep.CoveredVerticesInZone) * 1u<maxPossibleReward>
    
    Reward (rewardForCoverage, rewardForVisitedInstructions, maxPossibleReward)

let saveExpectedResult (movedStateId:uint) (statistics1:Statistics) (statistics2:Statistics) =
    let reward = computeReward statistics1 statistics1
    
    System.IO.File.AppendAllLines(fileForExpectedResults, [sprintf $"%d{firstFreeEpisodeNumber} %d{movedStateId} %d{reward.ForMove.ForCoverage} %d{reward.ForMove.ForVisitedInstructions} %d{reward.MaxPossibleReward}"])
    firstFreeEpisodeNumber <- firstFreeEpisodeNumber + 1