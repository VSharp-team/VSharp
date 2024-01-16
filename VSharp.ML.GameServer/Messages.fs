module VSharp.ML.GameServer.Messages

open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open VSharp
  
type searcher =
    | BFS = 0
    | DFS = 1
    
[<Struct>]
type RawInputMessage =
    val MessageType: string
    val MessageBody: string
    [<JsonConstructor>]
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}

type IRawOutgoingMessageBody = interface end

type [<Measure>] test
type [<Measure>] error
type [<Measure>] step
type [<Measure>] percent
type [<Measure>] basicBlockGlobalId
type [<Measure>] instruction

[<Struct>]
type GameOverMessageBody =    
     interface IRawOutgoingMessageBody
     val ActualCoverage: System.Nullable<uint>
     val TestsCount: uint32<test>
     val ErrorsCount: uint32<error>
     new (actualCoverage, testsCount, errorsCount) = {ActualCoverage = actualCoverage; TestsCount = testsCount; ErrorsCount = errorsCount}
       
[<Struct>]
type RawOutgoingMessage =
    val MessageType: string
    val MessageBody: obj
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}
       
type [<Measure>] stateId

[<Struct>]
type GameStep =
    val StateId: uint<stateId>    
    
    [<JsonConstructor>]
    new (stateId) = {StateId = stateId}
        

[<Struct>]
type StateHistoryElem =
    val GraphVertexId: uint<basicBlockGlobalId>
    val NumOfVisits: uint
    val StepWhenVisitedLastTime: uint<step>
    new (graphVertexId, numOfVisits, stepWhenVisitedLastTime) =
        {
            GraphVertexId = graphVertexId
            NumOfVisits = numOfVisits
            StepWhenVisitedLastTime = stepWhenVisitedLastTime
        }

[<Struct>]
type State =
    val Id: uint<stateId>
    val Position: uint<byte_offset>
    val PathConditionSize: uint
    val VisitedAgainVertices: uint
    val VisitedNotCoveredVerticesInZone: uint
    val VisitedNotCoveredVerticesOutOfZone: uint
    val StepWhenMovedLastTime: uint<step>
    val InstructionsVisitedInCurrentBlock: uint<instruction>
    val History: array<StateHistoryElem>
    val Children: array<uint<stateId>> 
    new(id,
        position,        
        pathConditionSize,
        visitedAgainVertices,
        visitedNotCoveredVerticesInZone,
        visitedNotCoveredVerticesOutOfZone,
        stepWhenMovedLastTime,
        instructionsVisitedInCurrentBlock,
        history,
        children) =
        {
            Id = id
            Position = position
            PathConditionSize = pathConditionSize
            VisitedAgainVertices = visitedAgainVertices
            VisitedNotCoveredVerticesInZone = visitedNotCoveredVerticesInZone
            VisitedNotCoveredVerticesOutOfZone = visitedNotCoveredVerticesOutOfZone
            StepWhenMovedLastTime = stepWhenMovedLastTime
            InstructionsVisitedInCurrentBlock = instructionsVisitedInCurrentBlock
            History = history
            Children = children
        }
    
[<Struct>]    
type GameMapVertex =
    val Id: uint<basicBlockGlobalId>
    val InCoverageZone: bool
    val BasicBlockSize: uint
    val CoveredByTest: bool
    val VisitedByState: bool
    val TouchedByState: bool
    val ContainsCall: bool
    val ContainsThrow: bool
    val States: uint<stateId>[]
    new (id,
         inCoverageZone,
         basicBlockSize,
         containsCall,
         containsThrow,
         coveredByTest,
         visitedByState,
         touchedByState,
         states) =
        {
            Id = id
            InCoverageZone = inCoverageZone
            BasicBlockSize = basicBlockSize
            CoveredByTest = coveredByTest
            VisitedByState = visitedByState
            TouchedByState = touchedByState
            ContainsCall = containsCall
            ContainsThrow = containsThrow
            States = states
        }

[<Struct>]
type GameEdgeLabel =
    val Token: int
    new (token) = {Token = token}

[<Struct>]
type GameMapEdge =
    val VertexFrom: uint<basicBlockGlobalId>
    val VertexTo: uint<basicBlockGlobalId>
    val Label: GameEdgeLabel
    new (vFrom, vTo, label) = {VertexFrom = vFrom; VertexTo = vTo; Label = label}
    
[<Struct>]
type GameState =
    interface IRawOutgoingMessageBody
    val GraphVertices: GameMapVertex[]
    val States: State[]
    val Map: GameMapEdge[]
    new (graphVertices, states, map) = {GraphVertices = graphVertices; States = states; Map = map}
    
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
    interface IRawOutgoingMessageBody
    val ForMove: MoveReward
    val MaxPossibleReward: uint<maxPossibleReward>
    new (forMove, maxPossibleReward) = {ForMove = forMove; MaxPossibleReward = maxPossibleReward}
    new (forCoverage, forVisitedInstructions, maxPossibleReward) = {ForMove = MoveReward(forCoverage,forVisitedInstructions); MaxPossibleReward = maxPossibleReward}

type Feedback =
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint<stateId>
    | ServerError of string

[<Struct>]
type GameMap =
    val Id: uint
    val StepsToPlay: uint<step>
    val StepsToStart: uint<step>
    [<JsonConverter(typeof<JsonStringEnumConverter>)>]
    val DefaultSearcher: searcher 
    val AssemblyFullName: string
    val NameOfObjectToCover: string
    val MapName: string
    new (id, stepsToPlay, stepsToStart, assembly, defaultSearcher, objectToCover) =
        {
            Id = id
            StepsToPlay = stepsToPlay
            StepsToStart = stepsToStart
            AssemblyFullName = assembly
            NameOfObjectToCover = objectToCover
            DefaultSearcher = defaultSearcher
            MapName = $"{objectToCover}_{defaultSearcher}_{stepsToStart}"
        }
        
    [<JsonConstructor>]
    new (id, stepsToPlay, stepsToStart, assemblyFullName, defaultSearcher, nameOfObjectToCover, mapName) =
        {
            Id = id
            StepsToPlay = stepsToPlay
            StepsToStart = stepsToStart
            AssemblyFullName = assemblyFullName
            DefaultSearcher = defaultSearcher
            NameOfObjectToCover = nameOfObjectToCover
            MapName = mapName
        }

type InputMessage =
    | ServerStop
    | Start of GameMap
    | Step of GameStep
    
[<Struct>]
 type ServerErrorMessageBody =
     interface IRawOutgoingMessageBody
     val ErrorMessage: string
     new (errorMessage) = {ErrorMessage = errorMessage}

[<Struct>]
 type IncorrectPredictedStateIdMessageBody =
     interface IRawOutgoingMessageBody
     val StateId: uint<stateId>
     new (stateId) = {StateId = stateId}     
 
type OutgoingMessage =
    | GameOver of System.Nullable<uint>*uint32<test>*uint32<error>    
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint<stateId>
    | ReadyForNextStep of GameState
    | ServerError of string
    
let (|MsgTypeStart|MsgTypeStep|MsgStop|) (str:string) =
    let normalized = str.ToLowerInvariant().Trim()
    if normalized = "start"
    then MsgTypeStart
    elif normalized = "step"
    then MsgTypeStep
    elif normalized = "stop"
    then MsgStop
    else failwithf $"Unexpected message type %s{str}"
    
let deserializeInputMessage (messageData:byte[]) =    
    let rawInputMessage =
        let str = Encoding.UTF8.GetString messageData
        str |> JsonSerializer.Deserialize<RawInputMessage>
    match rawInputMessage.MessageType with
    | MsgStop -> ServerStop
    | MsgTypeStart -> Start (JsonSerializer.Deserialize<GameMap> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step (JsonSerializer.Deserialize<GameStep>(rawInputMessage.MessageBody))

let serializeOutgoingMessage (message:OutgoingMessage) =
    match message with
    | GameOver (actualCoverage,testsCount, errorsCount) -> RawOutgoingMessage("GameOver", box (GameOverMessageBody (actualCoverage, testsCount, errorsCount)))   
    | MoveReward reward -> RawOutgoingMessage("MoveReward", reward)
    | IncorrectPredictedStateId stateId -> RawOutgoingMessage("IncorrectPredictedStateId", IncorrectPredictedStateIdMessageBody stateId)
    | ReadyForNextStep state -> RawOutgoingMessage("ReadyForNextStep", state)
    | ServerError errorMessage -> RawOutgoingMessage("ServerError", ServerErrorMessageBody errorMessage)
    |> JsonSerializer.Serialize
        

   