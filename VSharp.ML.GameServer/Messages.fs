module VSharp.ML.GameServer.Messages

open System.Text
open System.Text.Json
open System.Text.Json.Serialization

[<Struct>]
type RawInputMessage =
    val MessageType: string
    val MessageBody: string
    [<JsonConstructor>]
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}

type IRawOutgoingMessageBody = interface end

type [<Measure>] test
type [<Measure>] error

type [<Measure>] basicBlockGlobalId

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
    val MessageBody: obj //IRawOutgoingMessageBody
    new (messageType, messageBody) = {MessageBody = messageBody; MessageType = messageType}
       
[<Struct>]
type GameStartParams =
    val MapId: uint
    val StepsToPlay: uint
    
    [<JsonConstructor>]
    new (mapId, stepsToPlay) = {MapId = mapId; StepsToPlay = stepsToPlay}

[<Struct>]
type GameStep =
    val StateId: uint
    val PredictedStateUsefulness: float
    
    [<JsonConstructor>]
    new (stateId, predictedStateUsefulness) = {StateId = stateId; PredictedStateUsefulness = predictedStateUsefulness}
        
type InputMessage =
    | ServerStop
    | GetTrainMaps
    | GetValidationMaps 
    | Start of GameStartParams
    | Step of GameStep    

[<Struct>]
type StateHistoryElem =
    val GraphVertexId: uint<basicBlockGlobalId>
    val NumOfVisits: uint
    new (graphVertexId, numOfVisits) =
        {
            GraphVertexId = graphVertexId
            NumOfVisits = numOfVisits
        }

type [<Measure>] stateId

[<Struct>]
type State =
    val Id: uint<stateId>
    val Position: uint
    val PredictedUsefulness: float
    val PathConditionSize: uint
    val VisitedAgainVertices: uint
    val VisitedNotCoveredVerticesInZone: uint
    val VisitedNotCoveredVerticesOutOfZone: uint
    val History: array<StateHistoryElem>
    val Children: array<uint<stateId>> 
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
    val Id: uint<basicBlockGlobalId>
    val InCoverageZone: bool
    val BasicBlockSize: uint
    val CoveredByTest: bool
    val VisitedByState: bool
    val TouchedByState: bool
    val States: uint<stateId>[]
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

type CoverageZone =
    | Method = 0
    | Class = 1

type [<Measure>] step
type [<Measure>] percent

[<Struct>]
type GameMap =
    val Id: uint
    val MaxSteps: uint<step>
    val CoverageToStart: uint<percent>
    val AssemblyFullName: string
    val CoverageZone: CoverageZone
    val NameOfObjectToCover: string
    val MapName: string
    new (id, maxSteps, coverageToStart, assembly, coverageZone, objectToCover) =
        {
            Id = id
            MaxSteps = maxSteps
            CoverageToStart = coverageToStart
            AssemblyFullName = assembly
            CoverageZone = coverageZone
            NameOfObjectToCover = objectToCover
            MapName = $"{objectToCover}_{coverageZone}_{coverageToStart}"
        }

[<Struct>]
 type ServerErrorMessageBody =
     interface IRawOutgoingMessageBody
     val ErrorMessage: string
     new (errorMessage) = {ErrorMessage = errorMessage}

[<Struct>]
 type MapsMessageBody =
     interface IRawOutgoingMessageBody
     val Maps: array<GameMap>
     new (maps) = {Maps = maps}
     
     
[<Struct>]
 type IncorrectPredictedStateIdMessageBody =
     interface IRawOutgoingMessageBody
     val StateId: uint<stateId>
     new (stateId) = {StateId = stateId}     
 
type OutgoingMessage =
    | GameOver of System.Nullable<uint>*uint32<test>*uint32<error>
    | Maps of seq<GameMap>
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint<stateId>
    | ReadyForNextStep of GameState
    | ServerError of string
    
let (|MsgTypeStart|MsgTypeStep|MsgGetTrainMaps|MsgGetValidationMaps|MsgStop|) (str:string) =
    let normalized = str.ToLowerInvariant().Trim()
    if normalized = "start"
    then MsgTypeStart
    elif normalized = "step"
    then MsgTypeStep
    elif normalized = "gettrainmaps"
    then MsgGetTrainMaps
    elif normalized = "getvalidationmaps"
    then MsgGetValidationMaps
    elif normalized = "stop"
    then MsgStop
    else failwithf $"Unexpected message type %s{str}"
    
let deserializeInputMessage (messageData:byte[]) =    
    let rawInputMessage =
        let str = Encoding.UTF8.GetString messageData
        str |> JsonSerializer.Deserialize<RawInputMessage>
    match rawInputMessage.MessageType with
    | MsgStop -> ServerStop
    | MsgTypeStart -> Start (JsonSerializer.Deserialize<GameStartParams> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step (JsonSerializer.Deserialize<GameStep>(rawInputMessage.MessageBody))
    | MsgGetTrainMaps -> GetTrainMaps
    | MsgGetValidationMaps -> GetValidationMaps

let serializeOutgoingMessage (message:OutgoingMessage) =
    match message with
    | GameOver (actualCoverage,testsCount, errorsCount) -> RawOutgoingMessage("GameOver", box (GameOverMessageBody (actualCoverage, testsCount, errorsCount)))
    | Maps maps -> RawOutgoingMessage("Maps", MapsMessageBody (Array.ofSeq maps))
    | MoveReward reward -> RawOutgoingMessage("MoveReward", reward)
    | IncorrectPredictedStateId stateId -> RawOutgoingMessage("IncorrectPredictedStateId", IncorrectPredictedStateIdMessageBody stateId)
    | ReadyForNextStep state -> RawOutgoingMessage("ReadyForNextStep", state)
    | ServerError errorMessage -> RawOutgoingMessage("ServerError", ServerErrorMessageBody errorMessage)
    |> JsonSerializer.Serialize
        

   