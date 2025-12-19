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
    new(messageType, messageBody) =
        { MessageBody = messageBody
          MessageType = messageType }

type IRawOutgoingMessageBody = interface end

[<Measure>]
type pathConditionVertexId

type pathConditionVertexType =
    | UnaryMinus = 0
    | BitwiseNot = 1
    | BitwiseAnd = 2
    | BitwiseOr = 3
    | BitwiseXor = 4
    | LogicalNot = 5
    | LogicalAnd = 6
    | LogicalOr = 7
    | LogicalXor = 8
    | Equal = 9
    | NotEqual = 10
    | Greater = 11
    | Greater_Un = 12
    | Less = 13
    | Less_Un = 14
    | GreaterOrEqual = 15
    | GreaterOrEqual_Un = 16
    | LessOrEqual = 17
    | LessOrEqual_Un = 18
    | Add = 19
    | AddNoOvf = 20
    | AddNoOvf_Un = 21
    | Subtract = 22
    | SubNoOvf = 23
    | SubNoOvf_Un = 24
    | Divide = 25
    | Divide_Un = 26
    | Multiply = 27
    | MultiplyNoOvf = 28
    | MultiplyNoOvf_Un = 29
    | Remainder = 30
    | Remainder_Un = 31
    | ShiftLeft = 32
    | ShiftRight = 33
    | ShiftRight_Un = 34
    | Nop = 35
    | Concrete = 36
    | Constant = 37
    | Struct = 39
    | HeapRef = 40
    | Ref = 41
    | Ptr = 42
    | Slice = 43
    | Ite = 44
    | StandardFunctionApplication = 45
    | Cast = 46
    | Combine = 47
    | PathConditionRoot = 48


[<Struct>]
type PathConditionVertex =
    val Id: uint<pathConditionVertexId>
    val Type: pathConditionVertexType
    val Children: array<uint<pathConditionVertexId>>

    new(id, pathConditionVertexType, children) =
        { Id = id
          Type = pathConditionVertexType
          Children = children }

[<Measure>]
type test

[<Measure>]
type error

[<Measure>]
type step

[<Measure>]
type percent

[<Measure>]
type basicBlockGlobalId

[<Measure>]
type instruction

[<Struct>]
type GameOverMessageBody =
    interface IRawOutgoingMessageBody
    val ActualCoverage: uint<percent>
    val TestsCount: uint32<test>
    val StepsCount: uint32<step>
    val ErrorsCount: uint32<error>

    new(actualCoverage, testsCount, stepsCount, errorsCount) =
        { ActualCoverage = actualCoverage
          TestsCount = testsCount
          StepsCount = stepsCount
          ErrorsCount = errorsCount }

[<Struct>]
type RawOutgoingMessage =
    val MessageType: string
    val MessageBody: obj

    new(messageType, messageBody) =
        { MessageBody = messageBody
          MessageType = messageType }

[<Measure>]
type stateId

[<Struct>]
type GameStep =
    val StateId: uint<stateId>

    [<JsonConstructor>]
    new(stateId) = { StateId = stateId }


[<Struct>]
type StateHistoryElem =
    val GraphVertexId: uint<basicBlockGlobalId>
    val NumOfVisits: uint
    val StepWhenVisitedLastTime: uint<step>

    new(graphVertexId, numOfVisits, stepWhenVisitedLastTime) =
        { GraphVertexId = graphVertexId
          NumOfVisits = numOfVisits
          StepWhenVisitedLastTime = stepWhenVisitedLastTime }

[<Struct>]
type State =
    val Id: uint<stateId>
    val Position: uint<byte_offset> // to basic block id
    val PathCondition: uint<pathConditionVertexId> array
    val VisitedAgainVertices: uint
    val VisitedNotCoveredVerticesInZone: uint
    val VisitedNotCoveredVerticesOutOfZone: uint
    val StepWhenMovedLastTime: uint<step>
    val InstructionsVisitedInCurrentBlock: uint<instruction>
    val History: array<StateHistoryElem>
    val Children: array<uint<stateId>>

    new
        (
            id,
            position,
            pathCondition,
            visitedAgainVertices,
            visitedNotCoveredVerticesInZone,
            visitedNotCoveredVerticesOutOfZone,
            stepWhenMovedLastTime,
            instructionsVisitedInCurrentBlock,
            history,
            children
        ) =
        { Id = id
          Position = position
          PathCondition = pathCondition
          VisitedAgainVertices = visitedAgainVertices
          VisitedNotCoveredVerticesInZone = visitedNotCoveredVerticesInZone
          VisitedNotCoveredVerticesOutOfZone = visitedNotCoveredVerticesOutOfZone
          StepWhenMovedLastTime = stepWhenMovedLastTime
          InstructionsVisitedInCurrentBlock = instructionsVisitedInCurrentBlock
          History = history
          Children = children }

[<Struct>]
type GameMapVertex =
    val Id: uint<basicBlockGlobalId>
    val InCoverageZone: bool
    val BasicBlockSize: uint // instructions
    val CoveredByTest: bool
    val VisitedByState: bool
    val TouchedByState: bool
    val ContainsCall: bool
    val ContainsThrow: bool
    val States: uint<stateId>[]

    new
        (
            id,
            inCoverageZone,
            basicBlockSize,
            containsCall,
            containsThrow,
            coveredByTest,
            visitedByState,
            touchedByState,
            states
        ) =
        { Id = id
          InCoverageZone = inCoverageZone
          BasicBlockSize = basicBlockSize
          CoveredByTest = coveredByTest
          VisitedByState = visitedByState
          TouchedByState = touchedByState
          ContainsCall = containsCall
          ContainsThrow = containsThrow
          States = states }

[<Struct>]
type GameEdgeLabel =
    val Token: int
    new(token) = { Token = token }

[<Struct>]
type GameMapEdge =
    val VertexFrom: uint<basicBlockGlobalId>
    val VertexTo: uint<basicBlockGlobalId>
    val Label: GameEdgeLabel

    new(vFrom, vTo, label) =
        { VertexFrom = vFrom
          VertexTo = vTo
          Label = label }

[<Struct>]
type GameState =
    interface IRawOutgoingMessageBody
    val GraphVertices: GameMapVertex[]
    val States: State[]
    val PathConditionVertices: PathConditionVertex[]
    val Map: GameMapEdge[]

    new(graphVertices, states, pathConditionVertices, map) =
        { GraphVertices = graphVertices
          States = states
          PathConditionVertices = pathConditionVertices
          Map = map }

    member this.ToDot file drawHistoryEdges =
        let vertices = ResizeArray<_>()
        let edges = ResizeArray<_>()

        for v in this.GraphVertices do
            let color =
                if v.CoveredByTest then "green"
                elif v.VisitedByState then "red"
                elif v.TouchedByState then "yellow"
                else "white"

            vertices.Add($"{v.Id} [label={v.Id}, shape=box, style=filled, fillcolor={color}]")

            for s in v.States do
                edges.Add($"99{s}00 -> {v.Id} [label=L]")

        for s in this.States do
            vertices.Add($"99{s.Id}00 [label={s.Id}, shape=circle]")

            for v in s.Children do
                edges.Add($"99{s.Id}00 -> 99{v}00 [label=ch]")

            if drawHistoryEdges then
                for v in s.History do
                    edges.Add($"99{s.Id}00 -> {v.GraphVertexId} [label={v.NumOfVisits}]")

        for e in this.Map do
            edges.Add($"{e.VertexFrom}->{e.VertexTo}[label={e.Label.Token}]")

        let dot =
            seq {
                "digraph g{"
                yield! vertices
                yield! edges
                "}"
            }

        System.IO.File.WriteAllLines(file, dot)

[<Measure>]
type coverageReward

[<Measure>]
type visitedInstructionsReward

[<Measure>]
type maxPossibleReward

[<Struct>]
type MoveReward =
    val ForCoverage: uint<coverageReward>
    val ForVisitedInstructions: uint<visitedInstructionsReward>

    new(forCoverage, forVisitedInstructions) =
        { ForCoverage = forCoverage
          ForVisitedInstructions = forVisitedInstructions }

[<Struct>]
type Reward =
    interface IRawOutgoingMessageBody
    val ForMove: MoveReward
    val MaxPossibleReward: uint<maxPossibleReward>

    new(forMove, maxPossibleReward) =
        { ForMove = forMove
          MaxPossibleReward = maxPossibleReward }

    new(forCoverage, forVisitedInstructions, maxPossibleReward) =
        { ForMove = MoveReward(forCoverage, forVisitedInstructions)
          MaxPossibleReward = maxPossibleReward }

type Feedback =
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint<stateId>
    | ServerError of string

[<Struct>]
type GameMap =
    val StepsToPlay: uint<step>
    val StepsToStart: uint<step>

    [<JsonConverter(typeof<JsonStringEnumConverter>)>]
    val DefaultSearcher: searcher

    val AssemblyFullName: string
    val NameOfObjectToCover: string
    val MapName: string

    new(stepsToPlay, stepsToStart, assembly, defaultSearcher, objectToCover) =
        { StepsToPlay = stepsToPlay
          StepsToStart = stepsToStart
          AssemblyFullName = assembly
          NameOfObjectToCover = objectToCover
          DefaultSearcher = defaultSearcher
          MapName = $"{objectToCover}_{defaultSearcher}_{stepsToStart}" }

    [<JsonConstructor>]
    new(stepsToPlay, stepsToStart, assemblyFullName, defaultSearcher, nameOfObjectToCover, mapName) =
        { StepsToPlay = stepsToPlay
          StepsToStart = stepsToStart
          AssemblyFullName = assemblyFullName
          DefaultSearcher = defaultSearcher
          NameOfObjectToCover = nameOfObjectToCover
          MapName = mapName }

type InputMessage =
    | ServerStop
    | Start of GameMap
    | Step of GameStep

[<Struct>]
type ServerErrorMessageBody =
    interface IRawOutgoingMessageBody
    val ErrorMessage: string
    new(errorMessage) = { ErrorMessage = errorMessage }

[<Struct>]
type IncorrectPredictedStateIdMessageBody =
    interface IRawOutgoingMessageBody
    val StateId: uint<stateId>
    new(stateId) = { StateId = stateId }

type OutgoingMessage =
    | GameOver of uint<percent> * uint32<test> * uint32<step> * uint32<error>
    | MoveReward of Reward
    | IncorrectPredictedStateId of uint<stateId>
    | ReadyForNextStep of GameState
    | ServerError of string

let (|MsgTypeStart|MsgTypeStep|MsgStop|) (str: string) =
    let normalized = str.ToLowerInvariant().Trim()

    if normalized = "start" then MsgTypeStart
    elif normalized = "step" then MsgTypeStep
    elif normalized = "stop" then MsgStop
    else failwithf $"Unexpected message type %s{str}"

let deserializeInputMessage (messageData: byte[]) =
    let rawInputMessage =
        let str = Encoding.UTF8.GetString messageData
        str |> JsonSerializer.Deserialize<RawInputMessage>

    match rawInputMessage.MessageType with
    | MsgStop -> ServerStop
    | MsgTypeStart -> Start(JsonSerializer.Deserialize<GameMap> rawInputMessage.MessageBody)
    | MsgTypeStep -> Step(JsonSerializer.Deserialize<GameStep>(rawInputMessage.MessageBody))

let serializeOutgoingMessage (message: OutgoingMessage) =
    match message with
    | GameOver(actualCoverage, testsCount, stepsCount, errorsCount) ->
        RawOutgoingMessage("GameOver", box (GameOverMessageBody(actualCoverage, testsCount, stepsCount, errorsCount)))
    | MoveReward reward -> RawOutgoingMessage("MoveReward", reward)
    | IncorrectPredictedStateId stateId ->
        RawOutgoingMessage("IncorrectPredictedStateId", IncorrectPredictedStateIdMessageBody stateId)
    | ReadyForNextStep state -> RawOutgoingMessage("ReadyForNextStep", state)
    | ServerError errorMessage -> RawOutgoingMessage("ServerError", ServerErrorMessageBody errorMessage)
    |> JsonSerializer.Serialize
