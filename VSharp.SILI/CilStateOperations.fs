namespace VSharp.Interpreter.IL

open VSharp
open System.Text
open System.Reflection
open VSharp.Core
open ipOperations

type cilState =
    { ipStack : ipStack
      state : state
      filterResult : term option
      //TODO: #mb frames list #mb transfer to Core.State
      iie : InsufficientInformationException option
      level : level
      startingIP : ip
      initialEvaluationStackSize : uint32
    }

module internal CilStateOperations =

    let makeCilState curV initialEvaluationStackSize state =
        { ipStack = [curV]
          state = state
          filterResult = None
          iie = None
          level = PersistentDict.empty
          startingIP = curV
          initialEvaluationStackSize = initialEvaluationStackSize
        }

    let makeInitialState m state = makeCilState (instruction m 0) 0u state

    let isIIEState (s : cilState) = Option.isSome s.iie

    let isExecutable (s : cilState) =
        match s.ipStack with
        | [] -> __unreachable__()
        | Exit _ :: [] -> false
        | _ -> true

    let isError (s : cilState) =
        match s.state.exceptionsRegister with
        | NoException -> false
        | _ -> true
    let isUnhandledError (s : cilState) =
        match s.state.exceptionsRegister with
        | Unhandled _ -> true
        | _ -> false

    let currentIp (s : cilState) = List.head s.ipStack

    // obtaining Method where Execution occurs
    let currentMethod = function
        | Exit m
        | Instruction(_, m)
        | Leave(_, _, _, m) -> m
        | _ -> __notImplemented__()
    let startsFromMethodBeginning (s : cilState) =
        match s.startingIP with
        | Instruction (0, _) -> true
        | _ -> false
    let pushToIp (ip : ip) (cilState : cilState) = {cilState with ipStack = ip :: cilState.ipStack}
    let setCurrentIp (ip : ip) (cilState : cilState) = {cilState with ipStack = ip :: List.tail cilState.ipStack}

    let withIpStack (ipStack : ipStack) (cilState : cilState) = {cilState with ipStack = ipStack}
    let startingIpOf (cilState : cilState) = cilState.startingIP

    let composeIps (oldIpStack : ipStack) (newIpStack : ipStack) = newIpStack @ oldIpStack

    let composePopsCount (min1, cnt1) (_, cnt2) =
        let cnt = cnt1 + cnt2
        min min1 cnt, cnt

    let compose (cilState1 : cilState) (cilState2 : cilState) =
        assert(currentIp cilState1 = cilState2.startingIP)
        let level =
            PersistentDict.fold (fun (acc : level) k v ->
                let oldValue = if PersistentDict.contains k acc then PersistentDict.find acc k else 0u
                PersistentDict.add k (v + oldValue) acc
            ) cilState1.level cilState2.level
        let iie = None // we might concretize state, so we should try executed instructions again
        let ip = composeIps (List.tail cilState1.ipStack) cilState2.ipStack
        let states = Memory.ComposeStates cilState1.state cilState2.state
        let _, leftEvaluationStack = EvaluationStack.PopArguments (int cilState2.initialEvaluationStackSize) cilState1.state.evaluationStack
        let makeResultState (state : state) =
            let state' = {state with evaluationStack = EvaluationStack.Union leftEvaluationStack state.evaluationStack }
            {cilState2 with state = state'; ipStack = ip; level = level; initialEvaluationStackSize = cilState1.initialEvaluationStackSize
                            startingIP = cilState1.startingIP; iie = iie}
        List.map makeResultState states

    let incrementLevel (cilState : cilState) k =
        let lvl = cilState.level
        let newValue = if PersistentDict.contains k lvl then PersistentDict.find lvl k + 1u else 1u
        {cilState with level = PersistentDict.add k newValue lvl}

    // ------------------------------- Helper functions for cilState and state interaction -------------------------------

    let stateOf (cilState : cilState) = cilState.state
    let popFrameOf (cilState : cilState) =
        let s = Memory.PopFrame cilState.state
        let ip = List.tail cilState.ipStack
        {cilState with state = s; ipStack = ip}

    let emptyEvaluationStack = Memory.EmptyState.evaluationStack
    let withCurrentTime time (cilState : cilState) = {cilState with state = {cilState.state with currentTime = time}}
    let withEvaluationStack evaluationStack (cilState : cilState) = {cilState with state = {cilState.state with evaluationStack = evaluationStack}}

    let clearEvaluationStackLastFrame (cilState : cilState) =
        {cilState with state = {cilState.state with evaluationStack = EvaluationStack.ClearActiveFrame cilState.state.evaluationStack}}

    let withState state (cilState : cilState) = {cilState with state = state}
    let changeState (cilState : cilState) state = {cilState with state = state}

    let withNoResult (cilState : cilState) = {cilState with state = {cilState.state with returnRegister = None}}
    let withException exc (cilState : cilState) = {cilState with state = {cilState.state with exceptionsRegister = exc}}

    let push v (cilState : cilState) = {cilState with state = {cilState.state with evaluationStack = EvaluationStack.Push v cilState.state.evaluationStack}}
    let pop (cilState : cilState) =
        let v, evaluationStack = EvaluationStack.Pop cilState.state.evaluationStack
        v, {cilState with state = {cilState.state with evaluationStack = evaluationStack}}
    let pop2 (cilState : cilState) =
        let arg2, cilState = pop cilState
        let arg1, cilState = pop cilState
        arg2, arg1, cilState
    let pop3 (cilState : cilState) =
        let arg3, cilState = pop cilState
        let arg2, cilState = pop cilState
        let arg1, cilState = pop cilState
        arg3, arg2, arg1, cilState

    let pushNewObjForValueTypes (afterCall : cilState) =
        let ref, cilState = pop afterCall
        let value = Memory.ReadSafe cilState.state ref
        push value cilState

    let pushResultToEvaluationStack (cilStates : cilState list) =
        cilStates |> List.map (fun (cilState : cilState) ->
            let state = cilState.state
            if state.exceptionsRegister.UnhandledError then cilState // TODO: check whether evaluationStack := [] is needed
            else
                let evaluationStack =
                    match state.returnRegister with
                    | None -> state.evaluationStack
                    | Some r -> EvaluationStack.Push r state.evaluationStack
                let state = {state with returnRegister = None; evaluationStack = evaluationStack}
                {cilState with state = state})

    // ------------------------------- Helper functions for cilState -------------------------------

    let moveIp offset m cilState =
        let cfg = CFG.findCfg m
        let opCode = Instruction.parseInstruction m offset
        let newIps =
            let nextTargets = Instruction.findNextInstructionOffsetAndEdges opCode cfg.ilBytes offset
            match nextTargets with
            | UnconditionalBranch nextInstruction
            | FallThrough nextInstruction -> instruction m nextInstruction :: []
            | Return -> exit m :: []
            | ExceptionMechanism ->
                let toObserve = __notImplemented__()
                searchingForHandler toObserve 0 :: []
            | ConditionalBranch (fall, targets) -> fall :: targets |> List.map (instruction m)
        List.map (fun ip -> setCurrentIp ip cilState) newIps

    let StatedConditionalExecutionCIL (cilState : cilState) (condition : state -> (term * state -> 'a) -> 'a) (thenBranch : cilState -> ('c list -> 'a) -> 'a) (elseBranch : cilState -> ('c list -> 'a) -> 'a) (k : 'c list -> 'a) =
        StatedConditionalExecution cilState.state condition
            (fun state k -> thenBranch {cilState with state = state} k)
            (fun state k -> elseBranch {cilState with state = state} k)
            (fun x y -> List.append x y |> List.singleton)
            (List.head >> k)
    let GuardedApplyCIL (cilState : cilState) term (f : cilState -> term -> ('a list -> 'b) -> 'b) (k : 'a list -> 'b) =
        GuardedStatedApplyk
            (fun state term k -> f {cilState with state = state} term k)
            cilState.state term id (List.concat >> k)

    let StatedConditionalExecutionAppendResultsCIL (cilState : cilState) conditionInvocation (thenBranch : (cilState -> (cilState list -> 'a) -> 'a)) elseBranch k =
        StatedConditionalExecution cilState.state conditionInvocation
            (fun state k -> thenBranch {cilState with state = state} k)
            (fun state k -> elseBranch {cilState with state = state} k)
            (fun x y -> [x; y])
            (List.concat >> k)

    let BranchOnNullCIL (cilState : cilState) term thenBranch elseBranch k =
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (IsNullReference term, state))
            thenBranch
            elseBranch
            k

    // ------------------------------- Pretty printing for cilState -------------------------------

    let private dumpSectionValue section value (sb : StringBuilder) =
        let sb = Utils.PrettyPrinting.dumpSection section sb
        Utils.PrettyPrinting.appendLine sb value

    let private dumpIp (ipStack : ipStack) =
        List.fold (fun acc entry -> sprintf "%s\n%O" acc entry) "" ipStack

    let ipAndMethodBase2String (codeLocation : codeLocation) =
        sprintf "Method: %O, offset = %d" codeLocation.method codeLocation.offset

    // TODO: print filterResult and IIE ?
    let dump (cilState : cilState) : string =
        let sb = (StringBuilder())
        let sb = dumpSectionValue "Starting ip" (sprintf "%O" cilState.startingIP) sb
        let sb = dumpSectionValue "IP" (dumpIp cilState.ipStack) sb
        let sb = dumpSectionValue "IIE" (sprintf "%O" cilState.iie) sb
        let sb = dumpSectionValue "Initial EvaluationStack Size" (sprintf "%O" cilState.initialEvaluationStackSize) sb
        let sb = Utils.PrettyPrinting.dumpDict "Level" id ipAndMethodBase2String id sb cilState.level
        let stateDump = Memory.Dump cilState.state
        let sb = dumpSectionValue "State" stateDump sb
        if sb.Length = 0 then "<EmptyCilState>" else sb.ToString()
