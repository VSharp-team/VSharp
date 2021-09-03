namespace VSharp.Interpreter.IL

open VSharp
open System.Text
open VSharp.Core
open ipOperations

[<ReferenceEquality>]
type cilState =
    { mutable ipStack : ipStack
      state : state
      mutable filterResult : term option
      //TODO: #mb frames list #mb transfer to Core.State
      mutable iie : InsufficientInformationException option
      mutable level : level
      mutable startingIP : ip
      mutable initialEvaluationStackSize : uint32
      mutable stepsNumber : uint
      mutable ownedByConcolic : bool
      mutable lastPushInfo : term option
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
          stepsNumber = 0u
          ownedByConcolic = false
          lastPushInfo = None
        }

    let makeInitialState m state = makeCilState (instruction m 0) 0u state


    let isIIEState (s : cilState) = Option.isSome s.iie

    let isExecutable (s : cilState) =
        match s.ipStack with
        // TODO: do better #do #Dima
//        | [] -> false
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


    let levelToUnsignedInt (lvl : level) = PersistentDict.fold (fun acc _ v -> max acc v) 0u lvl //TODO: remove it when ``level'' subtraction would be generalized
    let currentIp (s : cilState) =
        match s.ipStack with
        | [] -> __unreachable__()
        | h::_ -> h
//        List.head s.ipStack

    let currentLoc = currentIp >> ip2codeLocation >> Option.get
    let startingLoc (s : cilState) = s.startingIP |> ip2codeLocation |> Option.get
    let methodOf = function
        | Exit m
        | Instruction(_, m)
        | Leave(_, _, _, m) -> m
        | _ -> __notImplemented__()

    let offsetOf = function
        | Instruction(offset, _) -> Some offset
        | Exit _
        | Leave _ -> None
        | _ -> __notImplemented__()

    // [NOTE] Obtaining exploring method
    let currentMethod = currentIp >> methodOf

    let currentOffset = currentIp >> offsetOf

    let startsFromMethodBeginning (s : cilState) =
        match s.startingIP with
        | Instruction (0, _) -> true
        | _ -> false
    let pushToIp (ip : ip) (cilState : cilState) = cilState.ipStack <- ip :: cilState.ipStack
    let setCurrentIp (ip : ip) (cilState : cilState) = cilState.ipStack <- ip :: List.tail cilState.ipStack

    let setIpStack (ipStack : ipStack) (cilState : cilState) = cilState.ipStack <- ipStack
    let startingIpOf (cilState : cilState) = cilState.startingIP

    let composeIps (oldIpStack : ipStack) (newIpStack : ipStack) = newIpStack @ oldIpStack

    let composeLevel (lvl1 : level) (lvl2 : level) =
        let composeOne (lvl : level) k v =
            let oldValue = PersistentDict.tryFind lvl k |> Option.defaultValue 0u
            PersistentDict.add k (v + oldValue) lvl
        PersistentDict.fold composeOne lvl1 lvl2

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
        let _, leftEvaluationStack = EvaluationStack.PopMany (int cilState2.initialEvaluationStackSize) cilState1.state.evaluationStack
        let makeResultState (state : state) =
            let state' = { state with evaluationStack = EvaluationStack.Union leftEvaluationStack state.evaluationStack }
            {cilState2 with state = state'; ipStack = ip; level = level; initialEvaluationStackSize = cilState1.initialEvaluationStackSize
                            startingIP = cilState1.startingIP; iie = iie}
        List.map makeResultState states

    let incrementLevel (cilState : cilState) k =
        let lvl = cilState.level
        let oldValue = PersistentDict.tryFind lvl k |> Option.defaultValue 0u
        cilState.level <- PersistentDict.add k (oldValue + 1u) lvl

    let decrementLevel (cilState : cilState) k =
        let lvl = cilState.level
        let oldValue = PersistentDict.tryFind lvl k
        match oldValue with
        | Some value when value > 0u -> cilState.level <- PersistentDict.add k (value - 1u) lvl
        | _ -> ()

    // ------------------------------- Helper functions for cilState and state interaction -------------------------------

    let stateOf (cilState : cilState) = cilState.state
    let popFrameOf (cilState : cilState) =
        Memory.PopFrame cilState.state
        let ip = List.tail cilState.ipStack
        cilState.ipStack <- ip

    let setCurrentTime time (cilState : cilState) = cilState.state.currentTime <- time
    let setEvaluationStack evaluationStack (cilState : cilState) = cilState.state.evaluationStack <- evaluationStack

    let clearEvaluationStackLastFrame (cilState : cilState) =
        cilState.state.evaluationStack <- EvaluationStack.ClearActiveFrame cilState.state.evaluationStack

    // TODO: Not mutable -- copies cilState #do
    let changeState (cilState : cilState) state =
        if LanguagePrimitives.PhysicalEquality state cilState.state then cilState
        else {cilState with state = state}

    let setException exc (cilState : cilState) = cilState.state.exceptionsRegister <- exc

    let push v (cilState : cilState) =
        cilState.state.evaluationStack <- EvaluationStack.Push v cilState.state.evaluationStack
        cilState.lastPushInfo <- Some v
    let pushMany vs (cilState : cilState) = cilState.state.evaluationStack <- EvaluationStack.PushMany vs cilState.state.evaluationStack

    let peek (cilState : cilState) =
        EvaluationStack.Pop cilState.state.evaluationStack |> fst
    let peek2 (cilState : cilState) =
        let stack = cilState.state.evaluationStack
        let arg2, stack = EvaluationStack.Pop stack
        let arg1, _ = EvaluationStack.Pop stack
        arg2, arg1
    let pop (cilState : cilState) =
        let v, evaluationStack = EvaluationStack.Pop cilState.state.evaluationStack
        cilState.state.evaluationStack <- evaluationStack
        v
    let pop2 (cilState : cilState) =
        let arg2 = pop cilState
        let arg1 = pop cilState
        arg2, arg1
    let pop3 (cilState : cilState) =
        let arg3 = pop cilState
        let arg2 = pop cilState
        let arg1 = pop cilState
        arg3, arg2, arg1

    let pushNewObjForValueTypes (afterCall : cilState) =
        let ref = pop afterCall
        let value = Memory.Read afterCall.state ref
        push value afterCall

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
        let mutable onDemandCopy = cilState
        StatedConditionalExecution cilState.state condition
            (fun state k -> thenBranch (onDemandCopy <- {cilState with state = state}; changeState cilState state) k)
            (fun state k -> elseBranch (changeState onDemandCopy state) k)
            (fun x y -> List.append x y |> List.singleton)
            (List.head >> k)
    let GuardedApplyCIL (cilState : cilState) term (f : cilState -> term -> ('a list -> 'b) -> 'b) (k : 'a list -> 'b) =
        let mkCilState state =
            if LanguagePrimitives.PhysicalEquality state cilState.state then cilState
            else {cilState with state = state}
        GuardedStatedApplyk
            (fun state term k -> f (mkCilState state) term k)
            cilState.state term id (List.concat >> k)

    let StatedConditionalExecutionAppendResultsCIL (cilState : cilState) conditionInvocation thenBranch elseBranch k =
        let mkCilState state =
            if LanguagePrimitives.PhysicalEquality state cilState.state then cilState
            else {cilState with state = state}
        StatedConditionalExecution cilState.state conditionInvocation
            (fun state k -> thenBranch (mkCilState state) k)
            (fun state k -> elseBranch (mkCilState state) k)
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
        let stateDump = Print.Dump cilState.state
        let sb = dumpSectionValue "State" stateDump sb
        if sb.Length = 0 then "<EmptyCilState>" else sb.ToString()
