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
      framesForBypass : frames                             // Observed stack frames not having an exception handler
      iie : InsufficientInformationException option
      level : level
      startingIP : ip
      initialOpStackSize : uint32
    }

module internal CilStateOperations =

    let makeCilState curV initialOpStackSize state =
        { ipStack = [curV]
          state = state
          filterResult = None
          framesForBypass = []
          iie = None
          level = PersistentDict.empty
          startingIP = curV
          initialOpStackSize = initialOpStackSize
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
    let rec currentMethod = function
        | Exit m
        | Instruction(_, m)
        | Leave(_, _, m) -> m
        | _ -> __notImplemented__()
    let startsFromMethodBeginning (s : cilState) =
        match s.startingIP with
        | Instruction (0, _) -> true
        | _ -> false
//    let currentMethod (s : cilState) = (List.head s.state.frames).func.Method
    let pushToIp (ip : ip) (cilState : cilState) = {cilState with ipStack = ip :: cilState.ipStack}

    let rec withLastIp (ip : ip) (cilState : cilState) =
        match cilState.ipStack with
        | _ :: ips -> {cilState with ipStack = ip :: ips}
        | [] -> __unreachable__()
//        | {label = Exit} :: ips -> moveCurrentIp ips
//        | {label = Instruction _} as ip :: ips ->
//            let nextIps = CFG.moveCurrentIp ip
//            assert(List.length nextIps = 1)
//            List.head nextIps :: ips
//        | [] -> __unreachable__()
//        | _ -> __notImplemented__()
    let withIp (ipStack : ipStack) (cilState : cilState) = {cilState with ipStack = ipStack}
    let startingIpOf (cilState : cilState) = cilState.startingIP

    let composeIps (oldIpStack : ipStack) (newIpStack : ipStack) = newIpStack @ oldIpStack
//        match newIps, oldIps with
//        | {label = Exit} :: [] as exit, [] -> exit
//        | {label = Exit} :: [], _ -> oldIps // no need to moveCurrentIp, maybe we want to execute current ip
//        | _ -> newIps @ oldIps

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
        let _, leftOpStack = Memory.PopArgumentsFromOpStack (int cilState2.initialOpStackSize) cilState1.state.opStack
        let makeResultState (state : state) =
            let state' = {state with opStack = Memory.UnionOpStacks state.opStack leftOpStack}
            {cilState2 with state = state'; ipStack = ip; level = level; initialOpStackSize = cilState1.initialOpStackSize
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

    let emptyOpStack = Memory.EmptyState.opStack
    let withCurrentTime time (cilState : cilState) = {cilState with state = {cilState.state with currentTime = time}}
    let withOpStack opStack (cilState : cilState) = {cilState with state = {cilState.state with opStack = opStack}}

    let withState state (cilState : cilState) = {cilState with state = state}
    let changeState (cilState : cilState) state = {cilState with state = state}

    let withNoResult (cilState : cilState) = {cilState with state = {cilState.state with returnRegister = None}}
    let withException exc (cilState : cilState) = {cilState with state = {cilState.state with exceptionsRegister = exc}}

    let push v (cilState : cilState) = {cilState with state = {cilState.state with opStack = Memory.PushToOpStack v cilState.state.opStack}}
    let pop (cilState : cilState) =
        let v, opStack = Memory.PopFromOpStack cilState.state.opStack
        v, {cilState with state = {cilState.state with opStack = opStack}}
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

    let pushResultToOperationalStack (cilStates : cilState list) =
        cilStates |> List.map (fun (cilState : cilState) ->
            let state = cilState.state
            if state.exceptionsRegister.UnhandledError then cilState // TODO: check whether opStack := [] is needed
            else
                let opStack =
                    match state.returnRegister with
                    | None -> state.opStack
                    | Some r -> Memory.PushToOpStack r state.opStack
                let state = {state with returnRegister = None; opStack = opStack}
                {cilState with state = state})

    // ------------------------------- Helper functions for cilState -------------------------------

    let rec moveIpStack (cilState : cilState) : cilState list =
        match cilState.ipStack with
        | Instruction(offset, m) :: _ ->
            if offset = 0 then Logger.info "Starting to explore method %O" (Reflection.getFullMethodName m) // TODO: delete (for info) #do
            let cfg = CFG.findCfg m
            let opCode = Instruction.parseInstruction m offset
            let newIps =
                if Instruction.isLeaveOpCode opCode || opCode = Emit.OpCodes.Endfinally
                then cfg.graph.[offset] |> Seq.map (instruction m) |> List.ofSeq
                else
                    let nextTargets = Instruction.findNextInstructionOffsetAndEdges opCode cfg.ilBytes offset
                    match nextTargets with
                    | UnconditionalBranch nextInstruction
                    | FallThrough nextInstruction -> instruction m nextInstruction :: []
                    | Return -> exit m :: []
                    | ExceptionMechanism ->
                        let toObserve = __notImplemented__()
                        searchingForHandler toObserve 0 :: []
                    | ConditionalBranch targets -> targets |> List.map (instruction m)
            List.map (fun ip -> withLastIp ip cilState) newIps
        | Exit _ :: [] when startsFromMethodBeginning cilState ->
            // the whole method is executed
            withCurrentTime [] cilState |> popFrameOf |> List.singleton // TODO: #ask Misha about current time
        | Exit _ :: [] ->
            popFrameOf cilState :: [] // some part of method is executed
        | Exit m :: ips' when Reflection.isStaticConstructor m ->
            Logger.info "Done with method %s" (Reflection.getFullMethodName m) // TODO: delete (for info) #do
            cilState |> popFrameOf |> withIp ips' |> List.singleton
        | Exit callee :: (Instruction(offset, caller) as ip) :: ips' ->
            Logger.info "Done with method %s" (Reflection.getFullMethodName callee) // TODO: delete (for info) #do
            // TODO: assert(isCallIp ip)
            let callSite = Instruction.parseCallSite caller offset
            let cilState =
                if callSite.opCode = Emit.OpCodes.Newobj && callSite.calledMethod.DeclaringType.IsValueType then
                    pushNewObjForValueTypes cilState
                else cilState
            cilState |> popFrameOf |> withIp (ip :: ips') |> moveIpStack
        | Exit _ :: Exit _ :: _ -> __unreachable__()
        | _ -> __notImplemented__()

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
        let sb = dumpSectionValue "Initial OpStack Size" (sprintf "%O" cilState.initialOpStackSize) sb
        let sb = Utils.PrettyPrinting.dumpDict "Level" id ipAndMethodBase2String id sb cilState.level
        let stateDump = Memory.Dump cilState.state
        let sb = dumpSectionValue "State" stateDump sb
        if sb.Length = 0 then "<EmptyCilState>" else sb.ToString()
