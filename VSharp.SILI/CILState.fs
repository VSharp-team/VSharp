namespace VSharp.Interpreter.IL

open VSharp
open System.Text
open System.Collections.Generic
open VSharp.Core
open VSharp.Interpreter.IL
open ipOperations

[<ReferenceEquality>]
type cilState =
    { mutable ipStack : ipStack
      // TODO: get rid of currentLoc!
      mutable currentLoc : codeLocation // This field stores only approximate information and can't be used for getting the precise location. Instead, use ipStack.Head
      state : state
      mutable filterResult : term option
      //TODO: #mb frames list #mb transfer to Core.State
      mutable iie : InsufficientInformationException option
      mutable level : level
      startingIP : ip
      mutable initialEvaluationStackSize : uint32
      mutable stepsNumber : uint
      mutable suspended : bool
      mutable targets : Set<codeLocation> option
      mutable lastPushInfo : term option
      /// <summary>
      /// All basic blocks visited by the state.
      /// </summary>
      mutable history : Set<codeLocation>
      /// <summary>
      /// If the state is not isolated (produced during forward execution), Some of it's entry point method, else None.
      /// </summary>
      entryMethod : Method option
      /// <summary>
      /// Deterministic state id.
      /// </summary>
      id : uint
    }
    with
    member x.Result with get() =
//        assert(Memory.CallStackSize x.state = 1)
        match EvaluationStack.Length x.state.evaluationStack with
        | _ when Memory.CallStackSize x.state > 2 -> internalfail "Finished state has many frames on stack! (possibly unhandled exception)"
        | 0 -> Nop
        | 1 ->
            let result = EvaluationStack.Pop x.state.evaluationStack |> fst
            match x.ipStack with
            | [Exit m] -> Types.Cast result m.ReturnType
            | _ when x.state.exceptionsRegister.UnhandledError -> Nop
            | _ -> internalfailf "Method is not finished! IpStack = %O" x.ipStack
        | _ -> internalfail "EvaluationStack size was bigger than 1"

    interface IGraphTrackableState with
        override this.CodeLocation = this.currentLoc
        override this.CallStack = Memory.StackTrace this.state.stack |> List.map (fun m -> m :?> Method)

type cilStateComparer(comparer) =
    interface IComparer<cilState> with
        override _.Compare(x : cilState, y : cilState) =
            comparer x y

module internal CilStateOperations =

    let mutable currentStateId = 0u
    let getNextStateId() =
        let nextId = currentStateId
        currentStateId <- currentStateId + 1u
        nextId

    let makeCilState entryMethod curV initialEvaluationStackSize state =
        let currentLoc = ip2codeLocation curV |> Option.get
        { ipStack = [curV]
          currentLoc = currentLoc
          state = state
          filterResult = None
          iie = None
          level = PersistentDict.empty
          startingIP = curV
          initialEvaluationStackSize = initialEvaluationStackSize
          stepsNumber = 0u
          suspended = false
          targets = None
          lastPushInfo = None
          history = Set.empty
          entryMethod = Some entryMethod
          id = getNextStateId()
        }

    let makeInitialState m state = makeCilState m (instruction m 0<offsets>) 0u state

    let mkCilStateHashComparer = cilStateComparer (fun a b -> a.GetHashCode().CompareTo(b.GetHashCode()))

    let isIsolated state = state.entryMethod.IsNone

    let entryMethodOf state =
        if isIsolated state then
            invalidOp "Isolated state doesn't have an entry method"
        state.entryMethod.Value

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

    let levelToUnsignedInt (lvl : level) = PersistentDict.fold (fun acc _ v -> max acc v) 0u lvl //TODO: remove it when ``level'' subtraction would be generalized
    let currentIp (s : cilState) =
        match s.ipStack with
        | [] -> __unreachable__()
        | h::_ -> h
//        List.head s.ipStack

    let stoppedByException (s : cilState) =
        match currentIp s with
        | SearchingForHandler([], []) -> true
        | _ -> false

    let hasRuntimeException (s : cilState) =
        match s.state.exceptionsRegister with
        | Unhandled(_, isRuntime) -> isRuntime
        | _ -> false

    let isStopped s = isIIEState s || stoppedByException s || not(isExecutable(s))

    let tryCurrentLoc = currentIp >> ip2codeLocation
    let currentLoc = tryCurrentLoc >> Option.get
    let startingLoc (s : cilState) = s.startingIP |> ip2codeLocation |> Option.get

    let violatesLevel (s : cilState) maxBound =
        match tryCurrentLoc s with
        | Some currLoc when PersistentDict.contains currLoc s.level ->
            s.level.[currLoc] >= maxBound
        | _ -> false

    // [NOTE] Obtaining exploring method
    let currentMethod = currentIp >> forceMethodOf

    let currentOffset = currentIp >> offsetOf

    let startsFromMethodBeginning (s : cilState) =
        match s.startingIP with
        | Instruction (0<offsets>, _) -> true
        | _ -> false

    let private moveCodeLoc (cilState : cilState) (ip : ip) =
        match ip2codeLocation ip with
        | Some loc when loc.method.HasBody -> cilState.currentLoc <- loc
        | _ -> ()

    let pushToIp (ip : ip) (cilState : cilState) =
        let loc = cilState.currentLoc
        match ip2codeLocation ip with
        | Some loc' when loc'.method.HasBody ->
            cilState.currentLoc <- loc'
            Application.addCallEdge loc loc'
        | _ -> ()
        cilState.ipStack <- ip :: cilState.ipStack

    let setCurrentIp (ip : ip) (cilState : cilState) =
        moveCodeLoc cilState ip
        cilState.ipStack <- ip :: List.tail cilState.ipStack

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
                            startingIP = cilState1.startingIP; iie = iie; id = getNextStateId()}
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

    let addLocationToHistory (cilState : cilState) (loc : codeLocation) =
        cilState.history <- Set.add loc cilState.history

    // ------------------------------- Helper functions for cilState and state interaction -------------------------------

    let stateOf (cilState : cilState) = cilState.state
    let popFrameOf (cilState : cilState) =
        Memory.PopFrame cilState.state
        let ip = List.tail cilState.ipStack
        cilState.ipStack <- ip
        match ip with
        | ip::_ -> moveCodeLoc cilState ip
        | [] -> ()

    let setCurrentTime time (cilState : cilState) = cilState.state.currentTime <- time
    let setEvaluationStack evaluationStack (cilState : cilState) = cilState.state.evaluationStack <- evaluationStack

    let clearEvaluationStackLastFrame (cilState : cilState) =
        cilState.state.evaluationStack <- EvaluationStack.ClearActiveFrame cilState.state.evaluationStack

    // TODO: Not mutable -- copies cilState #do
    let changeState (cilState : cilState) state =
        if LanguagePrimitives.PhysicalEquality state cilState.state then cilState
        else {cilState with state = state; id = getNextStateId()}

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

    let addTarget (state : cilState) target =
        match state.targets with
        | Some targets -> state.targets <- Some <| Set.add target targets
        | None -> state.targets <- Some (Set.add target Set.empty)

    let removeTarget (state : cilState) target =
        match state.targets with
        | Some targets ->
            let newTargets = Set.remove target targets
            if newTargets.Count = 0 then
                state.targets <- None
            else
                state.targets <- Some <| Set.remove target targets
        | None -> ()

    let checkTargets (state : cilState) =
        match state.targets with
        | Some targets -> targets.Count <> 0
        | None -> true

    // ------------------------------- Helper functions for cilState -------------------------------

    let moveIp offset (m : Method) cilState =
        assert m.HasBody
        let opCode = MethodBody.parseInstruction m offset
        let newIps =
            let nextTargets = MethodBody.findNextInstructionOffsetAndEdges opCode m.ILBytes offset
            match nextTargets with
            | UnconditionalBranch nextInstruction
            | FallThrough nextInstruction -> instruction m nextInstruction :: []
            | Return -> exit m :: []
            | ExceptionMechanism ->
                // TODO: use ExceptionMechanism? #do
//                let toObserve = __notImplemented__()
//                searchingForHandler toObserve 0 :: []
                __notImplemented__()
            | ConditionalBranch (fall, targets) -> fall :: targets |> List.map (instruction m)
        List.map (fun ip -> setCurrentIp ip cilState) newIps

    let GuardedApplyCIL (cilState : cilState) term (f : cilState -> term -> ('a list -> 'b) -> 'b) (k : 'a list -> 'b) =
        let mkCilState state =
            if LanguagePrimitives.PhysicalEquality state cilState.state then cilState
            else {cilState with state = state; id = getNextStateId()}
        GuardedStatedApplyk
            (fun state term k -> f (mkCilState state) term k)
            cilState.state term id (List.concat >> k)

    let StatedConditionalExecutionCIL (cilState : cilState) conditionInvocation thenBranch elseBranch k =
        let origCilState = {cilState with state = cilState.state}
        let mkCilState state =
            if LanguagePrimitives.PhysicalEquality state cilState.state then cilState
            else {origCilState with state = state; id = getNextStateId()}
        StatedConditionalExecution cilState.state conditionInvocation
            (fun state k -> thenBranch (mkCilState state) k)
            (fun state k -> elseBranch (mkCilState state) k)
            (fun x y -> [x; y])
            (List.concat >> k)

    let BranchOnNullCIL (cilState : cilState) term thenBranch elseBranch k =
        StatedConditionalExecutionCIL cilState
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
