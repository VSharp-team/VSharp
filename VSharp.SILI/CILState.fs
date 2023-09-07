namespace VSharp.Interpreter.IL

open VSharp
open System.Text
open System.Collections.Generic
open VSharp.Core
open VSharp.Interpreter.IL
open ipOperations

type prefix =
    | Constrained of System.Type

[<ReferenceEquality>]
type cilState =
    {
        mutable ipStack : ipStack
        // This field stores information about instruction prefix (for example, '.constrained' prefix)
        mutable prefixContext : prefix list
        // TODO: get rid of currentLoc!
        // This field stores only approximate information and can't be used for getting the precise location. Instead, use ipStack.Head
        mutable currentLoc : codeLocation
        state : state
        mutable errorReported : bool
        mutable filterResult : term option
        //TODO: #mb frames list #mb transfer to Core.State
        mutable iie : InsufficientInformationException option
        mutable level : level
        startingIP : ip
        mutable initialEvaluationStackSize : uint32
        mutable stepsNumber : uint
        mutable suspended : bool
        mutable targets : Set<codeLocation>
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

    interface IGraphTrackableState with
        override this.CodeLocation = this.currentLoc
        override this.CallStack = Memory.StackTrace this.state.stack |> List.map (fun m -> m :?> Method)

type cilStateComparer(comparer) =
    interface IComparer<cilState> with
        override _.Compare(x : cilState, y : cilState) =
            comparer x y

module CilStateOperations =

    let mutable currentStateId = 0u
    let getNextStateId() =
        let nextId = currentStateId
        currentStateId <- currentStateId + 1u
        nextId

    let makeCilState entryMethod curV initialEvaluationStackSize state =
        let currentLoc = ip2codeLocation curV |> Option.get
        {
            ipStack = [curV]
            prefixContext = List.empty
            currentLoc = currentLoc
            state = state
            errorReported = false
            filterResult = None
            iie = None
            level = PersistentDict.empty
            startingIP = curV
            initialEvaluationStackSize = initialEvaluationStackSize
            stepsNumber = 0u
            suspended = false
            targets = Set.empty
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
        | [ Exit _ ] -> false
        | _ -> true

    let isUnhandledException (s : cilState) =
        match s.state.exceptionsRegister with
        | Unhandled _ -> true
        | _ -> false

    let isUnhandledExceptionOrError (s : cilState) =
        match s.state.exceptionsRegister with
        | Unhandled _ -> true
        | _ -> s.errorReported

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

    let hasRuntimeExceptionOrError (s : cilState) =
        match s.state.exceptionsRegister with
        | _ when s.errorReported -> true
        | Unhandled(_, isRuntime, _) -> isRuntime
        | _ -> false

    let hasReportedError (s : cilState) = s.errorReported

    let isStopped s = isIIEState s || stoppedByException s || not(isExecutable(s))

    let tryCurrentLoc = currentIp >> ip2codeLocation
    let currentLoc = tryCurrentLoc >> Option.get
    let startingLoc (s : cilState) = s.startingIP |> ip2codeLocation |> Option.get

    let violatesLevel (s : cilState) maxBound =
        match tryCurrentLoc s with
        | Some currLoc when PersistentDict.contains currLoc s.level ->
            s.level[currLoc] >= maxBound
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
        assert(List.isEmpty cilState.ipStack |> not)
        cilState.ipStack <- ip :: List.tail cilState.ipStack

    let setIpStack (ipStack : ipStack) (cilState : cilState) = cilState.ipStack <- ipStack
    let startingIpOf (cilState : cilState) = cilState.startingIP

    let pushPrefixContext (cilState : cilState) (prefix : prefix) =
        cilState.prefixContext <- prefix :: cilState.prefixContext

    let popPrefixContext (cilState : cilState) =
        match cilState.prefixContext with
        | prefix :: context ->
            cilState.prefixContext <- context
            Some prefix
        | _ -> None

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

    let incrementLevel (cilState : cilState) codeLocation =
        let lvl = cilState.level
        let oldValue = PersistentDict.tryFind lvl codeLocation |> Option.defaultValue 0u
        cilState.level <- PersistentDict.add codeLocation (oldValue + 1u) lvl

    let decrementLevel (cilState : cilState) codeLocation =
        let lvl = cilState.level
        let oldValue = PersistentDict.tryFind lvl codeLocation
        match oldValue with
        | Some value when value = 1u ->
            cilState.level <- PersistentDict.remove codeLocation lvl
        | Some value when value > 0u ->
            cilState.level <- PersistentDict.add codeLocation (value - 1u) lvl
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
        match v.term with
        | Nop -> internalfail "pushing 'NOP' value onto evaluation stack"
        | _ ->
            cilState.state.evaluationStack <- EvaluationStack.Push v cilState.state.evaluationStack
            cilState.lastPushInfo <- Some v

    let pushMany vs (cilState : cilState) =
        if List.contains (Nop()) vs then
            internalfail "pushing 'NOP' value onto evaluation stack"
        cilState.state.evaluationStack <- EvaluationStack.PushMany vs cilState.state.evaluationStack

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
        let prev = state.targets
        state.targets <- Set.add target prev
        prev.Count <> state.targets.Count

    let removeTarget (state : cilState) target =
        let prev = state.targets
        state.targets <- Set.remove target prev
        prev.Count <> state.targets.Count

    // ------------------------------------ Memory Interaction ------------------------------------

    let mutable private reportError : cilState -> string -> unit =
        fun _ _ -> internalfail "'reportError' is not ready"

    let mutable private reportFatalError : cilState -> string -> unit =
        fun _ _ -> internalfail "'reportError' is not ready"

    let configureErrorReporter reportErrorFunc reportFatalErrorFunc =
        reportError <- reportErrorFunc
        reportFatalError <- reportFatalErrorFunc

    type public ErrorReporter internal (reportError, reportFatalError, cilState) =
        let mutable cilState = cilState
        let mutable stateConfigured : bool = false

        static member ReportError cilState message =
            cilState.errorReported <- true
            reportError cilState message

        static member ReportFatalError cilState message =
            cilState.errorReported <- true
            reportFatalError cilState message

        interface IErrorReporter with
            override x.ReportError msg failCondition =
                assert stateConfigured
                let report state k =
                    let cilState = changeState cilState state
                    cilState.errorReported <- true
                    reportError cilState msg |> k
                StatedConditionalExecution cilState.state
                    (fun state k -> k (!!failCondition, state))
                    (fun _ k -> k ())
                    report
                    (fun _ _ -> [])
                    ignore

            override x.ReportFatalError msg failCondition =
                assert stateConfigured
                let report state k =
                    let cilState = changeState cilState state
                    cilState.errorReported <- true
                    reportFatalError cilState msg |> k
                StatedConditionalExecution cilState.state
                    (fun state k -> k (!!failCondition, state))
                    (fun _ k -> k ())
                    report
                    (fun _ _ -> [])
                    ignore

            override x.ConfigureState state =
                cilState <- changeState cilState state
                stateConfigured <- true

    let internal createErrorReporter cilState =
        ErrorReporter(reportError, reportFatalError, cilState)

    let read cilState ref =
        let reporter = createErrorReporter cilState
        Memory.ReadUnsafe reporter cilState.state ref

    let readField cilState term field =
        let reporter = createErrorReporter cilState
        Memory.ReadFieldUnsafe reporter cilState.state term field

    let readIndex cilState term index valueType =
        let reporter = createErrorReporter cilState
        Memory.ReadArrayIndexUnsafe reporter cilState.state term index valueType

    let write cilState ref value =
        let reporter = createErrorReporter cilState
        let states = Memory.WriteUnsafe reporter cilState.state ref value
        List.map (changeState cilState) states

    let writeClassField cilState ref field value =
        let states = Memory.WriteClassField cilState.state ref field value
        List.map (changeState cilState) states

    let writeStructField cilState term field value =
        let reporter = createErrorReporter cilState
        Memory.WriteStructFieldUnsafe reporter cilState.state term field value

    let writeIndex cilState term index value valueType =
        let reporter = createErrorReporter cilState
        let states = Memory.WriteArrayIndexUnsafe reporter cilState.state term index value valueType
        List.map (changeState cilState) states

    // ------------------------------- Helper functions for cilState -------------------------------

    // TODO: not used
    let moveIp offset (m : Method) cilState =
        assert m.HasBody
        let opCode = MethodBody.parseInstruction m offset
        let newIps =
            let nextTargets = MethodBody.findNextInstructionOffsetAndEdges opCode m.ILBytes offset
            match nextTargets with
            | UnconditionalBranch nextInstruction
            | FallThrough nextInstruction -> instruction m nextInstruction |> List.singleton
            | Return -> exit m |> List.singleton
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
