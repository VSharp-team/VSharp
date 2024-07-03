namespace VSharp.Interpreter.IL

open System.IO
open VSharp
open System.Text
open System.Collections.Generic
open VSharp.Core
open VSharp.Interpreter.IL
open IpOperations

module CilState =

    type prefix =
        | Constrained of System.Type

    let mutable currentStateId = 0u
    let getNextStateId() =
        let nextId = currentStateId
        currentStateId <- currentStateId + 1u
        nextId

    type public ErrorReporter internal (cilState : cilState) =
        let mutable cilState = cilState
        let mutable stateConfigured : bool = false

        static let mutable reportError : cilState -> string -> unit =
            fun _ _ -> internalfail "'reportError' is not ready"
        static let mutable reportFatalError : cilState -> string -> unit =
            fun _ _ -> internalfail "'reportFatalError' is not ready"

        static member Configure reportErrorFunc reportFatalErrorFunc =
            reportError <- reportErrorFunc
            reportFatalError <- reportFatalErrorFunc

        static member ReportError (cilState : cilState) message =
            cilState.ReportError()
            reportError cilState message

        static member ReportFatalError (cilState : cilState) message =
            cilState.ReportError()
            reportFatalError cilState message

        interface IErrorReporter with
            override x.ReportError msg failCondition =
                assert stateConfigured
                let report state k =
                    let cilState = cilState.ChangeState state
                    cilState.ReportError()
                    reportError cilState msg |> k
                let mutable isAlive = false
                StatedConditionalExecution cilState.state
                    (fun state k -> k (!!failCondition, state))
                    (fun _ k -> k (isAlive <- true))
                    report
                    (fun _ _ -> [])
                    ignore
                isAlive

            override x.ReportFatalError msg failCondition =
                assert stateConfigured
                let report state k =
                    let cilState = cilState.ChangeState state
                    cilState.ReportError()
                    reportFatalError cilState msg |> k
                let mutable isAlive = false
                StatedConditionalExecution cilState.state
                    (fun state k -> k (!!failCondition, state))
                    (fun _ k -> k (isAlive <- true))
                    report
                    (fun _ _ -> [])
                    ignore
                isAlive

            override x.ConfigureState state =
                cilState <- cilState.ChangeState state
                stateConfigured <- true

    and webConfiguration =
        {
            environmentName : string
            contentRootPath : DirectoryInfo
            applicationName : string
        }

    and [<ReferenceEquality>] cilState =
        {
            mutable ipStack : ipStack
            // This field stores information about instruction prefix (for example, '.constrained' prefix)
            mutable prefixContext : prefix list
            // TODO: get rid of approximateLoc!
            // This field stores only approximate information and can't be used for getting the precise location. Instead, use ipStack.Head
            mutable approximateLoc : codeLocation
            state : state
            mutable stackArrays : pset<concreteHeapAddress>
            mutable errorReported : bool
            mutable filterResult : term option
            mutable iie : InsufficientInformationException option
            mutable level : level
            startingIP : instructionPointer
            mutable initialEvaluationStackSize : uint32
            mutable stepsNumber : uint
            mutable suspended : bool
            mutable targets : Set<codeLocation>
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
            internalId : uint
            webConfiguration : webConfiguration option
        }

        static member private CommonCreateInitial (m : Method) (state : state) webConfiguration =
            let ip = Instruction(0<offsets>, m)
            let approximateLoc = ip.ToCodeLocation() |> Option.get
            {
                ipStack = List.singleton ip
                prefixContext = List.empty
                approximateLoc = approximateLoc
                state = state
                stackArrays = PersistentSet.empty
                errorReported = false
                filterResult = None
                iie = None
                level = PersistentDict.empty
                startingIP = ip
                initialEvaluationStackSize = 0u
                stepsNumber = 0u
                suspended = false
                targets = Set.empty
                history = Set.empty
                entryMethod = Some m
                internalId = getNextStateId()
                webConfiguration = webConfiguration
            }

        static member CreateInitial (m : Method) (state : state) =
            cilState.CommonCreateInitial m state None

        static member CreateWebInitial (m : Method) (state : state) (webConfiguration : webConfiguration) =
            cilState.CommonCreateInitial m state (Some webConfiguration)

        member private x.ErrorReporter = lazy ErrorReporter(x)

        member x.IsIsolated with get() = x.entryMethod.IsNone

        member x.WebExploration with get() = Option.isSome x.webConfiguration

        member x.EntryMethod with get() =
            if x.IsIsolated then invalidOp "Isolated state doesn't have an entry method"
            x.entryMethod.Value

        member x.StartsFromMethodBeginning with get() =
            match x.startingIP with
            | Instruction (0<offsets>, _) -> true
            | _ -> false

        member x.SetCurrentTime time = x.state.currentTime <- time

        // -------------------- Exception and errors operations --------------------

        member x.SetException exc =
            x.state.exceptionsRegister <- x.state.exceptionsRegister.Tail.Push exc

        member x.IsUnhandledException with get() =
            match x.state.exceptionsRegister.Peek with
            | Unhandled _ -> true
            | _ -> false

        member x.IsUnhandledExceptionOrError with get() =
            match x.state.exceptionsRegister.Peek with
            | Unhandled _ -> true
            | _ -> x.errorReported

        member x.HasReportedError with get() = x.errorReported
        member x.ReportError() = x.errorReported <- true

        member x.IsStoppedByException with get() =
            match x.CurrentIp with
            | EmptySearchingForHandler -> true
            | _ -> false

        member x.HasRuntimeExceptionOrError with get() =
            match x.state.exceptionsRegister.Peek with
            | _ when x.errorReported -> true
            | Unhandled(_, isRuntime, _) -> isRuntime
            | _ -> false

        member x.IsIIEState with get() = Option.isSome x.iie

        member x.SetIIE (e : InsufficientInformationException) =
            x.iie <- Some e

        member x.IsExecutable with get() =
            match x.ipStack with
            | [] -> __unreachable__()
            | [ Exit _ ] -> false
            | _ -> true

        member x.IsStopped with get() =
            x.IsIIEState || x.IsStoppedByException || not x.IsExecutable

        member x.NewExceptionRegister() =
            x.state.exceptionsRegister <- x.state.exceptionsRegister.Push NoException

        member x.PopExceptionRegister() =
            x.state.exceptionsRegister <- x.state.exceptionsRegister.Tail

        member x.ToUnhandledException() =
            x.state.exceptionsRegister <- x.state.exceptionsRegister.TransformToUnhandled()

        member x.ToCaughtException() =
            x.state.exceptionsRegister <- x.state.exceptionsRegister.TransformToCaught()

        member x.MoveDownExceptionRegister() =
            let elem, rest = x.state.exceptionsRegister.Pop()
            x.state.exceptionsRegister <- rest.Tail.Push elem

        // -------------------- Instruction pointer operations --------------------

        member x.CurrentIp with get() =
            match x.ipStack with
            | [] -> internalfail "currentIp: 'IP' stack is empty"
            | h :: _ -> h

        // Obtaining exploring method
        member x.CurrentMethod with get() = x.CurrentIp.ForceMethod()

        member x.CurrentOffset with get() = x.CurrentIp.Offset

        member x.PushToIp (ip : instructionPointer) =
            let loc = x.approximateLoc
            match ip.ToCodeLocation() with
            | Some loc' when loc'.method.HasBody ->
                x.approximateLoc <- loc'
                Application.addCallEdge loc loc'
            | _ -> ()
            x.ipStack <- ip :: x.ipStack

        member x.SetCurrentIp (ip : instructionPointer) =
            x.MoveCodeLoc ip
            assert(List.isEmpty x.ipStack |> not)
            x.ipStack <- ip :: List.tail x.ipStack

        member x.SetCurrentIpSafe (ip : instructionPointer) =
            let ip = x.CurrentIp.ChangeInnerIp ip
            x.SetCurrentIp ip

        member x.ReplaceLastIp (ip : instructionPointer) =
            let newIp = x.CurrentIp.ReplaceRecIp ip
            x.SetCurrentIp newIp

        member x.MarkExit (m : Method) =
            match x.ipStack with
            | ip :: ips ->
                assert(ip.Method = Some m)
                x.ipStack <- (Exit m) :: ips
            | [] -> __unreachable__()

        member x.TryGetFilterIp with get() = x.ipStack |> List.tryFind (fun ip -> ip.IsInFilter)

        member x.TryCurrentLoc with get() = x.CurrentIp.ToCodeLocation()

        member x.CurrentLoc with get() = x.TryCurrentLoc |> Option.get

        member x.StartingLoc with get() = x.startingIP.ToCodeLocation() |> Option.get

        member x.CodeLocations with get() =
            x.ipStack
            |> List.takeWhile (fun ip -> not ip.IsFilter)
            |> List.map (fun ip -> ip.ForceCodeLocation())

        member private x.MoveCodeLoc (ip : instructionPointer) =
            match ip.ToCodeLocation() with
            | Some loc when loc.method.HasBody -> x.approximateLoc <- loc
            | _ -> ()

        // -------------------- Prefix context operations --------------------

        member x.PushPrefixContext (prefix : prefix) =
            x.prefixContext <- prefix :: x.prefixContext

        member x.PopPrefixContext() =
            match x.prefixContext with
            | prefix :: context ->
                x.prefixContext <- context
                Some prefix
            | _ -> None

        // -------------------- Stack arrays operations --------------------

        member x.AddStackArray (address : concreteHeapAddress) =
            x.stackArrays <- PersistentSet.add x.stackArrays address

        member x.IsStackArray ref =
            match ref.term with
            | HeapRef({term = ConcreteHeapAddress address}, _)
            | Ref(ArrayIndex({term = ConcreteHeapAddress address}, _, _))
            | Ptr(HeapLocation({term = ConcreteHeapAddress address}, _), _, _) ->
                PersistentSet.contains address x.stackArrays
            | _ -> false

        // -------------------- Level operations --------------------

        member x.IncrementLevel codeLocation =
            let oldValue = PersistentDict.tryFind x.level codeLocation |> Option.defaultValue 0u
            x.level <- PersistentDict.add codeLocation (oldValue + 1u) x.level

        member x.DecrementLevel codeLocation =
            let oldValue = PersistentDict.tryFind x.level codeLocation
            match oldValue with
            | Some value when value = 1u ->
                x.level <- PersistentDict.remove codeLocation x.level
            | Some value when value > 0u ->
                x.level <- PersistentDict.add codeLocation (value - 1u) x.level
            | _ -> ()

        member x.ViolatesLevel maxBound =
            match x.TryCurrentLoc with
            | Some currLoc when PersistentDict.contains currLoc x.level ->
                x.level[currLoc] >= maxBound
            | _ -> false

        member x.LevelOfLocation loc =
            if PersistentDict.contains loc x.level then x.level[loc] else 0u

        member x.Level with get() = Level.levelToUnsignedInt x.level

        // -------------------- History operations --------------------

        member x.AddLocationToHistory (loc : codeLocation) =
            x.history <- Set.add loc x.history

        // -------------------- EvaluationStack operations --------------------

        member x.ClearEvaluationStackLastFrame() =
            let memory = x.state.memory
            memory.EvaluationStack <- EvaluationStack.ClearActiveFrame memory.EvaluationStack

        member x.Push v =
            let memory = x.state.memory
            match v.term with
            | Nop -> internalfail "pushing 'NOP' value onto evaluation stack"
            | _ -> memory.EvaluationStack <- EvaluationStack.Push v memory.EvaluationStack

        member x.PushMany vs =
            if List.contains (Nop()) vs then
                internalfail "pushing 'NOP' value onto evaluation stack"
            let memory = x.state.memory
            memory.EvaluationStack <- EvaluationStack.PushMany vs memory.EvaluationStack

        member x.Peek() = EvaluationStack.Pop x.state.memory.EvaluationStack |> fst

        member x.Peek2() =
            let stack = x.state.memory.EvaluationStack
            let arg2, stack = EvaluationStack.Pop stack
            let arg1, _ = EvaluationStack.Pop stack
            arg2, arg1

        member x.Pop() =
            let memory = x.state.memory
            let v, evaluationStack = EvaluationStack.Pop memory.EvaluationStack
            memory.EvaluationStack <- evaluationStack
            v

        member x.Pop2() =
            let arg2 = x.Pop()
            let arg1 = x.Pop()
            arg2, arg1

        member x.Pop3() =
            let arg3 = x.Pop()
            let arg2 = x.Pop()
            let arg1 = x.Pop()
            arg3, arg2, arg1

        member x.PopMany (count : int) =
            let memory = x.state.memory
            let parameters, evaluationStack = EvaluationStack.PopMany count memory.EvaluationStack
            memory.EvaluationStack <- evaluationStack
            parameters

        member x.PushNewObjForValueTypes() =
            let ref = x.Pop()
            let value = Memory.Read x.state ref
            x.Push value

        // -------------------- Filter result operations --------------------

        member x.SetFilterResult (value : term) =
            x.filterResult <- Some value

        member x.ClearFilterResult() =
            x.filterResult <- None

        // -------------------- Targets operations --------------------

        member x.AddTarget target =
            let prev = x.targets
            x.targets <- Set.add target prev
            prev.Count <> x.targets.Count

        member x.RemoveTarget target =
            let prev = x.targets
            x.targets <- Set.remove target prev
            prev.Count <> x.targets.Count

        member x.ClearTargets() =
            x.targets <- Set.empty

        // -------------------- Memory interaction --------------------

        member x.StackSize with get() =
            assert(EvaluationStack.FramesCount x.state.memory.EvaluationStack = Memory.CallStackSize x.state)
            List.length x.ipStack

        member x.PopFrame() =
            Memory.PopFrame x.state
            let ip = List.tail x.ipStack
            x.ipStack <- ip
            assert(EvaluationStack.FramesCount x.state.memory.EvaluationStack = Memory.CallStackSize x.state)
            match ip with
            | ip :: _ -> x.MoveCodeLoc ip
            | [] -> ()

        member x.ClearStack() =
            Memory.ClearStack x.state
            x.ipStack <- List.empty

        member x.Read ref =
            Memory.ReadUnsafe x.ErrorReporter.Value x.state ref

        member x.ReadField term field =
            Memory.ReadFieldUnsafe x.ErrorReporter.Value x.state term field

        member x.ReadIndex term index valueType =
            Memory.ReadArrayIndexUnsafe x.ErrorReporter.Value x.state term index valueType

        member x.Write ref value =
            Memory.WriteUnsafe x.ErrorReporter.Value x.state ref value

        member x.WriteClassField ref field value =
            Memory.WriteClassFieldUnsafe x.ErrorReporter.Value x.state ref field value

        member x.WriteStructField term field value =
            Memory.WriteStructFieldUnsafe x.ErrorReporter.Value x.state term field value

        member x.WriteIndex term index value valueType =
            Memory.WriteArrayIndexUnsafe x.ErrorReporter.Value x.state term index value valueType

        // -------------------------- Branching --------------------------

        member x.GuardedApplyCIL term (f : cilState -> term -> ('a list -> 'b) -> 'b) (k : 'a list -> 'b) =
            GuardedStatedApplyk
                (fun state term k -> f (x.ChangeState state) term k)
                x.state term id (List.concat >> k)

        member x.StatedConditionalExecutionCIL conditionInvocation thenBranch elseBranch k =
            let clone = { x with state = x.state }
            let mkCilState state' =
                if LanguagePrimitives.PhysicalEquality state' x.state then x
                else clone.Copy(state')
            StatedConditionalExecution x.state conditionInvocation
                (fun state k -> thenBranch (mkCilState state) k)
                (fun state k -> elseBranch (mkCilState state) k)
                (fun x y -> [x; y])
                (List.concat >> k)

        member x.BranchOnNullCIL term thenBranch elseBranch k =
            x.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference term, state))
                thenBranch
                elseBranch
                k

        // -------------------- Dumping --------------------

        member private x.DumpSectionValue section value (sb : StringBuilder) =
            let sb = Utils.PrettyPrinting.dumpSection section sb
            Utils.PrettyPrinting.appendLine sb value

        member private x.DumpIpStack (ipStack : ipStack) =
            List.fold (fun acc entry -> $"{acc}\n{entry}") "" ipStack

        member private x.Dump() : string =
            let sb = StringBuilder()
            let sb = x.DumpSectionValue "Starting ip" $"{x.startingIP}" sb
            let sb = x.DumpSectionValue "IP" (x.DumpIpStack x.ipStack) sb
            let sb = x.DumpSectionValue "IIE" $"{x.iie}" sb
            let sb = x.DumpSectionValue "Initial EvaluationStack Size" $"{x.initialEvaluationStackSize}" sb
            let sb = Utils.PrettyPrinting.dumpDict "Level" id toString id sb x.level
            let sb = x.DumpSectionValue "State" (Print.Dump x.state) sb
            if sb.Length = 0 then "<EmptyCilState>" else sb.ToString()

        // -------------------- Changing inner state --------------------

        member x.Copy(state : state) =
            { x with state = state; internalId = getNextStateId() }

        // This function copies cilState, instead of mutation
        member x.ChangeState state' : cilState =
            if LanguagePrimitives.PhysicalEquality state' x.state then x
            else x.Copy(state')

        // -------------------- Steps number --------------------

        member x.IncrementStepsNumber() =
            x.stepsNumber <- x.stepsNumber + 1u

        // -------------------- Overriding methods --------------------

        override x.ToString() = System.String.Empty

        interface IGraphTrackableState with
            override this.CodeLocation = this.approximateLoc
            override this.CallStack = Memory.StackTrace this.state.memory.Stack |> List.map (fun m -> m :?> Method)

module CilStateOperations =
    open CilState

    type cilStateComparer() =
        interface IComparer<cilState> with
            override _.Compare(x : cilState, y : cilState) =
                x.GetHashCode().CompareTo(y.GetHashCode())

    let mkCilStateHashComparer = cilStateComparer()
