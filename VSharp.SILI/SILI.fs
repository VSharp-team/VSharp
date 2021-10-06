namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Collections.Generic
open System.IO
open FSharpx.Collections

open VSharp
open VSharp.Concolic
open VSharp.Core
open CilStateOperations

type public SILI(options : SiliOptions) =

    let bidirectionalEngineStatistics = BidirectionalEngineStatistics()
    let infty = UInt32.MaxValue
    let emptyState = Memory.EmptyState()
    let interpreter = ILInterpreter()
    let mutable entryIP : ip = Exit null
    let mutable reportFinished : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportException : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportIncomplete : cilState -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable reportInternalFail : Exception -> unit = fun _ -> internalfail "reporter not configured!"
    let mutable concolicMachines : Dictionary<cilState, ClientMachine> = Dictionary<cilState, ClientMachine>()
    let mutable startTime = DateTime.Now
    let internalFails = List<Exception>()
    let iies = List<cilState>()

    let isSat pc =
        // TODO: consider trivial cases
        emptyState.pc <- pc
        match SolverInteraction.checkSat emptyState with
        | SolverInteraction.SmtSat _
        | SolverInteraction.SmtUnknown _ -> true
        | _ -> false
    let mkForwardSearcher = function
        | BFSMode -> BFSSearcher(options.bound) :> IForwardSearcher
        | DFSMode -> DFSSearcher(options.bound) :> IForwardSearcher

    let searcher : IBidirectionalSearcher =
        match options.explorationMode with
        | TestCoverageMode(coverageZone, searchMode) ->
            BidirectionalSearcher(mkForwardSearcher searchMode, BackwardSearcher(), TargetedSearcher.DummyTargetedSearcher()) :> IBidirectionalSearcher
        | StackTraceReproductionMode _ -> __notImplemented__()

    let coveragePobsForMethod (method : MethodBase) =
        let cfg = CFG.findCfg method
        cfg.sortedOffsets |> Seq.map (fun offset ->
            {loc = {offset = offset; method = method}; lvl = infty; pc = EmptyPathCondition})
        |> List.ofSeq

    let term2obj = function
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | _ -> __notImplemented__()

    let state2test (m : MethodBase) (cilState : cilState) =
        let test = UnitTest m
        match cilState.state.model with
        | Some model ->
            model.Iter (fun kvp ->
                let value : obj = term2obj kvp.Value
                match kvp.Key with
                | StackReading key ->
                    match key with
                    | ParameterKey pi -> test.AddArg pi value
                    | ThisKey m ->
                        let pi = m.GetParameters().[0]
                        assert(pi.Name = "this")
                        test.AddArg pi value
                    | _ -> __notImplemented__()
                | _ -> __notImplemented__())
            let retVal = model.Eval cilState.Result
            test.Expected <- term2obj retVal
        | None ->
            m.GetParameters() |> Seq.iter (fun pi ->
                let defaultValue = System.Runtime.Serialization.FormatterServices.GetUninitializedObject pi.ParameterType
                test.AddArg pi defaultValue)
            let emptyModel = model.DefaultComplete
            let retVal = emptyModel.Eval cilState.Result
            test.Expected <- term2obj retVal
        test

    let wrapOnTest (action : Action<UnitTest>) method state =
        let test = state2test method state
        action.Invoke test

    let wrapOnError (action : Action<UnitTest>) method state =
        let test = state2test method state
        action.Invoke test

    let wrapOnIIE (action : Action<InsufficientInformationException>) (state : cilState) =
        iies.Add(state)
        action.Invoke state.iie.Value

    let wrapOnInternalFail (action : Action<Exception>) (e : Exception) =
        internalFails.Add(e)
        action.Invoke e

    static member private FormInitialStateWithoutStatics (method : MethodBase) =
        let initialState = Memory.EmptyState()
        let this(*, isMethodOfStruct*) =
            let declaringType = Types.FromDotNetType method.DeclaringType
            Memory.InitializeStaticMembers initialState declaringType
            if method.IsStatic then None // *TODO: use hasThis flag from Reflection
            else
                let this = Memory.MakeSymbolicThis method
                !!(IsNullReference this) |> AddConstraint initialState
                Some this
        ILInterpreter.InitFunctionFrame initialState method this None
        initialState

    member private x.FormInitialStates (method : MethodBase) : cilState list =
        let state = SILI.FormInitialStateWithoutStatics method
        let cilState = makeInitialState method state
        interpreter.InitializeStatics cilState method.DeclaringType List.singleton

    member private x.Forward (s : cilState) =
        // TODO: update pobs when visiting new methods; use coverageZone
        bidirectionalEngineStatistics.TrackStepForward s
        let goodStates, iieStates, errors = interpreter.ExecuteOneInstruction s
        let goodStates, toReport = goodStates |> List.partition (fun s -> isExecutable s || s.startingIP <> entryIP)
        // TODO: need to report? #do
        toReport |> List.iter reportFinished
        let errors, toReport = errors |> List.partition (fun s -> s.startingIP <> entryIP)
        toReport |> List.iter reportException
        let iieStates, toReport = iieStates |> List.partition (fun s -> s.startingIP <> entryIP)
        toReport |> List.iter reportIncomplete
        let newStates =
            match goodStates with
            | s'::goodStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
            | _ ->
                match iieStates with
                | s'::iieStates when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                | _ ->
                    match errors with
                    | s'::errors when LanguagePrimitives.PhysicalEquality s s' -> goodStates @ iieStates @ errors
                    | _ -> goodStates @ iieStates @ errors
        let concolicMachine : ClientMachine ref = ref null
        if concolicMachines.TryGetValue(s, concolicMachine) then
            let machine = concolicMachine.Value
            let cilState' = machine.StepDone (s::newStates)
            if not <| LanguagePrimitives.PhysicalEquality s cilState' then
                concolicMachines.Remove(s) |> ignore
                concolicMachines.Add(cilState', machine)
        searcher.UpdateStates s newStates

    member private x.Backward p' s' EP =
        assert(currentLoc s' = p'.loc)
        let sLvl = levelToUnsignedInt s'.level
        if p'.lvl >= sLvl then
            let lvl = p'.lvl - sLvl
            let pc = Memory.WLP s'.state p'.pc
            match isSat pc with
            | true when s'.startingIP = EP -> searcher.Answer p' (Witnessed s')
            | true ->
                bidirectionalEngineStatistics.TrackStepBackward p' s'
                let p = {loc = startingLoc s'; lvl = lvl; pc = pc}
                Logger.trace "Backward:\nWas: %O\nNow: %O\n\n" p' p
                searcher.UpdatePobs p' p
            | false ->
                Logger.trace "UNSAT for pob = %O and s'.PC = %s" p' (API.Print.PrintPC s'.state.pc)

    member private x.BidirectionalSymbolicExecution (EP : ip) =
        let mutable action = Stop
        let pick() =
            match searcher.Pick() with
            | Stop -> false
            | a -> action <- a; true
        while pick() do
            match action with
            | GoFront s -> x.Forward(s)
            | GoBack(s, p) -> x.Backward p s EP
            | Stop -> __unreachable__()

    member private x.AnswerPobs entryPoint initialStates =
        startTime <- DateTime.Now
        let mainPobs = coveragePobsForMethod entryPoint |> Seq.filter (fun pob -> pob.loc.offset <> 0)
        searcher.Init entryPoint initialStates mainPobs
        entryIP <- Instruction(0x0, entryPoint)
        match options.executionMode with
        | ConcolicMode ->
            __notImplemented'__ "Concolic mode"
            initialStates |> List.iter (fun initialState ->
                let machine = ClientMachine(entryPoint, (fun _ -> ()), initialState)
                if not <| machine.Spawn() then
                    internalfail "Unable to spawn concolic machine!"
                concolicMachines.Add(initialState, machine))
            let machine =
                if concolicMachines.Count = 1 then Seq.head concolicMachines.Values
                else __notImplemented'__ "Forking in concolic mode"
            while machine.ExecCommand() do
                x.BidirectionalSymbolicExecution entryIP
            // TODO: need to report? #do
//            Logger.error "result state = %O" machine.State
//            reportFinished.Invoke machine.State
        | SymbolicMode ->
            x.BidirectionalSymbolicExecution entryIP
            Logger.info "BidirectionalSymbolicExecution Statistics:\n%s" (bidirectionalEngineStatistics.PrintStatistics(searcher.GetType().ToString()))
        searcher.Statuses() |> Seq.iter (fun (pob, status) ->
            match status with
            | pobStatus.Unknown ->
                Logger.warning "Unknown status for pob at %O" pob.loc
            | _ -> ())

    member x.InterpretEntryPoint (method : MethodBase) (onFinished : Action<UnitTest>)
                                 (onException : Action<UnitTest>)  (onIIE : Action<InsufficientInformationException>)
                                 (onInternalFail : Action<Exception>) : unit =
        assert method.IsStatic
        reportFinished <- wrapOnTest onFinished method
        reportException <- wrapOnError onException method
        reportIncomplete <- wrapOnIIE onIIE
        reportInternalFail <- wrapOnInternalFail onInternalFail
        let state = Memory.EmptyState()
        Memory.InitializeStaticMembers state (Types.FromDotNetType method.DeclaringType)
        let initialState = makeInitialState method state
        x.AnswerPobs method [initialState]

    member x.InterpretIsolated (method : MethodBase) (onFinished : Action<UnitTest>)
                               (onException : Action<UnitTest>) (onIIE : Action<InsufficientInformationException>)
                               (onInternalFail : Action<Exception>) : unit =
        Reset()
        reportFinished <- wrapOnTest onFinished method
        reportException <- wrapOnError onException method
        reportIncomplete <- wrapOnIIE onIIE
        reportInternalFail <- wrapOnInternalFail onInternalFail
        let initialStates = x.FormInitialStates method
        x.AnswerPobs method initialStates
        Restore()

    member x.GenerateReport (writer : TextWriter) =
        let time = DateTime.Now - startTime
        writer.WriteLine("Total time: {0:00}:{1:00}:{2:00}.{3}.", time.Hours, time.Minutes, time.Seconds, time.Milliseconds)
        if internalFails.Count > 0 then
            writer.WriteLine()
            writer.WriteLine()
            writer.WriteLine("{0} error(s) occured!")
            internalFails |> Seq.iter writer.WriteLine
        if iies.Count > 0 then
            writer.WriteLine()
            writer.WriteLine()
            writer.WriteLine("{0} branch(es) with insufficient input information!")
            iies |> Seq.iter (fun state -> writer.WriteLine state.iie.Value.Message)

    member x.IncompleteStates with get() = iies
