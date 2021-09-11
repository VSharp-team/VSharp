namespace VSharp.Interpreter.IL

open System
open System.Reflection
open FSharpx.Collections

open VSharp
open VSharp.Core
open CilStateOperations

type public SILI(options : siliOptions) =

    let bidirectionalEngineStatistics = BidirectionalEngineStatistics()
    let infty = UInt32.MaxValue
    let emptyState = Memory.EmptyState()
    let interpreter = ILInterpreter()
    let mutable entryIP : ip = Exit null
    let mutable reportFinished : Action<cilState> = Action<_>(fun _ -> internalfail "reporter not configured!")
    let mutable reportError : Action<cilState> = Action<_>(fun _ -> internalfail "reporter not configured!")
    let mutable reportIncomplete : Action<cilState> = Action<_>(fun _ -> internalfail "reporter not configured!")
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

    member x.InterpretEntryPoint (method : MethodBase) (onFinished : Action<cilState>) (onError : Action<cilState>)  (onIIE : Action<cilState>) : unit =
        assert method.IsStatic
        reportFinished <- onFinished
        reportError <- onError
        reportIncomplete <- onIIE
        let state = Memory.EmptyState()
        Memory.InitializeStaticMembers state (Types.FromDotNetType method.DeclaringType)
        let initialState = makeInitialState method state
        x.AnswerPobs method [initialState]

    member x.InterpretIsolated (method : MethodBase) (onFinished : Action<cilState>) (onError : Action<cilState>) (onIIE : Action<cilState>) : unit =
        Reset()
        reportFinished <- onFinished
        reportError <- onError
        reportIncomplete <- onIIE
        let initialStates = x.FormInitialStates method
        x.AnswerPobs method initialStates
        Restore()

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
        toReport |> List.iter reportFinished.Invoke
        let errors, toReport = errors |> List.partition (fun s -> s.startingIP <> entryIP)
        toReport |> List.iter reportError.Invoke
        let iieStates, toReport = iieStates |> List.partition (fun s -> s.startingIP <> entryIP)
        toReport |> List.iter reportIncomplete.Invoke
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

        Logger.warning "BidirectionalSymbolicExecution Statistics:\n%s" (bidirectionalEngineStatistics.PrintStatistics(searcher.GetType().ToString()))

    member private x.AnswerPobs entryPoint initialStates =
        let mainPobs = coveragePobsForMethod entryPoint |> Seq.filter (fun pob -> pob.loc.offset <> 0)
        searcher.Init entryPoint initialStates mainPobs
        entryIP <- Instruction(0x0, entryPoint)
        x.BidirectionalSymbolicExecution entryIP
        searcher.Statuses() |> Seq.iter (fun (pob, status) ->
            match status with
            | pobStatus.Unknown ->
                Logger.warning "Unknown status for pob at %O" pob.loc
            | _ -> ())
