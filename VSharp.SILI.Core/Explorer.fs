namespace VSharp.Core

#nowarn "69"

open VSharp
open System.Collections.Generic

type public IInterpreter =
    abstract member Reset : ('a -> 'b) -> ('a -> 'b)
    abstract member InitEntryPoint : state -> string -> (state -> 'a) -> 'a
    abstract member Invoke : IFunctionIdentifier -> state -> term option -> (statementResult * state -> 'a) -> 'a
type IMethodIdentifier =
    inherit IFunctionIdentifier
    abstract IsStatic : bool
    abstract DeclaringTypeAQN : string
    abstract Token : string
type IDelegateIdentifier =
    inherit IFunctionIdentifier
    abstract ContextFrames : frames

module internal Explorer =
    open System

    let private currentlyExploredFunctions = new HashSet<IFunctionIdentifier>()
    let private currentlyCalledFunctions = new HashSet<IFunctionIdentifier>()

    type private NullInterpreter() =
        interface IInterpreter with
            member this.Reset _ = internalfail "interpreter is not ready"
            member this.InitEntryPoint _ _ _ = internalfail "interpreter is not ready"
            member this.Invoke _ _ _ _ = internalfail "interpreter is not ready"
    let mutable private interpreter : IInterpreter = new NullInterpreter() :> IInterpreter
    let configure itprtr = interpreter <- itprtr

    let private formInitialStatics metadata typ typeName =
        let staticMemoryKey = makeStringKey typeName
        let staticMemoryEntry = Struct metadata Heap.empty typ
        Heap.empty.Add(staticMemoryKey, { value = staticMemoryEntry; created = Timestamp.zero; modified = Timestamp.zero; typ = typ })

    let private invoke id state this k =
        interpreter.Invoke id state this k

    let interpretEntryPoint (id : IFunctionIdentifier) k =
        let initialState = State.emptyRestricted
        match id with
        | :? IMethodIdentifier as m ->
            assert(m.IsStatic)
            interpreter.InitEntryPoint initialState m.DeclaringTypeAQN (fun state ->
            interpreter.Invoke id state None k)
        | _ -> internalfail "unexpected entry point: expected regular method, but got %O" id

    let explore (id : IFunctionIdentifier) k =
        let k = interpreter.Reset k
        let metadata = Metadata.empty
        currentlyExploredFunctions.Add id |> ignore
        let this, state, isMethodOfStruct =
            match id with
            | :? IMethodIdentifier as m ->
                let declaringQualifiedName = m.DeclaringTypeAQN
                let declaringType = declaringQualifiedName |> System.Type.GetType |> Types.Constructor.fromDotNetType
                let initialState = { State.empty with statics = State.Defined false (formInitialStatics metadata declaringType declaringQualifiedName) }
                if m.IsStatic then (None, initialState, false)
                else
                    Memory.makeSymbolicThis metadata initialState m.Token declaringType
                    |> (fun (f, s, flag) -> Some f, s, flag)
            | :? IDelegateIdentifier as dlgt ->
                let state = { State.empty with frames = dlgt.ContextFrames }
                // TODO: Create dummy frame
                (None, state, false)
            | _ -> __notImplemented__()
        let state = if Option.isSome this then State.withPathCondition state (!!( Pointers.isNull metadata (Option.get this))) else state
        invoke id state this (fun (res, state) ->
            let state = if Option.isSome this then State.popPathCondition state else state
            let state = if isMethodOfStruct then State.popStack state else state
            currentlyExploredFunctions.Remove id |> ignore
            Database.report id (res, state)
            k (res, state))

    let private detectUnboundRecursion id s =
        let isRecursiveFrame (frame : stackFrame) =
            match frame.func with
            | Some(id', _) when id = id' -> true
            | _ -> false
        let bottomOccurence = Stack.tryFindBottom isRecursiveFrame s.frames.f
        match bottomOccurence with
        | None -> false
        | Some { func = Some(_, p'); entries = _; time =  _ } when s.pc = p' ->
            match Options.RecursionUnrollingMode() with
            | NeverUnroll -> true
            | _ -> false
        | _ ->
            match Options.RecursionUnrollingMode() with
            | AlwaysUnroll -> false
            | _ -> true

    type private recursionOutcomeSource =
        {id : IFunctionIdentifier; state : state; name : string; typ : termType; location : term option}
        interface IStatedSymbolicConstantSource

    let (|RecursionOutcome|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? extractingSymbolicConstantSource as esrc ->
            match esrc.source with
            | :? recursionOutcomeSource as ro -> Some(ro.id, ro.state, ro.location, esrc.extractor :? IdTermExtractor)
            | _ -> None
        | _ -> None

    let private mutateStackClosure mtd (funcId : IFunctionIdentifier) time state =
        match funcId with
        | :? IDelegateIdentifier as di ->
            let mutateLocation st (frame : entry) =
                let location = StackRef mtd frame.key []
                let name = sprintf "μ[%O, %s]" funcId (fst frame.key)
                let typ = frame.typ
                let source = {id = funcId; state = state; name = name; typ = typ; location = Some location} |> extractingSymbolicConstantSource.wrap
                let value = Memory.makeSymbolicInstance mtd time source name typ
                Memory.mutateStack mtd st frame.key [] time value |> snd
            di.ContextFrames.f |> List.fold (fun state frame -> List.fold mutateLocation state frame.entries) state
        | _ -> state

    let reproduceEffect mtd funcId state k =
        let addr = [Memory.freshAddress()]
        let time = Memory.tick()
        if currentlyExploredFunctions.Contains funcId then
            let typ = funcId.ReturnType
            let name = IdGenerator.startingWith <| sprintf "μ[%O]_" funcId
            let source = {id = funcId; state = state; name = name; typ = typ; location = None} |> extractingSymbolicConstantSource.wrap
            let recursiveResult = Memory.makeSymbolicInstance mtd time source name typ |> ControlFlow.throwOrReturn
            let recursiveState = { mutateStackClosure mtd funcId time state with heap = RecursiveApplication(funcId, addr, time); statics = RecursiveApplication(funcId, addr, time) }
            k (recursiveResult, recursiveState)
        else
            let ctx : compositionContext = { mtd = mtd; addr = addr; time = time }
            let getExplored k =
                match Database.query funcId with
                | Some r -> k r
                | None -> explore funcId k
            getExplored (fun (exploredResult, exploredState) ->
            let result = Memory.fillHoles ctx state (ControlFlow.resultToTerm exploredResult) |> ControlFlow.throwOrReturn
            let state = Memory.composeStates ctx state exploredState
            k (result, state))

    let callOrApplyEffect mtd areWeStuck body id state setup teardown k =
        if areWeStuck then
            reproduceEffect mtd id state k
        else
            setup id
            body state (fun (result, state) ->
            teardown id
            k (result, state))

    let call mtd funcId state body k =
        let managedCallOrApply k =
            match Options.RecursionUnrollingMode () with
            | RecursionUnrollingModeType.SmartUnrolling ->
                callOrApplyEffect mtd (detectUnboundRecursion funcId state) body funcId state ignore ignore k
            | RecursionUnrollingModeType.NeverUnroll ->
                let shouldStopUnrolling = currentlyCalledFunctions.Contains funcId || not <| currentlyExploredFunctions.Contains funcId
                let setup id = currentlyCalledFunctions.Add id |> ignore
                let teardown id = currentlyCalledFunctions.Remove id |> ignore
                callOrApplyEffect mtd shouldStopUnrolling body funcId state setup teardown k
            | RecursionUnrollingModeType.AlwaysUnroll -> callOrApplyEffect mtd false body funcId state ignore ignore k
        managedCallOrApply (fun (result, state) -> k (result, State.popStack state))

    let higherOrderApply mtd funcId (state : state) parameters returnType k =
        let addr = [Memory.freshAddress()]
        let time = Memory.tick()
        let expr = Expression mtd (Application funcId) parameters returnType
        let ctx : compositionContext = { mtd = mtd; addr = addr; time = time }
        let hopHeap = HigherOrderApplication(expr, addr, time)
        k (expr |> ControlFlow.throwOrReturn, {state with heap = Memory.composeHeapsOf ctx state hopHeap})

    type recursionOutcomeSource with
        interface IStatedSymbolicConstantSource with
            override x.SubTerms = Seq.empty
            override x.Compose ctx state =
                let state' = Memory.composeStates ctx state x.state
                let source' = {x with state = state'}
                Constant ctx.mtd x.name source' x.typ
