namespace VSharp.Core

open VSharp
open System.Collections.Generic

type public IInterpreter =
    abstract member Reset : unit -> unit
    abstract member InitEntryPoint : state -> string -> (state -> 'a) -> 'a
    abstract member Invoke : IFunctionIdentifier -> state -> term option -> (statementResult * state -> 'a) -> 'a

module internal Explorer =

    let private currentlyExploredFunctions = new HashSet<IFunctionIdentifier>()

    type private NullInterpreter() =
        interface IInterpreter with
            member this.Reset() = internalfail "interpreter is not ready"
            member this.InitEntryPoint _ _ _ = internalfail "interpreter is not ready"
            member this.Invoke _ _ _ _ = internalfail "interpreter is not ready"
    let mutable private interpreter : IInterpreter = new NullInterpreter() :> IInterpreter
    let configure itprtr = interpreter <- itprtr

    let private formInitialStatics metadata typ typeName =
        let staticMemoryKey = makeStringKey typeName
        let staticMemoryEntry = Struct metadata Heap.empty typ
        Heap.empty.Add(staticMemoryKey, { value = staticMemoryEntry; created = Timestamp.zero; modified = Timestamp.zero })

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
        interpreter.Reset()
        let metadata = Metadata.empty
        currentlyExploredFunctions.Add id |> ignore
        let this, state =
            match id with
            | :? IMethodIdentifier as m ->
                let declaringQualifiedName = m.DeclaringTypeAQN
                let declaringType = declaringQualifiedName |> System.Type.GetType |> Types.Constructor.fromDotNetType
                let initialState = { State.empty with statics = State.Defined false (formInitialStatics metadata declaringType declaringQualifiedName) }
                if m.IsStatic then (None, initialState)
                else
                    let instance, state = Memory.allocateSymbolicInstance metadata initialState declaringType
                    if Terms.isHeapRef instance then (Some instance, state)
                    else
                        let key = ("external data", m.Token)
                        let state = Memory.newStackFrame state metadata (EmptyIdentifier()) [(key, Specified instance, Some declaringType)]
                        (Some <| Memory.referenceLocalVariable metadata state key true, state)
            | _ -> __notImplemented__()
        invoke id state this (fun r ->
            currentlyExploredFunctions.Remove id |> ignore
            Database.report id r
            k r)

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
            | AlwaysDisableUnrolling -> true
            | _ -> false
        | _ ->
            match Options.RecursionUnrollingMode() with
            | AlwaysEnableUnrolling -> false
            | _ -> true

    let reproduceEffect mtd funcId state k =
        let addr = [Memory.freshAddress()]
        let time = Memory.tick()
        if currentlyExploredFunctions.Contains funcId then
            // TODO: this is just a temporary hack!!
            let recursiveResult = NoResult mtd
            let recursiveState = { state with heap = RecursiveApplication(funcId, addr, time); statics = RecursiveApplication(funcId, addr, time) }
            k (recursiveResult, recursiveState)
        else
            let ctx : compositionContext = { mtd = mtd; addr = addr; time = time }
            let exploredResult, exploredState = Database.query funcId ||?? lazy(explore funcId id)
            let result = Memory.fillHoles ctx state (ControlFlow.resultToTerm exploredResult) |> ControlFlow.throwOrReturn
            let state = Memory.composeStates ctx state exploredState
            k (result, state)

    let callOrApplyEffect mtd areWeStuck body id state k =
        if areWeStuck then
            reproduceEffect mtd id state k
        else
            body state k

    let call mtd funcId state body k =
        let shouldStopUnrolling = detectUnboundRecursion funcId state
        callOrApplyEffect mtd shouldStopUnrolling body funcId state (fun (result, state) ->
        k (result, State.popStack state))
