namespace VSharp.Interpreter.IL

open System.Collections.Generic
open VSharp
open CilStateOperations
open VSharp.Core

type IndexedQueue() =
    let q = List<cilState>()
//    let isRecursiveEffect (s : cilState) =
//        let isEffect = (Seq.last s.state.frames).isEffect
//        if isEffect then
//            let effectsMethod = (Seq.last s.state.frames).func.Method
//            match currentIp s with
//            | {label = Instruction offset; method = m} when Instruction.isDemandingCallOpCode (Instruction.parseInstruction m offset)->
//                let callSite = Instruction.parseCallSite m offset
//                callSite.calledMethod.Equals(effectsMethod)
//            | _ -> false
//        else false
    member x.Add (s : cilState) =
        assert(List.length s.ipStack = Memory.CallStackSize s.state)
        q.Add s

    member x.Remove s =
        let removed = q.Remove s
        if not removed then Logger.trace "CilState was not removed from IndexedQueue:\n%O" s
    member x.GetStates () = List.ofSeq q

[<AbstractClass>]
type ISearcher() =
    let maxBound = 100u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
    abstract member PickNext : IndexedQueue -> cilState option

    member x.Used (cilState : cilState) =
        match currentIp cilState with
        | Instruction(offset, m) ->
            let codeLocation = {offset = offset; method = m}
            match PersistentDict.tryFind cilState.level codeLocation with
            | Some current -> current >= maxBound
            | None -> false
        | _ -> false

    member x.GetResults initialState (q : IndexedQueue) =
        let (|CilStateWithIIE|_|) (cilState : cilState) = cilState.iie
        let isStartingDescender (s : cilState) = s.startingIP = initialState.startingIP
        let allStates = List.filter isStartingDescender (q.GetStates())
        let iieStates, nonIIEstates = List.partition isIIEState allStates
        let isFinished (s : cilState) = s.ipStack = [Exit <| methodOf initialState.startingIP]
        let finishedStates = List.filter isFinished nonIIEstates
        let isValid (cilState : cilState) =
           match IsValid cilState.state with
           | SolverInteraction.SmtUnsat _ -> false
           | _ -> true
        let validStates = List.filter isValid finishedStates
        match iieStates with
        | CilStateWithIIE iie :: _ -> raise iie
        | _ :: _ -> __unreachable__()
        | _ when validStates = [] ->
            internalfailf "No states were obtained. Most likely such a situation is a bug. Check it!"
        | _ -> validStates

    member x.CanBePropagated (s : cilState) =
        not (isIIEState s || isUnhandledError s || x.Used s) && isExecutable s

type DummySearcher() =
    inherit ISearcher() with
        override x.PickNext q =
            let states = q.GetStates() |> Seq.filter x.CanBePropagated
            match states with
            | Seq.Cons(x, _) -> Some x
            | Seq.Empty -> None

// Most probably won't be used in real testing
// Aimed to test composition and Interpreter--Searcher feature
type CFASearcher() =
    inherit ISearcher() with
        override x.PickNext q =
            // 1) should append states to Queue
            // 2) should stop executing states and produce states with proper
            //    a) current time
            //    b) evaluationStack (including FRCS)

            let states = q.GetStates() |> Seq.filter x.CanBePropagated
            match states with
            | Seq.Cons(x, _) -> Some x
            | Seq.Empty -> None

type EffectsFirstSearcher() =
    inherit ISearcher()
    override x.PickNext q =
        let states = q.GetStates() |> Seq.filter x.CanBePropagated
        match states with
        | Seq.Empty -> None
        | Seq.Cons(s, _) when x.ShouldStartExploringInIsolation (q, s) ->
            try
                let m = currentMethod s
                let stateForComposition = ExplorerBase.FormInitialStateWithoutStatics m
                let cilStateForComposition = makeInitialState m stateForComposition
                Some cilStateForComposition
            with
            | :? InsufficientInformationException -> Some s
        | Seq.Cons(s, _) -> Some s
    abstract member ShouldStartExploringInIsolation: IndexedQueue * cilState -> bool
    default x.ShouldStartExploringInIsolation (_,_) = false

type AllMethodsExplorationSearcher() =
    inherit EffectsFirstSearcher()
    override x.ShouldStartExploringInIsolation(q, s) =
        let states = q.GetStates()
        match currentIp s with
        | Instruction(0, _) as ip when states |> Seq.filter (startingIpOf >> (=) ip) |> Seq.length = 0 -> true
        | _ -> false

type ParameterlessMethodsExplorationSearcher() =
    inherit AllMethodsExplorationSearcher()
    override x.ShouldStartExploringInIsolation(q, s) =
        base.ShouldStartExploringInIsolation(q, s) &&
            let m = Memory.GetCurrentExploringFunction s.state
            (m.IsConstructor || m.IsStatic) && m.GetParameters().Length = 0

type ExceptionsExplorationSearcher() =
    inherit ParameterlessMethodsExplorationSearcher()
    override x.ShouldStartExploringInIsolation(q, s) =
        base.ShouldStartExploringInIsolation(q, s) &&
            let m = Memory.GetCurrentExploringFunction s.state
            m.DeclaringType.IsSubclassOf(typeof<System.Exception>)
