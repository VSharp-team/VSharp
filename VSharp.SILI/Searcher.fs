namespace VSharp.Interpreter.IL

open System.Collections.Generic
open System.Reflection
open VSharp
open CilStateOperations
open VSharp.Core

type FrontQueue(maxBound) =
    let goodStates = List<cilState>()
    // NOTE: ``justAddedGoodStates'' should be used by TargetedSearcher to update its priorityQueue
    let justAddedGoodStates = List<cilState>()
    let iieStates = List<cilState>()
    let erroredStates = List<cilState>()
    let maxBoundViolatingStates = List<cilState>()
    let nonExecutableStates = List<cilState>()

    let doesViolateMaxBound (s : cilState) =
        match currentIp s with
        | Instruction(offset, m) ->
            let codeLocation = {offset = offset; method = m}
            match PersistentDict.tryFind s.level codeLocation with
            | Some current -> current >= uint32 maxBound
            | None -> false
        | _ -> false
    let addGoodState (s : cilState) =
        if doesViolateMaxBound s then maxBoundViolatingStates.Add(s)
        elif not (isExecutable s) then nonExecutableStates.Add(s)
        else justAddedGoodStates.Add(s)
    member x.Add(s : cilState) =
        assert(List.length s.ipStack = Memory.CallStackSize s.state)
        if isIIEState s then iieStates.Add(s)
        elif isError s then erroredStates.Add(s) // TODO: check it. Maybe ''isUnhandledError'' should be here
        else addGoodState s
//    member x.AddAll(newStates : cilState seq) = Seq.iter x.Add newStates
    member x.AddGoodStates(newStates : cilState seq) = Seq.iter addGoodState newStates
    member x.AddIIEStates(newStates : cilState seq) = Seq.iter iieStates.Add newStates
    member x.AddErroredStates(newStates : cilState seq) = Seq.iter erroredStates.Add newStates

    member x.Remove(s : cilState) = goodStates.Remove(s)

    member x.RemoveAll(pred) : int = goodStates.RemoveAll(pred)

    member x.StatesForPropagation() : cilState seq =
        // TODO: profile it, because clearing might give overhead
        goodStates.AddRange(justAddedGoodStates)
        justAddedGoodStates.Clear()
        seq goodStates

    // TODO: Is this method really needed?
    member x.GetAllStates() =
        goodStates |> Seq.append iieStates |> Seq.append erroredStates
        |> Seq.append maxBoundViolatingStates |> Seq.append nonExecutableStates

    member x.Clear() =
        goodStates.Clear()
        iieStates.Clear()
        erroredStates.Clear()
        maxBoundViolatingStates.Clear()
        nonExecutableStates.Clear()

//type IndexedQueue() =
//    let q = List<cilState>()
////    let isRecursiveEffect (s : cilState) =
////        let isEffect = (Seq.last s.state.frames).isEffect
////        if isEffect then
////            let effectsMethod = (Seq.last s.state.frames).func.Method
////            match currentIp s with
////            | {label = Instruction offset; method = m} when Instruction.isDemandingCallOpCode (Instruction.parseInstruction m offset)->
////                let callSite = Instruction.parseCallSite m offset
////                callSite.calledMethod.Equals(effectsMethod)
////            | _ -> false
////        else false
//    member x.Add (s : cilState) =
//        if List.length s.ipStack <> Memory.CallStackSize s.state then __unreachable__() // TODO: change to assert; this falls in factAgain #do
//        q.Add s
//
//    member x.Remove s =
//        let removed = q.Remove s
//        if not removed then Logger.trace "CilState was not removed from IndexedQueue:\n%O" s
//    member x.GetStates () = List.ofSeq q

type SearchDirection =
    | Stop
    | Start of ip
    | GoForward of cilState
    | GoBackward of pob * cilState

type INewSearcher =
    abstract member ChooseAction : FrontQueue * (pob * cilState) seq * pob seq * MethodBase -> SearchDirection
    abstract member CanReach : ip stack * ip * ip list -> bool
    abstract member Reset : unit -> unit
//    abstract member ClosePob : pob -> unit
    abstract member Init : MethodBase * codeLocation seq -> unit
    abstract member TotalNumber : uint

[<AbstractClass>]
type ForwardSearcher() = // TODO: max bound is needed, when we are in recursion, but when we go to one method many time -- it's okay #do
//    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
    static let mutable totalNumber = 0u
    let mutable stepsNumber = 0u
    interface INewSearcher with
        override x.CanReach(_,_,_) = true
        override x.TotalNumber = totalNumber
        override x.Init (_,_) = ()
//        override x.ClosePob (_) = ()
        override x.Reset () =
            Logger.warning "steps number done by %O = %d" (x.GetType()) stepsNumber
            totalNumber <- totalNumber + stepsNumber
            stepsNumber <- 0u
        override x.ChooseAction(fq, bq, _, main) =
            match fq.StatesForPropagation(), bq with
            | _, Seq.Cons(ps, _) -> GoBackward ps
            | Seq.Empty, Seq.Empty when stepsNumber = 0u -> Start <| Instruction(0, main)
            | states, Seq.Empty ->
                match x.PickNext states with
                | None -> Stop
                | Some s ->
                    stepsNumber <- stepsNumber + 1u
                    GoForward s

    abstract member PickNext : cilState seq -> cilState option
    default x.PickNext (_ : cilState seq) = None

type BFSSearcher() =
    inherit ForwardSearcher() with
        override x.PickNext states =
            match states with
            | Seq.Cons(x, _) -> Some x
            | _ -> None

type DFSSearcher() =
    inherit ForwardSearcher() with
        override x.PickNext states =
            match states with
            | Seq.Cons(_,_) -> Seq.last states |> Some
            | _ -> None


type EffectsFirstSearcher() =
    inherit ForwardSearcher()
    override x.PickNext q =
        let canBePropagated (s : cilState) =
            let conditions = [isIIEState; isUnhandledError; isExecutable >> not]
            conditions |> List.fold (fun acc f -> acc || f s) false |> not

        let states = Seq.filter canBePropagated q
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
    abstract member ShouldStartExploringInIsolation: cilState seq * cilState -> bool
    default x.ShouldStartExploringInIsolation (_,_) = false

type AllMethodsExplorationSearcher() =
    inherit EffectsFirstSearcher()
    override x.ShouldStartExploringInIsolation(states, s) =
        match currentIp s with
        | Instruction(0, _) as ip when states |> Seq.filter (startingIpOf >> (=) ip) |> Seq.length = 0 -> true
        | _ -> false

type ParameterlessMethodsExplorationSearcher() =
    inherit AllMethodsExplorationSearcher()
    override x.ShouldStartExploringInIsolation(states, s) =
        base.ShouldStartExploringInIsolation(states, s) &&
            let m = Memory.GetCurrentExploringFunction s.state
            (m.IsConstructor || m.IsStatic) && m.GetParameters().Length = 0

type ExceptionsExplorationSearcher() =
    inherit ParameterlessMethodsExplorationSearcher()
    override x.ShouldStartExploringInIsolation(states, s) =
        base.ShouldStartExploringInIsolation(states, s) &&
            let m = Memory.GetCurrentExploringFunction s.state
            m.DeclaringType.IsSubclassOf(typeof<System.Exception>)
