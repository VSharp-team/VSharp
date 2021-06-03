namespace VSharp.Interpreter.IL

open System.Collections.Generic
open System.Reflection
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

type SearchDirection =
    | Stop
    | Start of ip
    | GoForward of cilState
    | GoBackward of pob * cilState

type INewSearcher =
    abstract member ChooseAction : list<cilState> * list<pob * cilState> * pob list * MethodBase -> SearchDirection
    abstract member CanReach : ip stack * ip * ip list -> bool
    abstract member Reset : unit -> unit
//    abstract member ClosePob : pob -> unit
    abstract member Init : MethodBase * codeLocation seq -> unit
    abstract member AppendNewStates : list<cilState> -> unit
    abstract member MaxBound : int
    abstract member TotalNumber : uint

[<AbstractClass>]
type ForwardSearcher(maxBound) = // TODO: max bound is needed, when we are in recursion, but when we go to one method many time -- it's okay #do
//    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
    static let mutable totalNumber = 0u
    let mutable stepsNumber = 0u
    interface INewSearcher with
        override x.CanReach(_,_,_) = true
        override x.MaxBound = maxBound
        override x.TotalNumber = totalNumber
        override x.Init (_,_) = ()
//        override x.ClosePob (_) = ()
        override x.Reset () =
            Logger.warning "steps number done by %O = %d" (x.GetType()) stepsNumber
            totalNumber <- totalNumber + stepsNumber
            stepsNumber <- 0u
        override x.AppendNewStates _ = ()
        override x.ChooseAction(fq, bq, pobs, main) =
            match fq, bq with
            | _, ps :: _ -> GoBackward ps
            | [], [] when stepsNumber = 0u -> Start <| Instruction(0, main)
            | _, [] ->
                match x.PickNext fq with
                | None -> Stop
                | Some s ->
                    stepsNumber <- stepsNumber + 1u
                    GoForward s

    abstract member PickNext : cilState list -> cilState option
    default x.PickNext (_ : cilState list) = None
    member x.Used (cilState : cilState) =
        let maxBound : int = (x :> INewSearcher).MaxBound
        match currentIp cilState with
        | Instruction(offset, m) ->
            let codeLocation = {offset = offset; method = m}
            match PersistentDict.tryFind cilState.level codeLocation with
            | Some current -> current >= uint maxBound
            | None -> false
        | _ -> false


    member x.CanBePropagated (s : cilState) =
        not (isIIEState s || isUnhandledError s || x.Used s) && isExecutable s

type BFSSearcher(maxBound) =
    inherit ForwardSearcher(maxBound) with
        override x.PickNext fq =
            let canBePropagated (s : cilState) =
                not (isIIEState s || isUnhandledError s) && isExecutable s && not (x.Used s)
            let states = fq |> List.filter canBePropagated
            match states with
            | x :: _ -> Some x
            | [] -> None

type DFSSearcher(maxBound) =
    inherit ForwardSearcher(maxBound) with
        override x.PickNext fq =
            let canBePropagated (s : cilState) =
                not (isIIEState s || isUnhandledError s) && isExecutable s && not (x.Used s)
            let states = fq |> List.filter canBePropagated
            match states with
            | _ :: _ -> List.last states |> Some
            | [] -> None


type EffectsFirstSearcher(maxBound) =
    inherit ForwardSearcher(maxBound)
    override x.PickNext q =
        let canBePropagated (s : cilState) =
            let conditions = [isIIEState; isUnhandledError; x.Used; isExecutable >> not]
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
    abstract member ShouldStartExploringInIsolation: cilState list * cilState -> bool
    default x.ShouldStartExploringInIsolation (_,_) = false

type AllMethodsExplorationSearcher(maxBound) =
    inherit EffectsFirstSearcher(maxBound)
    override x.ShouldStartExploringInIsolation(states, s) =
        match currentIp s with
        | Instruction(0, _) as ip when states |> Seq.filter (startingIpOf >> (=) ip) |> Seq.length = 0 -> true
        | _ -> false

type ParameterlessMethodsExplorationSearcher(maxBound) =
    inherit AllMethodsExplorationSearcher(maxBound)
    override x.ShouldStartExploringInIsolation(states, s) =
        base.ShouldStartExploringInIsolation(states, s) &&
            let m = Memory.GetCurrentExploringFunction s.state
            (m.IsConstructor || m.IsStatic) && m.GetParameters().Length = 0

type ExceptionsExplorationSearcher(maxBound) =
    inherit ParameterlessMethodsExplorationSearcher(maxBound)
    override x.ShouldStartExploringInIsolation(states, s) =
        base.ShouldStartExploringInIsolation(states, s) &&
            let m = Memory.GetCurrentExploringFunction s.state
            m.DeclaringType.IsSubclassOf(typeof<System.Exception>)
