namespace VSharp.Interpreter.IL

open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Interpreter.IL
open VSharp.Utils
open CilStateOperations

type action =
    | GoFront of cilState
    | GoBack of cilState * pob
    | Stop

type IBidirectionalSearcher =
    abstract member Init : cilState list -> pob seq -> unit
    abstract member UpdateStates : cilState -> cilState seq -> unit
    abstract member UpdatePobs : pob -> pob -> unit
    abstract member Pick : unit -> action
    abstract member Answer : pob -> pobStatus -> unit
    abstract member Statuses : unit -> seq<pob * pobStatus>
    abstract member States : unit -> cilState seq
    abstract member Reset : unit -> unit
    abstract member Remove : cilState -> unit

type IForwardSearcher =
    abstract member Init : cilState seq -> unit
    abstract member Update : cilState * cilState seq -> unit
    abstract member Pick : unit -> cilState option
    abstract member Pick : (cilState -> bool) -> cilState option
    abstract member States : unit -> cilState seq
    abstract member Reset : unit -> unit
    abstract member Remove : cilState -> unit

type ITargetedSearcher =
    abstract member SetTargets : ip -> ip seq -> unit
    abstract member Update : cilState -> cilState seq -> cilState seq // returns states that reached its target
    abstract member Pick : unit -> cilState option
    abstract member Reset : unit -> unit
    abstract member Remove : cilState -> unit

type backwardAction = Propagate of cilState * pob | InitTarget of ip * pob seq | NoAction

type IBackwardSearcher =
    abstract member Init : pob seq -> unit
    abstract member Update : pob -> pob -> unit
    abstract member Answer : pob -> pobStatus -> unit
    abstract member Statuses : unit -> seq<pob * pobStatus>
    abstract member Pick : unit -> backwardAction
    abstract member AddBranch : cilState -> pob list
    abstract member Reset : unit -> unit
    abstract member Remove : cilState -> unit

type IpStackComparer() =
    interface IComparer<cilState> with
        override _.Compare(x : cilState, y : cilState) =
            let res = (List.length x.ipStack).CompareTo(List.length y.ipStack)
            res

[<AbstractClass>]
type SimpleForwardSearcher(maxBound) =
//    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
//    let mutable mainMethod = null
//    let mutable startedFromMain = false
    let forPropagation = List<cilState>()

    let add (states : List<cilState>) (s : cilState) =
        if not <| isStopped s then
            assert(states.Contains s |> not)
            states.Add(s)

    interface IForwardSearcher with
        override x.Init states = x.Init forPropagation states
        override x.Pick selector = x.Choose forPropagation selector
        override x.Pick() = x.Choose forPropagation (Prelude.always true)
        override x.Update (parent, newStates) =
            x.Insert forPropagation (parent, newStates)
        override x.States() = forPropagation
        override x.Reset() = forPropagation.Clear()
        override x.Remove cilState = forPropagation.Remove cilState |> ignore

    abstract member Choose : seq<cilState> -> (cilState -> bool) -> cilState option
    default x.Choose states selector = Seq.tryFindBack selector states

    abstract member Insert : List<cilState> -> cilState * seq<cilState> -> unit
    default x.Insert states (parent, newStates) =
        if violatesLevel parent maxBound then
            states.Remove(parent) |> ignore
        Seq.iter (add states) newStates

    abstract member Init : List<cilState> -> seq<cilState> -> unit
    default x.Init states initStates = states.AddRange(initStates)

type BFSSearcher(maxBound) =
    inherit SimpleForwardSearcher(maxBound) with
        let add (states : List<cilState>) (s : cilState) =
            if not <| isStopped s then
                assert(states.Contains s |> not)
                states.Add(s)
        override x.Choose states selector = Seq.tryFind selector states
        override x.Insert states (parent, newStates) =
            if violatesLevel parent maxBound then
                states.Remove(parent) |> ignore
            else if not <| Seq.isEmpty newStates then
                states.Remove(parent) |> ignore
                states.Add(parent)
            Seq.iter (add states) newStates

type DFSSearcher(maxBound) =
    inherit SimpleForwardSearcher(maxBound)

type IWeighter =
    abstract member Weight : cilState -> uint option
    abstract member Next : unit -> uint

type WeightedSearcher(maxBound, weighter : IWeighter, storage : IPriorityCollection<cilState>) =
    let optionWeight s =
        try
            option {
                if not <| violatesLevel s maxBound then return! weighter.Weight s
            }
        with :? InsufficientInformationException as e ->
            s.iie <- Some e
            None
    let add (s : cilState) =
        let weight = optionWeight s
        match weight with
        | Some w ->
            assert(not <| storage.Contains s)
            storage.Insert s w
        | None -> ()
    let update s =
        if storage.Contains s then
            let weight = optionWeight s
            match weight with
            | Some w -> storage.Update s w
            | None -> storage.Remove s

    abstract member Insert : cilState seq -> unit
    default x.Insert states =
        Seq.iter add states

    member x.Pick() = storage.Choose()

    member x.Pick selector = storage.Choose(selector)

    abstract member Update : cilState * cilState seq -> unit
    default x.Update (parent, newStates) =
        update parent
        x.Insert newStates

    interface IForwardSearcher with
        override x.Init states = x.Insert states
        override x.Pick() = x.Pick()
        override x.Pick selector = x.Pick selector
        override x.Update (parent, newStates) = x.Update (parent, newStates)
        override x.States() = storage.ToSeq
        override x.Reset() = storage.Clear()
        override x.Remove cilState = if storage.Contains cilState then storage.Remove cilState

    member x.Weighter = weighter
    member x.Count = storage.Count
    member x.TryGetWeight state = storage.TryGetPriority state
    member x.ToSeq() = storage.ToSeq
