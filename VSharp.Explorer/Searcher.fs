namespace VSharp.Explorer

open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Interpreter.IL
open VSharp.Utils

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
    abstract member StatesCount : int

type IForwardSearcher =
    abstract member Init : cilState seq -> unit
    abstract member Update : cilState * cilState seq -> unit
    abstract member Pick : unit -> cilState option
    abstract member Pick : (cilState -> bool) -> cilState option
    abstract member States : unit -> cilState seq
    abstract member Reset : unit -> unit
    abstract member Remove : cilState -> unit
    abstract member StatesCount : int

type ITargetedSearcher =
    abstract member SetTargets : ip -> ip seq -> unit
    abstract member Update : cilState -> cilState seq -> cilState seq // returns states that reached its target
    abstract member Pick : unit -> cilState option
    abstract member Reset : unit -> unit
    abstract member Remove : cilState -> unit
    abstract member StatesCount : int

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
    abstract member StatesCount : int

type IpStackComparer() =
    interface IComparer<cilState> with
        override _.Compare(x : cilState, y : cilState) =
            let res = (List.length x.ipStack).CompareTo(List.length y.ipStack)
            res

type DFSSearcher() =
    let states = List<cilState>()

    let update parent newStates =
        let last = states.Count - 1
        if last >= 0 && states[last] = parent then
            if Seq.isEmpty newStates |> not then
                assert(states[last] = parent)
                states.RemoveAt(last)
                for newState in newStates do
                    assert(states.Contains newState |> not)
                    states.Add newState
                states.Add(parent)
        else
            let wasParentRemoved = states.Remove(parent)
            for newState in newStates do
                assert(states.Contains newState |> not)
                states.Add newState
            if wasParentRemoved then
                states.Add(parent)

    interface IForwardSearcher with
        override x.Init initialStates = states.AddRange initialStates
        override x.Pick selector = Seq.tryFindBack selector states
        override x.Pick() =
            let last = states.Count - 1
            if last >= 0 then Some states[last]
            else None
        override x.Update(parent, newStates) = update parent newStates
        override x.States() = states
        override x.Reset() = states.Clear()
        override x.Remove cilState = states.Remove cilState |> ignore
        override x.StatesCount with get() = states.Count

type BFSSearcher() =
    let states = List<cilState>()

    let update parent newStates =
        if states.Remove(parent) then
            states.Add(parent)
        for newState in newStates do
            assert(states.Contains newState |> not)
            states.Add newState

    interface IForwardSearcher with
        override x.Init initialStates = states.AddRange initialStates
        override x.Pick selector = Seq.tryFind selector states
        override x.Pick() = Seq.tryHead states
        override x.Update(parent, newStates) = update parent newStates
        override x.States() = states
        override x.Reset() = states.Clear()
        override x.Remove cilState = states.Remove cilState |> ignore
        override x.StatesCount with get() = states.Count

type IWeighter =
    abstract member Weight : cilState -> uint option

type WeightedSearcher(weighter : IWeighter, storage : IPriorityCollection<cilState>) =
    let optionWeight s =
        try
            weighter.Weight s
        with
        | :? InsufficientInformationException as e ->
            s.iie <- Some e
            None
        | :? InternalException as e ->
            Logger.error $"WeightedSearcher: failed to get weight of state {e}"
            None
    let add (s : cilState) =
        let weight = optionWeight s
        match weight with
        | Some w ->
            assert(not <| storage.Contains s)
            storage.Insert s w
            true
        | None -> false
    let update s =
        if not <| storage.Contains s then
            false
        else
            let weight = optionWeight s
            match weight with
            | Some w ->
                storage.Update s w
                true
            | None ->
                storage.Remove s
                false

    abstract member Insert : cilState -> bool
    default x.Insert states = add states

    member x.Pick() = storage.Choose()

    member x.Pick selector = storage.Choose(selector)

    abstract member Update : cilState * cilState seq -> bool
    default x.Update (parent, newStates) =
        let wasParentUpdated = update parent
        Seq.fold (fun r s -> x.Insert s || r) wasParentUpdated newStates

    interface IForwardSearcher with
        override x.Init states = Seq.iter (x.Insert >> ignore) states
        override x.Pick() = x.Pick()
        override x.Pick selector = x.Pick selector
        override x.Update (parent, newStates) = x.Update (parent, newStates) |> ignore
        override x.States() = storage.ToSeq
        override x.Reset() = storage.Clear()
        override x.Remove cilState = if storage.Contains cilState then storage.Remove cilState
        override x.StatesCount with get() = int x.Count

    member x.Weighter = weighter
    member x.Count = storage.Count
    member x.TryGetWeight state = storage.TryGetPriority state
    member x.ToSeq() = storage.ToSeq
