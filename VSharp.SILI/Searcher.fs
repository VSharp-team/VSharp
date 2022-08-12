namespace VSharp.Interpreter.IL

open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Utils
open CilStateOperations

type action =
    | GoFront of cilState
    | GoBack of cilState * pob
    | Stop

type IBidirectionalSearcher =
    abstract member Init : Method -> cilState list -> pob seq -> unit
    abstract member UpdateStates : cilState -> cilState seq -> unit
    abstract member UpdatePobs : pob -> pob -> unit
    abstract member Pick : unit -> action
    abstract member Answer : pob -> pobStatus -> unit
    abstract member Statuses : unit -> seq<pob * pobStatus>
    abstract member States : unit -> cilState seq

type IForwardSearcher =
    abstract member Init : cilState seq -> unit
    abstract member Update : cilState * cilState seq -> unit
    abstract member Pick : unit -> cilState option
    abstract member States : unit -> cilState seq

type ITargetedSearcher =
    abstract member SetTargets : ip -> ip seq -> unit
    abstract member Update : cilState -> cilState seq -> cilState seq // returns states that reached its target
    abstract member Pick : unit -> cilState option

type backwardAction = Propagate of cilState * pob | InitTarget of ip * pob seq | NoAction

type IBackwardSearcher =
    abstract member Init : Method -> pob seq -> unit
    abstract member Update : pob -> pob -> unit
    abstract member Answer : pob -> pobStatus -> unit
    abstract member Statuses : unit -> seq<pob * pobStatus>
    abstract member Pick : unit -> backwardAction
    abstract member AddBranch : cilState -> pob list

    // TODO: get rid of this!
    abstract member RemoveBranch : cilState -> unit

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
    let isStopped s = isStopped s || violatesLevel s maxBound
    let add (states : List<cilState>) (s : cilState) =
        if not <| isStopped s then
            assert(states.Contains s |> not)
            states.Add(s)

    interface IForwardSearcher with
        override x.Init states =
            forPropagation.AddRange(states)
        override x.Pick() =
            x.Choose (forPropagation |> Seq.filter (fun cilState -> not cilState.suspended))
        override x.Update (parent, newStates) =
            x.Insert forPropagation (parent, newStates)
        override x.States() = forPropagation

    abstract member Choose : seq<cilState> -> cilState option
    default x.Choose states = Seq.tryLast states
    abstract member Insert : List<cilState> -> cilState * seq<cilState> -> unit
    default x.Insert states (parent, newStates) =
        if isStopped parent then
            states.Remove(parent) |> ignore
        Seq.iter (add states) newStates

type BFSSearcher(maxBound) =
    inherit SimpleForwardSearcher(maxBound) with
        let isStopped s = isStopped s || violatesLevel s maxBound
        let add (states : List<cilState>) (s : cilState) =
            if not <| isStopped s then
                assert(states.Contains s |> not)
                states.Add(s)
        override x.Choose states = Seq.tryHead states
        override x.Insert states (parent, newStates) =
            if isStopped parent then
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
    let modMax n =
        if storage.MaxPriority = 0u then 0u
        else n % storage.MaxPriority
    let isStopped s = isStopped s || violatesLevel s maxBound
    let optionWeight s =
        option {
            if not <| s.suspended && not <| isStopped s then
                return! weighter.Weight s
        }
    let add (s : cilState) =
        let weight = optionWeight s
        match weight with
        | Some w ->
            assert(not <| storage.Contains s)
            storage.Insert s w
        | None -> ()
    let update s =
        let weight = optionWeight s
        match weight with
        | Some w ->
            if not <| storage.Contains s then
                storage.Insert s w
            else storage.Update s w
        | None ->
            if storage.Contains s then
                storage.Remove s

    abstract member Insert : cilState seq -> unit
    default x.Insert states =
        Seq.iter add states

    member x.Pick() =
        storage.Choose (modMax <| weighter.Next())

    abstract member Update : cilState * cilState seq -> unit
    default x.Update (parent, newStates) =
        update parent
        x.Insert newStates

    interface IForwardSearcher with
        override x.Init states = x.Insert states
        override x.Pick() = x.Pick()
        override x.Update (parent, newStates) = x.Update (parent, newStates)
        override x.States() = storage.ToSeq

    member x.Weighter = weighter
    member x.TryGetWeight state = storage.TryGetPriority state
    member x.ToSeq() = storage.ToSeq

type SampledWeightedSearcher(maxBound, weighter : IWeighter) =
    inherit WeightedSearcher(maxBound, weighter, DiscretePDF(CilStateOperations.mkCilStateHashComparer))
