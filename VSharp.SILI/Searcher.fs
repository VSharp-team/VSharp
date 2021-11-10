namespace VSharp.Interpreter.IL

open System.Collections.Generic
open FSharpx.Collections
open VSharp
open VSharp.Utils
open VSharp.Core
open CilStateOperations

type action =
    | GoFront of cilState
    | GoBack of cilState * pob
    | Stop

type IBidirectionalSearcher =
    abstract member Init : System.Reflection.MethodBase -> cilState list -> pob seq -> unit
    abstract member UpdateStates : cilState -> cilState seq -> unit
    abstract member UpdatePobs : pob -> pob -> unit
    abstract member Pick : unit -> action
    abstract member Answer : pob -> pobStatus -> unit
    abstract member Statuses : unit -> seq<pob * pobStatus>

type IForwardSearcher =
    abstract member Init : cilState seq -> unit
    abstract member Update : cilState * cilState seq -> unit
    abstract member Pick : unit -> cilState option

type ITargetedSearcher =
    abstract member SetTargets : ip -> ip seq -> unit
    abstract member Update : cilState -> cilState seq -> cilState seq // returns states that reached its target
    abstract member Pick : unit -> cilState option

type backwardAction = Propagate of cilState * pob | InitTarget of ip * pob seq | NoAction

type IBackwardSearcher =
    abstract member Init : System.Reflection.MethodBase -> pob seq -> unit
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

type cilStateComparer(comparer) =
    interface IComparer<cilState> with
        override _.Compare(x : cilState, y : cilState) =
            comparer x y


[<AbstractClass>]
type SimpleForwardSearcher(maxBound) =
//    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
//    let mutable mainMethod = null
//    let mutable startedFromMain = false
    let forPropagation = List<cilState>()
    let isStopped s = isStopped s || violatesLevel s maxBound
    let add (s : cilState) =
        if not <| isStopped s then
            assert(forPropagation.Contains s |> not)
            forPropagation.Add(s)

    interface IForwardSearcher with
        override x.Init states =
            forPropagation.AddRange(states)
        override x.Pick() =
            x.Choose (forPropagation |> Seq.filter (fun cilState -> not cilState.suspended))
        override x.Update (parent, newStates) =
            if isStopped parent then
                forPropagation.Remove(parent) |> ignore
            Seq.iter add newStates
    abstract member Choose : seq<cilState> -> cilState option
    default x.Choose states = Seq.tryLast states

type BFSSearcher(maxBound) =
    inherit SimpleForwardSearcher(maxBound) with
        override x.Choose states = Seq.tryHead states

type DFSSearcher(maxBound) =
    inherit SimpleForwardSearcher(maxBound)

type IWeighter =
    abstract member Weight : cilState -> uint option
    abstract member Next : unit -> uint

type WeightedSearcher(maxBound, weighter : IWeighter) =
    let dpdf = DiscretePDF(cilStateComparer(fun a b -> a.GetHashCode().CompareTo(b.GetHashCode())))
    let suspended = HashSet<cilState>()
    let modMax n =
        if DiscretePDF.maxWeight dpdf = 0u then 0u
        else n % DiscretePDF.maxWeight dpdf
    let isStopped s = isStopped s || violatesLevel s maxBound
    let optionWeight s =
        option {
            if s.suspended then
                suspended.Add s |> ignore
            elif not <| isStopped s then
                return! weighter.Weight s
        }
    let add (s : cilState) =
        let weight = optionWeight s
        match weight with
        | Some w ->
            assert(not <| DiscretePDF.contains dpdf s)
            DiscretePDF.insert dpdf s w
        | None -> ()
    let update s =
        let weight = optionWeight s
        match weight with
        | Some w ->
            if suspended.Contains s || not <| DiscretePDF.contains dpdf s then
                if suspended.Contains s then suspended.Remove s |> ignore
                assert(not <| DiscretePDF.contains dpdf s)
                DiscretePDF.insert dpdf s w
            else DiscretePDF.update dpdf s w
        | None ->
            if not <| suspended.Contains s then
                assert(DiscretePDF.contains dpdf s)
                DiscretePDF.remove dpdf s

    abstract member Insert : cilState seq -> unit
    default x.Insert states =
        Seq.iter add states

    member x.Pick() =
        DiscretePDF.choose dpdf (modMax <| weighter.Next())

    abstract member Update : cilState * cilState seq -> unit
    default x.Update (parent, newStates) =
        update parent
        x.Insert newStates

    interface IForwardSearcher with
        override x.Init states = x.Insert states
        override x.Pick() = x.Pick()
        override x.Update (parent, newStates) = x.Update (parent, newStates)

    member x.Weighter = weighter
    member x.GetWeight state = DiscretePDF.tryGetWeight dpdf state
