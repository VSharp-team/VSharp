namespace VSharp.Interpreter.IL

open System.Collections.Generic
open FSharpx.Collections
open VSharp
open CilStateOperations
open VSharp.Core
open VSharp.Interpreter.IL

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
    abstract member Init : cilState list -> unit
    abstract member Update : cilState -> cilState seq -> unit
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
type ForwardSearcher(maxBound) =
//    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
//    let mutable mainMethod = null
//    let mutable startedFromMain = false
    let forPropagation = List<cilState>()
    let violatesLevel (s : cilState) =
        // TODO: report states which violate level as incomplete
        levelToUnsignedInt s.level > maxBound
    let isStopped s = isIIEState s || stoppedByException s || not(isExecutable(s)) || violatesLevel s
    let add (s : cilState) =
        if not <| isStopped s then
            assert(forPropagation.Contains s |> not)
            forPropagation.Add(s)

    interface IForwardSearcher with
        override x.Init state =
            forPropagation.AddRange(state)
        override x.Pick() =
            let detachedConcolicStates = forPropagation |> Seq.filter (fun s -> s.concolicStatus = concolicStatus.Detached)
            let availableStates =
                if not <| Seq.isEmpty detachedConcolicStates then detachedConcolicStates
                else
                    let pendingConcolicStates = forPropagation |> Seq.filter (fun s -> s.concolicStatus = concolicStatus.Waiting)
                    if Seq.isEmpty pendingConcolicStates then forPropagation |> Seq.filter (fun s -> s.concolicStatus = concolicStatus.PurelySymbolic)
                    else pendingConcolicStates
            x.Choose availableStates
        override x.Update parent newStates =
            if isStopped parent then
                forPropagation.Remove(parent) |> ignore
            Seq.iter add newStates
    abstract member Choose : seq<cilState> -> cilState option
    default x.Choose states = Seq.tryLast states

type BFSSearcher(maxBound) =
    inherit ForwardSearcher(maxBound) with
        override x.Choose states = Seq.tryHead states

type DFSSearcher(maxBound) =
    inherit ForwardSearcher(maxBound)
