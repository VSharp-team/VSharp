namespace VSharp.Interpreter.IL

open System
open System.Collections
open System.Collections.Generic
open System.Reflection
open FSharpx.Collections
open VSharp
open CilStateOperations
open VSharp.Core
open VSharp.Utils

type IResettableSearcher =
    abstract member Init : MethodBase -> pob seq -> unit
    abstract member Reset : unit -> unit
    abstract member ShouldWork : unit -> bool

type action =
    | GoFront of cilState
    | GoBack of cilState * pob
    | Stop
type IBidirectionalSearcher =
    abstract member UpdateStates : cilState -> cilState seq -> unit
    abstract member UpdatePobs : pob -> pob -> unit          // updatePob q q' добавляет pob q в backwardSearcher с родителем q'
    abstract member Pick : unit -> action
    abstract member Answer : pob -> pobStatus -> unit
//    abstract member IsFinished : unit -> bool
//    abstract member Init : MethodBase -> pob seq -> unit
    abstract member Statuses : unit -> seq<pob * pobStatus>
    inherit IResettableSearcher

type IForwardSearcher =
    abstract member Update : cilState -> cilState seq -> unit
    abstract member Pick : unit -> cilState
//    abstract member Init : cilState -> unit
//    abstract member IsEmpty : unit -> bool
    abstract member FinishedStates : unit -> cilState seq
    inherit IResettableSearcher

type ITargetedSearcher =
    inherit IResettableSearcher
    abstract member SetTargets : ip -> ip seq -> unit  // setTargets from to: добавить eps в точке from, вести его в точки to
    abstract member Update : cilState -> cilState seq -> cilState seq // возвращаемое значение: какие состояния достигли цели
    abstract member Pick : unit -> cilState
//    abstract member IsEmpty : unit -> bool
type backwardAction = Propagate of cilState * pob | InitTarget of ip * pob seq | NoAction
type IBackwardSearcher =
    inherit IResettableSearcher
//    abstract member HasPobs : unit -> bool
    abstract member Update : pob -> pob -> unit
    abstract member Answer : pob -> pobStatus -> unit
    abstract member Statuses : unit -> seq<pob * pobStatus>
//    abstract member Init : MethodBase -> pob seq -> unit
    abstract member Pick : unit -> backwardAction // выбрать, к каким pobs двигаться и откуда
    abstract member AddBranch : cilState -> pob list // как только targeted-состояние достигает target, серчер уведомляется об этом через этот метод. возвращается список pob-ов в этой точке

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
    let finished = List<cilState>()
    let violatesLevel (s : cilState) =
        levelToUnsignedInt s.level > maxBound
    let add (s : cilState) =
        if isIIEState s || isError s || not(isExecutable(s)) || violatesLevel s then finished.Add(s)
        else forPropagation.Add(s)
    interface IResettableSearcher with
        override x.ShouldWork () = forPropagation.Count > 0
        override x.Init m _ =
            let initial = ExplorerBase.FormInitialStateWithoutStatics m
            let initial = makeInitialState m initial
            forPropagation.Add initial
        override x.Reset() =
            forPropagation.Clear()
            finished.Clear()
    interface IForwardSearcher with
//        override x.Reset () =
//            forPropagation.Clear()
        override x.FinishedStates () = seq finished
        override x.Pick() = x.Choose(forPropagation)
        override x.Update parent newStates =
            if isIIEState parent || isError parent || not(isExecutable(parent)) || violatesLevel parent then
                forPropagation.Remove(parent) |> ignore
                finished.Add(parent)
            Seq.iter add newStates
//            match bq with
//            | Seq.Cons(ps, _) -> GoBackward ps
//            | Seq.Empty when not startedFromMain && fq.IsEmpty ->
//                startedFromMain <- true
//                Start <| Instruction(0, mainMethod)
//            | Seq.Empty ->
//                match fq.ExtractMin() with
//                | None -> Stop
//                | Some s ->
//                    let removed = fq.DeleteMin() in assert removed
//                    GoForward s

//    interface IBidirectionalSearcher with
//        override x.UpdateStates parent children = (x :> IForwardSearcher).Update parent children
//        override x.UpdatePobs parent child = __notImplemented__()
//        override x.ChooseAction () = __notImplemented__()
//        override x.Reset () = __notImplemented__()
//        override x.Init (_,_) = __notImplemented__()
    abstract member Choose : seq<cilState> -> cilState
    default x.Choose states =
        if Seq.isEmpty states then ()
        Seq.last states

type BFSSearcher(maxBound) =
    inherit ForwardSearcher(maxBound) with
        override x.Choose states = Seq.head states

type DFSSearcher(maxBound) =
    inherit ForwardSearcher(maxBound)
