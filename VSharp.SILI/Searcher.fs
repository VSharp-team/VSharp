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

type IpStackComparer() =
    interface IComparer<cilState> with
        override _.Compare(x : cilState, y : cilState) =
            let res = (List.length x.ipStack).CompareTo(List.length y.ipStack)
            res

type cilStateComparer(comparer) =
    interface IComparer<cilState> with
        override _.Compare(x : cilState, y : cilState) =
            comparer x y

type SearchDirection =
    | Stop
    | Start of ip
    | GoForward of cilState
    | GoBackward of pob * cilState

type INewSearcher =
    abstract member ChooseAction : IPriorityQueue<cilState> * (pob * cilState) seq * pob seq -> SearchDirection
    abstract member CanReach : ip stack * ip * ip list -> bool
    abstract member Reset : unit -> unit
    abstract member Init : MethodBase * codeLocation seq -> unit
    abstract member PriorityQueue : uint -> IPriorityQueue<cilState>
    abstract member TotalNumber : uint

type FrontQueue(maxBound, searcher : INewSearcher) =
    let goodStates : IPriorityQueue<cilState> = searcher.PriorityQueue(maxBound)
    // NOTE: ``justAddedGoodStates'' should be used by TargetedSearcher to update its priorityQueue
    let justAddedGoodStates =
        let emptyState = makeInitialState null Memory.EmptyState
        Array.init 10 (fun _ -> emptyState)
    let mutable justAddedSize = 0
    let iieStates = List<cilState>()
    let erroredStates = List<cilState>()
    let maxBoundViolatingStates = List<cilState>()
    let nonExecutableStates = List<cilState>()

    let doesViolateMaxBound (s : cilState) =
        levelToUnsignedInt s.level > maxBound
//        match currentIp s with
//        | Instruction(offset, m) ->
//            let codeLocation = {offset = offset; method = m}
//            match PersistentDict.tryFind s.level codeLocation with
//            | Some current -> current >= uint32 maxBound
//            | None -> false
//        | _ -> false
    let addGoodState (s : cilState) =
        if doesViolateMaxBound s then maxBoundViolatingStates.Add(s)
        elif not (isExecutable s) then nonExecutableStates.Add(s)
        else justAddedGoodStates.[justAddedSize] <- s
             justAddedSize <- justAddedSize + 1

    let transformJustAdded2GoodStates() =
        // TODO: profile it, because clearing might give overhead
        for i = 0 to justAddedSize - 1 do
            goodStates.Push(justAddedGoodStates.[i])
        justAddedSize <- 0
    member x.Add(s : cilState) =
        assert(List.length s.ipStack = Memory.CallStackSize s.state)
        if isIIEState s then iieStates.Add(s)
        elif isUnhandledError s then erroredStates.Add(s) // TODO: check it. Maybe ''isUnhandledError'' should be here
        else addGoodState s
    member x.AddGoodStates(newStates : cilState seq) = Seq.iter addGoodState newStates
    member x.AddIIEStates(newStates : cilState seq) = Seq.iter iieStates.Add newStates
    member x.AddErroredStates(newStates : cilState seq) = Seq.iter erroredStates.Add newStates
    member x.Remove(s : cilState) : bool =
        assert(goodStates.ExtractMin() = Some s)
        goodStates.DeleteMin()
    member x.ExtractMin() : cilState option =
        transformJustAdded2GoodStates()
        goodStates.ExtractMin()

    member x.StatesForPropagation() : IPriorityQueue<cilState> =
        transformJustAdded2GoodStates()
        goodStates

    // TODO: Is this method really needed?
    member x.GetAllStates() =
        goodStates.ToSeq() |> Seq.append iieStates |> Seq.append erroredStates
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



[<AbstractClass>]
type ForwardSearcher() = // TODO: max bound is needed, when we are in recursion, but when we go to one method many time -- it's okay #do
//    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
    static let mutable totalNumber = 0u
    let mutable stepsNumber = 0u
    let mutable mainMethod = null
    interface INewSearcher with
        override x.CanReach(_,_,_) = true
        override x.TotalNumber = totalNumber
        override x.PriorityQueue maxBound : IPriorityQueue<cilState> = x.PriorityQueue maxBound
        override x.Init (m,_) =
            mainMethod <- m
//        override x.ClosePob (_) = ()
        override x.Reset () =
            Logger.warning "steps number done by %O = %d" (x.GetType()) stepsNumber
            totalNumber <- totalNumber + stepsNumber
            stepsNumber <- 0u
            mainMethod <- null
        override x.ChooseAction(fq, bq, _) =
            stepsNumber <- stepsNumber + 1u
            match bq with
            | Seq.Cons(ps, _) -> GoBackward ps
            | Seq.Empty when stepsNumber = 1u && fq.IsEmpty -> Start <| Instruction(0, mainMethod)
            | Seq.Empty ->
                match fq.ExtractMin() with
                | None -> Stop
                | Some s -> GoForward s
    abstract member PriorityQueue : uint -> IPriorityQueue<cilState>
    default x.PriorityQueue _ = StackFrontQueue() :> IPriorityQueue<cilState>

type BFSSearcher() =
    inherit ForwardSearcher() with
        override x.PriorityQueue _ = TrivialPriorityQueue<cilState>() :> IPriorityQueue<cilState>

type DFSSearcher() =
    inherit ForwardSearcher()

