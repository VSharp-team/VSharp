namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic

open VSharp
open VSharp.Utils

type IEstimator =
    abstract member Estimate : cilState option * cilState -> int // parent and child

type ProbabilityPriorityQueue(estimator : IEstimator) =
    let mutable totalSum = 0
    let mutable indexToRemove = -1
    let mutable parent = None
    let rand = Random()
    let list = List<'a * int>()
    let isEmpty() = list.Count = 0
    let choose value =
        let mutable current = 0
        let mutable i = 0
        let count = list.Count
        while current + snd(list.[i]) < value && i < count do
            current <- current + snd(list.[i])
            i <- i + 1
        i

    interface IPriorityQueue<cilState> with
        override x.IsEmpty = isEmpty()
        override x.ExtractMin() =
            if isEmpty() then None
            else
                let value = rand.Next(totalSum)
                let index = choose value
                indexToRemove <- index
                parent <- Some (fst list.[index])
                parent
        override x.DeleteMin () =
            totalSum <- totalSum - snd(list.[indexToRemove])
            list.RemoveAt(indexToRemove)
            true
        override x.Push cilState =
            let value = estimator.Estimate(parent, cilState)
            totalSum <- totalSum + value
            list.Add(cilState, value)
        override x.ToSeq () = Seq.map fst list
        override x.Clear () =
            indexToRemove <- -1
            list.Clear()

type public RandomSearcher(weightedSearcher : IEstimator) =
    interface INewSearcher with
        override  x.ChooseAction(_,_,_) =
            __notImplemented__()
        override x.Reset () = __notImplemented__()
        override x.Init (_,_) = __notImplemented__()
        override x.PriorityQueue _ = ProbabilityPriorityQueue(weightedSearcher) :> IPriorityQueue<cilState>
    member x.F() = ()

