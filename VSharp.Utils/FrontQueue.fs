namespace VSharp.Utils

open System.Collections.Generic

open VSharp

type IFrontQueue<'a when 'a: equality> =
    abstract Remove : 'a -> bool
    abstract RemoveAll : System.Predicate<'a> -> int
    abstract Add : 'a -> unit
    abstract GetElement : unit -> 'a option
    abstract ToSeq : unit -> 'a seq // TODO: get rid of it!
    abstract Clear : unit -> unit

type StackFrontQueue<'a when 'a : equality>() =
    let list = List<'a>()
    let isEmpty() = list.Count = 0

    interface IFrontQueue<'a> with
        override x.GetElement() =
            if isEmpty() then None
            else list.[list.Count - 1] |> Some
        override x.Remove(elem: 'a) : bool = list.Remove(elem)

        override x.RemoveAll (pred : System.Predicate<'a>) = list.RemoveAll(pred)

        override x.Add elem = list.Add elem
        override x.ToSeq() = seq list

        override x.Clear() = list.Clear()


type ListFrontQueue<'a when 'a : equality>(comparer : IComparer<'a>) =
    let list = List<'a>()
    let mutable minimumIndex = -1
    let isEmpty () = list.Count = 0
    let isNewMinimum (elem : 'a) =
        comparer.Compare(elem, list.[minimumIndex]) <> 1
    let updateMinimumIndex () =
        if isEmpty() then minimumIndex <- -1
        else
            minimumIndex <- 0
            for i = 1 to list.Count - 1 do
                if isNewMinimum list.[i] then
                    minimumIndex <- i
    interface IPriorityQueue<'a> with
        override x.IsEmpty = isEmpty()
        override x.ExtractMin() =
            if isEmpty() then internalfailf "ExtractMin from empty ListPQ"
            list.[minimumIndex]
        override x.DeleteMin() =
            list.RemoveAt(minimumIndex)
            updateMinimumIndex()
            true
        override x.Push elem =
            if isEmpty() then
                minimumIndex <- 0
            elif isNewMinimum elem then
                minimumIndex <- list.Count
            list.Add(elem)

    interface IFrontQueue<'a> with

        override x.GetElement() =
            if isEmpty() then None
            else (x :> IPriorityQueue<_>).ExtractMin() |> Some
        override x.Remove(elem: 'a) : bool =
            if list.[minimumIndex] = elem then
                (x :> IPriorityQueue<'a>).DeleteMin()
            else
                let res = list.Remove(elem)
                updateMinimumIndex ()
                res

        override x.RemoveAll (pred : System.Predicate<'a>) =
            let res = list.RemoveAll(pred)
            updateMinimumIndex ()
            res

        override x.Add elem = (x :> IPriorityQueue<'a>).Push(elem)
        override x.ToSeq() = seq list

        override x.Clear() =
            list.Clear()
            minimumIndex <- -1



