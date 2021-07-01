namespace VSharp.Utils

open System.Collections.Generic

open VSharp

type IFrontQueue<'a when 'a: equality> =
    abstract Remove : 'a -> bool
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
        override x.Add elem = list.Add elem
        override x.ToSeq() = seq list

        override x.Clear() = list.Clear()

type ListFrontQueue<'a when 'a : equality>(comparer : IComparer<'a>) =
    let list = List<'a>(50)
    let mutable minimumIndex = -1
    let isEmpty () = list.Count = 0
    let deleteMin () =
        list.RemoveAt(minimumIndex)
        minimumIndex <- minimumIndex - 1
        true
    interface IPriorityQueue<'a> with
        override x.IsEmpty = isEmpty()
        override x.ExtractMin() =
            if isEmpty() then internalfailf "ExtractMin from empty ListPQ"
            list.[minimumIndex]
        override x.DeleteMin() = deleteMin()
        override x.Push elem =
            let mutable current = list.Count
            list.Add(elem)
            while current > 0 && comparer.Compare(elem, list.[current - 1]) = -1 do
                let tmp = list.[current]
                list.[current] <- list.[current - 1]
                current <- current - 1
                list.[current] <- tmp
            minimumIndex <- minimumIndex + 1

    interface IFrontQueue<'a> with
        override x.GetElement() =
            if isEmpty() then None
            else (x :> IPriorityQueue<_>).ExtractMin() |> Some
        override x.Remove(elem: 'a) : bool =
            assert(list.[minimumIndex] = elem)
            deleteMin()
        override x.Add elem = (x :> IPriorityQueue<'a>).Push(elem)
        override x.ToSeq() = seq list

        override x.Clear() =
            list.Clear()
            minimumIndex <- -1

