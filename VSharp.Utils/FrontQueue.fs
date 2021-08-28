namespace VSharp.Utils

open System.Collections.Generic

open VSharp

type StackFrontQueue<'a when 'a : equality>() =
    let list = List<'a>()
    let isEmpty() = list.Count = 0

    interface IPriorityQueue<'a> with
        override x.IsEmpty = isEmpty()
        override x.ExtractMin() = if isEmpty() then None else Some list.[list.Count - 1]
        override x.DeleteMin() : bool = list.RemoveAt(list.Count - 1); true
        override x.Push elem = list.Add elem
        override x.ToSeq() = seq list
        override x.Clear() = list.Clear()

type ComparerPriorityQueue<'a when 'a : equality>(comparer : IComparer<'a>) =
    let list = List<'a>(50)
    let mutable minimumIndex = -1
    let isEmpty () = list.Count = 0
    let deleteMin () =
        if isEmpty() then internalfailf "ExtractMin from empty ListPQ"
        list.RemoveAt(minimumIndex)
        minimumIndex <- minimumIndex - 1
        true
    interface IPriorityQueue<'a> with
        override x.IsEmpty = isEmpty()
        override x.ExtractMin() =
            if isEmpty() then None
            else Some list.[minimumIndex]
        override x.DeleteMin() = deleteMin()
        override x.Push elem =
            let mutable current = list.Count
            list.Add(elem)
            while current > 0 && comparer.Compare(elem, list.[current - 1]) <> -1 do
                let tmp = list.[current]
                list.[current] <- list.[current - 1]
                current <- current - 1
                list.[current] <- tmp
            minimumIndex <- minimumIndex + 1
        override x.ToSeq() = seq list
        override x.Clear() =
            list.Clear()
            minimumIndex <- -1

