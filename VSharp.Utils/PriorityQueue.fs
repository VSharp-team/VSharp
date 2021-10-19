namespace VSharp.Utils

open System.Collections
open System.Collections.Generic

open VSharp

type IPriorityQueue<'a when 'a : equality> =
    abstract IsEmpty : bool
    abstract ExtractMin : unit -> 'a option
    abstract DeleteMin : unit -> bool
    abstract Push : 'a -> unit
    abstract ToSeq : unit -> 'a seq
    abstract Clear : unit -> unit

type TrivialPriorityQueue<'a when 'a : equality>() =
    let q = Queue<'a>()
    let isEmpty() = q.Count = 0
    interface IPriorityQueue<'a> with
        override x.IsEmpty = isEmpty()
        override x.ExtractMin() =
            if isEmpty() then None
            else Some (q.Peek())
        override x.DeleteMin () =
            q.Dequeue() |> ignore
            true
        override x.Push elem = q.Enqueue(elem)
        override x.ToSeq () = seq q
        override x.Clear () = q.Clear()

