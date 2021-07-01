namespace VSharp.Utils

open System.Collections.Generic

open VSharp

type IPriorityQueue<'a when 'a : equality> =
    abstract IsEmpty : bool
    abstract ExtractMin : unit -> 'a
    abstract DeleteMin : unit -> bool
    abstract Push : 'a -> unit




