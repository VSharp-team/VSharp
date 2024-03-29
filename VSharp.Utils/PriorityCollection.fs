namespace VSharp.Utils

open System.Collections.Generic
open System.Linq

open VSharp

type IPriorityCollection<'a> =
    abstract member Insert : 'a -> uint -> unit
    abstract member Remove : 'a -> unit
    abstract member Update : 'a -> uint -> unit
    abstract member Choose : unit -> 'a option
    abstract member Choose : ('a -> bool) -> 'a option
    abstract member Contains : 'a -> bool
    abstract member TryGetPriority : 'a -> uint option
    abstract member Clear : unit -> unit
    abstract member Count : uint
    abstract member ToSeq : 'a seq

type BidictionaryPriorityQueue<'a when 'a : equality>() =
    let valuesToPriority = Dictionary<'a, uint>()
    let priorityToList = SortedDictionary<uint, LinkedList<'a>>()
    let isEmpty () = valuesToPriority.Count = 0
    let insert item priority =
        assert(not <| valuesToPriority.ContainsKey item)
        valuesToPriority.Add(item, priority)
        let priorityList = Dict.getValueOrUpdate priorityToList priority (fun () -> LinkedList<_>())
        priorityList.AddFirst item |> ignore
    let remove item =
        assert(valuesToPriority.ContainsKey item)
        let priority = valuesToPriority.[item]
        valuesToPriority.Remove item |> ignore
        let priorityList = priorityToList.[priority]
        priorityList.Remove item |> ignore
        if Seq.isEmpty priorityList then
            priorityToList.Remove priority |> ignore
    let chooseWithSelector selector =
        let sortedPriorities = priorityToList.Keys |> List.ofSeq
        if not <| Seq.isEmpty sortedPriorities then
            sortedPriorities |> Seq.tryPick (fun p -> priorityToList.[p] |> Seq.tryFind selector)
        else None
    let choose () =
        let sortedPriorities = priorityToList.Keys |> List.ofSeq
        if not <| Seq.isEmpty sortedPriorities then
            priorityToList.[sortedPriorities.Head].First.Value |> Some
        else None
    let contains item = valuesToPriority.ContainsKey item
    let tryGetPriority item =
        if valuesToPriority.ContainsKey item then
            Some <| valuesToPriority.[item]
        else None
    let clear () =
        valuesToPriority.Clear()
        priorityToList.Clear()

    interface IPriorityCollection<'a> with
        override x.Insert item priority = insert item priority
        override x.Remove item = remove item
        override x.Update item priority =
            remove item
            insert item priority
        override x.Choose() = choose ()
        override x.Choose(selector) = chooseWithSelector selector
        override x.Contains item = contains item
        override x.TryGetPriority item = tryGetPriority item
        override x.Clear() = clear ()
        override x.Count = uint valuesToPriority.Count
        override x.ToSeq = valuesToPriority.Keys |> seq
