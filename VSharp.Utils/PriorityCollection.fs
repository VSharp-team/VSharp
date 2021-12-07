namespace VSharp.Utils

open System.Collections
open System.Collections.Generic

open VSharp

type IPriorityCollection<'a> =
    abstract member Insert : 'a -> uint -> unit
    abstract member Remove : 'a -> unit
    abstract member Update : 'a -> uint -> unit
    abstract member Choose : uint -> 'a option
    abstract member Contains : 'a -> bool
    abstract member TryGetPriority : 'a -> uint option
    abstract member MaxPriority : uint
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
    let minPriority () =
        if not <| Seq.isEmpty priorityToList then
            priorityToList.Keys |> Seq.head
        else 0u
    let maxPriority () =
        if not <| Seq.isEmpty priorityToList then
            priorityToList.Keys |> Seq.last
        else 0u
    let choose priority =
        let sortedPriorities = priorityToList.Keys |> seq
        if not <| Seq.isEmpty sortedPriorities then
            let maxP = maxPriority ()
            let item =
                if priority > maxP then
                    priorityToList.[maxP].First.Value
                else
                    let infimumPriority = sortedPriorities |> Seq.find (fun key -> priority <= key)
                    priorityToList.[infimumPriority].First.Value
            Some item
        else None
    let contains item = valuesToPriority.ContainsKey item
    let tryGetPriority item =
        if valuesToPriority.ContainsKey item then
            Some <| valuesToPriority.[item]
        else None

    interface IPriorityCollection<'a> with
        override x.Insert item priority = insert item priority
        override x.Remove item = remove item
        override x.Update item priority =
            remove item
            insert item priority
        override x.Choose priority = choose priority
        override x.Contains item = contains item
        override x.TryGetPriority item = tryGetPriority item
        override x.MaxPriority = maxPriority ()
        override x.ToSeq = valuesToPriority.Keys |> seq

