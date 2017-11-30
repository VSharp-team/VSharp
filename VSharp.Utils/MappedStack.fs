namespace VSharp.Utils

open VSharp
open MemoryCell

module public MappedStack =
    type stackContents<'a, 'b> when 'a : comparison = Map<'a * uint32, 'b>
    type stackPeaks<'a> when 'a : comparison = Map<'a, uint32>
    type stack<'a, 'b> when 'a : comparison = stackContents<'a, 'b> * stackPeaks<'a>

    let private defaultPeak = 0ul

    let private makeExtendedKey = makePair

    let private peakIdx peaks key =
        match Map.tryFind key peaks with
        | Some counter -> counter
        | None -> defaultPeak

    let empty = (Map.empty, Map.empty)

    let private addToStack key value (contents, peaks) idx =
        let peaks' = Map.add key idx peaks in
        let key' = makeExtendedKey key idx in
        let contents' = Map.add key' value contents in
        contents', peaks'

    let reserve key ((contents, peaks) as stack) =
        let idx = peakIdx peaks key + 1ul in
        let peaks' = Map.add key idx peaks in
        contents, peaks'

    let push key value ((contents, peaks) as stack) =
        let idx = peakIdx peaks key + 1ul in
        addToStack key value stack idx

    let add key value ((contents, peaks) as stack) =
        let idx = Dict.tryGetValue peaks key 1ul in
        addToStack key value stack idx

    let remove (contents, peaks) key =
        let idx = peakIdx peaks key in
        assert (idx > defaultPeak)
        let key' = makeExtendedKey key idx in
        let contents' = Map.remove key' contents in
        let peaks' =
            if idx = 1ul then Map.remove key peaks
            else Map.add key (idx - 1ul) peaks
        contents', peaks'

    let find ((name, token) as key) (contents, peaks) =
        let idx = peakIdx peaks key in
        assert (idx > defaultPeak)
        let key' = makeExtendedKey key idx in
        match Map.tryFind key' contents with
        | Some term -> term
        | None -> failwith "Attempt get value by key which is not presented in stack"

    let containsKey key (contents, peaks) = Map.containsKey key peaks && Map.containsKey (makeExtendedKey key peaks.[key]) contents

    let map f (contents, peaks) =
        Map.fold
            (fun m k v ->
                let key = makeExtendedKey k v in
                Map.add key (f k (Map.find key contents)) m)
            contents peaks, peaks

    let fold f state (contents, peaks) =
        Map.fold (fun s k v -> f s k (Map.find (makeExtendedKey k v) contents)) state peaks

    let compare keyMapper valueMapper (contents1, peaks1) (contents2, peaks2) =
        assert(peaks1 = peaks2)
        peaks1 |> Map.toList |> List.filterMap
            (fun (k, v) ->
                let key = makeExtendedKey k v in
                let v1 = Map.find key contents1 in
                let v2 = Map.find key contents2 in
                if v1 = v2 then None
                else
                    let v = valueMapper v2 in
                    Some(keyMapper k v, v))

    let public merge guards stacks resolve lazyInstantiate =
        assert(not <| List.isEmpty stacks)
        let peaks = List.head stacks |> snd in
        assert(List.forall (snd >> ((=) peaks)) (List.tail stacks))
        let keys = new System.Collections.Generic.HashSet<_>() in
        List.iter (fst >> Map.toSeq >> Seq.map (fun (k, cell) -> (k, cell.created)) >> keys.UnionWith) stacks
        let mergeOneKey (k, time) =
            let vals = List.map2 (fun g (s, _) -> (g, if Map.containsKey k s then s.[k] else k |> fst |> lazyInstantiate time)) guards stacks in
            (k, resolve vals)
        in (keys |> Seq.map mergeOneKey |> Map.ofSeq, peaks)

    let merge2 (contents1, peaks1) (contents2, peaks2) resolve lazyInstantiate =
        assert(peaks1 = peaks2)
        let newEntries = contents2 |> Map.toSeq |> Seq.filterMap (fun (k, cell) -> if Map.containsKey k contents1 then None else Some(k, cell.created)) in
        let modifiedEntries =
            contents1 |> Map.toSeq |> Seq.filterMap (fun (k, cell) ->
                if not <| Map.containsKey k contents2 || contents2.[k].value <> cell.value then Some(k, cell.created) else None) in
        let relevantEntries = new System.Collections.Generic.HashSet<_>(newEntries) in
        relevantEntries.UnionWith(modifiedEntries)
        let mergeOneKey result (k, time) =
            let val1 = if Map.containsKey k contents1 then contents1.[k] else k |> fst |> lazyInstantiate time in
            let val2 = if Map.containsKey k contents2 then contents2.[k] else k |> fst |> lazyInstantiate time in
            Map.add k (resolve val1 val2) result
        in (relevantEntries |> Seq.fold mergeOneKey contents1, peaks1)
