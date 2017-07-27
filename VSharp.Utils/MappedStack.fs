namespace VSharp.Utils

open VSharp

module public MappedStack =
    type stack<'a, 'b> when 'a : comparison = Map<'a * uint32, 'b> * Map<'a, uint32>

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

    let containsKey key (_, peaks) = Map.containsKey key peaks

    let map f (contents, peaks) =
        Map.fold
            (fun m k v ->
                let key = makeExtendedKey k v in
                Map.add key (f k (Map.find key contents)) m)
            contents peaks, peaks

    let fold f state (contents, peaks) =
        Map.fold (fun s k v -> f s k (Map.find (makeExtendedKey k v) contents)) state peaks

    let compare keyMapper (contents1, peaks1) (contents2, peaks2) =
        assert(peaks1 = peaks2)
        peaks1 |> Map.toList |> List.filterMap
            (fun (k, v) ->
                let key = makeExtendedKey k v in
                let v1 = Map.find key contents1 in
                let v2 = Map.find key contents2 in
                if v1 = v2 then None else Some(keyMapper k v2, v2))

    let public merge guards stacks resolve =
        assert(not <| List.isEmpty stacks)
        let peaks = List.head stacks |> snd in
        assert(List.forall (snd >> ((=) peaks)) stacks)
        let keys = new System.Collections.Generic.HashSet<'a>() in
        List.iter (fst >> Map.toSeq >> Seq.map fst >> keys.UnionWith) stacks
        let mergeOneKey k =
            let vals = List.filterMap2 (fun g (s, _) -> if Map.containsKey k s then Some(g, s.[k]) else None) guards stacks in
            (k, resolve vals)
        in
        (keys |> Seq.map mergeOneKey |> Map.ofSeq, peaks)

    let merge2 s1 s2 resolve =
        let resolveIfShould map key value =
            if containsKey key map then
                let oldValue = find key map in
                let newValue = value in
                if oldValue = newValue then map
                else
                    add key (resolve oldValue newValue) map
            else
                add key value map
        fold resolveIfShould s1 s2
