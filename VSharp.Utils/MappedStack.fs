namespace VSharp.Utils

open VSharp

type mappedStackContents<'a, 'b> when 'a : comparison = Map<'a * uint32, 'b>
type mappedStackPeaks<'a> when 'a : comparison = Map<'a, uint32>
type mappedStack<'a, 'b> when 'a : comparison = mappedStackContents<'a, 'b> * mappedStackPeaks<'a>

module public MappedStack =

    let private defaultTop = 0ul

    let private makeExtendedKey = makePair

    let private peakIdx peaks key = Map.tryFind key peaks |?? defaultTop

    let empty = (Map.empty, Map.empty)

    let private addToStack key value (contents, peaks) idx =
        let peaks' = Map.add key idx peaks
        let key' = makeExtendedKey key idx
        let contents' = Map.add key' value contents
        contents', peaks'

    let reserve key ((contents, peaks) as stack) =
        let idx = peakIdx peaks key + 1ul
        let peaks' = Map.add key idx peaks
        contents, peaks'

    let push key value ((contents, peaks) as stack) =
        let idx = peakIdx peaks key + 1ul
        addToStack key value stack idx

    let add key value ((contents, peaks) as stack) =
        let idx = Dict.tryGetValue peaks key 1ul
        addToStack key value stack idx

    let remove (contents, peaks) key =
        let idx = peakIdx peaks key
        assert (idx > defaultTop)
        let key' = makeExtendedKey key idx
        let contents' = Map.remove key' contents
        let peaks' =
            if idx = 1ul then Map.remove key peaks
            else Map.add key (idx - 1ul) peaks
        contents', peaks'

    let find key (contents, peaks) =
        let idx = peakIdx peaks key
        assert (idx > defaultTop)
        let key' = makeExtendedKey key idx
        match Map.tryFind key' contents with
        | Some term -> term
        | None -> failwith "Attempt get value by key which is not presented in stack"

    let containsKey key (contents, peaks) = Map.containsKey key peaks && Map.containsKey (makeExtendedKey key peaks.[key]) contents

    let map f (contents, peaks) =
        Map.fold
            (fun m k v ->
                let key = makeExtendedKey k v
                Map.add key (f k (Map.find key contents)) m)
            contents peaks, peaks

    let fold f state (contents, peaks) =
        Map.fold (fun s k v -> f s k (Map.find (makeExtendedKey k v) contents)) state peaks

    let compare keyMapper valueMapper (contents1, peaks1) (contents2, peaks2) =
        assert(peaks1 = peaks2)
        peaks1 |> Map.toList |> List.choose
            (fun (k, v) ->
                let key = makeExtendedKey k v
                let v1 = Map.find key contents1
                let v2 = Map.find key contents2
                if v1 = v2 then None
                else
                    let v = valueMapper v2
                    Some(keyMapper k v, v))

    let public merge stacks resolve =
        assert(not <| List.isEmpty stacks)
        let peaks = List.head stacks |> snd
        assert(List.forall (snd >> ((=) peaks)) (List.tail stacks))
        let keys = new System.Collections.Generic.HashSet<_>()
        List.iter (fst >> Map.toSeq >> Seq.map fst >> keys.UnionWith) stacks
        let mergeOneKey k =
            let vals = List.map (fun (s, _) -> if Map.containsKey k s then Some(s.[k]) else None) stacks
            (k, resolve (fst k) vals)
        (keys |> Seq.map mergeOneKey |> Map.ofSeq, peaks)

    let merge2 (contents1, peaks1) (contents2, peaks2) resolve =
//        assert(peaks1 = peaks2)
        let newEntries = contents2 |> Map.toSeq |> Seq.choose (fun (k, _) -> if Map.containsKey k contents1 then None else Some k)
        let modifiedEntries =
            contents1 |> Map.toSeq |> Seq.choose (fun (k, cell) ->
                if not <| Map.containsKey k contents2 || contents2.[k].value <> cell.value then Some k else None)
        let relevantEntries = new System.Collections.Generic.HashSet<_>(newEntries)
        relevantEntries.UnionWith(modifiedEntries)
        let mergeOneKey result k =
            let val1 = if Map.containsKey k contents1 then Some(contents1.[k]) else None
            let val2 = if Map.containsKey k contents2 then Some(contents2.[k]) else None
            Map.add k (resolve (fst k) val1 val2) result
        (relevantEntries |> Seq.fold mergeOneKey contents1, peaks1)
