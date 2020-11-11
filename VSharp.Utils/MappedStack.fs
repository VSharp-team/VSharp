namespace VSharp.Utils

open VSharp

type mappedStackContents<'a, 'b> when 'a : comparison = Map<'a * uint32, 'b>
type mappedStackPeaks<'a> when 'a : comparison = Map<'a, uint32>
type mappedStack<'a, 'b> when 'a : comparison = mappedStackContents<'a, 'b> * mappedStackPeaks<'a>

module public MappedStack =

    let private defaultPeak = 0ul

    let private makeExtendedKey = makePair

    let private peakIdx peaks key = Map.tryFind key peaks |?? defaultPeak

    let empty = (Map.empty, Map.empty)

    let private addToStack key value (contents, peaks) idx =
        let peaks' = Map.add key idx peaks
        let key' = makeExtendedKey key idx
        let contents' = Map.add key' value contents
        contents', peaks'

    let reserve key (contents, peaks) =
        let idx = peakIdx peaks key + 1ul
        let peaks' = Map.add key idx peaks
        contents, peaks'

    let push key value ((_, peaks) as stack) =
        let idx = peakIdx peaks key + 1ul
        addToStack key value stack idx

    let add key value ((_, peaks) as stack) =
        let idx = Dict.tryGetValue peaks key 1ul
        addToStack key value stack idx

    let containsKey key (contents, peaks) = Map.containsKey key peaks && Map.containsKey (makeExtendedKey key peaks.[key]) contents

    let remove (contents, peaks) key =
        let idx = peakIdx peaks key
        match containsKey key (contents, peaks) with
        | false ->
            // this is a case when variable was reserved but never assigned
            assert (idx = 0u)
            (contents, peaks)
        | _ ->
            assert (idx > defaultPeak)
            let key' = makeExtendedKey key idx
            let contents' = Map.remove key' contents
            let peaks' =
                if idx = 1ul then Map.remove key peaks
                else Map.add key (idx - 1ul) peaks
            contents', peaks'

    let tryFind key (contents, peaks) =
        let idx = peakIdx peaks key
        assert (idx > defaultPeak)
        let key' = makeExtendedKey key idx
        Map.tryFind key' contents

    let find key stack =
        match tryFind key stack with
        | Some term -> term
        | None -> failwith "Attempt get value by key which is not presented in stack"


    let map f (contents, peaks) =
        Map.fold
            (fun m k v ->
                let key = makeExtendedKey k v
                Map.add key (f k (Map.find key contents)) m)
            contents peaks, peaks

    let fold f state (contents, peaks) =
        Map.fold (fun s k v ->
            Option.map (f s k) (Map.tryFind (makeExtendedKey k v) contents) |?? s) state peaks

    let concat (bottomContents, bottomPeaks) (topContents, topPeaks) =
        let peaks = topPeaks |> Map.fold (fun peaks k idx -> Map.add k (peakIdx bottomPeaks k + idx) peaks) bottomPeaks
        let contents = topContents |> Map.fold (fun contents (k, idx) v -> Map.add (k, peakIdx bottomPeaks k + idx) v contents) bottomContents
        (contents, peaks) : mappedStack<_, _>

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

    let public merge guards stacks resolve lazyInstantiate =
        assert(not <| List.isEmpty stacks)
        let peaks = List.head stacks |> snd
        assert(List.forall (snd >> ((=) peaks)) (List.tail stacks))
        let keys = System.Collections.Generic.HashSet<_>()
        List.iter (fst >> Map.toSeq >> Seq.map fst >> keys.UnionWith) stacks
        let mergeOneKey k =
            let vals = List.map2 (fun g (s, _) -> (g, Map.tryFind k s |?? lazyInstantiate (fst k) ())) guards stacks
            (k, resolve vals)
        (keys |> Seq.map mergeOneKey |> Map.ofSeq, peaks)

    let merge2 (contents1, peaks1) (contents2, peaks2) resolve lazyInstantiate =
        assert(peaks1 = peaks2)
        let newEntries = contents2 |> Map.toSeq |> Seq.choose (fun (k, _) -> if Map.containsKey k contents1 then None else Some k)
        let modifiedEntries =
            contents1 |> Map.toSeq |> Seq.choose (fun (k, v) ->
                if not <| Map.containsKey k contents2 || contents2.[k] <> v then Some k else None)
        let relevantEntries = System.Collections.Generic.HashSet<_>(newEntries)
        relevantEntries.UnionWith(modifiedEntries)
        let mergeOneKey result k =
            let val1 = lazyInstantiate (fst k) |> Map.findOrDefaultWith k contents1
            let val2 = lazyInstantiate (fst k) |> Map.findOrDefaultWith k contents2
            Map.add k (resolve val1 val2) result
        (relevantEntries |> Seq.fold mergeOneKey contents1, peaks1)
