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
        let idx = tryGetDictValue peaks key 1ul in
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

    let compare (contents1, peaks1) (contents2, peaks2) =
        assert(peaks1 = peaks2)
        Map.filterMap
            (fun k v ->
                let key = makeExtendedKey k v in
                let v1 = Map.find key contents1 in
                let v2 = Map.find key contents2 in
                if v1 = v2 then None else Some v2)
            peaks1
