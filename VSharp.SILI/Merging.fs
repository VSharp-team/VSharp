namespace VSharp

open VSharp.Terms

module internal Merging =

    let private boolMerge = function
        | [] -> []
        | [_] as gvs -> gvs
        | [(g1, v1); (g2, v2)] -> [(g1 ||| g2, (g1 &&& v1) ||| (g2 &&& v2))]
        | (g, v)::gvs ->
            let guard = List.fold (|||) g (List.map fst gvs) in
            let value = List.fold (fun acc (g, v) -> acc ||| (g &&& v)) (g &&& v) gvs in
            [(guard, value)]

    let rec private structMerge t = function
        | [] -> []
        | [_] as gvs -> gvs
        | gvs ->
            let gs, vs = List.unzip gvs in
            let extractFields = function
                | Struct(fs, _) -> fs
                | t -> "Expected struct, got " + (toString t) |> internalfail
            let fss = vs |> List.map extractFields in
            let keys = fss |> List.fold (fun st fs -> fs |> Map.toList |> List.unzip |> fst |> Set.ofList |> Set.union st) Set.empty in
            let mergeOneKey k =
                let vals = fss |> List.map (fun fs -> if Map.containsKey k fs then fs.[k] else Nop) in
                (k, vals |> List.zip gs |> merge)
            in
            let result = keys |> Seq.map mergeOneKey |> Map.ofSeq in
            [(Terms.MakeTrue, Struct(result, t))]

    and private simplify gvs =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | (Terms.True, v)::gvs' -> [List.head gvs]
            | (Terms.False, v)::gvs' -> loop gvs' out
            | (g, Union us)::gvs' when not (List.isEmpty us) ->
                let guarded = us |> List.map (fun (g', v) -> (g &&& g', v)) in
                loop gvs' (List.append (simplify guarded) out)
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs []

    and internal mergeSame = function
        | [] -> []
        | [_] as xs -> xs
        | [(g1, v1); (g2, v2)] as gvs -> if v1 = v2 then [(g1 ||| g2, v1)] else gvs
        | gvs ->
            let rec loop gvs out =
                match gvs with
                | [] -> out
                | (g, v)::gvs' ->
                    let eq, rest = List.partition (snd >> (=) v) gvs' in
                    let joined = List.fold (|||) g (List.map fst eq)
                    match joined with
                    | True -> [(joined, v)]
                    | False -> loop rest out
                    | _ -> loop rest ((joined, v)::out)
            loop gvs []

    and private typedMerge gvs t =
        match t with
        | Bool -> boolMerge gvs
        // TODO: merge generalizations too
        | StructType _
        | ClassType _ ->
            structMerge t gvs
        | Numeric _
        | String
        | _ -> gvs

    and private compress = function
        | [] -> []
        | [_] as gvs -> gvs
        | [(_, v1); (_, v2)] as gvs when TypeOf v1 = TypeOf v2 -> typedMerge (mergeSame gvs) (TypeOf v1)
        | [_; _] as gvs -> gvs
        | gvs -> List.groupBy (snd >> TypeOf) gvs |> List.map (fun (t, gvs) -> typedMerge gvs t) |> List.concat

    and internal merge gvs =
        match compress (simplify gvs) with
        | [(True, v)] -> v
        | [(g, v)] when Terms.IsBool v -> g &&& v
        | gvs' -> Union gvs'

    let internal merge2Terms g h u v =
        match g, h, u, v with
        | _, _, _, _ when u = v -> u
        | True, _, _, _ -> u
        | False, _, _, _ -> v
        | _, True, _, _ -> v
        | _, False, _, _ -> u
        | Error _, _, _, _ -> g
        | _ -> merge [(g, u); (h, v)]

    let internal merge2States condition1 condition2 state1 state2 =
        match condition1, condition2 with
        | True, _ -> state1
        | False, _ -> state2
        | _, True -> state2
        | _, False -> state1
        | _ -> State.merge state1 state2 (merge2Terms condition1 condition2)

    let internal mergeStates conditions states =
        let gcs = List.zip conditions states in
        let merger (g1, s1) (g2, s2) = (g1 ||| g2, merge2States g1 g2 s1 s2) in
        match gcs with
        | [] -> State.empty
        | [(_, s)] -> s
        | _ -> List.reduce merger gcs |> snd

    let internal guardedMap mapper gvs =
        let gs, vs = List.unzip gvs in
        vs |> List.map mapper |> List.zip gs |> merge
