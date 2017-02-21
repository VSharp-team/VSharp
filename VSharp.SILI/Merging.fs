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

    let private typedMerge gvs t =
        match t with
        | Bool -> boolMerge gvs
        | Numeric _
        | String -> gvs
        | _ -> raise (new System.NotImplementedException())

    let private simplify gvs =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | (Terms.True, v)::gvs' -> [List.head gvs]
            | (Terms.False, v)::gvs' -> loop gvs' out
            | (g, Union us)::gvs' when not (List.isEmpty us) ->
                loop gvs' (List.append (Unions.guardWith g us) out)
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs []

    let internal mergeSame = function
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
                    if Terms.IsTrue joined then [(joined, v)]
                    else loop rest ((joined, v)::out)
            loop gvs []

    let private compress = function
        | [] -> []
        | [_] as gvs -> gvs
        | [(_, v1); (_, v2)] as gvs when TypeOf v1 = TypeOf v2 -> typedMerge (mergeSame gvs) (TypeOf v1)
        | [_; _] as gvs -> gvs
        | gvs -> List.groupBy (snd >> TypeOf) gvs |> List.map (fun (t, gvs) -> typedMerge gvs t) |> List.concat

    let internal merge gvs state =
        match compress (simplify gvs) with
        | [(g, v)] -> (v, State.addAssertion state g)
        | gvs' -> (Union gvs', state)

    let internal merge2Terms b u v =
        match b, u, v with
        | _, _, _ when u = v -> u
        | True, _, _ -> u
        | False, _, _ -> v
        | _ -> merge [(b, u); (!!b, v)] State.empty |> fst

    let internal mergeStates condition state1 state2 =
        match condition with
        | True -> state1
        | False -> state2
        | _ ->
            assert(State.path state1 = State.path state2)
            assert(State.frames state1 = State.frames state2)
            let mergeIfShould id u =
                if State.hasEntry state2 id
                then merge2Terms condition u (State.eval state2 id)
                else u
            state1
                |> State.mapKeys mergeIfShould
                |> State.union state2
                |> State.withAssertions (State.uniteAssertions (State.assertions state1) (State.assertions state2))
