namespace VSharp.Core

open VSharp
open TypeUtils

module internal Merging =

    let guardOf term =
        match term.term with
        | GuardedValues(gs, _) -> disjunction gs
        | _ -> True()

    let private boolMerge gvs =
        let guard = List.fold (|||) (False()) (List.map fst gvs)
        let value = List.fold (fun acc (g, v) -> acc ||| (g &&& v)) (False()) gvs
        [(guard, value)]

    let rec private structMerge typ gvs =
        let gs, vs = List.unzip gvs
        let fss = vs |> List.map fieldsOf
        let mergedFields = PersistentDict.merge gs fss merge
        [(disjunction gs, Struct mergedFields typ)]

    and private simplify (|Unguard|_|) gvs =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | (True, _ as gv)::_ -> [gv]
            | (False, _)::gvs' -> loop gvs' out
            | (g, Unguard us)::gvs' ->
                let guarded = us |> List.map (fun (g', v) -> (g &&& g', v))
                loop gvs' (List.append (simplify (|Unguard|_|) guarded) out)
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs []

    and mergeSame<'a when 'a : equality> : (term * 'a) list -> (term * 'a) list = function
        | [] -> []
        | [_] as xs -> xs
        | [(g1, v1); (g2, v2)] as gvs -> if v1 = v2 then [(g1 ||| g2, v1)] else gvs
        | gvs ->
            let rec loop gvs out =
                match gvs with
                | [] -> out
                | (g, v)::gvs' ->
                    let eq, rest = List.partition (snd >> (=) v) gvs'
                    let joined = List.fold (|||) g (List.map fst eq)
                    match joined with
                    | True -> [(joined, v)]
                    | False -> loop rest out
                    | _ -> loop rest ((joined, v)::out)
            loop gvs []

    and private typedMerge = function
        | Bool -> boolMerge
        | StructType _ as typ-> structMerge typ
        | _ -> id

    and private compress = function
        | [] -> []
        | [(_, v)] -> [True(), v]
        | [(_, v1); (_, v2)] as gvs when typeOf v1 = typeOf v2 -> typedMerge (typeOf v1) (mergeSame gvs)
        | [_; _] as gvs -> gvs
        | gvs ->
            gvs
            |> mergeSame
            |> List.groupBy (snd >> typeOf)
            |> List.collect (fun (t, gvs) -> if List.length gvs >= 2 then typedMerge t gvs else gvs)

    and merge (gvs : (term * term) list) : term =
        match compress (simplify (|UnionT|_|) gvs) with
        | [(True, v)] -> v
        | [(g, v)] when isBool v -> g &&& v
        | gvs' -> Union gvs'

    let merge2Terms g h u v =
        let g = guardOf u &&& g
        let h = guardOf v &&& h
        match g, h with
        | _, _ when u = v -> u
        | True, _
        | _, False -> u
        | False, _
        | _, True -> v
        | _ -> merge [(g, u); (h, v)]

// ------------------------------------ Mapping non-term sequences ------------------------------------

    let guardedMapWithoutMerge f gvs =
        List.map (fun (g, v) -> (g, f v)) gvs

    let commonGuardedMapk mapper gvs merge k =
        Cps.List.mapk (fun (g, v) k -> mapper v (fun t -> k (g, t))) gvs (merge >> k)

    let guardedMap mapper gvs = commonGuardedMapk (Cps.ret mapper) gvs merge id

// ---------------------- Applying functions to terms and mapping term sequences ----------------------

    let commonGuardedApplyk f term merge k =
        match term.term with
        | Union gvs -> commonGuardedMapk f gvs merge k
        | _ -> f term k
    let commonGuardedApply f term merge = commonGuardedApplyk (Cps.ret f) term merge id

    let guardedApplyk f term k = commonGuardedApplyk f term merge k
    let guardedApply f term = guardedApplyk (Cps.ret f) term id

    let commonGuardedMapkWithPC pc mapper gvs merge k =
        let foldFunc gvs (g, v) k =
            let pc' = PC.add pc g
            if PC.isFalse pc' then k gvs
            else mapper v (fun t -> k ((g, t) :: gvs))
        Cps.List.foldlk foldFunc [] gvs (merge >> k)

    let commonGuardedApplykWithPC pc f term merge k =
        match term.term with
        | Union gvs -> commonGuardedMapkWithPC pc f gvs merge k
        | _ -> f term k
    let guardedApplykWithPC pc f term k = commonGuardedApplykWithPC pc f term merge k
    let guardedApplyWithPC pc f term = guardedApplykWithPC pc (Cps.ret f) term id

    let unguard = function
        | {term = Union gvs} -> gvs
        | t -> [(True(), t)]

    let unguardMerge = unguard >> merge

// ----------------------------------------------------------------------------------------------------

    let genericSimplify gvs : (term * 'a) list =
        let rec loop gvs out =
            match gvs with
            | [] -> out
            | (True, _ as gv)::_ -> [gv]
            | (False, _)::gvs' -> loop gvs' out
            | gv::gvs' -> loop gvs' (gv::out)
        loop gvs [] |> mergeSame

    let rec private genericGuardedCartesianProductRec mapper gacc xsacc = function
        | x::xs ->
            mapper x
            |> List.collect (fun (g, v) ->
                genericGuardedCartesianProductRec mapper (gacc &&& g) (List.append xsacc [v]) xs)
            |> genericSimplify
        | [] -> [(gacc, xsacc)]
    let genericGuardedCartesianProduct mapper xs =
        genericGuardedCartesianProductRec mapper (True()) [] xs

    let rec private guardedCartesianProductRec mapper ctor gacc xsacc = function
        | x::xs ->
            mapper x
            |> List.collect (fun (g, v) ->
                let g' = gacc &&& g
                guardedCartesianProductRec mapper ctor g' (List.append xsacc [v]) xs)
            |> genericSimplify
        | [] -> [(gacc, ctor xsacc)]

    let guardedCartesianProduct mapper terms ctor =
        guardedCartesianProductRec mapper ctor (True()) [] terms
