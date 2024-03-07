namespace VSharp.Core

open VSharp
open TypeUtils

module internal Merging =

    let guardOf term =
        match term.term with
        | GuardedValues(gs, _) -> disjunction gs
        | _ -> True()

    let private boolMerge iteType =
        let disjointIte, elseGuard = List.mapFold (fun disjG (g, v) -> (g &&& disjG , v) , !!g &&& disjG) (True()) iteType.ite
        List.fold (fun acc (g, v) -> acc ||| (g &&& v)) (elseGuard &&& iteType.elseValue) disjointIte

    let rec private structMerge typ iteType =
        let iteFs = iteType.ite |> List.map (fun (g, v) -> (g, fieldsOf v))
        let e = iteType.elseValue |> fieldsOf
        let union ite e = {ite = ite; elseValue = e}
        let mergedFields = PersistentDict.merge iteFs e union merge
        Struct mergedFields typ

    and private simplify iteType =
        let folder gv (accIte, accElse) =
            match gv with
            | True, IteT ite -> let simplified = simplify ite in simplified.ite, simplified.elseValue
            | True, t -> [], t
            | False, _ -> accIte, accElse
            | g, IteT ite ->
                let guarded = List.map (fun (g', v) -> (g &&& g', v)) ite.ite
                let simplified = simplify {ite with ite = guarded}
                simplified.ite @ (g, simplified.elseValue)::accIte, accElse
            | gv -> gv::accIte, accElse
        let accIte, accElse =
            match iteType.elseValue with
            | IteT t ->
                let simplified = simplify t
                simplified.ite, simplified.elseValue
            | _ -> [], iteType.elseValue
        let ite, e = List.foldBack folder iteType.ite (accIte, accElse)
        {ite = ite; elseValue = e}

    and private compress = function
        // | [(_, v1); (_, v2)] as gvs when typeOf v1 = typeOf v2 -> typedMerge (typeOf v1) gvs
        // | [_; _] as gvs -> gvs
        | {ite = []; elseValue = e} -> e
        | {ite = _; elseValue =  {term = Struct(_, t)}} as iteType -> structMerge t iteType
        | {ite = _; elseValue = v} as iteType when isBool v -> boolMerge iteType
        | iteType -> Ite iteType

    and merge ite : term =
        match compress (simplify ite) with
        | IteT {ite = []; elseValue = e} -> e
        | IteT iteType -> Ite iteType
        | t -> t

    let merge2Terms g h u v =
        let g = guardOf u &&& g
        let h = guardOf v &&& h
        match g, h with
        | _, _ when u = v -> u
        | True, _
        | _, False -> u
        | False, _
        | _, True -> v
        | _ -> merge {ite = [(g, u)]; elseValue = v}

// ------------------------------------ Mapping non-term sequences ------------------------------------

    let commonGuardedMapk mapper iteType merge k =
        Cps.List.mapk (fun (g, v) k -> mapper v (fun t -> k (g, t))) iteType.ite (fun ite' ->
        mapper iteType.elseValue (fun e' ->
        {ite = ite'; elseValue = e';} |> merge |> k))

    let guardedMap mapper iteType = commonGuardedMapk (Cps.ret mapper) iteType merge id
    let guardedMapWithoutMerge mapper iteType = commonGuardedMapk (Cps.ret mapper) iteType id id

// ---------------------- Applying functions to terms and mapping term sequences ----------------------

    let commonGuardedApplyk f term merge k =
        match term.term with
        | Ite iteType -> commonGuardedMapk f iteType merge k
        | _ -> f term k
    let commonGuardedApply f term merge = commonGuardedApplyk (Cps.ret f) term merge id

    let guardedApplyk f term k = commonGuardedApplyk f term merge k
    let guardedApply f term = guardedApplyk (Cps.ret f) term id

    let commonGuardedMapkWithPC pc mapper iteType merge k =
        let foldFunc (g, v) ite k =
            let pc' = PC.add pc g
            if PC.isFalse pc' then k ite
            else mapper v (fun t -> k ((g, t) :: ite))
        Cps.List.foldrk foldFunc [] iteType.ite (fun ite' ->
        mapper iteType.elseValue (fun e' ->
        {ite = ite'; elseValue = e'} |> merge |> k))

    let commonGuardedApplykWithPC pc f term merge k =
        match term.term with
        | Ite iteType -> commonGuardedMapkWithPC pc f iteType merge k
        | _ -> f term k
    let guardedApplykWithPC pc f term k = commonGuardedApplykWithPC pc f term merge k
    let guardedApplyWithPC pc f term = guardedApplykWithPC pc (Cps.ret f) term id

    let unguard = function
        | IteT iteType -> iteType
        | t -> {ite = []; elseValue = t}
    let unguardGvs = function
        | GvsT gvs -> gvs
        | t -> [(True(), t)]
    let unguardMerge = unguard >> merge

// ----------------------------------------------------------------------------------------------------

    let genericSimplify gvs : (term * 'a) list =
        let folder acc (g, _ as gv) k =
            match g with
            | True -> List.singleton gv
            | False -> k acc
            | _ -> k (gv :: acc)
        Cps.List.foldlk folder gvs List.empty id

    let rec private genericGuardedCartesianProductRec mapper gacc xsacc = function
        | x::xs ->
            mapper x
            |> List.collect (fun (g, v) ->
                genericGuardedCartesianProductRec mapper (gacc &&& g) (List.append xsacc [v]) xs)
            |> genericSimplify
        | [] -> [(gacc, xsacc)]

    let genericGuardedCartesianProduct mapper xs =
        genericGuardedCartesianProductRec mapper (True()) [] xs

    let rec private guardedCartesianProductRecK mapper ctor gacc xsacc terms k =
        match terms with
        | x::xs ->
            let cartesian (g, v) k =
                let g' = gacc &&& g
                guardedCartesianProductRecK mapper ctor g' (xsacc @ [v]) xs k
            mapper x (fun iteType ->
            Cps.List.mapk cartesian iteType.ite (fun iteCartesian ->
            cartesian (True(), iteType.elseValue) (fun e' ->
            (List.concat iteCartesian) @ e' |> k)))
        | [] -> List.singleton (gacc, ctor xsacc) |> k

    let guardedCartesianProductK mapper terms ctor k =
        guardedCartesianProductRecK mapper ctor (True()) [] terms k
