namespace VSharp.Core

open VSharp
open Propositional
open TypeUtils

module internal Merging =

    let guardOf term =
        match term.term with
        | GuardedValues(gs, _) -> disjunction gs
        | _ -> True()

    let private boolMerge (iteType : iteType) =
        let gvs = iteType.ToDisjunctiveGvs()
        List.fold (fun acc (g, v) -> acc ||| (g &&& v)) (False()) gvs

    let rec private structMerge typ iteType =
        let branchesFs = iteType.branches |> List.map (fun (g, v) -> (g, fieldsOf v))
        let e = iteType.elseValue |> fieldsOf
        let union branches e = {branches = branches; elseValue = e}
        let mergedFields = PersistentDict.merge branchesFs e union merge
        Struct mergedFields typ

    and private simplify iteType =
        let folder gv (accBranches, accElse) =
            match gv with
            | True, IteT ite -> let simplified = simplify ite in simplified.branches, simplified.elseValue
            | True, t -> [], t
            | False, _ -> accBranches, accElse
            | g, IteT ite ->
                let guarded = List.map (fun (g', v) -> (g &&& g', v)) ite.branches
                let simplified = simplify {ite with branches = guarded}
                simplified.branches @ (g, simplified.elseValue)::accBranches, accElse
            | gv -> gv::accBranches, accElse
        let accBranches, accElse =
            match iteType.elseValue with
            | IteT t ->
                let simplified = simplify t
                simplified.branches, simplified.elseValue
            | _ -> [], iteType.elseValue
        let ite, e = List.foldBack folder iteType.branches (accBranches, accElse)
        {branches = ite; elseValue = e}

    and private compress = function
        // | [(_, v1); (_, v2)] as gvs when typeOf v1 = typeOf v2 -> typedMerge (typeOf v1) gvs
        // | [_; _] as gvs -> gvs
        | {branches = []; elseValue = e} -> e
        | {branches = _; elseValue =  {term = Struct(_, t)}} as iteType -> structMerge t iteType
        | {branches = _; elseValue = v} as iteType when isBool v -> boolMerge iteType
        | iteType -> Ite iteType

    and merge ite : term =
        match compress (simplify ite) with
        | IteT {branches = []; elseValue = e} -> e
        | IteT {branches = branches; elseValue = e} when List.forall (fun (g, v) -> v = e) branches -> e
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
        | _ -> merge {branches = [(g, u)]; elseValue = v}

// ------------------------------------ Mapping non-term sequences ------------------------------------

    let commonGuardedMapk mapper iteType merge k =
        Cps.List.mapk (fun (g, v) k -> mapper v (fun t -> k (g, t))) iteType.branches (fun branches' ->
        mapper iteType.elseValue (fun e' ->
        {branches = branches'; elseValue = e';} |> merge |> k))

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
        let chooser (g, v) k =
            let pc' = PC.add pc g
            if PC.isFalse pc' then k None
            else mapper v (fun t -> k (Some (g, t)))
        Cps.List.choosek chooser iteType.branches (fun branches' ->
        mapper iteType.elseValue (fun e' ->
        {branches = branches'; elseValue = e'} |> merge |> k))

    let commonGuardedApplykWithPC pc f term merge k =
        match term.term with
        | Ite iteType -> commonGuardedMapkWithPC pc f iteType merge k
        | _ -> f term k
    let guardedApplykWithPC pc f term k = commonGuardedApplykWithPC pc f term merge k
    let guardedApplyWithPC pc f term = guardedApplykWithPC pc (Cps.ret f) term id

    let unguard = function
        | IteT iteType -> iteType
        | t -> {branches = []; elseValue = t}
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
            Cps.List.mapk cartesian iteType.branches (fun branchesCartesian ->
            cartesian (True(), iteType.elseValue) (fun e' ->
            (List.concat branchesCartesian) @ e' |> k)))
        | [] -> List.singleton (gacc, ctor xsacc) |> k

    let guardedCartesianProductK mapper terms ctor k =
        guardedCartesianProductRecK mapper ctor (True()) [] terms k
