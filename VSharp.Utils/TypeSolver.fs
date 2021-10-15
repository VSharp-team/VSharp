namespace VSharp

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections

type typeConstraints = { supertypes : Type list; subtypes : Type list; notSubtypes : Type list; notSupertypes : Type list}

module TypeSolver =
    let cache = Dictionary<typeConstraints, Type option>()

    let rec private findNonAbstractSupertype predicate (typ : Type) =
        if typ = null then None
        elif not typ.IsAbstract && predicate typ then Some typ
        else findNonAbstractSupertype predicate typ.BaseType

    let rec private findNonAbstractType predicate (assemblies : Assembly seq) =
        assemblies |> Seq.tryPick (fun assembly ->
            assembly.GetExportedTypes() |> Seq.tryPick (fun typ ->
                if (not typ.IsAbstract) && (predicate typ) then Some typ
                else None))

//    let rec private constraintsOfTypeVariable (tv : Type) =
//        assert tv.IsGenericTypeParameter
//        let supertypes = tv.GetGenericParameterConstraints() |> List.ofArray
//        let supertypes = if tv.IsValueType then typeof<ValueType> :: supertypes else supertypes
//        let notSupertypes = if tv.IsClass then [typeof<ValueType>] else []
//        // TODO: validate unmanaged and new() constraints
//        let constraints = { supertypes = supertypes; subtypes = []; notSupertypes = notSupertypes; notSubtypes = [] }
//        satisfy constraints

    and private satisfyCandidate (constraints : typeConstraints) subst validateSubst (candidate : Type) =
        // TODO: need to find subst to generic parameters satisfying constraints
        constraints.subtypes |> List.forall candidate.IsAssignableFrom &&
        constraints.supertypes |> List.forall candidate.IsAssignableTo  &&
        constraints.notSubtypes |> List.forall (candidate.IsAssignableFrom >> not) &&
        constraints.notSupertypes |> List.forall (candidate.IsAssignableTo >> not)

    and private satisfy constraints subst validate =
        let mutable subst = subst
        Dict.getValueOrUpdate cache constraints (fun () ->
            let checkCandidate typ =
                satisfyCandidate constraints subst (fun _ -> true) typ
            match constraints.subtypes with
            | [] -> findNonAbstractType checkCandidate (AssemblyManager.assemblies())
            | t :: _ -> t |> findNonAbstractSupertype checkCandidate)
        |> Option.map (withSnd subst)

    let solve (constraintsList : typeConstraints list) =
        let rec solveRec subst = function
            | [] -> Some ([], subst)
            | constraints::tail ->
                match satisfy constraints subst (fun _ -> true) with
                | Some (t, subst) ->
                    match solveRec subst tail with
                    | None -> None
                    | Some(ts, subst) -> Some (t::ts, subst)
                | None -> None
        solveRec PersistentDict.empty constraintsList

    let reset() =
        cache.Clear()
