namespace VSharp

open System
open System.Reflection
open System.Collections.Generic
open FSharpx.Collections

type typeConstraints = { supertypes : Type list; subtypes : Type list; notSubtypes : Type list; notSupertypes : Type list}
type typeSolvingResult =
    | TypeSat of Type list * Type list
    | TypeUnsat
    | TypesOfInputsUnknown
    | TypeVariablesUnknown

module TypeSolver =
    type private 'a searchResult =
        | Found of 'a
        | NotFound
        | NotExists

    type private typeSearchResult = Type list searchResult

    type private substitution = pdict<Type, Type>

    let private typeVarsCache = Dictionary<Type * substitution, substitution searchResult>()
    let private inputsCache = Dictionary<typeConstraints * substitution, typeSearchResult>()

    let private findNonAbstractSupertype predicate (typ : Type) =
        let rec findRec predicate (typ : Type) sure =
            if typ = null then
                if sure then NotExists else NotFound
            else
                match if typ.IsAbstract then NotExists else predicate typ with
                | Found ts -> Found ts
                | NotExists -> findRec predicate typ.BaseType sure
                | NotFound -> findRec predicate typ.BaseType false
        findRec predicate typ true

    let private findType (supertypes : Type list) searchRest (assemblies : Assembly seq) =
        let mutable sure = true
        let mutable result = NotExists
        let predicate t =
            match searchRest t with
            | Found res -> result <- Found res; true
            | NotExists -> false
            | NotFound -> sure <- false; false
        if Seq.exists predicate supertypes then result
        else
            let assemblies =
                match supertypes |> Seq.tryFind (fun u -> u.IsNotPublic) with
                | Some u -> [u.Assembly] :> _ seq
                | None -> sure <- false; assemblies
            let found = assemblies |> Seq.exists (fun assembly -> assembly.GetExportedTypes() |> Seq.exists predicate)
            if found then result
            elif sure then NotExists
            else NotFound

    let private findNonAbstractType supertypes searchRest (assemblies : Assembly seq) =
        findType supertypes (fun t -> if t.IsAbstract then NotExists else searchRest t) assemblies

    let private isContradicting (c : typeConstraints) =
        // X <: u and u <: t and X </: t
        c.supertypes |> List.exists (fun u -> c.notSupertypes |> List.exists u.IsAssignableTo)
        || // u <: X and t <: u and t </: X
        c.subtypes |> List.exists (fun u -> c.notSubtypes |> List.exists u.IsAssignableFrom)
        || // u <: X and X <: t and u </: t
        c.subtypes |> List.exists (fun u -> c.supertypes |> List.exists (u.IsAssignableTo >> not))
        || // No multiple inheritance -- X <: u and X <: t and u </: t and t </: u and t, u are classes
        c.supertypes |> List.exists (fun u -> c.supertypes |> List.exists (fun t -> u.IsClass && t.IsClass && (not <| u.IsAssignableTo t) && (not <| u.IsAssignableFrom t)))

    let rec private substitute (subst : substitution) (t : Type) =
        if not t.IsGenericType then t
        elif t.IsGenericParameter then
            if PersistentDict.contains t subst then subst.[t] else t
//        elif t.HasElementType then
//            let e' = t.GetElementType() |> substPartially subst
        else
            let args = t.GetGenericArguments()
            let args' = args |> Array.map (substitute subst)
            if Array.forall2 (=) args args' then t
            else t.GetGenericTypeDefinition().MakeGenericType(args')

    let private satisfiesTypeParameterConstraints (parameter : Type) subst (t : Type) =
        let isReferenceType = parameter.GenericParameterAttributes &&& GenericParameterAttributes.ReferenceTypeConstraint = GenericParameterAttributes.ReferenceTypeConstraint
        let isNotNullableValueType = parameter.GenericParameterAttributes &&& GenericParameterAttributes.NotNullableValueTypeConstraint = GenericParameterAttributes.NotNullableValueTypeConstraint
        let hasDefaultConstructor = parameter.GenericParameterAttributes &&& GenericParameterAttributes.DefaultConstructorConstraint = GenericParameterAttributes.NotNullableValueTypeConstraint
        // TODO: check 'managed' constraint
        (not isReferenceType || not t.IsValueType) &&
        (not isNotNullableValueType || (t.IsValueType && Nullable.GetUnderlyingType t = null)) &&
        (not hasDefaultConstructor || t.GetConstructor(Type.EmptyTypes) <> null) &&
        (parameter.GetGenericParameterConstraints() |> Array.forall (substitute subst >> t.IsAssignableTo))

    let private satisfiesConstraints (constraints : typeConstraints) subst (candidate : Type) =
        // TODO: need to find subst to generic parameters satisfying constraints
        constraints.subtypes |> List.forall (substitute subst >> candidate.IsAssignableFrom) &&
        constraints.supertypes |> List.forall (substitute subst >> candidate.IsAssignableTo) &&
        constraints.notSubtypes |> List.forall (substitute subst >> candidate.IsAssignableFrom >> not) &&
        constraints.notSupertypes |> List.forall (substitute subst >> candidate.IsAssignableTo >> not)

    let private satisfyInput constraints subst validate =
//        match PersistentDict.tryFind inputsCache (constraints, subst) with
//        | Some t when validate t -> Found t
//        | _ ->
            let validate typ =
                if satisfiesConstraints constraints subst typ then
                    validate typ
                else NotExists
            match constraints.subtypes with
            | [] -> findNonAbstractType constraints.supertypes validate (AssemblyManager.assemblies())
            | t :: _ -> findNonAbstractSupertype validate t//)

    let private satisfyTypeParameter parameter subst validate =
//        Dict.getValueOrUpdate typeVarsCache (parameter, subst) (fun () ->
            let validate typ =
                if satisfiesTypeParameterConstraints parameter subst typ then
                    validate typ
                else NotExists
            let supertypes = parameter.GetGenericParameterConstraints() |> Array.map (substitute subst) |> List.ofArray
            findType supertypes validate (AssemblyManager.assemblies())//)

    let rec private collectTypeVariables (acc : Type list) (typ : Type) =
        if typ.IsGenericParameter then
            if List.contains typ acc then acc
            else
                typ.GetGenericParameterConstraints() |> Array.fold collectTypeVariables (typ::acc)
        elif typ.HasElementType then
            collectTypeVariables acc (typ.GetElementType())
        elif not typ.IsGenericType then acc
        else
            typ.GetGenericArguments() |> Array.fold collectTypeVariables acc

    let solve (inputConstraintsList : typeConstraints list) (typeParameters : Type list) =
        if inputConstraintsList |> List.exists isContradicting then TypeUnsat
        else
            let typeVars = typeParameters |> List.fold collectTypeVariables []
            let typeVars = inputConstraintsList |> List.fold (fun acc constraints ->
                            let acc = constraints.supertypes |> List.fold collectTypeVariables acc
                            let acc = constraints.subtypes |> List.fold collectTypeVariables acc
                            let acc = constraints.notSupertypes |> List.fold collectTypeVariables acc
                            let acc = constraints.notSubtypes |> List.fold collectTypeVariables acc
                            acc) typeVars
            let decodeTypeSubst (subst : substitution) =
                 List.map (Reflection.concretizeType (fun t -> if subst.impl.ContainsKey t then subst.impl.[t] else t)) typeParameters
            let mutable inputs = []
            let mutable typeVariablesUnknown = true
            let rec solveInputsRec acc subst = function
                | [] -> Found (List.rev acc)
                | constraints::rest ->
                    satisfyInput constraints subst (fun t -> solveInputsRec (t::acc) subst rest) // TODO: for ref <: ref constraints we should also accumulate t into another subst
            let rec solveTypesVarsRec subst = function
                | [] ->
                    typeVariablesUnknown <- false
                    match solveInputsRec [] subst inputConstraintsList with
                    | Found ts -> inputs <- ts; Found subst
                    | NotFound -> NotFound
                    | NotExists -> NotExists
                | t::ts ->
                    satisfyTypeParameter t subst (fun u -> solveTypesVarsRec (PersistentDict.add t u subst) ts)
            match solveTypesVarsRec PersistentDict.empty typeVars with
            | Found subst -> TypeSat(inputs, decodeTypeSubst subst)
            | NotExists -> TypeUnsat
            | NotFound ->
                if typeVariablesUnknown then TypeVariablesUnknown
                else TypesOfInputsUnknown


    let reset() =
        typeVarsCache.Clear();
        inputsCache.Clear()
