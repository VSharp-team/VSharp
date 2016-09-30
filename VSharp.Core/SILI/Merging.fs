namespace VSharp.Core.Symbolic

open VSharp.Core.Symbolic
open VSharp.Core.Symbolic.Terms
open VSharp.Core.Symbolic.Propositional

module internal Merging =

    let private __notImplemented__() = raise (new System.NotImplementedException())

// ------------------------------- Typed merging -------------------------------

    let private mergeable u v =
        let ut, vt = TypeOf u, TypeOf v
        u = v || Types.IsPrimitive ut && ut = vt || false //TODO: lists of equal length go here

    let private mergePrimitive b u v t =
        match t with
        | Bool -> b &&& u ||| !!b &&& v
        | Numeric _
        | String -> ite b u v
        | _ -> raise(new System.NotImplementedException())

    let private mergeSameType b u v =
        let ut, vt = TypeOf u, TypeOf v
        match u, v with
        | _, _ when u = v -> u
        | _, _ when Types.IsPrimitive ut && ut = vt -> mergePrimitive b u v ut
        // TODO: here should follow lists with equal lengths
        | _ -> __notImplemented__()

// ------------------------------- Union merging -------------------------------

    let private mergeOneUnion b us v =
        let equal, rest = us |> List.partition (snd >> mergeable v)
        let uPrime = Unions.guardWith b rest
        let vPrime =
            match equal with
            | [] -> !!b, v
            | [(g, u)] -> !!b ||| g, mergeSameType b u v
            | _ -> raise(new System.InvalidOperationException("Every union should have at most one element of each category"))
        vPrime :: uPrime

    let private mergeWithDifferentGuards b (gu, u) (gv, v) =
        (b &&& gu ||| !!b &&& gv, mergeSameType b u v)

    let private mergeBothUnions b us vs =
        let hasMergeableWith xs y = List.exists (snd >> mergeable (snd y)) xs
        let findMergeableWith xs y = List.pick (fun x -> if mergeable (snd x) (snd y) then Some x else None) xs
        let findAndMerge ys x = findMergeableWith ys x |> mergeWithDifferentGuards b x

        let uEqual, uPrime = us |> List.partition (hasMergeableWith vs)
        let vEqual, vPrime = vs |> List.partition (hasMergeableWith us)

        let w = uEqual |> List.map (findAndMerge vEqual)
        List.append uPrime vPrime |> List.append w

// ------------------------------- General functions -------------------------------

    let private mergeTerms b u v =
        match b, u, v with
        | _, _, _ when u = v -> u
        | True, _, _ -> u
        | False, _, _ -> v
        | _, _, _ when mergeable u v -> mergeSameType b u v
        | _, Union us, _ when not (IsUnion v) -> mergeOneUnion b us v |> Unions.make
        | _, _, Union vs when not (IsUnion u) -> mergeOneUnion !!b vs u |> Unions.make
        | _, Union us, Union vs -> mergeBothUnions b us vs |> Unions.make
        | _ -> __notImplemented__()

    let private mergeStates condition baseState state1 state2 =
        baseState
            |> State.map (fun id _ -> mergeTerms condition (State.eval state1 id) (State.eval state2 id))
            |> State.withAssertions (State.uniteAssertions (State.assertions state1) (State.assertions state2))
            // Do we need to merge rest objects from then and else branches? Seems like no, but we'll see.

    let internal merge condition thenValue elseValue conditionState thenState elseState  =
        (mergeTerms condition thenValue elseValue, mergeStates condition conditionState thenState elseState)
