namespace VSharp.Core.Symbolic.Reduction

open VSharp.Core.Symbolic
open VSharp.Core.Symbolic.Terms

module internal Merging =

    let private __notImplemented__() = raise (new System.NotImplementedException())

    let private mergePrimitive b u v t =
        match t with
        | Bool
        | Numeric _
        | String -> __notImplemented__()
        | _ -> raise(new System.NotImplementedException())

    let private mergeOneUnion b u v =
        

    let internal conditionState thenState elseState merge condition thenValue elseValue =
        match condition, thenValue, elseValue with
        | True, _, _ -> thenValue
        | False, _, _ -> elseValue
        | _, _, _ when thenValue = elseValue -> thenValue
        | _, _, _ ->
            let u, v = thenValue, elseValue
            let ut, vt = TypeOf u, TypeOf v
            match u, v with
            | _, _ when Types.IsPrimitiveSolvable ut && ut = vt -> mergePrimitive condition u v ut
            // TODO: here follow lists with equal length
            | Union us, _ when not (IsUnion v) -> mergeOneUnion condition us v
            | _, Union vs when not (IsUnion u) -> mergeOneUnion (Negate condition) vs u
            | _ -> __notImplemented__()
