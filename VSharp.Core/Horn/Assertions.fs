namespace VSharp.Core.Horn

open Microsoft
open System.Collections.Generic

/// <summary>
/// Convenient immutable storage for boolean assertions about code constructions.
/// All passed assertions are combined with logical "and" connective.
/// Provides conveniences for scopes: when the scope is met Saved() may be called,
/// Restored() unrolls assertions to latest Saved() point.
/// </summary>
type Assertions private(assertions) =
    let notNull (x : Z3.BoolExpr) = x <> null
    let print obj = (if obj = null then "---<new scope>---" else obj.ToString()) + "\n"
    let beautifulSeparator = "=================================================================\n"
    let separate str = beautifulSeparator + str + beautifulSeparator

    member public this.With(expr : Z3.BoolExpr) = Assertions(expr :: assertions) 
    member public this.Saved() = Assertions(null :: assertions)
    member public this.Restored() = Assertions(List.skipWhile notNull assertions |> List.tail)

    member public this.Combined(ctx : Z3.Context) = ctx.MkAnd(List.filter notNull assertions |> List.toArray)
    override this.ToString() = List.fold (fun acc assertion -> acc + (print assertion)) "" assertions |> separate

    new() = Assertions([null])
