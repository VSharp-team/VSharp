namespace VSharp.Core.Horn

open Microsoft
open System.Collections.Generic

type 'a AssertionElement = Just of Z3.BoolExpr | Fork of 'a * 'a

/// <summary>
/// Convenient immutable storage for boolean assertions about code constructions.
/// All passed assertions are combined with logical "and" connective.
/// Provides conveniences for scopes: when the scope is met Saved() may be called,
/// Restored() unrolls assertions to latest Saved() point.
/// </summary>
type Assertions private(assertions) =
    member public this.With(expr : Z3.BoolExpr) = Assertions(Just expr :: assertions)
    member public this.Fork(branch1 : Assertions, branch2 : Assertions) = Assertions(Fork(branch1, branch2) :: assertions)

    member public this.Print(ctx : Z3.Context) = this.PrintImpl(ctx, [])

    member public this.PrintImpl(ctx : Z3.Context, appendix) =
        let rec cutAndMap mapper list firstResult =
            match list with
            | [] -> (firstResult, [])
            | x::xs ->
                let mappedX = mapper x
                if mappedX <> null then cutAndMap mapper xs (mappedX::firstResult)
                else (firstResult, list)

        let toSimple elem =
            match elem with
            | Just expr -> expr
            | Fork (_, _) -> null

        let cutted = cutAndMap toSimple (List.rev assertions) []
        let linear = fst cutted
        let forked = snd cutted
        let fork =
            match forked with
            | (Fork (branch1, branch2))::xs ->
                let newAppendix = List.append xs appendix
                ctx.MkOr(branch1.PrintImpl(ctx, newAppendix), branch2.PrintImpl(ctx, newAppendix))
            | _ -> ctx.MkTrue()

        ctx.MkAnd(List.append linear (List.singleton fork) |> List.toArray)

    override this.ToString() = List.foldBack (fun assertion acc -> acc + assertion.ToString()) assertions ""

    new() = Assertions([])
