namespace VSharp

open JetBrains.Decompiler.Ast
open Microsoft.Z3
open System.Collections.Generic

module internal Z3 =

    let private ctx = new Context()
    let private solver = ctx.MkSolver()
    let private sorts = new Dictionary<VSharp.TermType, Microsoft.Z3.Sort>()

// ------------------------------- Cache -------------------------------

    type EncodingCache = { e2t : IDictionary<Expr, Term>; t2e : IDictionary<Term, Expr> }

    let private freshCache () = {e2t = new Dictionary<Expr, Term>(); t2e = new Dictionary<Term, Expr>()}

// ------------------------------- Encoding -------------------------------

    let type2Sort typ =
        Dict.getValueOrUpdate sorts typ (fun () ->
            match typ with
            | Bool -> ctx.MkBoolSort() :> Sort
            | Numeric _ as t when Types.IsInteger t -> ctx.MkIntSort() :> Sort
            | Numeric _ as t when Types.IsReal t -> ctx.MkRealSort() :> Sort
            | Numeric t -> ctx.MkEnumSort(t.FullName, t.GetEnumNames()) :> Sort
            | String
            | ArrayType _
            | Func _
            | Void
            | Bottom
            | VSharp.Null
            | StructType _
            | ClassType _
            | SubType _
            | PointerType _ -> __notImplemented__())

    let encodeConcrete (obj : obj) typ =
        match typ with
        | Bool -> ctx.MkBool(obj :?> bool) :> Expr
        | Numeric t when t = typeof<char> -> ctx.MkNumeral(obj :?> int |> toString, type2Sort typ)
        | Numeric t when t.IsEnum ->
            let sort = type2Sort typ :?> EnumSort in
            let name = obj.ToString() in
            FSharp.Collections.Array.find (toString >> ((=) name)) sort.Consts
        | Numeric _ as t when Types.IsInteger t -> ctx.MkNumeral(obj.ToString(), type2Sort typ)
        | _ -> __notImplemented__()

    let encodeConstant (cache : EncodingCache) (name : string) typ term =
        Dict.tryGetValue2 cache.t2e term (fun () ->
            let result = ctx.MkConst(name, type2Sort typ) in
            cache.e2t.[result] <- term
            cache.t2e.[term] <- result
            result)

    let rec encodeExpression cache stopper term op args typ =
        Dict.tryGetValue2 cache.t2e term (fun () ->
            match op with
            | Operator(operator, _) ->
                if stopper operator args then
                    let name = IdGenerator.startingWith "%tmp" in
                    encodeConstant cache name typ term
                else
                    match operator with
                    | OperationType.LogicalNeg -> makeUnary cache stopper ctx.MkNot args :> Expr
                    | OperationType.LogicalAnd -> ctx.MkAnd(encodeTerms cache stopper args) :> Expr
                    | OperationType.LogicalOr -> ctx.MkOr(encodeTerms cache stopper args) :> Expr
                    | OperationType.Equal -> makeBinary cache stopper ctx.MkEq args :> Expr
                    | OperationType.Greater -> makeBinary cache stopper ctx.MkGt args :> Expr
                    | OperationType.GreaterOrEqual -> makeBinary cache stopper ctx.MkGe args :> Expr
                    | OperationType.Less -> makeBinary cache stopper ctx.MkLt args :> Expr
                    | OperationType.LessOrEqual -> makeBinary cache stopper ctx.MkLe args :> Expr
                    | OperationType.Add -> ctx.MkAdd(encodeTerms cache stopper args) :> Expr
                    | OperationType.Multiply -> ctx.MkMul(encodeTerms cache stopper args) :> Expr
                    | OperationType.Subtract -> ctx.MkSub(encodeTerms cache stopper args) :> Expr
                    | OperationType.Divide -> makeBinary cache stopper ctx.MkDiv args :> Expr
                    | OperationType.Remainder -> makeBinary cache stopper ctx.MkRem args :> Expr
                    | OperationType.UnaryMinus -> makeUnary cache stopper ctx.MkUnaryMinus args :> Expr
                    | OperationType.Not -> makeUnary cache stopper ctx.MkNot args :> Expr
                    | OperationType.ShiftLeft
                    | OperationType.ShiftRight -> __notImplemented__()
                    | _ -> __notImplemented__()
            | Application id ->
                let decl =
                    match id with
                    | MetadataMethodIdentifier mm -> __notImplemented__()
                    | DelegateIdentifier _ -> __notImplemented__()
                    | StandardFunctionIdentifier sf -> ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, type2Sort typ)
                in ctx.MkApp(decl, encodeTerms cache stopper args)
            | Cast _ ->
                __notImplemented__())

    and makeUnary<'a, 'b when 'a :> Expr and 'b :> Expr>
            (cache : EncodingCache)
            (stopper : OperationType -> Term list -> bool)
            (constructor : 'a -> 'b)
            (args : Term list) : 'b =
        match args with
        | [x] -> constructor (encodeTermExt<'a> cache stopper x)
        | _ -> internalfail "unary operation should have exactly one argument"

    and makeBinary<'a, 'b, 'c when 'a :> Expr and 'b :> Expr and 'c :> Expr>
            (cache : EncodingCache)
            (stopper : OperationType -> Term list -> bool)
            (constructor : 'a * 'b -> 'c)
            (args : Term list) : 'c =
        match args with
        | [x; y] -> constructor(encodeTermExt<'a> cache stopper x, encodeTermExt<'b> cache stopper y)
        | _ -> internalfail "binary operation should have exactly two arguments"

    and encodeTerms<'a when 'a :> Expr> (cache : EncodingCache) (stopper : OperationType -> Term list -> bool) (ts : Term seq) : 'a array =
        ts |> Seq.map (encodeTermExt<'a> cache stopper) |> FSharp.Collections.Array.ofSeq

    and encodeTermExt<'a when 'a :> Expr> (cache : EncodingCache) (stopper : OperationType -> Term list -> bool) (t : Term) : 'a =
        match t.term with
        | Concrete(obj, typ) -> encodeConcrete obj typ :?> 'a
        | Constant(name, source, typ) -> encodeConstant cache name typ t :?> 'a
        | Expression(op, args, typ) -> encodeExpression cache stopper t op args typ :?> 'a
        | _ -> __notImplemented__()

    let encodeTerm t =
        let cache = freshCache() in
        (encodeTermExt cache (fun _ _ -> false) t :> AST, cache)


// ------------------------------- Decoding -------------------------------

    let rec decodeExpr cache op t mtd (expr : Expr) =
        Expression (Operator(op, false)) (expr.Args |> Seq.map (decode cache) |> List.ofSeq) t mtd

    and decodeBoolExpr cache op mtd (expr : BoolExpr) =
        decodeExpr cache op Bool mtd expr

    and decode (cache : EncodingCache) (expr : Expr) =
        if cache.e2t.ContainsKey(expr) then cache.e2t.[expr]
        else
            match expr with
            | :? IntNum as i -> Terms.Concrete i.Int (Numeric typeof<int>) Metadata.empty
            | :? RatNum as r -> Terms.Concrete (double(r.Numerator.Int) * 1.0 / double(r.Denominator.Int)) (Numeric typeof<int>) Metadata.empty
            | :? BoolExpr as b ->
                let mtd = Metadata.empty in
                if b.IsTrue then MakeTrue mtd
                elif b.IsFalse then MakeFalse mtd
                elif b.IsNot then decodeBoolExpr cache OperationType.LogicalNeg mtd b
                elif b.IsAnd then decodeBoolExpr cache OperationType.LogicalAnd mtd b
                elif b.IsOr then decodeBoolExpr cache OperationType.LogicalOr mtd b
                elif b.IsEq then decodeBoolExpr cache OperationType.Equal mtd b
                elif b.IsGT then decodeBoolExpr cache OperationType.Greater mtd b
                elif b.IsGE then decodeBoolExpr cache OperationType.GreaterOrEqual mtd b
                elif b.IsLT then decodeBoolExpr cache OperationType.Less mtd b
                elif b.IsLE then decodeBoolExpr cache OperationType.LessOrEqual mtd b
                else __notImplemented__()
            | _ ->
                __notImplemented__()


// ------------------------------- Solving, etc. -------------------------------

    let solve expr =
        let result = solver.Check(expr) in
        match result with
        | Status.SATISFIABLE -> SmtSat solver.Model
        | Status.UNSATISFIABLE -> SmtUnsat
        | Status.UNKNOWN -> SmtUnknown solver.ReasonUnknown
        | _ -> __unreachable__()

    let simplifyPropositional t =
        let cache = freshCache() in
        let stopper op args =
            match op with
            | OperationType.LogicalNeg
            | OperationType.LogicalAnd
            | OperationType.LogicalOr
            | OperationType.Equal when Seq.forall IsBool args ->
                false
            | _ -> true
        in
        let encoded = encodeTermExt cache stopper t in
        let simple = encoded.Simplify() in
        let result = decode cache simple
        printfn "SIMPLIFICATION of %O   GAVE   %O" t result
        printfn "ON SMT level encodings are %O    AND     %O" encoded simple
        result
