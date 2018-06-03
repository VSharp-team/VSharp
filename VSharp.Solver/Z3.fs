namespace VSharp

open JetBrains.Decompiler.Ast
open Microsoft.Z3
open System.Collections.Generic
open VSharp.Core
open Logger

module internal Z3 =

    let private ctx = new Context(Dictionary<string, string>(dict[ ("model", "true")])) // TODO: ctx should be disposed!
    let private solver = ctx.MkSolver()
    let private fp = ctx.MkFixedpoint()
    let private sorts = new Dictionary<termType, Microsoft.Z3.Sort>()

// ------------------------------- Cache -------------------------------

    type EncodingCache =
        { e2t : IDictionary<Expr, term>; t2e : IDictionary<term, Expr> }
        member x.Get term encoder =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = encoder()
                x.e2t.[result] <- term
                x.t2e.[term] <- result
                result)


    let private freshCache () = {e2t = new Dictionary<Expr, term>(); t2e = new Dictionary<term, Expr>()}

// ------------------------------- Encoding -------------------------------

    let validateId id =
        assert(not <| System.String.IsNullOrWhiteSpace id)
        if System.Char.IsDigit id.[0] then "_" + id else id

    let type2Sort typ =
        Dict.getValueOrUpdate sorts typ (fun () ->
            match typ with
            | Bool -> ctx.MkBoolSort() :> Sort
            | Numeric _ as t when Types.IsInteger t -> ctx.MkIntSort() :> Sort
            | Numeric _ as t when Types.IsReal t -> ctx.MkRealSort() :> Sort
            | Numeric t -> ctx.MkEnumSort(t.FullName, t.GetEnumNames()) :> Sort
            | ArrayType _
            | Func _
            | Void
            | Bottom
            | Null
            | StructType _
            | ClassType _
            | InterfaceType _
            | TypeVariable _
            | Reference _
            | Pointer _ -> __notImplemented__())

    let encodeConcrete (obj : obj) typ =
        match typ with
        | Bool -> ctx.MkBool(obj :?> bool) :> Expr
        | Numeric t when t = typeof<char> -> ctx.MkNumeral(System.Convert.ToInt32(obj :?> char) |> toString, type2Sort typ)
        | Numeric t when t.IsEnum ->
            let sort = type2Sort typ :?> EnumSort in
            let name = obj.ToString() in
            FSharp.Collections.Array.find (toString >> ((=) name)) sort.Consts
        | Numeric _ ->
            match obj with
            | :? concreteHeapAddress as addr ->
                match addr with
                | [addr] -> ctx.MkNumeral(addr.ToString(), type2Sort typ)
                | _ -> __notImplemented__()
            | _ -> ctx.MkNumeral(obj.ToString(), type2Sort typ)
        | _ -> __notImplemented__()

    let encodeConstantSimple (cache : EncodingCache) name typ term =
        cache.Get term (fun () -> ctx.MkConst(validateId name, type2Sort typ))

    let encodeConstant (cache : EncodingCache) name (source : ISymbolicConstantSource) typ term =
        match source with
        | LazyInstantiation(location, heap, _) ->
            match heap with
            | None -> encodeConstantSimple cache name typ term
            | Some heap ->
                __notImplemented__()
        | RecursionOutcome(id, state, location, _) ->
            __notImplemented__()
        | _ -> encodeConstantSimple cache name typ term

    let rec encodeExpression (cache : EncodingCache) stopper term op args typ =
        cache.Get term (fun () ->
            match op with
            | Operator(operator, _) ->
                if stopper operator args then
                    let name = IdGenerator.startingWith "%tmp"
                    encodeConstantSimple cache name typ term
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
                    | :? IMethodIdentifier -> __notImplemented__()
                    | :? IDelegateIdentifier -> __notImplemented__()
                    | :? StandardFunctionIdentifier as sf -> ctx.MkConstDecl(sf.Function |> toString |> IdGenerator.startingWith, type2Sort typ)
                    | _ -> __notImplemented__()
                ctx.MkApp(decl, encodeTerms cache stopper args)
            | Cast _ ->
                __notImplemented__())

    and makeUnary<'a, 'b when 'a :> Expr and 'b :> Expr>
            (cache : EncodingCache)
            (stopper : OperationType -> term list -> bool)
            (constructor : 'a -> 'b)
            (args : term list) : 'b =
        match args with
        | [x] -> constructor (encodeTermExt<'a> cache stopper x)
        | _ -> internalfail "unary operation should have exactly one argument"

    and makeBinary<'a, 'b, 'c when 'a :> Expr and 'b :> Expr and 'c :> Expr>
            (cache : EncodingCache)
            (stopper : OperationType -> term list -> bool)
            (constructor : 'a * 'b -> 'c)
            (args : term list) : 'c =
        match args with
        | [x; y] -> constructor(encodeTermExt<'a> cache stopper x, encodeTermExt<'b> cache stopper y)
        | _ -> internalfail "binary operation should have exactly two arguments"

    and encodeTerms<'a when 'a :> Expr> (cache : EncodingCache) (stopper : OperationType -> term list -> bool) (ts : term seq) : 'a array =
        ts |> Seq.map (encodeTermExt<'a> cache stopper) |> FSharp.Collections.Array.ofSeq

    and encodeTermExt<'a when 'a :> Expr> (cache : EncodingCache) (stopper : OperationType -> term list -> bool) (t : term) : 'a =
        match t.term with
        | Concrete(obj, typ) -> encodeConcrete obj typ :?> 'a
        | Constant(name, source, typ) -> encodeConstant cache name.v source typ t :?> 'a
        | Expression(op, args, typ) -> encodeExpression cache stopper t op args typ :?> 'a
        | _ -> __notImplemented__()

    let encodeTerm t =
        printLog Trace "SOLVER: trying to encode %O" t
        let cache = freshCache()
        (encodeTermExt cache (fun _ _ -> false) t :> AST, cache)


// ------------------------------- Decoding -------------------------------

    let rec decodeExpr cache op t (expr : Expr) =
        Expression (Operator(op, false)) (expr.Args |> Seq.map (decode cache) |> List.ofSeq) t

    and decodeBoolExpr cache op (expr : BoolExpr) =
        decodeExpr cache op Bool expr

    and decode (cache : EncodingCache) (expr : Expr) =
        if cache.e2t.ContainsKey(expr) then cache.e2t.[expr]
        else
            match expr with
            | :? IntNum as i -> Concrete i.Int (Numeric typeof<int>)
            | :? RatNum as r -> Concrete (double(r.Numerator.Int) * 1.0 / double(r.Denominator.Int)) (Numeric typeof<int>)
            | :? BoolExpr as b ->
                if b.IsTrue then True
                elif b.IsFalse then False
                elif b.IsNot then decodeBoolExpr cache OperationType.LogicalNeg b
                elif b.IsAnd then decodeBoolExpr cache OperationType.LogicalAnd b
                elif b.IsOr then decodeBoolExpr cache OperationType.LogicalOr b
                elif b.IsEq then decodeBoolExpr cache OperationType.Equal b
                elif b.IsGT then decodeBoolExpr cache OperationType.Greater b
                elif b.IsGE then decodeBoolExpr cache OperationType.GreaterOrEqual b
                elif b.IsLT then decodeBoolExpr cache OperationType.Less b
                elif b.IsLE then decodeBoolExpr cache OperationType.LessOrEqual b
                else __notImplemented__()
            | _ ->
                __notImplemented__()


// ------------------------------- Solving, etc. -------------------------------

    let solve (exprs : AST list) =
        printLog Trace "SOLVER: solving %O" exprs
        try
            exprs |> List.iter (fun expr -> solver.Assert(expr :?> BoolExpr))
            let result = solver.Check()
            printLog Trace "SOLVER: got %O" result
            match result with
            | Status.SATISFIABLE -> SmtSat solver.Model
            | Status.UNSATISFIABLE -> SmtUnsat
            | Status.UNKNOWN -> printLog Trace "SOLVER: reason: %O" solver.ReasonUnknown; SmtUnknown solver.ReasonUnknown
            | _ -> __unreachable__()
        finally
            solver.Reset()

    let simplifyPropositional t =
        let cache = freshCache()
        let stopper op args =
            match op with
            | OperationType.LogicalNeg
            | OperationType.LogicalAnd
            | OperationType.LogicalOr
            | OperationType.Equal when List.forall (TypeOf >> Types.IsBool) args ->
                false
            | _ -> true
        let encoded = encodeTermExt cache stopper t
        let simple = encoded.Simplify()
        let result = decode cache simple
        printLog Trace "SOLVER: simplification of %O   gave   %O" t result
        printLog Trace "SOLVER: on SMT level encodings are %O    and     %O" encoded simple
        result
