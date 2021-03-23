namespace VSharp.Solver

open Microsoft.Z3
open System.Collections.Generic
open VSharp
open VSharp.Core
open Logger

module internal Z3 =

// ------------------------------- Cache -------------------------------

    type private encodingCache = {
        sorts : IDictionary<symbolicType, Sort>
        e2t : IDictionary<Expr, term>
        t2e : IDictionary<term, Expr>
    } with
        member x.Get term encoder =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = encoder()
                x.e2t.[result] <- term
                x.t2e.[term] <- result
                result)

    let private freshCache () = {
        sorts = Dictionary<symbolicType, Sort>()
        e2t = Dictionary<Expr, term>()
        t2e = Dictionary<term, Expr>()
    }

// ------------------------------- Encoding: primitives -------------------------------

    type private Z3Builder(ctx : Context) =
        let cache = freshCache()

        member private x.ValidateId id =
            assert(not <| System.String.IsNullOrWhiteSpace id)
            if System.Char.IsDigit id.[0] then "_" + id else id

        member private x.Type2Sort typ =
            Dict.getValueOrUpdate cache.sorts typ (fun () ->
                match typ with
                | Bool -> ctx.MkBoolSort() :> Sort
                | Numeric(Id t) when t.IsEnum -> ctx.MkIntSort() :> Sort
                | Numeric _ as t when Types.IsInteger t -> ctx.MkIntSort() :> Sort
                | Numeric _ as t when Types.IsReal t -> ctx.MkRealSort() :> Sort
                | Numeric _
                | ArrayType _
                | Void
                | Null
                | StructType _
                | ClassType _
                | InterfaceType _
                | TypeVariable _
                | AddressType
                | ByRef _
                | Pointer _ -> __notImplemented__())

        member private x.EncodeConcrete (obj : obj) typ =
            match typ with
            | Bool -> ctx.MkBool(obj :?> bool) :> Expr
            | Numeric(Id t) when t = typeof<char> -> ctx.MkNumeral(System.Convert.ToInt32(obj :?> char) |> toString, x.Type2Sort typ)
            | Numeric(Id t) ->
                match obj with
                | :? concreteHeapAddress as addr ->
                    match addr with
                    | [addr] -> ctx.MkNumeral(addr.ToString(), x.Type2Sort typ)
                    | _ -> __notImplemented__()
                | _ when TypeUtils.isIntegral (obj.GetType()) -> ctx.MkInt(obj.ToString()) :> Expr
                | _ when t.IsEnum -> ctx.MkInt(System.Convert.ChangeType(obj, t.GetEnumUnderlyingType()).ToString()) :> Expr
                | _ -> ctx.MkNumeral(obj.ToString(), x.Type2Sort typ)
            | _ -> __notImplemented__()

        member private x.EncodeConstant name typ term =
            cache.Get term (fun () -> ctx.MkConst(x.ValidateId name, x.Type2Sort typ))

        member private x.EncodeExpression stopper term op args typ =
            cache.Get term (fun () ->
                match op with
                | Operator operator ->
                    if stopper operator args then
                        let name = IdGenerator.startingWith "%tmp"
                        x.EncodeConstant name typ term
                    else
                        match operator with
                        | OperationType.LogicalNeg -> x.MakeUnary stopper ctx.MkNot args :> Expr
                        | OperationType.LogicalAnd -> ctx.MkAnd(x.EncodeTerms stopper args) :> Expr
                        | OperationType.LogicalOr -> ctx.MkOr(x.EncodeTerms stopper args) :> Expr
                        | OperationType.Equal -> x.MakeBinary stopper ctx.MkEq args :> Expr
                        | OperationType.Greater -> x.MakeBinary stopper ctx.MkGt args :> Expr
                        | OperationType.GreaterOrEqual -> x.MakeBinary stopper ctx.MkGe args :> Expr
                        | OperationType.Less -> x.MakeBinary stopper ctx.MkLt args :> Expr
                        | OperationType.LessOrEqual -> x.MakeBinary stopper ctx.MkLe args :> Expr
                        | OperationType.Add -> ctx.MkAdd(x.EncodeTerms stopper args) :> Expr
                        | OperationType.Multiply -> ctx.MkMul(x.EncodeTerms stopper args) :> Expr
                        | OperationType.Subtract -> ctx.MkSub(x.EncodeTerms stopper args) :> Expr
                        | OperationType.Divide -> x.MakeBinary stopper ctx.MkDiv args :> Expr
                        | OperationType.Remainder -> x.MakeBinary stopper ctx.MkRem args :> Expr
                        | OperationType.UnaryMinus -> x.MakeUnary stopper ctx.MkUnaryMinus args :> Expr
                        | OperationType.Not -> x.MakeUnary stopper ctx.MkNot args :> Expr
                        | OperationType.ShiftLeft
                        | OperationType.ShiftRight -> __notImplemented__()
                        | _ -> __notImplemented__()
                | Application sf ->
                    let decl = ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, x.Type2Sort typ)
                    ctx.MkApp(decl, x.EncodeTerms stopper args)
                | Cast(Numeric _, Numeric _) -> x.EncodeTermExt stopper (List.head args)
                | Cast _ ->
                    __notImplemented__())

        member private this.MakeUnary<'a, 'b when 'a :> Expr and 'b :> Expr>
                (stopper : OperationType -> term list -> bool)
                (constructor : 'a -> 'b)
                (args : term list) : 'b =
            match args with
            | [x] -> constructor (this.EncodeTermExt<'a> stopper x)
            | _ -> internalfail "unary operation should have exactly one argument"

        member private this.MakeBinary<'a, 'b, 'c when 'a :> Expr and 'b :> Expr and 'c :> Expr>
                (stopper : OperationType -> term list -> bool)
                (constructor : 'a * 'b -> 'c)
                (args : term list) : 'c =
            match args with
            | [x; y] -> constructor(this.EncodeTermExt<'a> stopper x, this.EncodeTermExt<'b> stopper y)
            | _ -> internalfail "binary operation should have exactly two arguments"

        member private x.EncodeTerms<'a when 'a :> Expr> (stopper : OperationType -> term list -> bool) (ts : term seq) : 'a array =
            ts |> Seq.map (x.EncodeTermExt<'a> stopper) |> FSharp.Collections.Array.ofSeq

        member private x.EncodeTermExt<'a when 'a :> Expr> (stopper : OperationType -> term list -> bool) (t : term) : 'a =
            match t.term with
            | Concrete(obj, typ) -> x.EncodeConcrete obj typ :?> 'a
            | Constant(name, _, typ) -> x.EncodeConstant name.v typ t :?> 'a
            | Expression(op, args, typ) -> x.EncodeExpression stopper t op args typ :?> 'a
            | _ -> __notImplemented__()

        member public x.EncodeTerm<'a when 'a :> Expr> (t : term) : 'a =
            x.EncodeTermExt<'a> (fun _ _ -> false) t

    // ------------------------------- Decoding -------------------------------

        member private x.DecodeExpr op t (expr : Expr) =
            Expression (Operator op) (expr.Args |> Seq.map x.Decode |> List.ofSeq) t

        member private x.DecodeBoolExpr op (expr : Expr) =
            x.DecodeExpr op Bool expr

        member public x.Decode (expr : Expr) =
            if cache.e2t.ContainsKey(expr) then cache.e2t.[expr]
            else
                match expr with
                | :? IntNum as i -> Concrete i.Int (Numeric (Id typeof<int>))
                | :? RatNum as r -> Concrete (double(r.Numerator.Int) * 1.0 / double(r.Denominator.Int)) (Numeric (Id typeof<int>))
                | _ ->
                    if expr.IsTrue then True
                    elif expr.IsFalse then False
                    elif expr.IsNot then x.DecodeBoolExpr OperationType.LogicalNeg expr
                    elif expr.IsAnd then x.DecodeBoolExpr OperationType.LogicalAnd expr
                    elif expr.IsOr then x.DecodeBoolExpr OperationType.LogicalOr expr
                    elif expr.IsEq then x.DecodeBoolExpr OperationType.Equal expr
                    elif expr.IsGT then x.DecodeBoolExpr OperationType.Greater expr
                    elif expr.IsGE then x.DecodeBoolExpr OperationType.GreaterOrEqual expr
                    elif expr.IsLT then x.DecodeBoolExpr OperationType.Less expr
                    elif expr.IsLE then x.DecodeBoolExpr OperationType.LessOrEqual expr
                    else __notImplemented__()

    let private ctx = new Context()
    let private builder = Z3Builder(ctx)

    type internal Z3Solver() =
        let optCtx = ctx.MkOptimize()
        let levelAtoms = List<BoolExpr>()
        let mutable pathsCount = 0u
        let pathAtoms = Dictionary<level, List<BoolExpr>>()
        let paths = Dictionary<BoolExpr, path>()

        let getLevelAtom (lvl : level) =
            assert (not <| Level.isInf lvl)
            let idx = Level.toInt lvl
            if levelAtoms.Count > idx then
                levelAtoms.[idx]
            else
                let atom = ctx.MkBoolConst(sprintf "lvl!%O" lvl)
                levelAtoms.Insert(idx, atom)
                atom

        let addPath (p : path) =
            let idx = pathsCount
            pathsCount <- pathsCount + 1u
            let atom = ctx.MkBoolConst(sprintf "path!%O!%O" p.lvl idx)
            (Dict.tryGetValue2 pathAtoms p.lvl (fun () -> List<BoolExpr>())).Add atom
            paths.Add(atom, p)
            atom

        let addSoftConstraints lvl =
            let pathAtoms =
                seq {
                    for i in 0 .. min (levelAtoms.Count - 1) (Level.toInt lvl) do
                        if levelAtoms.[i] <> null then
                            yield! __notImplemented__() (* pathAtoms.[uint32(i)] *)
                }
            optCtx.Push()
            let weight = 1u
            let group = null
            pathAtoms |> Seq.iter (fun atom -> optCtx.AssertSoft(atom, weight, group) |> ignore)
            pathAtoms

        let convertZ3Model m =
            // TODO: implement model conversion!
            ignore m
            model()

        interface ISolver with
            member x.CheckSat (q : query) : smtResult =
                printLog Info "SOLVER: trying to solve constraints [level %O]..." q.lvl
                printLogLazy Trace "%s" (lazy(q.queryFml.ToString()))
                let query = builder.EncodeTerm q.queryFml
                let assumptions =
                    seq {
                        for i in 0 .. levelAtoms.Count - 1 do
                            let atom = levelAtoms.[i]
                            if atom <> null then
                                let lit = if i < Level.toInt q.lvl then atom else ctx.MkNot atom
                                yield lit :> Expr
                        yield query :> Expr
                    } |> Array.ofSeq
                let pathAtoms = addSoftConstraints q.lvl
                try
                    try
                        let result = optCtx.Check assumptions
                        printLog Info "SOLVER: got %O" result
                        match result with
                        | Status.SATISFIABLE ->
                            let z3Model = optCtx.Model
                            let model = convertZ3Model z3Model
                            let usedPaths =
                                pathAtoms
                                |> Seq.filter (fun atom -> z3Model.Eval(atom, false).IsTrue)
                                |> Seq.map (fun atom -> paths.[atom])
                            SmtSat { mdl = model; usedPaths = usedPaths }
                        | Status.UNSATISFIABLE -> SmtUnsat { core = unsatCore() }
                        | Status.UNKNOWN ->
                            printLog Trace "SOLVER: reason: %O" <| optCtx.ReasonUnknown
                            SmtUnknown optCtx.ReasonUnknown
                        | _ -> __unreachable__()
                    with
                    | :? Z3Exception as e ->
                        printLog Info "SOLVER: exception was thrown: %s" e.Message
                        SmtUnknown (sprintf "Z3 has thrown an exception: %s" e.Message)
                finally
                    optCtx.Pop()

            member x.Assert (lvl : level) (fml : formula) =
                printLog Trace "SOLVER: [lvl %O] Asserting (hard):" lvl
                printLogLazy Trace "%s" (lazy(fml.ToString()))
                let encoded = builder.EncodeTerm(fml)
                let leveled =
                    if Level.isInf lvl then encoded
                    else
                        let levelAtom = getLevelAtom lvl
                        ctx.MkImplies(levelAtom, encoded)
                optCtx.Assert(leveled)

            member x.AddPath (p : path) =
                printLog Trace "SOLVER: [lvl %O] Asserting path:" p.lvl
                printLogLazy Trace "    %s" (lazy(PathConditionToSeq p.state.pc |> Seq.map toString |> join " /\\ \n     "))
                let pathAtom = addPath p
                let encoded = PathConditionToSeq p.state.pc |> Seq.map builder.EncodeTerm |> ctx.MkAnd
                optCtx.Assert(ctx.MkImplies(pathAtom, encoded))
