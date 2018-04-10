namespace VSharp

open JetBrains.Decompiler.Ast
open Microsoft.Z3
open System.Collections.Generic
open VSharp.Core
open Logger

module internal Z3 =

// ------------------------------- Cache -------------------------------

    type emptySource() =
        interface INonComposableSymbolicConstantSource with
            override x.SubTerms = seq[]

    type heapSchemeKey = generalizedHeap * termType list

    type encodingCache = {
        sorts : IDictionary<termType, Sort>
        e2t : IDictionary<Expr, term>
        t2e : IDictionary<term, Expr>
        relationsForResult : IDictionary<IFunctionIdentifier, FuncDecl>
        rulesForResult : IDictionary<IFunctionIdentifier, BoolExpr seq>
        dependenciesOfResult : IDictionary<IFunctionIdentifier, (term * ISymbolicConstantSource) list>
        relationsForHeapReads : IDictionary<heapSchemeKey, FuncDecl>
        rulesForHeapReads : IDictionary<heapSchemeKey, BoolExpr seq>
        dependenciesOfHeapReads : IDictionary<heapSchemeKey, (term * ISymbolicConstantSource) list>
        exprConstraints : IDictionary<Expr, HashSet<BoolExpr>>
        mutable boundVarId : uint32
    } with
        member x.Get term encoder =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = encoder()
                x.e2t.[result] <- term
                x.t2e.[term] <- result
                result)

    let private freshCache (ctx : Context) =
        {
            sorts = new Dictionary<termType, Sort>()
            e2t = new Dictionary<Expr, term>()
            t2e = new Dictionary<term, Expr>()
            relationsForResult = new Dictionary<IFunctionIdentifier, FuncDecl>()
            rulesForResult = new Dictionary<IFunctionIdentifier, BoolExpr seq>()
            dependenciesOfResult = new Dictionary<IFunctionIdentifier, (term * ISymbolicConstantSource) list>()
            relationsForHeapReads = new Dictionary<heapSchemeKey, FuncDecl>()
            rulesForHeapReads = new Dictionary<heapSchemeKey, BoolExpr seq>()
            dependenciesOfHeapReads = new Dictionary<heapSchemeKey, (term * ISymbolicConstantSource) list>()
            exprConstraints = new Dictionary<Expr, HashSet<BoolExpr>>()
            boundVarId = 0u
        }

    type private EncodingContext() as this =
        inherit Context()
        let cache = freshCache this
        let fp = this.MkFixedpoint()
        member x.Cache = cache
        member x.FP = fp

    let private ctxs = new System.Collections.Generic.Stack<EncodingContext>()
    let private ctx() = ctxs.Peek()

    let freshBoundVar sort =
        (ctx()).Cache.boundVarId <- (ctx()).Cache.boundVarId + 1u
        (ctx()).MkBound((ctx()).Cache.boundVarId, sort)

    let rec private constraintsOfExprs<'a when 'a :> Expr> (exprs : 'a seq) =
        let constraints = exprs |> Seq.choose (constraintsOfExpr >> Option.ofObj)
        if Seq.isEmpty constraints then null
        else
            let result = new HashSet<BoolExpr>()
            Seq.iter result.UnionWith constraints
            result

    and private constraintsOfExpr (expr : Expr) =
        Dict.getValueOrUpdate (ctx()).Cache.exprConstraints expr (fun () ->
            match expr with
            | _ when expr.NumArgs = 0u || expr.IsVar -> null
            | _ -> constraintsOfExprs expr.Args)

// ------------------------------- Encoding: primitives -------------------------------

    let validateId id =
        assert(not <| System.String.IsNullOrWhiteSpace id)
        if System.Char.IsDigit id.[0] then "_" + id else id

    let compose state (term, source : ISymbolicConstantSource) =
        match source with
        | :? INonComposableSymbolicConstantSource -> term
        | :? IStatedSymbolicConstantSource as source -> source.Compose compositionContext.Empty state
        | _ -> __notImplemented__()

    let decomposeReference = function
        | { term = HeapRef(path, time, at, typ); metadata = mtd } ->
            let path = NonEmptyList.toList path
            let mkSymbolicAddress acc (l, t) =
                let lt = TypeOf l
                let sl, acc' =
                    if Types.IsInteger lt then
                        let c = Constant (IdGenerator.startingWith "__heap_addr_") (emptySource()) lt
                        c, (c, l)::acc
                    else l, acc
                (sl, t), acc'
            let symbolicPath, constants = List.mapFold mkSymbolicAddress [] path
            let symbolicRef = { term = HeapRef(NonEmptyList.ofList symbolicPath, time, at, typ); metadata = mtd }
            symbolicRef, constants, path
        | { term = StaticRef(_, path, _) } -> __notImplemented__()
        | t -> internalfailf "solver: expected reference, but got %O" t

    let type2Sort typ =
        Dict.getValueOrUpdate (ctx()).Cache.sorts typ (fun () ->
            match typ with
            | Bool -> (ctx()).MkBoolSort() :> Sort
            | Numeric _ as t when Types.IsInteger t -> (ctx()).MkIntSort() :> Sort
            | Numeric _ as t when Types.IsReal t -> (ctx()).MkRealSort() :> Sort
            | Numeric t -> (ctx()).MkEnumSort(t.FullName, t.GetEnumNames()) :> Sort
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

    let freshRelationForResult funcId (domain : Sort array) =
        Dict.getValueOrUpdate (ctx()).Cache.relationsForResult funcId (fun () ->
            let range = type2Sort Bool
            let freshRelationalSymbol = (ctx()).MkFuncDecl(funcId.ToString(), domain, range)
            (ctx()).FP.RegisterRelation freshRelationalSymbol
            freshRelationalSymbol)

    let freshRelationForHeapRead key (domain : Sort array) =
        Dict.getValueOrUpdate (ctx()).Cache.relationsForHeapReads key (fun () ->
            let range = type2Sort Bool
            let freshRelationalSymbol = (ctx()).MkFuncDecl(IdGenerator.startingWith "heap_read_", domain, range)
            (ctx()).FP.RegisterRelation freshRelationalSymbol
            freshRelationalSymbol)

    let chooseDependence = function
        | {term = Constant(name, source, typ)} as term ->
            match source with
            | RecursionOutcome _
            | LazyInstantiation(_, Some _, _) -> None
            | _ -> Some(term, source)
        | _ -> None

    let encodeConcrete (obj : obj) typ =
        match typ with
        | Bool -> (ctx()).MkBool(obj :?> bool) :> Expr
        | Numeric t when t = typeof<char> -> (ctx()).MkNumeral(System.Convert.ToInt32(obj :?> char) |> toString, type2Sort typ)
        | Numeric t when t.IsEnum ->
            let sort = type2Sort typ :?> EnumSort in
            let name = obj.ToString() in
            FSharp.Collections.Array.find (toString >> ((=) name)) sort.Consts
        | Numeric _ ->
            match obj with
            | :? concreteHeapAddress as addr ->
                match addr with
                | [addr] -> (ctx()).MkNumeral(addr.ToString(), type2Sort typ)
                | _ -> __notImplemented__()
            | _ -> (ctx()).MkNumeral(obj.ToString(), type2Sort typ)
        | _ -> __notImplemented__()

    let encodeConstantSimple name typ term =
        ignore name
        (ctx()).Cache.Get term (fun () -> freshBoundVar(type2Sort typ))
//        cache.Get term (fun () -> (ctx()).MkConst(validateId name, type2Sort typ))

    let rec encodeConstant name (source : ISymbolicConstantSource) typ term =
        match source with
        | LazyInstantiation(location, heap, _) ->
            match heap with
            | None -> encodeConstantSimple name typ term
            | Some heap -> encodeHeapRead location heap typ
        | RecursionOutcome(id, state, location, _) ->
            encodeRecursionOutcome id typ state location
        | _ -> encodeConstantSimple name typ term

    and encodeExpression stopper term op args typ =
        (ctx()).Cache.Get term (fun () ->
            match op with
            | Operator(operator, _) ->
                if stopper operator args then
                    let name = IdGenerator.startingWith "%tmp"
                    encodeConstantSimple name typ term
                else
                    match operator with
                    | OperationType.LogicalNeg -> makeUnary stopper (ctx()).MkNot args :> Expr
                    | OperationType.LogicalAnd -> (ctx()).MkAnd(encodeTerms stopper args) :> Expr
                    | OperationType.LogicalOr -> (ctx()).MkOr(encodeTerms stopper args) :> Expr
                    | OperationType.Equal -> makeBinary stopper (ctx()).MkEq args :> Expr
                    | OperationType.Greater -> makeBinary stopper (ctx()).MkGt args :> Expr
                    | OperationType.GreaterOrEqual -> makeBinary stopper (ctx()).MkGe args :> Expr
                    | OperationType.Less -> makeBinary stopper (ctx()).MkLt args :> Expr
                    | OperationType.LessOrEqual -> makeBinary stopper (ctx()).MkLe args :> Expr
                    | OperationType.Add -> (ctx()).MkAdd(encodeTerms stopper args) :> Expr
                    | OperationType.Multiply -> (ctx()).MkMul(encodeTerms stopper args) :> Expr
                    | OperationType.Subtract -> (ctx()).MkSub(encodeTerms stopper args) :> Expr
                    | OperationType.Divide -> makeBinary stopper (ctx()).MkDiv args :> Expr
                    | OperationType.Remainder -> makeBinary stopper (ctx()).MkRem args :> Expr
                    | OperationType.UnaryMinus -> makeUnary stopper (ctx()).MkUnaryMinus args :> Expr
                    | OperationType.Not -> makeUnary stopper (ctx()).MkNot args :> Expr
                    | OperationType.ShiftLeft
                    | OperationType.ShiftRight -> __notImplemented__()
                    | _ -> __notImplemented__()
            | Application id ->
                let decl =
                    match id with
                    | :? IMethodIdentifier -> __notImplemented__()
                    | :? IDelegateIdentifier -> __notImplemented__()
                    | :? StandardFunctionIdentifier as sf -> (ctx()).MkConstDecl(sf.Function |> toString |> IdGenerator.startingWith, type2Sort typ)
                    | _ -> __notImplemented__()
                (ctx()).MkApp(decl, encodeTerms stopper args)
            | Cast _ ->
                __notImplemented__())

    and makeUnary<'a, 'b when 'a :> Expr and 'b :> Expr>
            (stopper : OperationType -> term list -> bool)
            (constructor : 'a -> 'b)
            (args : term list) : 'b =
        match args with
        | [x] -> constructor (encodeTermExt<'a> stopper x)
        | _ -> internalfail "unary operation should have exactly one argument"

    and makeBinary<'a, 'b, 'c when 'a :> Expr and 'b :> Expr and 'c :> Expr>
            (stopper : OperationType -> term list -> bool)
            (constructor : 'a * 'b -> 'c)
            (args : term list) : 'c =
        match args with
        | [x; y] -> constructor(encodeTermExt<'a> stopper x, encodeTermExt<'b> stopper y)
        | _ -> internalfail "binary operation should have exactly two arguments"

    and encodeTerms<'a when 'a :> Expr> (stopper : OperationType -> term list -> bool) (ts : term seq) : 'a array =
        ts |> Seq.map (encodeTermExt<'a> stopper) |> FSharp.Collections.Array.ofSeq

    and encodeTermExt<'a when 'a :> Expr> (stopper : OperationType -> term list -> bool) (t : term) : 'a =
        match t.term with
        | Concrete(obj, typ) -> encodeConcrete obj typ :?> 'a
        | Constant(name, source, typ) -> encodeConstant name.v source typ t :?> 'a
        | Expression(op, args, typ) -> encodeExpression stopper t op args typ :?> 'a
        | _ -> __notImplemented__()

// ------------------------------- Encoding: Horn clauses -------------------------------

    and encodeIntoDnf = function
        | Disjunction ts ->
            let dnfs = List.map encodeIntoDnf ts
            List.concat dnfs
        | Conjunction ts ->
            let dnfs = List.map encodeIntoDnf ts
            let shuffle xss yss =
                List.map (fun xs -> List.map (List.append xs) yss) xss |> List.concat
            List.reduce shuffle dnfs
        | t -> [[encodeTermExt<BoolExpr> (fun _ _ -> false) t]]

    and encodeFunction funcId =
        if (ctx()).Cache.rulesForResult.ContainsKey funcId then (ctx()).Cache.relationsForResult.[funcId]
        else
            (ctx()).Cache.rulesForResult.Add(funcId, seq[])  // Mark that we are currently working on it, as we can recursively get here again
            let recres, recstate = Database.Query funcId
            let resType = recres |> TypeOf |> type2Sort
            let deps = Database.DependenciesOfRecursionResult funcId
            let readDeps = Seq.choose chooseDependence deps |> List.ofSeq
            let readDepsTypes = List.map (fst >> TypeOf >> type2Sort) readDeps
            (ctx()).Cache.dependenciesOfResult.Add(funcId, readDeps)
            let rel = freshRelationForResult funcId (List.append readDepsTypes [resType] |> List.toArray)
            let resultVar = freshBoundVar resType
            // TODO: for a mutual recursion we should take a union of reap dependencies!
            let depsVars = List.map2 (fun (term, _) typ -> (ctx()).Cache.Get term (fun () -> freshBoundVar typ)) readDeps readDepsTypes
            let head = (ctx()).MkApp(rel, List.append depsVars [resultVar] |> List.toArray) :?> BoolExpr
            let mkBody (guard, branchResult) =
                let branchResultExpr = encodeTermExt<Expr> (fun _ _ -> false) branchResult
                let constraintsOfResult = constraintsOfExpr branchResultExpr
                let returnExpr = (ctx()).MkEq(resultVar, branchResultExpr)
                let dnf = encodeIntoDnf guard
                let appendConstraints (clause : BoolExpr list) =
                    let bodyConstraints = constraintsOfExprs clause
                    if bodyConstraints <> null && constraintsOfResult <> null then
                        bodyConstraints.UnionWith constraintsOfResult
                    let constraints = if bodyConstraints <> null then bodyConstraints else constraintsOfResult
                    let clauseAndConstraints = if constraints = null then clause else List.append clause (List.ofSeq constraints)
                    (ctx()).MkAnd(returnExpr::clauseAndConstraints |> List.toArray)
                List.map appendConstraints dnf
            let bodies =
                match recres.term with
                | Union gvs ->
                    List.map mkBody gvs |> List.concat
                | _ -> mkBody (True, recres)
            let clauses = List.map (fun body -> (ctx()).MkImplies(body, head)) bodies
            (ctx()).Cache.rulesForResult.[funcId] <- clauses
            clauses |> List.iter (ctx()).FP.AddRule
            printfn "SOLVER: reported clauses: {"
            clauses |> List.iter (printfn "%O;")
            printfn "}"
            rel

    and encodeRecursionOutcome id typ state location =
        match location with
        | Some location -> __notImplemented__()
        | None ->
            let rel = encodeFunction id
            let resvar = typ |> type2Sort |> freshBoundVar
            let deps = (ctx()).Cache.dependenciesOfResult.[id]
            // TODO: this should be somehow memorized during the interpretation!
            let depsTerms = List.map (compose state) deps
            let depsExprs = List.map (encodeTermExt<Expr> (fun _ _ -> false)) depsTerms
            let app = (ctx()).MkApp(rel, List.append depsExprs [resvar] |> List.toArray)
            let appConstraints = constraintsOfExprs depsExprs
            let constraintsOfResult = new HashSet<BoolExpr>(seq[app :?> BoolExpr])
            if appConstraints <> null then constraintsOfResult.UnionWith appConstraints
            (ctx()).Cache.exprConstraints.Add(resvar, constraintsOfResult)
            resvar

    and encodeHeapReadScheme symbolicRef refConstants ((heap, path) as key : heapSchemeKey) resType =
        if (ctx()).Cache.rulesForHeapReads.ContainsKey key then (ctx()).Cache.relationsForHeapReads.[key]
        else
            (ctx()).Cache.rulesForHeapReads.Add(key, seq[])  // Mark that we are currently working on it, as we can recursively get here again
            let recres, readDeps =
                match heap with
                | RecursiveApplication(funcId, addr, time) ->
                    let _, state = Database.Query funcId
//                    // TODO: get rid of this shitty hack!
//                    let recres', _ =
//                        BranchStatementsOnNull state symbolicRef
//                            (fun state k -> k (NoComputation, state))
//                            (fun state k -> let r, s = Memory.Dereference state symbolicRef in k (Return r, s))
//                            id
//                    let recres =
//                        match recres'.result with
//                        | Guarded grs -> grs |> List.choose (fun (_, r) -> match r.result with | Return t -> Some t | _ -> None) |> List.head
//                        | Return t -> t
//                        | _ -> __notImplemented__()
                    let recres, _ = Memory.Dereference state symbolicRef
//                    let deps = Database.DependenciesOfState funcId
//                    let readDeps = Seq.choose chooseDependence deps |> List.ofSeq
                    recres, []//readDeps
                | Composition _ ->
                    __notImplemented__()
                | HigherOrderApplication _ ->
                    __notImplemented__()
                | _ -> internalfail "SOLVER: unexpected heap!"
            (ctx()).Cache.dependenciesOfHeapReads.Add(key, readDeps)
            let resType = type2Sort resType
//            let readDepsTypes = List.map (fst >> TypeOf >> type2Sort) readDeps
//            let rel = freshRelationForHeapRead key (List.append3 (List.map (TypeOf >> type2Sort) refConstants) readDepsTypes [resType] |> List.toArray)
            let rel = freshRelationForHeapRead key (List.append (List.map (TypeOf >> type2Sort) refConstants) [resType] |> List.toArray)
            let resultVar = freshBoundVar resType
//            let depsVars = List.map2 (fun (term, _) typ -> (ctx()).Cache.Get term (fun () -> freshBoundVar typ)) readDeps readDepsTypes
//            let head = (ctx()).MkApp(rel, List.append3 (List.map (fun c -> (ctx()).Cache.Get c (fun () -> freshBoundVar (TypeOf c |> type2Sort))) refConstants) depsVars [resultVar] |> List.toArray) :?> BoolExpr
            let head = (ctx()).MkApp(rel, List.append (List.map (fun c -> (ctx()).Cache.Get c (fun () -> freshBoundVar (TypeOf c |> type2Sort))) refConstants) [resultVar] |> List.toArray) :?> BoolExpr
            let mkBody (guard, branchResult) =
                match branchResult.term with
                | Error _ -> [] // TODO: errors should be processed too!
                | _ ->
                    let branchResultExpr = encodeTermExt<Expr> (fun _ _ -> false) branchResult
                    let constraintsOfResult = constraintsOfExpr branchResultExpr
                    let returnExpr = (ctx()).MkEq(resultVar, branchResultExpr)
                    let dnf = encodeIntoDnf guard
                    let appendConstraints (clause : BoolExpr list) =
                        let bodyConstraints = constraintsOfExprs clause
                        if bodyConstraints <> null && constraintsOfResult <> null then
                            bodyConstraints.UnionWith constraintsOfResult
                        let constraints = if bodyConstraints <> null then bodyConstraints else constraintsOfResult
                        let clauseAndConstraints = if constraints = null then clause else List.append clause (List.ofSeq constraints)
                        (ctx()).MkAnd(returnExpr::clauseAndConstraints |> List.toArray)
                    List.map appendConstraints dnf
            let bodies =
                match recres.term with
                | Union gvs ->
                    List.map mkBody gvs |> List.concat
                | _ -> mkBody (True, recres)
            let clauses = List.map (fun body -> (ctx()).MkImplies(body, head)) bodies
            (ctx()).Cache.rulesForHeapReads.[key] <- clauses
            clauses |> List.iter (ctx()).FP.AddRule
            printfn "SOLVER: reported clauses (FOR HEAP CASE): {"
            clauses |> List.iter (printfn "%O;")
            printfn "}"
            rel

    and encodeHeapRead location heap typ =
        let symbolicRef, refSubst, path = decomposeReference location
        let locations, types = List.unzip path
        let refConstants, refValues = List.unzip refSubst
        let refEncodings = List.map (encodeTermExt (fun _ _ -> false)) refConstants
        let key = (heap, types)
        let rel = encodeHeapReadScheme symbolicRef refConstants key typ
        let resvar = typ |> type2Sort |> freshBoundVar
//        let deps = (ctx()).Cache.dependenciesOfHeapReads.[key]
//        let depsTerms = List.map (compose state) deps
//        let depsExprs = List.map (encodeTermExt<Expr> (fun _ _ -> false)) depsTerms
//        let app = (ctx()).MkApp(rel, List.append3 refEncodings depsExprs [resvar] |> List.toArray)
        let app = (ctx()).MkApp(rel, List.append refEncodings [resvar] |> List.toArray)
        let appConstraints = constraintsOfExprs refEncodings
        let constraintsOfResult = new HashSet<BoolExpr>(seq[app :?> BoolExpr])
        if appConstraints <> null then constraintsOfResult.UnionWith appConstraints
        (ctx()).Cache.exprConstraints.Add(resvar, constraintsOfResult)
        resvar

    let encodeTerm t =
        printLog Trace "SOLVER: trying to encode %O" t
        encodeTermExt (fun _ _ -> false) t :> AST


// ------------------------------- Decoding -------------------------------

    let rec decodeExpr op t (expr : Expr) =
        Expression (Operator(op, false)) (expr.Args |> Seq.map decode |> List.ofSeq) t

    and decodeBoolExpr op (expr : BoolExpr) =
        decodeExpr op Bool expr

    and decode (expr : Expr) =
        if (ctx()).Cache.e2t.ContainsKey(expr) then (ctx()).Cache.e2t.[expr]
        else
            match expr with
            | :? IntNum as i -> Concrete i.Int (Numeric typeof<int>)
            | :? RatNum as r -> Concrete (double(r.Numerator.Int) * 1.0 / double(r.Denominator.Int)) (Numeric typeof<int>)
            | :? BoolExpr as b ->
                if b.IsTrue then True
                elif b.IsFalse then False
                elif b.IsNot then decodeBoolExpr OperationType.LogicalNeg b
                elif b.IsAnd then decodeBoolExpr OperationType.LogicalAnd b
                elif b.IsOr then decodeBoolExpr OperationType.LogicalOr b
                elif b.IsEq then decodeBoolExpr OperationType.Equal b
                elif b.IsGT then decodeBoolExpr OperationType.Greater b
                elif b.IsGE then decodeBoolExpr OperationType.GreaterOrEqual b
                elif b.IsLT then decodeBoolExpr OperationType.Less b
                elif b.IsLE then decodeBoolExpr OperationType.LessOrEqual b
                else __notImplemented__()
            | _ ->
                __notImplemented__()


// ------------------------------- Solving, etc. -------------------------------

    let solveFP (terms : term list) =
        let context = new EncodingContext()
        ctxs.Push(context)
        try
            printLog Trace "SOLVER: got terms %O" terms
            let exprs = List.map (encodeTermExt<BoolExpr> (fun _ _ -> false)) terms
            printLog Trace "SOLVER: solving %O" exprs
            let constraints = constraintsOfExprs exprs
            let constraints = if constraints = null then [] else List.ofSeq constraints
            let failRel =
                let decl = (ctx()).MkFuncDecl(IdGenerator.startingWith "fail", [||], (ctx()).MkBoolSort())
                (ctx()).FP.RegisterRelation decl
                (ctx()).MkApp(decl, [||]) :?> BoolExpr
            let queryClause = (ctx()).MkImplies(List.append exprs constraints |> Array.ofList |> (ctx()).MkAnd, failRel)
            (ctx()).FP.AddRule queryClause

            printLog Trace "SOLVER: adding query clause %O" queryClause
            let result = (ctx()).FP.Query(failRel)
            printLog Trace "SOLVER: got %O" result
            match result with
            | Status.SATISFIABLE -> SmtSat null
            | Status.UNSATISFIABLE -> SmtUnsat
            | Status.UNKNOWN -> printLog Trace "SOLVER: reason: %O" <| (ctx()).FP.GetReasonUnknown(); SmtUnknown ((ctx()).FP.GetReasonUnknown())
            | _ -> __unreachable__()
        finally
            ctxs.Pop() |> ignore
            context.Dispose()

    let solveSMT (exprs : AST list) =
        __notImplemented__() // All the code below works except constants are currently encoded into bound vars in the spirit of fixedpoint engine.
//        printfn "SOLVER: solving %O" exprs
//        try
//            exprs |> List.iter (fun expr -> solver.Assert(expr :?> BoolExpr))
//            let result = solver.Check()
//            printfn "SOLVER: got %O" result
//            match result with
//            | Status.SATISFIABLE -> SmtSat solver.Model
//            | Status.UNSATISFIABLE -> SmtUnsat
//            | Status.UNKNOWN -> printfn "SOLVER: reason: %O" solver.ReasonUnknown; SmtUnknown solver.ReasonUnknown
//            | _ -> __unreachable__()
//        finally
//            solver.Reset()

    let simplifyPropositional t =
        let stopper op args =
            match op with
            | OperationType.LogicalNeg
            | OperationType.LogicalAnd
            | OperationType.LogicalOr
            | OperationType.Equal when List.forall (TypeOf >> Types.IsBool) args ->
                false
            | _ -> true
        let encoded = encodeTermExt stopper t
        let simple = encoded.Simplify()
        let result = decode simple
        printLog Trace "SOLVER: simplification of %O   gave   %O" t result
        printLog Trace "SOLVER: on SMT level encodings are %O    and     %O" encoded simple
        result