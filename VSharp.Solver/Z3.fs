namespace VSharp.Solver

open Microsoft.Z3
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Core.SolverInteraction
open Logger

module internal Z3 =

    [<CustomEquality;NoComparison>]
    type 'a encodingResult when 'a :> Expr and 'a : equality =
        {expr : 'a; assumptions: BoolExpr list}
        static member Create(expr : 'a) = {expr = expr; assumptions = List.empty}
        static member Create(expr : 'a, conditions) = {expr = expr; assumptions = conditions}

        override x.GetHashCode() = x.expr.GetHashCode()

        override x.Equals(o : obj) =
            match o with
            | :? encodingResult<'a> as res -> x.expr = res.expr
            | _ -> __unreachable__() // TODO: test this: need false #do

    let getExpr encodingResult = encodingResult.expr
    let toTuple encodingResult = encodingResult.assumptions, encodingResult.expr

// ------------------------------- Cache -------------------------------

    type private encodingCache = {
        sorts : IDictionary<symbolicType, Sort>
        e2t : IDictionary<Expr, term>
        t2e : IDictionary<term, Expr encodingResult>
    } with
        member x.Get(term, encoder : unit -> Expr) =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = {expr = encoder(); assumptions = List.empty}
                x.e2t.[result.expr] <- term
                x.t2e.[term] <- result
                result)
        member x.Get(term, encoder : unit -> Expr encodingResult) =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = encoder()
                x.e2t.[result.expr] <- term
                x.t2e.[term] <- result
                result)


    let private freshCache () = {
        sorts = Dictionary<symbolicType, Sort>()
        e2t = Dictionary<Expr, term>()
        t2e = Dictionary<term, Expr encodingResult>()
    }

    let private regionConstants = Dictionary<regionSort, ArrayExpr>()

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
                | AddressType -> ctx.MkIntSort() :> Sort
                | Numeric _
                | ArrayType _
                | Void
                | Null
                | StructType _
                | ClassType _
                | InterfaceType _
                | TypeVariable _
                | ByRef _
                | Pointer _ -> __notImplemented__())

        member private x.EncodeConcreteAddress (encCtx : encodingContext) (address : concreteHeapAddress) =
            ctx.MkNumeral(encCtx.addressOrder.[address], x.Type2Sort AddressType)

        member private x.EncodeConcrete encCtx (obj : obj) typ : Expr encodingResult =
            let expr =
                match typ with
                | Bool -> ctx.MkBool(obj :?> bool) :> Expr
                | Numeric(Id t) when t = typeof<char> -> ctx.MkNumeral(System.Convert.ToInt32(obj :?> char) |> toString, x.Type2Sort typ)
                | Numeric(Id t) ->
                    match obj with
                    | _ when TypeUtils.isIntegral (obj.GetType()) -> ctx.MkInt(obj.ToString()) :> Expr
                    | _ when t.IsEnum -> ctx.MkInt(System.Convert.ChangeType(obj, t.GetEnumUnderlyingType()).ToString()) :> Expr
                    | _ -> ctx.MkNumeral(obj.ToString(), x.Type2Sort typ)
                | AddressType ->
                    match obj with
                    | :? concreteHeapAddress as addr ->
                        assert(List.isEmpty addr |> not)
                        x.EncodeConcreteAddress encCtx addr
                    | _ -> __unreachable__()
                | _ -> __notImplemented__()
            encodingResult<Expr>.Create(expr)

        member private x.CreateConstant name typ term =
            cache.Get(term, fun () -> ctx.MkConst(x.ValidateId name, x.Type2Sort typ))

        member private x.EncodeSymbolicAddress (addr : term) name = x.CreateConstant name AddressType addr

        // TODO: encode everything into BV? Yes, and arrays!
        // TODO: need arrays? Yes, because of last value (if not any update, then a[x])
        member public x.EncodeTerm<'a when 'a :> Expr and 'a : equality> encCtx (t : term) : 'a encodingResult =
            x.EncodeTermExt<'a> (fun _ _ -> false) encCtx t

// ------------------------------- Encoding: memory reading -------------------------------

        member private x.KeyInVectorTimeIntervals (encCtx : encodingContext) (key : Expr) acc (region : vectorTime intervals) = // TODO: check #do
            let onePointCondition acc (y : vectorTime endpoint) =
                let bound = ctx.MkNumeral(encCtx.addressOrder.[y.elem], x.Type2Sort AddressType) :?> ArithExpr
                let condition =
                    match y.sort with
                    | OpenRight -> ctx.MkLt(key :?> ArithExpr, bound)
                    | ClosedRight -> ctx.MkLe(key :?> ArithExpr, bound)
                    | OpenLeft -> ctx.MkGt(key :?> ArithExpr, bound)
                    | ClosedLeft -> ctx.MkGe(key :?> ArithExpr, bound)
                ctx.MkAnd(acc, condition)
            let intervalWithoutLeftZeroBound = List.except [{elem = VectorTime.zero; sort = ClosedLeft}] region.points
            List.fold onePointCondition acc intervalWithoutLeftZeroBound

        member private x.HeapAddressKeyInRegion (encCtx : encodingContext) acc (key : heapAddressKey) (keyExpr : Expr) = // TODO: check #do
            let region = (key :> IMemoryKey<heapAddressKey, vectorTime intervals>).Region
            x.KeyInVectorTimeIntervals encCtx keyExpr acc region

        member private x.KeyInIntPoints key acc (region : int points) = // TODO: check #do
            match region with
            | {points = points; thrown = true} ->
                let excludeOne acc (x : int) =
                    let notEq = ctx.MkNot(ctx.MkEq(key, ctx.MkNumeral(x, ctx.MkIntSort())))
                    ctx.MkAnd(acc, notEq)
                PersistentSet.fold excludeOne acc points
            | {points = points; thrown = false} ->
                let includeOne acc (x : int) =
                    let notEq = ctx.MkEq(key, ctx.MkNumeral(x, ctx.MkIntSort()))
                    ctx.MkAnd(acc, notEq)
                PersistentSet.fold includeOne acc points

        member private x.KeyInProductRegion keyInFst keyInSnd acc (region : productRegion<'a, 'b>) = // TODO: check #do
            let checkKeyInOne acc (fst, snd) = ctx.MkAnd(acc, ctx.MkAnd(keyInFst acc fst, keyInSnd acc snd))
            List.fold checkKeyInOne acc region.products

        member private x.KeysInIntPointsListProductRegion (keys : Expr seq) acc (region : int points listProductRegion) : BoolExpr = // TODO: check #do
            match region with
            | NilRegion when Seq.isEmpty keys -> acc
            | ConsRegion products ->
                let key = Seq.head keys
                let keyInPoints = x.KeyInIntPoints key
                let keysInProductList = x.KeysInIntPointsListProductRegion (Seq.tail keys)
                x.KeyInProductRegion keyInPoints keysInProductList acc products
            | NilRegion _ -> __unreachable__()

        member private x.ArrayIndexKeyInRegion (encCtx : encodingContext) acc (key : heapArrayIndexKey) (keyExpr : Expr[]) = // TODO: check #do
            assert(Array.length keyExpr = List.length key.indices + 1)
            let region = (key :> IMemoryKey<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>>).Region
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx (Array.head keyExpr)
            let indicesInRegion = x.KeysInIntPointsListProductRegion (Array.tail keyExpr)
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.VectorIndexKeyInRegion (encCtx : encodingContext) acc (key : heapVectorIndexKey) (keyExpr : Expr[]) = // TODO: check #do
            assert(Array.length keyExpr = 2)
            let region = (key :> IMemoryKey<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>).Region
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx keyExpr.[0]
            let indicesInRegion = x.KeyInIntPoints keyExpr.[1]
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.stackBufferIndexKeyInRegion (encCtx : encodingContext) acc (key : stackBufferIndexKey) keyExpr = // TODO: check #do
            let region = (key :> IMemoryKey<stackBufferIndexKey, int points>).Region
            x.KeyInIntPoints keyExpr acc region

        member private x.GetRegionConstant hasDefaultValue (name : string) arraySort regSort =
            let mkConst () =
                if hasDefaultValue then ctx.MkConstArray(arraySort, ctx.MkNumeral(0, ctx.MkIntSort()))
                else ctx.MkConst(name, arraySort) :?> ArrayExpr
            let result : ArrayExpr ref = ref null
            if regionConstants.TryGetValue(regSort, result) then !result
            else
                let regConst = mkConst()
                regionConstants.Add(regSort, regConst)
                regConst

        // TODO: delete all this generics! #do
        member private x.MemoryReading<'a, 'b, 'c when 'b : equality and 'b :> IMemoryKey<'b, 'c> and 'c :> IRegion<'c> and 'c : equality>
            (encCtx : encodingContext)
            (keyInRegion : BoolExpr -> 'b -> 'a -> BoolExpr)
            keysAreEqual
            (encodeKey : 'b -> BoolExpr list * 'a)
            hasDefaultValue sort select (left : 'b) mo source name =
                let updates = MemoryRegion.flatten mo
                let memoryRegionConst = GetHeapReadingRegionSort source |> x.GetRegionConstant hasDefaultValue name sort
                let assumptions, leftExpr = encodeKey left
                let leftInRegion = keyInRegion (ctx.MkTrue()) left leftExpr
                let assumptions = leftInRegion :: assumptions
                let inst = select memoryRegionConst leftExpr
                let checkOneKey (acc, assumptions) (right, value) =
                    let rightAssumptions, rightExpr = encodeKey right
                    let assumptions = List.append assumptions rightAssumptions // TODO: make better (mb monad) #do
                    // NOTE: we need constraints on right key, because value may contain it
                    let keysEquality = keysAreEqual leftExpr rightExpr
                    let keysAreMatch = keyInRegion keysEquality right rightExpr
                    let valueExpr = x.EncodeTerm encCtx value
                    let assumptions = List.append assumptions valueExpr.assumptions
                    ctx.MkITE(keysAreMatch, valueExpr.expr, acc), assumptions
                let expr, assumptions = List.fold checkOneKey (inst, assumptions) updates
                encodingResult<Expr>.Create(expr, assumptions)

        member private x.DefaultEquality left right = ctx.MkEq(left, right)

        member private x.HeapReading encCtx key mo typ source name =
            let encodeKey (k : heapAddressKey) = x.EncodeTerm encCtx k.address |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort AddressType, x.Type2Sort typ)
            let select array (k : Expr) = ctx.MkSelect(array, k)
            let keyInRegion = x.HeapAddressKeyInRegion encCtx
            x.MemoryReading encCtx keyInRegion x.DefaultEquality encodeKey false sort select key mo source name

        member private x.ArrayReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue indices key mo typ source name =
            let sort =
                let domainSort = x.Type2Sort AddressType :: List.map (ctx.MkIntSort() :> Sort |> always) indices |> Array.ofList
                ctx.MkArraySort(domainSort, x.Type2Sort typ)
            let select array (k : Expr[]) = ctx.MkSelect(array, k)
            x.MemoryReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue sort select key mo source name

        member private x.StackBufferReading encCtx key mo typ source name =
            let encodeKey (k : stackBufferIndexKey) = x.EncodeTerm encCtx k.index |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort AddressType, x.Type2Sort typ)
            let select array (k : Expr) = ctx.MkSelect(array, k)
            let keyInRegion = x.stackBufferIndexKeyInRegion encCtx
            x.MemoryReading encCtx keyInRegion x.DefaultEquality encodeKey false sort select key mo source name

        member private x.StaticsReading encCtx (key : symbolicTypeKey) mo typ source name = // TODO: make this, using equality of keys #do
            let encodeKey (k : symbolicTypeKey) = __notImplemented__() // TODO: how to encode symbolicType? Enumerate! #do
            let sort = ctx.MkArraySort(ctx.MkIntSort(), x.Type2Sort typ)
            let select array (k : Expr) = ctx.MkSelect(array, k)
            let keyInRegion _ _ _ = ctx.MkTrue()
            x.MemoryReading encCtx keyInRegion x.DefaultEquality encodeKey false sort select key mo source name
//            let staticsReading (key : symbolicTypeKey) mo =
//                let memoryRegionConst = ctx.MkArrayConst(name, ctx.MkIntSort(), x.Type2Sort typ)
//                let updates = MemoryRegion.flatten mo
//                let value = Seq.tryFind (fun (k, _) -> k = key) updates
//                match value with
//                | Some (_, v) -> x.EncodeTerm encCtx v
//                | None -> ctx.MkSelect(memoryRegionConst, ctx.MkNumeral(0, ctx.MkIntSort())) :?> Expr // TODO: what goes here?

        member private x.EncodeMemoryAccessConstant (encCtx : encodingContext) (name : string) (source : IMemoryAccessConstantSource) typ term =
            match source with // TODO: add caching #encode
            | HeapReading(key, mo) -> x.HeapReading encCtx key mo typ source name
            | ArrayIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapArrayIndexKey) =
                    k.address :: k.indices |> x.EncodeTerms (fun _ _ -> false) encCtx
                let keyInRegion = x.ArrayIndexKeyInRegion encCtx
                let arraysEquality left right =
                    Seq.zip left right |> Seq.fold (fun acc (x, y) -> ctx.MkAnd(acc, ctx.MkEq(x, y))) (ctx.MkTrue())
                x.ArrayReading encCtx keyInRegion arraysEquality encodeKey hasDefaultValue key.indices key mo typ source name
            | VectorIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapVectorIndexKey) =
                    [|k.address; k.index|] |> x.EncodeTerms (fun _ _ -> false) encCtx
                let keyInRegion = x.VectorIndexKeyInRegion encCtx
                let keysAreEqual (left : Expr[]) (right : Expr[]) =
                    ctx.MkAnd(ctx.MkEq(left.[0], right.[0]), ctx.MkEq(left.[1], right.[1]))
                x.ArrayReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue [key.index] key mo typ source name
            | StackBufferReading(key, mo) -> x.StackBufferReading encCtx key mo typ source name
            | StaticsReading(key, mo) -> x.StaticsReading encCtx key mo typ source name
            | StructFieldSource field -> __notImplemented__() // TODO: #do
            | HeapAddressSource -> x.EncodeSymbolicAddress term name
            | _ -> x.CreateConstant name typ term

        member private x.EncodeConstant encCtx name (source : ISymbolicConstantSource) typ term =
            match source with
            | :? IMemoryAccessConstantSource as source -> x.EncodeMemoryAccessConstant encCtx name source typ term
            | _ -> x.CreateConstant name typ term

        // TODO: make better! #do
        member private x.EncodeExpression stopper (encCtx : encodingContext) term op args typ : Expr encodingResult = // TODO: need stopper? delete? #do
            cache.Get(term, fun () ->
                match op with
                | Operator operator ->
                    if stopper operator args then
                        let name = IdGenerator.startingWith "%tmp"
                        x.CreateConstant name typ term
                    else
                        match operator with
                        | OperationType.LogicalNeg ->
                            let res = x.MakeUnary stopper encCtx ctx.MkNot args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.LogicalAnd ->
                            let assumptions, expressions = x.EncodeTerms stopper encCtx args
                            {expr = ctx.MkAnd(expressions) :> Expr; assumptions = assumptions}
                        | OperationType.LogicalOr ->
                            let assumptions, expressions = x.EncodeTerms stopper encCtx args
                            {expr = ctx.MkOr(expressions) :> Expr; assumptions = assumptions}
                        | OperationType.Equal ->
                            let res = x.MakeBinary stopper encCtx ctx.MkEq args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.Greater ->
                            let res = x.MakeBinary stopper encCtx ctx.MkGt args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.GreaterOrEqual ->
                            let res = x.MakeBinary stopper encCtx ctx.MkGe args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.Less ->
                            let res = x.MakeBinary stopper encCtx ctx.MkLt args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.LessOrEqual ->
                            let res = x.MakeBinary stopper encCtx ctx.MkLe args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.Add ->
                            let assumptions, expressions = x.EncodeTerms stopper encCtx args
                            {expr = ctx.MkAdd(expressions) :> Expr; assumptions = assumptions}
                        | OperationType.Multiply ->
                            let assumptions, expressions = x.EncodeTerms stopper encCtx args
                            {expr = ctx.MkMul(expressions) :> Expr; assumptions = assumptions}
                        | OperationType.Subtract ->
                            let assumptions, expressions = x.EncodeTerms stopper encCtx args
                            {expr = ctx.MkSub(expressions) :> Expr; assumptions = assumptions}
                        | OperationType.Divide ->
                            let res = x.MakeBinary stopper encCtx ctx.MkDiv args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.Remainder ->
                            let res = x.MakeBinary stopper encCtx ctx.MkRem args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.UnaryMinus ->
                            let res = x.MakeUnary stopper encCtx ctx.MkUnaryMinus args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.Not ->
                            let res = x.MakeUnary stopper encCtx ctx.MkNot args
                            {expr = res.expr :> Expr; assumptions = res.assumptions}
                        | OperationType.ShiftLeft
                        | OperationType.ShiftRight -> __notImplemented__()
                        | _ -> __notImplemented__()
                | Application sf ->
                    let decl = ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, x.Type2Sort typ)
                    let assumptions, expressions = x.EncodeTerms stopper encCtx args
                    {expr = ctx.MkApp(decl, expressions); assumptions = assumptions}
                | Cast(Numeric _, Numeric _) ->
                    let res = x.EncodeTermExt stopper encCtx (List.head args)
                    {expr = res.expr :> Expr; assumptions = res.assumptions}
                | Cast _ ->
                    __notImplemented__())

        member private x.EncodeTermExt<'a when 'a :> Expr and 'a : equality> (stopper : OperationType -> term list -> bool) encCtx (t : term) : 'a encodingResult =
            match t.term with
            | Concrete(obj, typ) ->
                let result = x.EncodeConcrete encCtx obj typ
                {expr = result.expr :?> 'a; assumptions = result.assumptions}
            | Constant(name, source, typ) ->
                let result = x.EncodeConstant encCtx name.v source typ t
                {expr = result.expr :?> 'a; assumptions = result.assumptions}
            | Expression(op, args, typ) ->
                let result = x.EncodeExpression stopper encCtx t op args typ
                {expr = result.expr :?> 'a; assumptions = result.assumptions}
            | _ -> __notImplemented__() // TODO: need to encode HeapRef? #do

        member private this.MakeUnary<'a, 'b when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality>
                (stopper : OperationType -> term list -> bool)
                (encCtx : encodingContext)
                (constructor : 'a -> 'b)
                (args : term list) : 'b encodingResult =
            match args with
            | [x] ->
                let result = this.EncodeTermExt<'a> stopper encCtx x
                {expr = constructor result.expr; assumptions = result.assumptions}
            | _ -> internalfail "unary operation should have exactly one argument"

        member private this.MakeBinary<'a, 'b, 'c when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality and 'c :> Expr and 'c : equality>
                (stopper : OperationType -> term list -> bool)
                (encCtx : encodingContext)
                (constructor : 'a * 'b -> 'c)
                (args : term list) : 'c encodingResult =
            match args with
            | [x; y] ->
                let x' = this.EncodeTermExt<'a> stopper encCtx x
                let y' = this.EncodeTermExt<'b> stopper encCtx y
                {expr = constructor(x'.expr, y'.expr); assumptions = x'.assumptions @ y'.assumptions}
            | _ -> internalfail "binary operation should have exactly two arguments"

        member internal x.EncodeTerms<'a when 'a :> Expr and 'a : equality> (stopper : OperationType -> term list -> bool) (encCtx : encodingContext) (ts : term seq) : BoolExpr list * 'a array =
            let encodeOne acc term =
                let result = x.EncodeTermExt<'a> stopper encCtx term
                result.expr, acc @ result.assumptions
            let expressions, assumptions = Seq.mapFold encodeOne List.empty ts
            assumptions, Array.ofSeq expressions

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
            member x.CheckSat (encCtx : encodingContext) (q : query) : smtResult =
                printLog Info "SOLVER: trying to solve constraints [level %O]..." q.lvl
                printLogLazy Trace "%s" (lazy(q.queryFml.ToString()))
                let query = builder.EncodeTerm encCtx q.queryFml
                let assumptions = query.assumptions
                let assumptions =
                    seq {
                        for i in 0 .. levelAtoms.Count - 1 do
                            let atom = levelAtoms.[i]
                            if atom <> null then
                                let lit = if i < Level.toInt q.lvl then atom else ctx.MkNot atom
                                yield lit :> Expr
                        for i in 0 .. assumptions.Length - 1 do // TODO: do better #do
                            yield assumptions.[i] :> Expr
                        yield query.expr
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

            member x.Assert encCtx (lvl : level) (fml : formula) =
                printLog Trace "SOLVER: [lvl %O] Asserting (hard):" lvl
                printLogLazy Trace "%s" (lazy(fml.ToString()))
                let encoded = builder.EncodeTerm encCtx fml // TODO: make this generic #do
                let encoded = List.fold (fun acc x -> ctx.MkAnd(acc, x)) encoded.expr encoded.assumptions
                let leveled =
                    if Level.isInf lvl then encoded
                    else
                        let levelAtom = getLevelAtom lvl
                        ctx.MkImplies(levelAtom, encoded)
                optCtx.Assert(leveled)

            member x.AddPath encCtx (p : path) =
                printLog Trace "SOLVER: [lvl %O] Asserting path:" p.lvl
                printLogLazy Trace "    %s" (lazy(PathConditionToSeq p.state.pc |> Seq.map toString |> join " /\\ \n     "))
                let pathAtom = addPath p
                let assumptions, encoded = PathConditionToSeq p.state.pc |> builder.EncodeTerms (fun _ _ -> false) encCtx
                let encoded = ctx.MkAnd(Seq.append assumptions encoded)
                optCtx.Assert(ctx.MkImplies(pathAtom, encoded))
