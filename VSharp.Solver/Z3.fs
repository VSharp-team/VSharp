namespace VSharp.Solver

open Microsoft.Z3
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Core.SolverInteraction
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

        member private x.EncodeConcreteAddress (encCtx : encodingContext) (addr : concreteHeapAddress) =
            ctx.MkNumeral(encCtx.addressOrder.[addr], x.Type2Sort AddressType)

        member private x.EncodeConcrete encCtx (obj : obj) typ =
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

        member private x.CreateConstant name typ term =
            cache.Get term (fun () -> ctx.MkConst(x.ValidateId name, x.Type2Sort typ))

        member private x.EncodeSymbolicAddress (addr : term) name = x.CreateConstant name AddressType addr

        // TODO: encode everything into BV? Yes, and arrays!
        // TODO: need arrays? Yes, because of last value (if not any update, then a[x])
        member public x.EncodeTerm<'a when 'a :> Expr> encCtx (t : term) : 'a =
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
            List.fold onePointCondition acc region.points

        member private x.HeapAddressKeyMatch (encCtx : encodingContext) (left : Expr) (right : heapAddressKey) = // TODO: check #do
            let rightRegion = (right :> IMemoryKey<heapAddressKey, vectorTime intervals>).Region
            let rightExpr = x.EncodeTerm encCtx right.address
            let rightInRegion = x.KeyInVectorTimeIntervals encCtx rightExpr (ctx.MkTrue()) rightRegion
            ctx.MkAnd(ctx.MkEq(left, rightExpr), rightInRegion)

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

        member private x.KeysInIntPointsListProductRegion (keys : Expr list) acc (region : int points listProductRegion) : BoolExpr = // TODO: check #do
            match region with
            | NilRegion when List.isEmpty keys -> acc
            | ConsRegion products ->
                let key = List.head keys
                let keyInPoints = x.KeyInIntPoints key
                let keysInProductList = x.KeysInIntPointsListProductRegion (List.tail keys)
                x.KeyInProductRegion keyInPoints keysInProductList acc products
            | NilRegion _ -> __unreachable__()

        member private x.ArrayIndexKeyMatch (encCtx : encodingContext) (left : Expr[]) (right : heapArrayIndexKey) = // TODO: check #do
            assert(Array.length left = List.length right.indices + 1)
            let rightRegion = (right :> IMemoryKey<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>>).Region
            let rightAddress = x.EncodeTerm encCtx right.address
            let rightIndices = List.map (x.EncodeTerm encCtx) right.indices
            let rightAddressInRegion = x.KeyInVectorTimeIntervals encCtx rightAddress
            let rightIndicesInRegion = x.KeysInIntPointsListProductRegion rightIndices
            let rightInRegion = x.KeyInProductRegion rightAddressInRegion rightIndicesInRegion (ctx.MkTrue()) rightRegion
            let keysAreEqual =
                let leftAddress = Array.head left
                let leftIndices = Array.tail left
                let addressesAreEqual = ctx.MkEq(leftAddress, rightAddress)
                let oneIndexEqual (acc, leftIndices) (rightIndex : Expr) =
                    let leftIndex = Array.head leftIndices
                    let oneIndexEq = ctx.MkEq(leftIndex, rightIndex)
                    (ctx.MkAnd(acc, oneIndexEq), Array.tail leftIndices)
                List.fold oneIndexEqual (addressesAreEqual, leftIndices) rightIndices |> fst
            ctx.MkAnd(keysAreEqual, rightInRegion)

        member private x.VectorIndexKeyMatch (encCtx : encodingContext) (left : Expr[]) (right : heapVectorIndexKey) = // TODO: check #do
            assert(Array.length left = 2)
            let rightRegion = (right :> IMemoryKey<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>).Region
            let rightAddress = x.EncodeTerm encCtx right.address
            let rightIndex = x.EncodeTerm encCtx right.index
            let rightAddressInRegion = x.KeyInVectorTimeIntervals encCtx rightAddress
            let rightIndicesInRegion = x.KeyInIntPoints rightIndex
            let rightInRegion = x.KeyInProductRegion rightAddressInRegion rightIndicesInRegion (ctx.MkTrue()) rightRegion
            let keysAreEqual =
                let leftAddress = left.[0]
                let leftIndex = left.[1]
                let addressesAreEqual = ctx.MkEq(leftAddress, rightAddress)
                let indicesAreEqual = ctx.MkEq(leftIndex, rightIndex)
                ctx.MkAnd(addressesAreEqual, indicesAreEqual)
            ctx.MkAnd(keysAreEqual, rightInRegion)

        member private x.stackBufferIndexKeyMatch (encCtx : encodingContext) (left : Expr) (right : stackBufferIndexKey) = // TODO: check #do
            let rightRegion = (right :> IMemoryKey<stackBufferIndexKey, int points>).Region
            let rightExpr = x.EncodeTerm encCtx right.index
            let rightInRegion = x.KeyInIntPoints rightExpr (ctx.MkTrue()) rightRegion
            let keysAreEqual = ctx.MkEq(left, rightExpr)
            ctx.MkAnd(keysAreEqual, rightInRegion)

        member private x.EncodeMemoryAccessConstant (encCtx : encodingContext) (name : string) (source : IMemoryAccessConstantSource) typ term =
            let getRegionConstant mkConst regSort =
                let result : ArrayExpr ref = ref null
                if regionConstants.TryGetValue(regSort, result) then !result
                else
                    let regConst = mkConst()
                    regionConstants.Add(regSort, regConst)
                    regConst
            let memoryReading keysAreMatch encodeKey hasDefaultValue sort select key mo = // TODO: strange warning here #do
                let updates = MemoryRegion.flatten mo
                let memoryRegionConst =
                    let mkConst () =
                        if hasDefaultValue then ctx.MkConstArray(sort, ctx.MkNumeral(0, ctx.MkIntSort()))
                        else ctx.MkConst(name, sort) :?> ArrayExpr
                    GetHeapReadingRegionSort source |> getRegionConstant mkConst
                let inst = select memoryRegionConst (encodeKey key)
                let key' = encodeKey key // TODO: key must be in it's region! #do
                let res = List.fold (fun acc (k, v) -> ctx.MkITE(keysAreMatch key' k, (x.EncodeTerm encCtx v), acc)) inst updates
                res
            let heapReading key mo =
                let encodeKey (k : heapAddressKey) = x.EncodeTerm encCtx k.address
                let sort = ctx.MkArraySort(x.Type2Sort AddressType, x.Type2Sort typ)
                let select array (k : Expr) = ctx.MkSelect(array, k)
                let keysAreMatch = x.HeapAddressKeyMatch encCtx
                memoryReading keysAreMatch encodeKey false sort select key mo
            let arrayReading keysAreMatch encodeKey hasDefaultValue indices key mo =
                let sort = // TODO: mb Array.replicate (List.length indices + 1) (ctx.MkIntSort() :> Sort)?
                    let domainSort = x.Type2Sort AddressType :: List.map (ctx.MkIntSort() :> Sort |> always) indices |> Array.ofList
                    ctx.MkArraySort(domainSort, x.Type2Sort typ)
                let select array (k : Expr[]) = ctx.MkSelect(array, k)
                memoryReading keysAreMatch encodeKey hasDefaultValue sort select key mo
            let stackBufferReading key mo =
                let encodeKey (k : stackBufferIndexKey) = x.EncodeTerm encCtx k.index
                let sort = ctx.MkArraySort(x.Type2Sort AddressType, x.Type2Sort typ)
                let select array (k : Expr) = ctx.MkSelect(array, k)
                let keysAreMatch = x.stackBufferIndexKeyMatch encCtx
                memoryReading keysAreMatch encodeKey false sort select key mo
            let staticsReading (key : symbolicTypeKey) mo =
                let encodeKey (k : symbolicTypeKey) = __notImplemented__() // TODO: how to encode symbolicType? #do
                let sort = ctx.MkArraySort(ctx.MkIntSort(), x.Type2Sort typ)
                let select array (k : Expr) = ctx.MkSelect(array, k)
                let keysAreMatch = __notImplemented__()
                memoryReading keysAreMatch encodeKey false sort select key mo
            match source with // TODO: add caching #encode
            | HeapReading(key, mo) -> heapReading key mo
            | ArrayIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapArrayIndexKey) = k.address :: k.indices |> Array.ofList |> Array.map (x.EncodeTerm encCtx)
                let keysAreMatch = x.ArrayIndexKeyMatch encCtx
                arrayReading keysAreMatch encodeKey hasDefaultValue key.indices key mo
            | VectorIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapVectorIndexKey) = Array.map (x.EncodeTerm encCtx) [|k.address; k.index|]
                let keysAreMatch = x.VectorIndexKeyMatch encCtx
                arrayReading keysAreMatch encodeKey hasDefaultValue [key.index] key mo
            | StackBufferReading(key, mo) -> stackBufferReading key mo
            | StaticsReading(key, mo) -> staticsReading key mo
            | StructFieldSource field -> __notImplemented__() // TODO: #do
            | HeapAddressSource -> x.EncodeSymbolicAddress term name
            | _ -> x.CreateConstant name typ term

        member private x.EncodeConstant encCtx name (source : ISymbolicConstantSource) typ term =
            match source with
            | :? IMemoryAccessConstantSource as source -> x.EncodeMemoryAccessConstant encCtx name source typ term
            | _ -> x.CreateConstant name typ term

        member private x.EncodeExpression stopper (encCtx : encodingContext) term op args typ = // TODO: need stopper? delete? #do
            cache.Get term (fun () ->
                match op with
                | Operator operator ->
                    if stopper operator args then
                        let name = IdGenerator.startingWith "%tmp"
                        x.CreateConstant name typ term
                    else
                        match operator with
                        | OperationType.LogicalNeg -> x.MakeUnary stopper encCtx ctx.MkNot args :> Expr
                        | OperationType.LogicalAnd -> ctx.MkAnd(x.EncodeTerms stopper encCtx args) :> Expr
                        | OperationType.LogicalOr -> ctx.MkOr(x.EncodeTerms stopper encCtx args) :> Expr
                        | OperationType.Equal -> x.MakeBinary stopper encCtx ctx.MkEq args :> Expr
                        | OperationType.Greater -> x.MakeBinary stopper encCtx ctx.MkGt args :> Expr
                        | OperationType.GreaterOrEqual -> x.MakeBinary stopper encCtx ctx.MkGe args :> Expr
                        | OperationType.Less -> x.MakeBinary stopper encCtx ctx.MkLt args :> Expr
                        | OperationType.LessOrEqual -> x.MakeBinary stopper encCtx ctx.MkLe args :> Expr
                        | OperationType.Add -> ctx.MkAdd(x.EncodeTerms stopper encCtx args) :> Expr
                        | OperationType.Multiply -> ctx.MkMul(x.EncodeTerms stopper encCtx args) :> Expr
                        | OperationType.Subtract -> ctx.MkSub(x.EncodeTerms stopper encCtx args) :> Expr
                        | OperationType.Divide -> x.MakeBinary stopper encCtx ctx.MkDiv args :> Expr
                        | OperationType.Remainder -> x.MakeBinary stopper encCtx ctx.MkRem args :> Expr
                        | OperationType.UnaryMinus -> x.MakeUnary stopper encCtx ctx.MkUnaryMinus args :> Expr
                        | OperationType.Not -> x.MakeUnary stopper encCtx ctx.MkNot args :> Expr
                        | OperationType.ShiftLeft
                        | OperationType.ShiftRight -> __notImplemented__()
                        | _ -> __notImplemented__()
                | Application sf ->
                    let decl = ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, x.Type2Sort typ)
                    ctx.MkApp(decl, x.EncodeTerms stopper encCtx args)
                | Cast(Numeric _, Numeric _) -> x.EncodeTermExt stopper encCtx (List.head args) :> Expr
                | Cast _ ->
                    __notImplemented__())

        member private x.EncodeTermExt<'a when 'a :> Expr> (stopper : OperationType -> term list -> bool) encCtx (t : term) : 'a =
            match t.term with
            | Concrete(obj, typ) -> x.EncodeConcrete encCtx obj typ :?> 'a
            | Constant(name, source, typ) -> x.EncodeConstant encCtx name.v source typ t :?> 'a
            | Expression(op, args, typ) -> x.EncodeExpression stopper encCtx t op args typ :?> 'a
            | _ -> __notImplemented__() // TODO: need to encode HeapRef? #do

        member private this.MakeUnary<'a, 'b when 'a :> Expr and 'b :> Expr>
                (stopper : OperationType -> term list -> bool)
                (encCtx : encodingContext)
                (constructor : 'a -> 'b)
                (args : term list) : 'b =
            match args with
            | [x] -> constructor (this.EncodeTermExt<'a> stopper encCtx x)
            | _ -> internalfail "unary operation should have exactly one argument"

        member private this.MakeBinary<'a, 'b, 'c when 'a :> Expr and 'b :> Expr and 'c :> Expr>
                (stopper : OperationType -> term list -> bool)
                (encCtx : encodingContext)
                (constructor : 'a * 'b -> 'c)
                (args : term list) : 'c =
            match args with
            | [x; y] -> constructor(this.EncodeTermExt<'a> stopper encCtx x, this.EncodeTermExt<'b> stopper encCtx y)
            | _ -> internalfail "binary operation should have exactly two arguments"

        member private x.EncodeTerms<'a when 'a :> Expr> (stopper : OperationType -> term list -> bool) (encCtx : encodingContext) (ts : term seq) : 'a array =
            ts |> Seq.map (x.EncodeTermExt<'a> stopper encCtx) |> FSharp.Collections.Array.ofSeq

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
                let assumptions =
                    seq {
                        for i in 0 .. levelAtoms.Count - 1 do
                            let atom = levelAtoms.[i]
                            if atom <> null then
                                let lit = if i < Level.toInt q.lvl then atom else ctx.MkNot atom
                                yield lit :> Expr
                        yield query
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
                let encoded = PathConditionToSeq p.state.pc |> Seq.map (builder.EncodeTerm encCtx) |> ctx.MkAnd
                optCtx.Assert(ctx.MkImplies(pathAtom, encoded))
