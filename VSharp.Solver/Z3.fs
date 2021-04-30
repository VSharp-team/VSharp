namespace VSharp.Solver

open System
open System.Runtime.InteropServices
open Microsoft.Z3
open System.Collections.Generic
open VSharp
open VSharp.Core
open VSharp.Core.SolverInteraction
open Logger

module internal Z3 =

// ------------------------------- Exceptions -------------------------------

    type EncodingException(msg : string) =
        inherit Exception(msg)

    let failToEncode msg = raise (EncodingException msg)

// ---------------------------- Encoding result -----------------------------
    [<CustomEquality;NoComparison>]
    type encodingResult =
        // TODO: use type for assumptions -- add only if element is not True #do
        {expr : Expr; assumptions: BoolExpr list} // TODO: mb assumptions = Expr seq? #do
        static member Create(expr : 'a) = {expr = expr; assumptions = List.empty}
        static member Create(expr : 'a, conditions) = {expr = expr; assumptions = conditions}

        override x.GetHashCode() = x.expr.GetHashCode()

        override x.Equals(o : obj) =
            match o with
            | :? encodingResult as res -> x.expr = res.expr
            | _ -> false

    let toTuple encodingResult = encodingResult.assumptions, encodingResult.expr

// ------------------------------- Cache -------------------------------

    type private encodingCache = {
        sorts : IDictionary<symbolicType, Sort>
        e2t : IDictionary<Expr, term>
        t2e : IDictionary<term, encodingResult>
    } with
        member x.Get(term, encoder : unit -> Expr) =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = {expr = encoder(); assumptions = List.empty}
                x.e2t.[result.expr] <- term
                x.t2e.[term] <- result
                result)
        member x.Get(term, encoder : unit -> encodingResult) =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = encoder()
                x.e2t.[result.expr] <- term
                x.t2e.[term] <- result
                result)


    let private freshCache () = {
        sorts = Dictionary<symbolicType, Sort>()
        e2t = Dictionary<Expr, term>()
        t2e = Dictionary<term, encodingResult>()
    }

    let private regionConstants = Dictionary<regionSort * fieldId list, ArrayExpr>()

    let private getMemoryConstant mkConst (typ : regionSort * fieldId list) =
        let result : ArrayExpr ref = ref null
        if regionConstants.TryGetValue(typ, result) then !result
        else
            let regConst = mkConst()
            regionConstants.Add(typ, regConst)
            regConst

// ------------------------------- Encoding: primitives -------------------------------

    // TODO: use unified architecture for any solver #do
    type private Z3Builder(ctx : Context) =
        let cache = freshCache()

        member private x.ValidateId id =
            assert(not <| String.IsNullOrWhiteSpace id)
            if Char.IsDigit id.[0] then "_" + id else id

        member private x.AddressSort = ctx.MkBitVecSort(32u) :> Sort

        member private x.Type2Sort typ = // TODO: unsigned encoded to signed, so need to use bitvecsize + 1? #do
            Dict.getValueOrUpdate cache.sorts typ (fun () ->
                match typ with
                | Bool -> ctx.MkBoolSort() :> Sort
                | Numeric(Id typ) when typ.IsEnum -> ctx.MkBitVecSort(TypeUtils.numericSizeOf typ) :> Sort
                | Numeric(Id typ) as t when Types.IsInteger t -> ctx.MkBitVecSort(TypeUtils.numericSizeOf typ) :> Sort
                | Numeric _ as t when Types.IsReal t -> failToEncode "encoding real numbers is not implemented"
                | AddressType -> x.AddressSort
                | StructType _ -> internalfailf "struct should not appear while encoding! type: %O" typ
                | Numeric _ -> __notImplemented__()
                | ArrayType _
                | Void
                | Null
                | ClassType _
                | InterfaceType _
                | TypeVariable _
                | ByRef _
                | Pointer _ -> __unreachable__())

        member private x.True = ctx.MkTrue()

        member private x.MkEq(left, right) =
            if left = right then x.True
            else ctx.MkEq(left, right)

        member private x.MkAnd(left, right) =
            if left = x.True then right
            elif right = x.True then left
            else ctx.MkAnd(left, right)

        member x.MkAnd ([<ParamArray>] elems) = // TODO: array or seq? #do
            let nonTrueElems = Array.filter (fun elem -> elem <> x.True) elems
            match nonTrueElems with
            | Seq.Empty -> x.True
            | Seq.Cons(head, tail) when Seq.isEmpty tail -> head
            | _ -> ctx.MkAnd(nonTrueElems)

        member private x.DefaultValue sort = ctx.MkNumeral(0, sort)
        member private x.EncodeConcreteAddress encCtx (address : concreteHeapAddress) =
            ctx.MkNumeral(encCtx.addressOrder.[address], x.Type2Sort AddressType)

        member private x.CreateConstant name typ =
            ctx.MkConst(x.ValidateId name, x.Type2Sort typ) |> encodingResult.Create

// ------------------------------- Encoding: common -------------------------------

        member public x.EncodeTerm encCtx (t : term) : encodingResult =
            x.EncodeTermExt (fun _ _ -> false) encCtx t

        member private x.EncodeTermExt (stopper : OperationType -> term list -> bool) encCtx (t : term) : encodingResult =
            let getResult () =
                match t.term with
                | Concrete(obj, typ) -> x.EncodeConcrete encCtx obj typ
                | Constant(name, source, typ) -> x.EncodeConstant encCtx name.v source typ
                | Expression(op, args, typ) -> x.EncodeExpression stopper encCtx t op args typ
                | _ -> __notImplemented__()
            cache.Get(t, getResult)

        member private x.EncodeConcrete encCtx (obj : obj) typ : encodingResult =
            let expr =
                match typ with
                | Bool -> ctx.MkBool(obj :?> bool) :> Expr
                | Numeric(Id t) when t = typeof<char> -> ctx.MkNumeral(Convert.ToInt32(obj :?> char) |> toString, x.Type2Sort typ)
                | Numeric(Id t) when t.IsEnum -> ctx.MkNumeral(Convert.ChangeType(obj, t.GetEnumUnderlyingType()) |> toString, x.Type2Sort typ)
                | Numeric _ -> ctx.MkNumeral(toString obj, x.Type2Sort typ)
                | AddressType ->
                    match obj with
                    | :? concreteHeapAddress as addr ->
                        assert(List.isEmpty addr |> not)
                        x.EncodeConcreteAddress encCtx addr
                    | _ -> __unreachable__()
                | _ -> __notImplemented__()
            encodingResult.Create(expr)

        member private x.EncodeConstant encCtx name (source : ISymbolicConstantSource) typ : encodingResult =
            match source with
            | :? IMemoryAccessConstantSource as source -> x.EncodeMemoryAccessConstant encCtx name source List.empty typ
            | _ -> x.CreateConstant name typ

// ------------------------------- Encoding: expression -------------------------------

        member private x.EncodeExpression stopper encCtx term op args typ = // TODO: need stopper? delete? #do
            cache.Get(term, fun () ->
                match op with
                | Operator operator ->
                    if stopper operator args then
                        let name = IdGenerator.startingWith "%tmp"
                        x.CreateConstant name typ
                    else
                        match operator with
                        | BitwiseNot -> x.MakeUnary stopper encCtx ctx.MkBVNot args
                        | BitwiseAnd -> x.MakeBinary stopper encCtx ctx.MkBVAND args
                        | BitwiseOr -> x.MakeBinary stopper encCtx ctx.MkBVOR args
                        | BitwiseXor -> x.MakeBinary stopper encCtx ctx.MkBVXOR args
                        | ShiftLeft -> x.MakeBinary stopper encCtx ctx.MkBVSHL args
                        | ShiftRight -> x.MakeBinary stopper encCtx ctx.MkBVLSHR args
                        | LogicalNot -> x.MakeUnary stopper encCtx ctx.MkNot args
                        | LogicalAnd -> x.MakeOperation stopper encCtx x.MkAnd args
                        | LogicalOr -> x.MakeOperation stopper encCtx ctx.MkOr args
                        | LogicalXor -> x.MakeOperation stopper encCtx ctx.MkXor args
                        | Equal -> x.MakeBinary stopper encCtx x.MkEq args
                        | NotEqual -> x.MakeBinary stopper encCtx (ctx.MkNot << x.MkEq) args
                        | Greater -> x.MakeBinary stopper encCtx ctx.MkBVSGT args
                        | GreaterOrEqual -> x.MakeBinary stopper encCtx ctx.MkBVSGE args
                        | Less -> x.MakeBinary stopper encCtx ctx.MkBVSLT args
                        | LessOrEqual -> x.MakeBinary stopper encCtx ctx.MkBVSLE args
                        | Add -> x.MakeBinary stopper encCtx ctx.MkBVAdd args
                        | Multiply -> x.MakeBinary stopper encCtx ctx.MkBVMul args
                        | Subtract -> x.MakeBinary stopper encCtx ctx.MkBVSub args
                        | Divide -> x.MakeBinary stopper encCtx ctx.MkBVSDiv args
                        | Remainder -> x.MakeBinary stopper encCtx ctx.MkBVSRem args
                        | UnaryMinus -> x.MakeUnary stopper encCtx ctx.MkBVNeg args
                | Application sf ->
                    let decl = ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, x.Type2Sort typ)
                    x.MakeOperation stopper encCtx (fun x -> ctx.MkApp(decl, x)) args
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.isLessForNumericTypes t1 t2 ->
                    let expr = x.EncodeTermExt stopper encCtx (List.head args)
                    let difference = TypeUtils.numericSizeOf t2 - TypeUtils.numericSizeOf t1
                    {expr = ctx.MkSignExt(difference, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.isLessForNumericTypes t2 t1 -> // TODO: what bits to slice? #do
                    let expr = x.EncodeTermExt stopper encCtx (List.head args)
                    let from = TypeUtils.numericSizeOf t2 - 1u
                    {expr = ctx.MkExtract(from, 0u, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.isReal t1 || TypeUtils.isReal t2 ->
                    failToEncode "encoding real numbers is not implemented"
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.numericSizeOf t1 = TypeUtils.numericSizeOf t2 ->
                    x.EncodeTermExt stopper encCtx (List.head args)
                | Cast _ ->
                    __notImplemented__())

        member private this.MakeUnary<'a, 'b when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality>
                (stopper : OperationType -> term list -> bool)
                (encCtx : encodingContext)
                (constructor : 'a -> 'b)
                (args : term list) : encodingResult =
            match args with
            | [x] ->
                let result = this.EncodeTermExt stopper encCtx x
                {expr = constructor (result.expr :?> 'a); assumptions = result.assumptions}
            | _ -> internalfail "unary operation should have exactly one argument"

        member private this.MakeBinary<'a, 'b, 'c when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality and 'c :> Expr and 'c : equality>
                (stopper : OperationType -> term list -> bool)
                (encCtx : encodingContext)
                (constructor : 'a * 'b -> 'c)
                (args : term list) : encodingResult =
            match args with
            | [x; y] ->
                let x' = this.EncodeTermExt stopper encCtx x
                let y' = this.EncodeTermExt stopper encCtx y
                {expr = constructor(x'.expr :?> 'a, y'.expr :?> 'b); assumptions = x'.assumptions @ y'.assumptions}
            | _ -> internalfail "binary operation should have exactly two arguments"

        member private this.MakeOperation<'a, 'b when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality>
                (stopper : OperationType -> term list -> bool)
                (encCtx : encodingContext)
                (constructor : 'a [] -> 'b)
                (args : term list) : encodingResult =
            let assumptions, expressions = this.EncodeTerms stopper encCtx args
            {expr = constructor expressions; assumptions = assumptions}

        member internal x.EncodeTerms<'a when 'a :> Expr and 'a : equality> stopper encCtx (ts : term seq) : BoolExpr list * 'a array =
            let encodeOne acc term =
                let result = x.EncodeTermExt stopper encCtx term
                result.expr :?> 'a, acc @ result.assumptions
            let expressions, assumptions = Seq.mapFold encodeOne List.empty ts
            assumptions, Array.ofSeq expressions

// ------------------------------- Encoding: memory reading -------------------------------

        member private x.EncodeSymbolicAddress encCtx (heapRefSource : IMemoryAccessConstantSource) structFields name =
            x.EncodeMemoryAccessConstant encCtx name heapRefSource structFields AddressType

        member private x.KeyInVectorTimeIntervals encCtx (key : Expr) acc (region : vectorTime intervals) =
            let onePointCondition acc (y : vectorTime endpoint) =
                let bound = ctx.MkNumeral(encCtx.addressOrder.[y.elem], x.Type2Sort AddressType) :?> BitVecExpr
                let condition =
                    match y.sort with
                    | OpenRight -> ctx.MkBVSLT(key :?> BitVecExpr, bound)
                    | ClosedRight -> ctx.MkBVSLE(key :?> BitVecExpr, bound)
                    | OpenLeft -> ctx.MkBVSGT(key :?> BitVecExpr, bound)
                    | ClosedLeft -> ctx.MkBVSGE(key :?> BitVecExpr, bound)
                x.MkAnd(acc, condition)
            let intervalWithoutLeftZeroBound = List.except [{elem = VectorTime.zero; sort = ClosedLeft}] region.points
            List.fold onePointCondition acc intervalWithoutLeftZeroBound

        member private x.HeapAddressKeyInRegion encCtx acc (key : heapAddressKey) (keyExpr : Expr) =
            let region = (key :> IMemoryKey<heapAddressKey, vectorTime intervals>).Region
            x.KeyInVectorTimeIntervals encCtx keyExpr acc region

        member private x.KeyInIntPoints key acc (region : int points) =
            let points, operation =
                match region with
                | {points = points; thrown = true} -> points, ctx.MkNot << x.MkEq
                | {points = points; thrown = false} -> points, x.MkEq
            let handleOne acc (point : int) =
                let pointExpr = ctx.MkNumeral(point, x.Type2Sort Types.IndexType)
                x.MkAnd(acc, operation(key, pointExpr))
            PersistentSet.fold handleOne acc points

        member private x.KeyInProductRegion keyInFst keyInSnd acc (region : productRegion<'a, 'b>) =
            let checkKeyInOne acc (fst, snd) = x.MkAnd(acc, keyInFst acc fst, keyInSnd acc snd)
            List.fold checkKeyInOne acc region.products

        member private x.KeysInIntPointsListProductRegion keys acc (region : int points listProductRegion) =
            match region, keys with
            | NilRegion, Seq.Empty -> acc
            | ConsRegion products, Seq.Cons(key, others) ->
                let keyInPoints = x.KeyInIntPoints key
                let keysInProductList = x.KeysInIntPointsListProductRegion others
                x.KeyInProductRegion keyInPoints keysInProductList acc products
            | _ -> __unreachable__()

        member private x.ArrayIndexKeyInRegion encCtx acc (key : heapArrayIndexKey) (keyExpr : Expr[]) = // TODO: check #do
            assert(Array.length keyExpr = List.length key.indices + 1)
            let region = (key :> IMemoryKey<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>>).Region
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx (Array.head keyExpr)
            let indicesInRegion = x.KeysInIntPointsListProductRegion (Array.tail keyExpr)
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.VectorIndexKeyInRegion encCtx acc (key : heapVectorIndexKey) (keyExpr : Expr[]) = // TODO: check #do
            assert(Array.length keyExpr = 2)
            let region = (key :> IMemoryKey<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>).Region
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx keyExpr.[0]
            let indicesInRegion = x.KeyInIntPoints keyExpr.[1]
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.stackBufferIndexKeyInRegion acc (key : stackBufferIndexKey) keyExpr = // TODO: check #do
            let region = (key :> IMemoryKey<stackBufferIndexKey, int points>).Region
            x.KeyInIntPoints keyExpr acc region

        member private x.GetRegionConstant hasDefaultValue (name : string) makeSort (structFields : fieldId list) typ (regSort : regionSort) =
            let valueSort = x.Type2Sort typ
            let sort = makeSort valueSort
            let mkConst () =
                if hasDefaultValue then ctx.MkConstArray(sort, x.DefaultValue valueSort)
                else ctx.MkConst(name, sort) :?> ArrayExpr
            getMemoryConstant mkConst (regSort, structFields)

        // TODO: delete some arguments #do
        member private x.MemoryReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue makeSort select left mo source structFields name typ =
            let updates = MemoryRegion.flatten mo
            let memoryRegionConst = GetHeapReadingRegionSort source |> x.GetRegionConstant hasDefaultValue name makeSort structFields typ
            let assumptions, leftExpr = encodeKey left
            let leftInRegion = keyInRegion x.True left leftExpr
            let assumptions = leftInRegion :: assumptions
            let inst = select memoryRegionConst leftExpr
            let checkOneKey (acc, assumptions) (right, value) =
                let rightAssumptions, rightExpr = encodeKey right
                let assumptions = List.append assumptions rightAssumptions // TODO: make better (mb monad) #do
                // NOTE: we need constraints on right key, because value may contain it // TODO: ? #do
                let keysEquality = keysAreEqual(leftExpr, rightExpr)
                let keysAreMatch = keyInRegion keysEquality right rightExpr
                let valueExpr = x.EncodeTerm encCtx value
                let assumptions = List.append assumptions valueExpr.assumptions
                ctx.MkITE(keysAreMatch, valueExpr.expr, acc), assumptions
            let expr, assumptions = List.fold checkOneKey (inst, assumptions) updates
            encodingResult.Create(expr, assumptions)

        member private x.HeapReading encCtx key mo typ source structFields name =
            let encodeKey (k : heapAddressKey) = x.EncodeTerm encCtx k.address |> toTuple
            let makeSort valueSort = ctx.MkArraySort(x.Type2Sort AddressType, valueSort)
            let select array (k : Expr) = ctx.MkSelect(array, k)
            let keyInRegion = x.HeapAddressKeyInRegion encCtx
            x.MemoryReading encCtx keyInRegion x.MkEq encodeKey false makeSort select key mo source structFields name typ

        member private x.ArrayReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue indices key mo typ source structFields name =
            let domainSort = x.Type2Sort AddressType :: List.map (x.Type2Sort Types.IndexType |> always) indices |> Array.ofList
            let makeSort valueSort =
                ctx.MkArraySort(domainSort, valueSort)
            let select array (k : Expr[]) = ctx.MkSelect(array, k)
            x.MemoryReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue makeSort select key mo source structFields name typ

        member private x.StackBufferReading encCtx key mo typ source structFields name =
            let encodeKey (k : stackBufferIndexKey) = x.EncodeTerm encCtx k.index |> toTuple
            let makeSort valueType = ctx.MkArraySort(x.Type2Sort AddressType, valueType)
            let select array (k : Expr) = ctx.MkSelect(array, k)
            x.MemoryReading encCtx x.stackBufferIndexKeyInRegion x.MkEq encodeKey false makeSort select key mo source structFields name typ

        member private x.StaticsReading encCtx key mo typ source structFields (name : string) = // TODO: redo #do
            let keyType = x.Type2Sort Types.IndexType
            let memoryRegionConst = ctx.MkArrayConst(name, keyType, x.Type2Sort typ)
            let updates = MemoryRegion.flatten mo
            let value = Seq.tryFind (fun (k, _) -> k = key) updates
            match value with
            | Some (_, v) -> x.EncodeTerm encCtx v
            | None -> {expr = ctx.MkSelect(memoryRegionConst, ctx.MkConst(key.ToString(), keyType)); assumptions = List.empty}

        member private x.StructReading encCtx (structSource : IMemoryAccessConstantSource) (field : fieldId) typ structFields name =
            x.EncodeMemoryAccessConstant encCtx name structSource (field :: structFields) typ

        member private x.EncodeMemoryAccessConstant encCtx name (source : IMemoryAccessConstantSource) (structFields : fieldId list) typ : encodingResult =
            match source with // TODO: add caching #encode
            | HeapReading(key, mo) -> x.HeapReading encCtx key mo typ source structFields name
            | ArrayIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapArrayIndexKey) =
                    k.address :: k.indices |> x.EncodeTerms (fun _ _ -> false) encCtx
                let keyInRegion = x.ArrayIndexKeyInRegion encCtx
                let arraysEquality (left, right) =
                    Seq.zip left right |> Seq.fold (fun acc (left, right) -> x.MkAnd(acc, x.MkEq(left, right))) x.True
                x.ArrayReading encCtx keyInRegion arraysEquality encodeKey hasDefaultValue key.indices key mo typ source structFields name
            | VectorIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapVectorIndexKey) =
                    [|k.address; k.index|] |> x.EncodeTerms (fun _ _ -> false) encCtx
                let keyInRegion = x.VectorIndexKeyInRegion encCtx
                let keysAreEqual (left : Expr[], right : Expr[]) =
                    x.MkAnd(x.MkEq(left.[0], right.[0]), x.MkEq(left.[1], right.[1]))
                x.ArrayReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue [key.index] key mo typ source structFields name
            | StackBufferReading(key, mo) -> x.StackBufferReading encCtx key mo typ source structFields name
            | StaticsReading(key, mo) -> x.StaticsReading encCtx key mo typ source structFields name
            | StructFieldSource(structSource, field) -> x.StructReading encCtx structSource field typ structFields name
            | HeapAddressSource source ->
                assert(typ = AddressType)
                x.EncodeSymbolicAddress encCtx source structFields name
            | _ -> x.CreateConstant name typ

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
                    elif expr.IsNot then x.DecodeBoolExpr OperationType.LogicalNot expr
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
                try
                    try
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
                    | :? Z3Exception
                    | :? EncodingException as e ->
                        printLog Info "SOLVER: exception was thrown: %s" e.Message
                        SmtUnknown (sprintf "Z3 has thrown an exception: %s" e.Message)
                finally
//                    optCtx.Pop() // TODO: need this? #do
                    ()

            member x.Assert encCtx (lvl : level) (fml : formula) =
                printLog Trace "SOLVER: [lvl %O] Asserting (hard):" lvl
                printLogLazy Trace "%s" (lazy(fml.ToString()))
                let encoded = builder.EncodeTerm encCtx fml
                let encoded = List.fold (fun acc x -> builder.MkAnd(acc, x)) (encoded.expr :?> BoolExpr) encoded.assumptions
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
                let encodedWithAssumptions = Seq.append assumptions encoded |> Array.ofSeq
                let encoded = builder.MkAnd encodedWithAssumptions
                optCtx.Assert(ctx.MkImplies(pathAtom, encoded))
