namespace VSharp.Solver

open System
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
        // TODO: use new type for assumptions -- add only if element is not True
        {expr : Expr; assumptions: BoolExpr list}
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

    let private solverResultCache = Dictionary<query, smtResult>()

    let private getSolverResult mkResult (q : query) =
        let result : smtResult ref = ref (SmtUnknown "not ready")
        if solverResultCache.TryGetValue(q, result) then !result
        else
            let result = mkResult()
            solverResultCache.Add(q, result)
            result


    let private regionConstants = Dictionary<regionSort * fieldId list, ArrayExpr>()

    let private getMemoryConstant mkConst (typ : regionSort * fieldId list) =
        let result : ArrayExpr ref = ref null
        if regionConstants.TryGetValue(typ, result) then !result
        else
            let regConst = mkConst()
            regionConstants.Add(typ, regConst)
            regConst

// ------------------------------- Encoding: primitives -------------------------------

    type private Z3Builder(ctx : Context) =
        let encodingCache = freshCache()

        member private x.ValidateId id =
            assert(not <| String.IsNullOrWhiteSpace id)
            if Char.IsDigit id.[0] then "_" + id else id

        member private x.AddressSort = ctx.MkBitVecSort(32u) :> Sort

        member private x.Type2Sort typ =
            Dict.getValueOrUpdate encodingCache.sorts typ (fun () ->
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
            let getResult () =
                match t.term with
                | Concrete(obj, typ) -> x.EncodeConcrete encCtx obj typ
                | Constant(name, source, typ) -> x.EncodeConstant encCtx name.v source typ
                | Expression(op, args, typ) -> x.EncodeExpression encCtx t op args typ
                | HeapRef(address, _) -> x.EncodeTerm encCtx address
                | _ -> __notImplemented__()
            encodingCache.Get(t, getResult)

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

        // [NOTE] using signed extend, because both arguments of shift are signed (always)
        member private x.ExtendIfNeed (x : BitVecExpr, y : BitVecExpr as args) =
            let difference = int x.SortSize - int y.SortSize
            if difference = 0 then args
            elif difference > 0 then
                x, ctx.MkSignExt(uint32 difference, y)
            else
                ctx.MkSignExt(uint32 -difference, x), y

        member private x.EncodeExpression encCtx term op args typ =
            encodingCache.Get(term, fun () ->
                match op with
                | Operator operator ->
                    match operator with
                    | OperationType.BitwiseNot -> x.MakeUnary encCtx ctx.MkBVNot args
                    | OperationType.BitwiseAnd -> x.MakeBinary encCtx ctx.MkBVAND args
                    | OperationType.BitwiseOr -> x.MakeBinary encCtx ctx.MkBVOR args
                    | OperationType.BitwiseXor -> x.MakeBinary encCtx ctx.MkBVXOR args
                    // [NOTE] IL specifies: arguments of SHL, SHR, SHR.UN are (int32 and int32), (int64 and int32)
                    // So it's needed to extend one of them for Z3
                    | OperationType.ShiftLeft -> x.MakeBinary encCtx (x.ExtendIfNeed >> ctx.MkBVSHL) args
                    | OperationType.ShiftRight -> x.MakeBinary encCtx (x.ExtendIfNeed >> ctx.MkBVASHR) args
                    | OperationType.ShiftRight_Un -> x.MakeBinary encCtx (x.ExtendIfNeed >> ctx.MkBVLSHR) args
                    | OperationType.LogicalNot -> x.MakeUnary encCtx ctx.MkNot args
                    | OperationType.LogicalAnd -> x.MakeOperation encCtx x.MkAnd args
                    | OperationType.LogicalOr -> x.MakeOperation encCtx ctx.MkOr args
                    | OperationType.LogicalXor -> x.MakeOperation encCtx ctx.MkXor args
                    | OperationType.Equal -> x.MakeBinary encCtx x.MkEq args
                    | OperationType.NotEqual -> x.MakeBinary encCtx (ctx.MkNot << x.MkEq) args
                    | OperationType.Greater -> x.MakeBinary encCtx ctx.MkBVSGT args
                    | OperationType.Greater_Un -> x.MakeBinary encCtx ctx.MkBVUGT args
                    | OperationType.GreaterOrEqual -> x.MakeBinary encCtx ctx.MkBVSGE args
                    | OperationType.GreaterOrEqual_Un -> x.MakeBinary encCtx ctx.MkBVUGE args
                    | OperationType.Less -> x.MakeBinary encCtx ctx.MkBVSLT args
                    | OperationType.Less_Un -> x.MakeBinary encCtx ctx.MkBVULT args
                    | OperationType.LessOrEqual -> x.MakeBinary encCtx ctx.MkBVSLE args
                    | OperationType.LessOrEqual_Un -> x.MakeBinary encCtx ctx.MkBVULE args
                    | OperationType.Add -> x.MakeBinary encCtx ctx.MkBVAdd args
                    | OperationType.Multiply -> x.MakeBinary encCtx ctx.MkBVMul args
                    | OperationType.Subtract -> x.MakeBinary encCtx ctx.MkBVSub args
                    | OperationType.Divide -> x.MakeBinary encCtx ctx.MkBVSDiv args
                    | OperationType.Divide_Un -> x.MakeBinary encCtx ctx.MkBVUDiv args
                    | OperationType.Remainder -> x.MakeBinary encCtx ctx.MkBVSRem args
                    | OperationType.Remainder_Un -> x.MakeBinary encCtx ctx.MkBVURem args
                    | OperationType.UnaryMinus -> x.MakeUnary encCtx ctx.MkBVNeg args
                    | _ -> __unreachable__()
                | Application sf ->
                    let decl = ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, x.Type2Sort typ)
                    x.MakeOperation encCtx (fun x -> ctx.MkApp(decl, x)) args
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.isLessForNumericTypes t1 t2 ->
                    let expr = x.EncodeTerm encCtx (List.head args)
                    let difference = TypeUtils.numericSizeOf t2 - TypeUtils.numericSizeOf t1
                    let extend = if TypeUtils.isUnsigned t2 then ctx.MkZeroExt else ctx.MkSignExt
                    {expr = extend(difference, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.isLessForNumericTypes t2 t1 ->
                    let expr = x.EncodeTerm encCtx (List.head args)
                    let from = TypeUtils.numericSizeOf t2 - 1u
                    {expr = ctx.MkExtract(from, 0u, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.isReal t1 || TypeUtils.isReal t2 ->
                    failToEncode "encoding real numbers is not implemented"
                | Cast(Numeric (Id t1), Numeric (Id t2)) when TypeUtils.numericSizeOf t1 = TypeUtils.numericSizeOf t2 ->
                    x.EncodeTerm encCtx (List.head args)
                | Cast _ ->
                    __notImplemented__())

        member private this.MakeUnary<'a, 'b when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality>
                (encCtx : encodingContext)
                (constructor : 'a -> 'b)
                (args : term list) : encodingResult =
            match args with
            | [x] ->
                let result = this.EncodeTerm encCtx x
                {expr = constructor (result.expr :?> 'a); assumptions = result.assumptions}
            | _ -> internalfail "unary operation should have exactly one argument"

        member private this.MakeBinary<'a, 'b, 'c when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality and 'c :> Expr and 'c : equality>
                (encCtx : encodingContext)
                (constructor : 'a * 'b -> 'c)
                (args : term list) : encodingResult =
            match args with
            | [x; y] ->
                let x' = this.EncodeTerm encCtx x
                let y' = this.EncodeTerm encCtx y
                {expr = constructor(x'.expr :?> 'a, y'.expr :?> 'b); assumptions = x'.assumptions @ y'.assumptions}
            | _ -> internalfail "binary operation should have exactly two arguments"

        member private this.MakeOperation<'a, 'b when 'a :> Expr and 'a : equality and 'b :> Expr and 'b : equality>
                (encCtx : encodingContext)
                (constructor : 'a [] -> 'b)
                (args : term list) : encodingResult =
            let assumptions, expressions = this.EncodeTerms encCtx args
            {expr = constructor expressions; assumptions = assumptions}

        member internal x.EncodeTerms<'a when 'a :> Expr and 'a : equality> encCtx (ts : term seq) : BoolExpr list * 'a array =
            let encodeOne acc term =
                let result = x.EncodeTerm encCtx term
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
                    | endpointSort.OpenRight -> ctx.MkBVSLT(key :?> BitVecExpr, bound)
                    | endpointSort.ClosedRight -> ctx.MkBVSLE(key :?> BitVecExpr, bound)
                    | endpointSort.OpenLeft -> ctx.MkBVSGT(key :?> BitVecExpr, bound)
                    | endpointSort.ClosedLeft -> ctx.MkBVSGE(key :?> BitVecExpr, bound)
                    | _ -> __unreachable__()
                x.MkAnd(acc, condition)
            let intervalWithoutLeftZeroBound = List.except [{elem = VectorTime.zero; sort = endpointSort.ClosedLeft}] region.points
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

        member private x.ArrayIndexKeyInRegion encCtx acc (key : heapArrayIndexKey) (keyExpr : Expr[]) =
            assert(Array.length keyExpr = List.length key.indices + 1)
            let region = (key :> IMemoryKey<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>>).Region
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx (Array.head keyExpr)
            let indicesInRegion = x.KeysInIntPointsListProductRegion (Array.tail keyExpr)
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.VectorIndexKeyInRegion encCtx acc (key : heapVectorIndexKey) (keyExpr : Expr[]) =
            assert(Array.length keyExpr = 2)
            let region = (key :> IMemoryKey<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>).Region
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx keyExpr.[0]
            let indicesInRegion = x.KeyInIntPoints keyExpr.[1]
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.stackBufferIndexKeyInRegion acc (key : stackBufferIndexKey) keyExpr =
            let region = (key :> IMemoryKey<stackBufferIndexKey, int points>).Region
            x.KeyInIntPoints keyExpr acc region

        member private x.GetRegionConstant (name : string) sort (structFields : fieldId list) (regSort : regionSort) =
            let mkConst () = ctx.MkConst(name, sort) :?> ArrayExpr
            getMemoryConstant mkConst (regSort, structFields)

        member private x.MemoryReading encCtx keyInRegion keysAreEqual encodeKey inst left mo =
            let updates = MemoryRegion.flatten mo
            let assumptions, leftExpr = encodeKey left
            let leftInRegion = keyInRegion x.True left leftExpr
            let assumptions = leftInRegion :: assumptions
            let inst = inst leftExpr
            let checkOneKey (acc, assumptions) (right, value) =
                let rightAssumptions, rightExpr = encodeKey right
                // TODO: [style] auto append assumptions
                let assumptions = List.append assumptions rightAssumptions
                // NOTE: we need constraints on right key, because value may contain it
                let keysEquality = keysAreEqual(leftExpr, rightExpr)
                let keysAreMatch = keyInRegion keysEquality right rightExpr
                let valueExpr = x.EncodeTerm encCtx value
                let assumptions = List.append assumptions valueExpr.assumptions
                ctx.MkITE(keysAreMatch, valueExpr.expr, acc), assumptions
            let expr, assumptions = List.fold checkOneKey (inst, assumptions) updates
            encodingResult.Create(expr, assumptions)

        member private x.HeapReading encCtx key mo typ source structFields name =
            let encodeKey (k : heapAddressKey) = x.EncodeTerm encCtx k.address |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort AddressType, x.Type2Sort typ)
            let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
            let inst (k : Expr) = ctx.MkSelect(array, k)
            let keyInRegion = x.HeapAddressKeyInRegion encCtx
            x.MemoryReading encCtx keyInRegion x.MkEq encodeKey inst key mo

        member private x.ArrayReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue indices key mo typ source structFields name =
            let domainSort = x.Type2Sort AddressType :: List.map (x.Type2Sort Types.IndexType |> always) indices |> Array.ofList
            let valueSort = x.Type2Sort typ
            let inst (k : Expr[]) =
                if hasDefaultValue then x.DefaultValue valueSort
                else
                    let sort = ctx.MkArraySort(domainSort, valueSort)
                    let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
                    ctx.MkSelect(array, k)
            x.MemoryReading encCtx keyInRegion keysAreEqual encodeKey inst key mo

        member private x.StackBufferReading encCtx key mo typ source structFields name =
            let encodeKey (k : stackBufferIndexKey) = x.EncodeTerm encCtx k.index |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort AddressType, x.Type2Sort typ)
            let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
            let inst (k : Expr) = ctx.MkSelect(array, k)
            x.MemoryReading encCtx x.stackBufferIndexKeyInRegion x.MkEq encodeKey inst key mo

        member private x.StaticsReading encCtx key mo typ source structFields (name : string) =
            let keyType = x.Type2Sort Types.IndexType
            let sort = ctx.MkArraySort(keyType, x.Type2Sort typ)
            let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
            let updates = MemoryRegion.flatten mo
            let value = Seq.tryFind (fun (k, _) -> k = key) updates
            match value with
            | Some (_, v) -> x.EncodeTerm encCtx v
            | None -> {expr = ctx.MkSelect(array, ctx.MkConst(key.ToString(), keyType)); assumptions = List.empty}

        member private x.StructReading encCtx (structSource : IMemoryAccessConstantSource) (field : fieldId) typ structFields name =
            x.EncodeMemoryAccessConstant encCtx name structSource (field :: structFields) typ

        member private x.EncodeMemoryAccessConstant encCtx name (source : IMemoryAccessConstantSource) (structFields : fieldId list) typ : encodingResult =
            match source with
            | HeapReading(key, mo) -> x.HeapReading encCtx key mo typ source structFields name
            | ArrayIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapArrayIndexKey) =
                    k.address :: k.indices |> x.EncodeTerms encCtx
                let keyInRegion = x.ArrayIndexKeyInRegion encCtx
                let arraysEquality (left, right) =
                    Seq.zip left right |> Seq.fold (fun acc (left, right) -> x.MkAnd(acc, x.MkEq(left, right))) x.True
                x.ArrayReading encCtx keyInRegion arraysEquality encodeKey hasDefaultValue key.indices key mo typ source structFields name
            | VectorIndexReading(hasDefaultValue, key, mo) ->
                let encodeKey (k : heapVectorIndexKey) =
                    [|k.address; k.index|] |> x.EncodeTerms encCtx
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

        member private x.GetTypeOfBV (bv : BitVecExpr) =
            let dotNetType =
                if bv.SortSize = 32u then typeof<int32>
                elif bv.SortSize = 64u then typeof<int64>
                else __unreachable__()
            Numeric (Id dotNetType)

        member public x.Decode (expr : Expr) =
            if encodingCache.e2t.ContainsKey(expr) then encodingCache.e2t.[expr]
            else
                match expr with
                | :? BitVecNum as bv -> x.GetTypeOfBV bv |> Concrete bv.Int64
                | :? BitVecExpr as bv when bv.IsConst -> x.GetTypeOfBV bv |> Concrete expr.String
                | :? IntNum as i -> Concrete i.Int (Numeric (Id typeof<int>))
                | :? RatNum as r -> Concrete (double(r.Numerator.Int) * 1.0 / double(r.Denominator.Int)) (Numeric (Id typeof<int>))
                | _ ->
                    if expr.IsTrue then True
                    elif expr.IsFalse then False
                    elif expr.IsNot then x.DecodeBoolExpr OperationType.LogicalNot expr
                    elif expr.IsAnd then x.DecodeBoolExpr OperationType.LogicalAnd expr
                    elif expr.IsOr then x.DecodeBoolExpr OperationType.LogicalOr expr
                    elif expr.IsEq then x.DecodeBoolExpr OperationType.Equal expr
                    elif expr.IsBVSGT then x.DecodeBoolExpr OperationType.Greater expr
                    elif expr.IsBVUGT then x.DecodeBoolExpr OperationType.Greater_Un expr
                    elif expr.IsBVSGE then x.DecodeBoolExpr OperationType.GreaterOrEqual expr
                    elif expr.IsBVUGE then x.DecodeBoolExpr OperationType.GreaterOrEqual_Un expr
                    elif expr.IsBVSLT then x.DecodeBoolExpr OperationType.Less expr
                    elif expr.IsBVULT then x.DecodeBoolExpr OperationType.Less_Un expr
                    elif expr.IsBVSLE then x.DecodeBoolExpr OperationType.LessOrEqual expr
                    elif expr.IsBVULE then x.DecodeBoolExpr OperationType.LessOrEqual_Un expr
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
                let mkResult () =
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
                                for i in 0 .. assumptions.Length - 1 do
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
                        | Status.UNSATISFIABLE ->
                            SmtUnsat { core = optCtx.UnsatCore |> Array.map builder.Decode }
                        | Status.UNKNOWN ->
                            printLog Trace "SOLVER: reason: %O" <| optCtx.ReasonUnknown
                            SmtUnknown optCtx.ReasonUnknown
                        | _ -> __unreachable__()
                    with
                    | :? EncodingException as e ->
                        printLog Info "SOLVER: exception was thrown: %s" e.Message
                        SmtUnknown (sprintf "Z3 has thrown an exception: %s" e.Message)
                getSolverResult mkResult q

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
                let assumptions, encoded = PathConditionToSeq p.state.pc |> builder.EncodeTerms encCtx
                let encodedWithAssumptions = Seq.append assumptions encoded |> Array.ofSeq
                let encoded = builder.MkAnd encodedWithAssumptions
                optCtx.Assert(ctx.MkImplies(pathAtom, encoded))
