namespace VSharp.Solver

open System
open Microsoft.Z3
open System.Collections.Generic
open VSharp
open VSharp.TypeUtils
open VSharp.Core
open VSharp.Core.SolverInteraction

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
        sorts : IDictionary<Type, Sort>
        e2t : IDictionary<Expr, term>
        t2e : IDictionary<term, encodingResult>
        heapAddresses : IDictionary<Expr, vectorTime>
        addressesConstants : HashSet<Expr>
        staticKeys : IDictionary<Expr, Type>
        regionConstants : Dictionary<regionSort * fieldId list, ArrayExpr>
        mutable lastSymbolicAddress : int32
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
        sorts = Dictionary<Type, Sort>()
        e2t = Dictionary<Expr, term>()
        t2e = Dictionary<term, encodingResult>()
        heapAddresses = Dictionary<Expr, vectorTime>()
        addressesConstants = HashSet<Expr>()
        staticKeys = Dictionary<Expr, Type>()
        regionConstants = Dictionary<regionSort * fieldId list, ArrayExpr>()
        lastSymbolicAddress = 0
    }

// ------------------------------- Encoding: primitives -------------------------------

    type private Z3Builder(ctx : Context) =
        let mutable encodingCache = freshCache()
        let emptyState = Memory.EmptyState()
        let mutable maxBufferSize = 128

        let getMemoryConstant mkConst (typ : regionSort * fieldId list) =
            let result : ArrayExpr ref = ref null
            if encodingCache.regionConstants.TryGetValue(typ, result) then result.Value
            else
                let regConst = mkConst()
                encodingCache.regionConstants.Add(typ, regConst)
                regConst

        let TrueExpr = ctx.MkTrue()
        let FalseExpr = ctx.MkFalse()

        let (|True|_|) (expr : Expr) =
            match expr with
            | _ when expr = TrueExpr -> Some(True)
            | _ -> None

        let (|False|_|) (expr : Expr) =
            match expr with
            | _ when expr = FalseExpr -> Some(False)
            | _ -> None

        member x.Reset() =
            encodingCache <- freshCache()

        member x.SetMaxBufferSize size =
            maxBufferSize <- size

        member private x.ValidateId id =
            assert(not <| String.IsNullOrWhiteSpace id)
            if Char.IsDigit id.[0] then "_" + id else id

        member private x.AddressSort = ctx.MkBitVecSort(32u) :> Sort

        member x.AddressesConstants with get() = encodingCache.addressesConstants

        member private x.Type2Sort typ =
            Dict.getValueOrUpdate encodingCache.sorts typ (fun () ->
                match typ with
                | Bool -> ctx.MkBoolSort() :> Sort
                | typ when typ.IsEnum -> ctx.MkBitVecSort(numericSizeOf typ) :> Sort
                | typ when Types.IsInteger typ -> ctx.MkBitVecSort(numericSizeOf typ) :> Sort
                | typ when Types.IsReal typ -> failToEncode "encoding real numbers is not implemented"
                | AddressType -> x.AddressSort
                | StructType _ -> internalfailf "struct should not appear while encoding! type: %O" typ
                | Numeric _ -> __notImplemented__()
                | ArrayType _
                | ClassType _
                | InterfaceType _
                | TypeVariable _
                | ByRef _
                | Pointer _
                | _ -> __unreachable__())

        member private x.DefaultValue (sort : Sort) =
            if sort = ctx.BoolSort then ctx.MkFalse() :> Expr
            else ctx.MkNumeral(0, sort)

        member private x.EncodeConcreteAddress encCtx (address : concreteHeapAddress) =
            let encoded = ctx.MkNumeral(encCtx.addressOrder[address], x.Type2Sort addressType)
            encodingCache.heapAddresses[encoded] <- address
            encoded

        member private x.CreateConstant name typ =
            let sort = x.Type2Sort typ
            let constant = ctx.MkConst(x.ValidateId name, sort)
            let assumptions =
                if typ = addressType then
                    let zero = ctx.MkNumeral(0, sort)
                    ctx.MkBVSLE(constant :?> BitVecExpr, zero :?> BitVecExpr) |> List.singleton
                else List.empty
            {expr = constant; assumptions = assumptions}

// ------------------------------- Encoding: logic simplifications -------------------------------

        member private x.MkNot expr =
            match expr with
            | True -> FalseExpr
            | False -> TrueExpr
            | _ -> ctx.MkNot expr

        member private x.MkEq(left, right) =
            if left = right then TrueExpr
            else ctx.MkEq(left, right)

        member x.MkAnd(left, right) =
            match left, right with
            | _ when left = right -> left
            | True, _ -> right
            | _, True -> left
            | False, _ -> left
            | _, False -> right
            | _ -> ctx.MkAnd(left, right)

        member x.MkOr(left, right) =
            match left, right with
            | _ when left = right -> left
            | True, _ -> left
            | _, True -> right
            | False, _ -> right
            | _, False -> left
            | _ -> ctx.MkOr(left, right)

        member private x.SimplifyAndElements (elems : BoolExpr seq) =
            Seq.filter (fun elem -> elem <> TrueExpr) elems |> Seq.distinct

        member private x.CreateAnd (elems : BoolExpr seq) =
            match elems with
            | Seq.Empty -> TrueExpr
            | Seq.Cons(head, tail) when Seq.isEmpty tail -> head
            | elems -> ctx.MkAnd(elems)

        member x.MkAnd ([<ParamArray>] elems : BoolExpr[]) =
            if Array.contains FalseExpr elems then FalseExpr
            else x.SimplifyAndElements elems |> x.CreateAnd

        member x.MkAnd (elems : BoolExpr seq) =
            if Seq.contains FalseExpr elems then FalseExpr
            else x.SimplifyAndElements elems |> x.CreateAnd

        member private x.SimplifyOrElements (nonFalseElems : BoolExpr seq) =
            match Seq.distinct nonFalseElems with
            | Seq.Empty -> FalseExpr
            | Seq.Cons(head, tail) when Seq.isEmpty tail -> head
            | elems -> ctx.MkOr(elems)

        member x.MkOr ([<ParamArray>] elems : BoolExpr[]) =
            if Array.contains TrueExpr elems then TrueExpr
            else
                let nonFalseElems = Array.filter (fun elem -> elem <> FalseExpr) elems
                x.SimplifyOrElements nonFalseElems

        member x.MkOr (elems : BoolExpr seq) =
            if Seq.contains TrueExpr elems then TrueExpr
            else
                let nonFalseElems = Seq.filter (fun elem -> elem <> FalseExpr) elems
                x.SimplifyOrElements nonFalseElems

        member x.MkITE(cond : BoolExpr, thenExpr, elseExpr) : Expr =
            match cond with
            | True -> thenExpr
            | False -> elseExpr
            | _ -> ctx.MkITE(cond, thenExpr, elseExpr)

// ------------------------------- Encoding: arithmetic simplifications -------------------------------

        member x.MkBVSLE(left, right) : BoolExpr =
            if left = right then TrueExpr
            else ctx.MkBVSLE(left, right)

        member x.MkBVSGE(left, right) : BoolExpr =
            if left = right then TrueExpr
            else ctx.MkBVSGE(left, right)

// ------------------------------- Encoding: common -------------------------------

        member public x.EncodeTerm encCtx (t : term) : encodingResult =
            let getResult () =
                let result =
                    match t.term with
                    | Concrete(obj, typ) -> x.EncodeConcrete encCtx obj typ
                    | Constant(name, source, typ) -> x.EncodeConstant encCtx name.v source typ
                    | Expression(op, args, typ) -> x.EncodeExpression encCtx t op args typ
                    | HeapRef(address, _) -> x.EncodeTerm encCtx address
                    | _ -> internalfail $"EncodeTerm: unexpected term: {t}"
                let typ = TypeOf t
                let result = if typ.IsEnum then x.AddEnumAssumptions encCtx typ result else result
                { result with assumptions = x.SimplifyAndElements result.assumptions |> List.ofSeq }
            encodingCache.Get(t, getResult)

        member private x.AddEnumAssumptions encCtx typ (encodingResult : encodingResult) =
            assert typ.IsEnum
            let expr = encodingResult.expr
            let values = Enum.GetValues typ |> System.Linq.Enumerable.OfType<obj>
            let createAssumption assumptions value =
                let assumptions', concrete = x.EncodeConcrete encCtx value typ |> toTuple
                x.MkEq(expr, concrete), assumptions @ assumptions'
            let options, assumptions = Seq.mapFold createAssumption encodingResult.assumptions values
            let enumAssumptions = x.MkOr options
            {expr = expr; assumptions = enumAssumptions :: assumptions}

        member private x.EncodeConcrete encCtx (obj : obj) typ : encodingResult =
            let expr =
                match typ with
                | Bool -> ctx.MkBool(obj :?> bool) :> Expr
                | t when t = typeof<char> -> ctx.MkNumeral(Convert.ToInt32(obj :?> char) |> toString, x.Type2Sort typ)
                | t when t.IsEnum -> ctx.MkNumeral(Convert.ChangeType(obj, EnumUtils.getEnumUnderlyingTypeChecked t) |> toString, x.Type2Sort typ)
                | Numeric _ -> ctx.MkNumeral(toString obj, x.Type2Sort typ)
                | AddressType ->
                    match obj with
                    | :? concreteHeapAddress as address ->
                        assert(List.isEmpty address |> not)
                        x.EncodeConcreteAddress encCtx address
                    | _ -> __unreachable__()
                | _ -> __notImplemented__()
            encodingResult.Create expr

        member private x.EncodeConstant encCtx name (source : ISymbolicConstantSource) typ : encodingResult =
            match source with
            | :? IMemoryAccessConstantSource as source ->
                x.EncodeMemoryAccessConstant encCtx name source List.empty typ
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

        member private x.EncodeOperation encCtx operation args =
            match operation with
            | OperationType.BitwiseNot -> x.MakeUnary encCtx ctx.MkBVNot args
            | OperationType.BitwiseAnd -> x.MakeBinary encCtx ctx.MkBVAND args
            | OperationType.BitwiseOr -> x.MakeBinary encCtx ctx.MkBVOR args
            | OperationType.BitwiseXor -> x.MakeBinary encCtx ctx.MkBVXOR args
            // [NOTE] IL specifies: arguments of SHL, SHR, SHR.UN are (int32 and int32), (int64 and int32)
            // So it's needed to extend one of them for Z3
            | OperationType.ShiftLeft -> x.MakeBinary encCtx (x.ExtendIfNeed >> ctx.MkBVSHL) args
            | OperationType.ShiftRight -> x.MakeBinary encCtx (x.ExtendIfNeed >> ctx.MkBVASHR) args
            | OperationType.ShiftRight_Un -> x.MakeBinary encCtx (x.ExtendIfNeed >> ctx.MkBVLSHR) args
            | OperationType.LogicalNot -> x.MakeUnary encCtx x.MkNot args
            | OperationType.LogicalAnd -> x.MakeOperation encCtx x.MkAnd args
            | OperationType.LogicalOr -> x.MakeOperation encCtx x.MkOr args
            | OperationType.LogicalXor -> x.MakeOperation encCtx ctx.MkXor args
            | OperationType.Equal -> x.MakeBinary encCtx x.MkEq args
            | OperationType.NotEqual -> x.MakeBinary encCtx (x.MkNot << x.MkEq) args
            | OperationType.Greater -> x.MakeBinary encCtx ctx.MkBVSGT args
            | OperationType.Greater_Un -> x.MakeBinary encCtx ctx.MkBVUGT args
            | OperationType.GreaterOrEqual -> x.MakeBinary encCtx x.MkBVSGE args
            | OperationType.GreaterOrEqual_Un -> x.MakeBinary encCtx ctx.MkBVUGE args
            | OperationType.Less -> x.MakeBinary encCtx ctx.MkBVSLT args
            | OperationType.Less_Un -> x.MakeBinary encCtx ctx.MkBVULT args
            | OperationType.LessOrEqual -> x.MakeBinary encCtx x.MkBVSLE args
            | OperationType.LessOrEqual_Un -> x.MakeBinary encCtx ctx.MkBVULE args
            | OperationType.Add -> x.MakeBinary encCtx ctx.MkBVAdd args
            | OperationType.AddNoOvf ->
                let operation (l, r) =
                    x.MkAnd(ctx.MkBVAddNoUnderflow(l, r), ctx.MkBVAddNoOverflow(l, r, true))
                x.MakeBinary encCtx operation args
            | OperationType.Multiply -> x.MakeBinary encCtx ctx.MkBVMul args
            | OperationType.MultiplyNoOvf ->
                let operation (l, r) =
                    x.MkAnd(ctx.MkBVMulNoUnderflow(l, r), ctx.MkBVMulNoOverflow(l, r, true))
                x.MakeBinary encCtx operation args
            | OperationType.Subtract -> x.MakeBinary encCtx ctx.MkBVSub args
            | OperationType.Divide -> x.MakeBinary encCtx ctx.MkBVSDiv args
            | OperationType.Divide_Un -> x.MakeBinary encCtx ctx.MkBVUDiv args
            | OperationType.Remainder -> x.MakeBinary encCtx ctx.MkBVSRem args
            | OperationType.Remainder_Un -> x.MakeBinary encCtx ctx.MkBVURem args
            | OperationType.UnaryMinus -> x.MakeUnary encCtx ctx.MkBVNeg args
            | _ -> __unreachable__()

        member private x.ExtractOrExtend (expr : BitVecExpr) size =
            let exprSize = expr.SortSize
            if exprSize > size then ctx.MkExtract(size - 1u, 0u, expr)
            else ctx.MkSignExt(size - exprSize, expr)

        member private x.ReverseBytes (expr : BitVecExpr) =
            let size = int expr.SortSize
            assert(size % 8 = 0)
            let bytes = List.init (size / 8) (fun byte -> ctx.MkExtract(uint ((byte + 1) * 8) - 1u, uint (byte * 8), expr))
            List.reduce (fun x y -> ctx.MkConcat(x, y)) bytes

        // TODO: make code better
        member private x.EncodeCombine encCtx slices typ =
            let res = ctx.MkNumeral(0, x.Type2Sort typ) :?> BitVecExpr
            let window = res.SortSize
            let windowExpr = ctx.MkNumeral(window, x.Type2Sort(Types.IndexType)) :?> BitVecExpr
            let zero = ctx.MkNumeral(0, x.Type2Sort(Types.IndexType)) :?> BitVecExpr
            let addOneSlice (res, assumptions) slice =
                let term, startByte, endByte, pos =
                    match slice.term with
                    | Slice(term, startByte, endByte, pos) ->
                        x.EncodeTerm encCtx term, x.EncodeTerm encCtx startByte, x.EncodeTerm encCtx endByte, x.EncodeTerm encCtx pos
                    | _ -> internalfailf "encoding combine: expected slice as argument, but got %O" slice
                let assumptions = assumptions @ term.assumptions @ startByte.assumptions @ endByte.assumptions @ pos.assumptions
                let term = term.expr :?> BitVecExpr
                let startByte = startByte.expr :?> BitVecExpr
                let endByte = endByte.expr :?> BitVecExpr
                let pos = pos.expr :?> BitVecExpr
                let pos = ctx.MkBVMul(pos, ctx.MkBV(8, pos.SortSize))
                let startBit = ctx.MkBVMul(startByte, ctx.MkBV(8, startByte.SortSize))
                let endBit = ctx.MkBVMul(endByte, ctx.MkBV(8, endByte.SortSize))
                let termSize = term.SortSize
                let sizeExpr = ctx.MkBV(termSize, endBit.SortSize)
                let left = x.MkITE(ctx.MkBVSGT(startBit, zero), startBit, zero) :?> BitVecExpr
                let right = x.MkITE(ctx.MkBVSGT(sizeExpr, endBit), endBit, sizeExpr) :?> BitVecExpr
                let size = ctx.MkBVSub(right, left)
                let intersects = ctx.MkBVSGT(size, zero)
                let term = x.ReverseBytes term
                let left = x.ExtractOrExtend left term.SortSize
                let term = ctx.MkBVLSHR(ctx.MkBVSHL(term, left), left)
                let toShiftRight = x.ExtractOrExtend (ctx.MkBVSub(sizeExpr, right)) term.SortSize
                let term = ctx.MkBVLSHR(term, toShiftRight)
                let term = if termSize > window then ctx.MkExtract(window - 1u, 0u, term) else ctx.MkZeroExt(window - termSize, term)
                let w = x.ExtractOrExtend windowExpr term.SortSize
                let s = x.ExtractOrExtend sizeExpr term.SortSize
                let pos = x.ExtractOrExtend pos term.SortSize
                let toShiftRight = x.ExtractOrExtend toShiftRight term.SortSize
                let shift = ctx.MkBVAdd(ctx.MkBVSub(w, ctx.MkBVSub(s, pos)), toShiftRight)
                let part = ctx.MkBVSHL(term, shift)
                let res = x.MkITE(intersects, ctx.MkBVOR(res, part), res) :?> BitVecExpr
                res, assumptions
            let result, assumptions = List.fold addOneSlice (res, List.empty) slices
            {expr = x.ReverseBytes result; assumptions = assumptions}

        member private x.EncodeExpression encCtx term op args typ =
            encodingCache.Get(term, fun () ->
                match op with
                | Operator operation ->
                    x.EncodeOperation encCtx operation args
                | Application sf ->
                    let decl = ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, x.Type2Sort typ)
                    x.MakeOperation encCtx (fun x -> ctx.MkApp(decl, x)) args
                | Cast(Numeric t1, Numeric t2) when isLessForNumericTypes t1 t2 ->
                    let expr = x.EncodeTerm encCtx (List.head args)
                    let difference = numericSizeOf t2 - numericSizeOf t1
                    let extend = if isUnsigned t2 then ctx.MkZeroExt else ctx.MkSignExt
                    {expr = extend(difference, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric t1, Numeric t2) when isLessForNumericTypes t2 t1 ->
                    let expr = x.EncodeTerm encCtx (List.head args)
                    let from = numericSizeOf t2 - 1u
                    {expr = ctx.MkExtract(from, 0u, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric t1, Numeric t2) when isReal t1 || isReal t2 ->
                    failToEncode "encoding real numbers is not implemented"
                | Cast(Numeric t1, Numeric t2) when numericSizeOf t1 = numericSizeOf t2 ->
                    x.EncodeTerm encCtx (List.head args)
                | Cast(Bool, Numeric t) ->
                    let arg = x.EncodeTerm encCtx (List.head args)
                    let expr = arg.expr :?> BoolExpr
                    let sort = x.Type2Sort(t)
                    let converted = x.MkITE(expr, ctx.MkNumeral(1, sort), ctx.MkNumeral(0, sort))
                    {expr = converted; assumptions = arg.assumptions}
                | Cast(fromType, toType) ->
                    internalfail $"EncodeExpression: encoding of term {term} from {fromType} to {toType} is not supported"
                | Combine -> x.EncodeCombine encCtx args typ)

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

        member private x.EncodeSymbolicAddress encCtx (heapRefSource : ISymbolicConstantSource) structFields name =
            let res = x.EncodeMemoryAccessConstant encCtx name heapRefSource structFields addressType
            encodingCache.addressesConstants.Add(res.expr) |> ignore
            res

        // TODO: [style] get rid of accumulators
        member private x.KeyInVectorTimeIntervals encCtx (key : Expr) acc (region : vectorTime intervals) =
            let onePointCondition acc (y : vectorTime endpoint) =
                let bound = ctx.MkNumeral(encCtx.addressOrder.[y.elem], x.Type2Sort addressType) :?> BitVecExpr
                let condition =
                    match y.sort with
                    | endpointSort.OpenRight -> ctx.MkBVSLT(key :?> BitVecExpr, bound)
                    | endpointSort.ClosedRight -> x.MkBVSLE(key :?> BitVecExpr, bound)
                    | endpointSort.OpenLeft -> ctx.MkBVSGT(key :?> BitVecExpr, bound)
                    | endpointSort.ClosedLeft -> x.MkBVSGE(key :?> BitVecExpr, bound)
                    | _ -> __unreachable__()
                x.MkAnd(acc, condition)
            let intervalWithoutLeftZeroBound =
                region.points |> List.filter (fun ep -> VectorTime.less VectorTime.zero ep.elem)
            List.fold onePointCondition acc intervalWithoutLeftZeroBound

        member private x.HeapAddressKeyInRegion encCtx acc (keyExpr : Expr) region =
            x.KeyInVectorTimeIntervals encCtx keyExpr acc region

        member private x.KeyInIntPoints key acc (region : int points) =
            let points, operation =
                match region with
                | {points = points; thrown = true} -> points, x.MkNot
                | {points = points; thrown = false} -> points, id
            let handleOne acc (point : int) =
                let pointExpr = ctx.MkNumeral(point, x.Type2Sort Types.IndexType)
                x.MkOr(acc, x.MkEq(key, pointExpr))
            let condition = PersistentSet.fold handleOne FalseExpr points |> operation
            x.MkAnd(acc, condition)

        member private x.KeyInProductRegion keyInFst keyInSnd acc (region : productRegion<'a, 'b>) =
            let checkKeyInOne acc (fst, snd) = x.MkAnd(acc, keyInFst TrueExpr fst, keyInSnd TrueExpr snd)
            List.fold checkKeyInOne acc region.products

        member private x.KeysInIntPointsListProductRegion keys acc (region : int points listProductRegion) =
            match region, keys with
            | NilRegion, Seq.Empty -> acc
            | ConsRegion products, Seq.Cons(key, others) ->
                let keyInPoints = x.KeyInIntPoints key
                let keysInProductList = x.KeysInIntPointsListProductRegion others
                x.KeyInProductRegion keyInPoints keysInProductList acc products
            | _ -> __unreachable__()

        member private x.ArrayOneIndexKeyInRegion encCtx acc (keyExpr : Expr[]) region =
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx (Array.head keyExpr)
            let indicesInRegion acc region = x.KeysInIntPointsListProductRegion (Array.tail keyExpr) acc region
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.VectorIndexKeyInRegion encCtx acc (keyExpr : Expr[]) region =
            assert(Array.length keyExpr = 2)
            let addressInRegion = x.KeyInVectorTimeIntervals encCtx keyExpr.[0]
            let indicesInRegion = x.KeyInIntPoints keyExpr.[1]
            x.KeyInProductRegion addressInRegion indicesInRegion acc region

        member private x.stackBufferIndexKeyInRegion acc keyExpr region =
            x.KeyInIntPoints keyExpr acc region

        member private x.GetRegionConstant (name : string) sort (structFields : fieldId list) (regSort : regionSort) =
            let mkConst () = ctx.MkConst(name, sort) :?> ArrayExpr
            getMemoryConstant mkConst (regSort, structFields)

        member private x.MemoryReading encCtx specializeWithKey keyInRegion keysAreMatch encodeKey inst structFields left mo typ =
            let updates = MemoryRegion.flatten mo
            let assumptions, leftExpr = encodeKey left
            let leftRegion = (left :> IMemoryKey<'a, 'b>).Region
            let leftInRegion = keyInRegion TrueExpr leftExpr leftRegion
            let assumptions = leftInRegion :: assumptions
            let inst, instAssumptions = inst leftExpr
            let assumptions = instAssumptions @ assumptions
            let checkOneKey (right, reg, value) (acc, assumptions) =
                // NOTE: we need constraints on right key, because path condition may contain it
                // EXAMPLE: a[k] = 1; if (k == 0 && a[i] == 1) {...}
                let matchAssumptions, keysAreMatch = keysAreMatch leftExpr right reg
                // TODO: [style] auto append assumptions
                let assumptions = List.append assumptions matchAssumptions
                let readFieldIfNeed structTerm field =
                    assert(IsStruct structTerm)
                    Memory.ReadField emptyState structTerm field
                let value = List.fold readFieldIfNeed value structFields
                let valueExpr = specializeWithKey value left right |> x.EncodeTerm encCtx
                let assumptions = List.append assumptions valueExpr.assumptions
                x.MkITE(keysAreMatch, valueExpr.expr, acc), assumptions
            let expr, assumptions = List.foldBack checkOneKey updates (inst, assumptions)
            encodingResult.Create(expr, assumptions)

        member private x.HeapReading encCtx key mo typ source structFields name =
            assert mo.defaultValue.IsNone
            let encodeKey (k : heapAddressKey) = x.EncodeTerm encCtx k.address |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort addressType, x.Type2Sort typ)
            let regionSort = GetHeapReadingRegionSort source
            let array = x.GetRegionConstant name sort structFields regionSort
            let inst (k : Expr) =
                let expr = ctx.MkSelect(array, k)
                expr, x.GenerateInstAssumptions expr typ
            let keyInRegion = x.HeapAddressKeyInRegion encCtx
            let keysAreMatch leftExpr right rightRegion =
                let assumptions, rightExpr = encodeKey right
                let eq = x.MkEq(leftExpr, rightExpr)
                assumptions, x.KeyInVectorTimeIntervals encCtx rightExpr eq rightRegion
            let specialize v _ _ = v
            let res = x.MemoryReading encCtx specialize keyInRegion keysAreMatch encodeKey inst structFields key mo typ
            match regionSort with
            | HeapFieldSort field when field = Reflection.stringLengthField -> x.GenerateLengthAssumptions res
            | _ -> res

        // Generating assumptions for instantiation
        member private x.GenerateInstAssumptions (inst : Expr) typ =
            if typ = addressType then
                // In case of symbolic address instantiation, adding assumption "inst < 0"
                let zero = ctx.MkNumeral(0, inst.Sort) :?> BitVecExpr
                let belowZero = ctx.MkBVSLE(inst :?> BitVecExpr, zero)
                belowZero |> List.singleton
            else List.empty

        // NOTE: temporary generating string with 0 <= length <= 20
        member private x.GenerateLengthAssumptions encodingResult =
            let expr = encodingResult.expr :?> BitVecExpr
            let assumptions = encodingResult.assumptions
            let lengthIsNonNegative = x.MkBVSGE(expr, ctx.MkBV(0, expr.SortSize))
            let assumptions = lengthIsNonNegative :: assumptions
            let assumptions =
                if maxBufferSize < 0 then assumptions
                else x.MkBVSLE(expr, ctx.MkBV(maxBufferSize, expr.SortSize)) :: assumptions
            // TODO: this limits length < 20, delete when model normalization is complete
            { encodingResult with assumptions = assumptions }

        // NOTE: XML serializer can not generate special char symbols (char <= 32) #XMLChar
        // TODO: use another serializer
        member private x.GenerateCharAssumptions encodingResult =
            // TODO: use stringRepr for serialization of strings
            let expr = encodingResult.expr :?> BitVecExpr
            let assumptions = encodingResult.assumptions
            let cond = ctx.MkBVSGT(expr, ctx.MkBV(32, expr.SortSize))
            {expr = expr; assumptions = cond :: assumptions}

        member private x.ArrayReading encCtx specialize keyInRegion keysAreMatch encodeKey hasDefaultValue indices key mo typ source structFields name =
            assert mo.defaultValue.IsNone
            let domainSort = x.Type2Sort addressType :: List.map (x.Type2Sort Types.IndexType |> always) indices |> Array.ofList
            let valueSort = x.Type2Sort typ
            let inst (k : Expr[]) =
                if hasDefaultValue then x.DefaultValue valueSort, List.empty
                else
                    let sort = ctx.MkArraySort(domainSort, valueSort)
                    let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
                    let expr = ctx.MkSelect(array, k)
                    expr, x.GenerateInstAssumptions expr typ
            let res = x.MemoryReading encCtx specialize keyInRegion keysAreMatch encodeKey inst structFields key mo typ
            let res = if typ = typeof<char> then x.GenerateCharAssumptions res else res
            match GetHeapReadingRegionSort source with
            | ArrayLengthSort _ -> x.GenerateLengthAssumptions res
            | _ -> res

        member private x.ArrayRangeKeyMatches encCtx leftExpr address fromIndices toIndices rightRegion =
            // Encoding of range key
            let addressAssumptions, addressExpr = x.EncodeTerm encCtx address |> toTuple
            let fromAssumptions, fromIndices = x.EncodeTerms encCtx fromIndices
            let toAssumptions, toIndices = x.EncodeTerms encCtx toIndices
            // Accumulating assumptions
            let assumptions = addressAssumptions @ fromAssumptions @ toAssumptions
            let leftAddress = Array.head leftExpr
            let leftIndices = Array.tail leftExpr
            // Calculating match conditions:
            // (1) left address equals right address
            let addressesEq = x.MkEq(addressExpr, leftAddress)
            // (2) right address and left indices are in right's region
            // NOTE: checking left indices, because right key is range of indices and we can not encode it as Expr[]
            let key = Array.append [|addressExpr|] leftIndices
            let keyInRegion = x.ArrayOneIndexKeyInRegion encCtx addressesEq key rightRegion
            // (3) every index of left key is in bounds of right key
            let keyInBounds (i : Expr) (l : Expr) (r : Expr) =
                let i = i :?> BitVecExpr
                x.MkAnd(x.MkBVSLE(l :?> BitVecExpr, i), x.MkBVSLE(i, r :?> BitVecExpr))
            let indicesConditions = Array.map3 keyInBounds leftIndices fromIndices toIndices
            assumptions, x.MkAnd(x.MkAnd indicesConditions, keyInRegion)

        member private x.ArrayIndexReading encCtx key hasDefaultValue mo typ source structFields name =
            let encodeKey = function
                | OneArrayIndexKey(address, indices) -> address :: indices |> x.EncodeTerms encCtx
                | RangeArrayIndexKey _ as key -> internalfail $"EncodeMemoryAccessConstant: unexpected array key {key}"
            let keyInRegion = x.ArrayOneIndexKeyInRegion encCtx
            let arraysEquality (left, right) =
                Seq.map2 (fun l r -> x.MkEq(l, r)) left right |> x.MkAnd
            let keysAreMatch leftExpr (right : heapArrayKey) rightRegion =
                match right with
                | OneArrayIndexKey _ ->
                    let assumptions, rightExpr = encodeKey right
                    let eq = arraysEquality(leftExpr, rightExpr)
                    assumptions, keyInRegion eq rightExpr rightRegion
                | RangeArrayIndexKey(address, fromIndices, toIndices) ->
                    x.ArrayRangeKeyMatches encCtx leftExpr address fromIndices toIndices rightRegion
            let indices =
                match key with
                | OneArrayIndexKey(_, indices) -> indices
                | _ -> internalfail $"EncodeMemoryAccessConstant: unexpected array key {key}"
            x.ArrayReading encCtx SpecializeWithKey keyInRegion keysAreMatch encodeKey hasDefaultValue indices key mo typ source structFields name

        member private x.VectorIndexReading encCtx (key : heapVectorIndexKey) hasDefaultValue mo typ source structFields name =
            let encodeKey (k : heapVectorIndexKey) =
                [|k.address; k.index|] |> x.EncodeTerms encCtx
            let keyInRegion = x.VectorIndexKeyInRegion encCtx
            let keysAreEqual (left : Expr[], right : Expr[]) =
                x.MkAnd(x.MkEq(left.[0], right.[0]), x.MkEq(left.[1], right.[1]))
            let keysAreMatch leftExpr right rightRegion =
                let assumptions, rightExpr = encodeKey right
                let eq = keysAreEqual(leftExpr, rightExpr)
                assumptions, keyInRegion eq rightExpr rightRegion
            let specialize v _ _ = v
            x.ArrayReading encCtx specialize keyInRegion keysAreMatch encodeKey hasDefaultValue [key.index] key mo typ source structFields name

        member private x.StackBufferReading encCtx key mo typ source structFields name =
            assert mo.defaultValue.IsNone
            let encodeKey (k : stackBufferIndexKey) = x.EncodeTerm encCtx k.index |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort addressType, x.Type2Sort typ)
            let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
            let keyInRegion = x.stackBufferIndexKeyInRegion
            let inst (k : Expr) =
                let expr = ctx.MkSelect(array, k)
                expr, x.GenerateInstAssumptions expr typ
            let keysAreMatch leftExpr right rightRegion =
                let assumptions, rightExpr = encodeKey right
                let eq = x.MkEq(leftExpr, rightExpr)
                assumptions, x.KeyInIntPoints rightExpr eq rightRegion
            let specialize v _ _ = v
            x.MemoryReading encCtx specialize keyInRegion keysAreMatch encodeKey inst structFields key mo typ

        member private x.StaticsReading encCtx (key : symbolicTypeKey) mo typ source structFields (name : string) =
            assert mo.defaultValue.IsNone
            let keyType = x.Type2Sort Types.IndexType
            let sort = ctx.MkArraySort(keyType, x.Type2Sort typ)
            let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
            let updates = MemoryRegion.flatten mo
            let value = Seq.tryFind (fun (k, _, _) -> k = key) updates
            match value with
            | Some (_, _, v) -> x.EncodeTerm encCtx v
            | None ->
                let encodedKey = ctx.MkConst(key.ToString(), keyType)
                encodingCache.staticKeys.Add(encodedKey, key.typ)
                {expr = ctx.MkSelect(array, encodedKey); assumptions = List.empty}

        member private x.StructReading encCtx (structSource : ISymbolicConstantSource) (field : fieldId) typ structFields name =
            let res = x.EncodeMemoryAccessConstant encCtx name structSource (field :: structFields) typ
            match field with
            | _ when field.declaringType = typeof<decimal> && field.name = "_flags" ->
                let expr = res.expr :?> BitVecExpr
                let lowerWord = ctx.MkExtract(15u, 0u, expr)
                let lowerIsZero = x.MkEq(lowerWord, ctx.MkBV(0, lowerWord.SortSize))
                let exp = ctx.MkExtract(23u, 16u, expr)
                let expSize = exp.SortSize
                let leftBound = ctx.MkBVUGE(exp, ctx.MkBV(0, expSize))
                let rightBound = ctx.MkBVULE(exp, ctx.MkBV(28, expSize))
                let expInBound = x.MkAnd(leftBound, rightBound)
                let upper = ctx.MkExtract(30u, 24u, expr)
                let upperIsZero = x.MkEq(upper, ctx.MkBV(0, upper.SortSize))
                { res with assumptions = lowerIsZero::expInBound::upperIsZero::res.assumptions }
            | _ -> res

        member private x.EncodeMemoryAccessConstant encCtx name (source : ISymbolicConstantSource) (structFields : fieldId list) typ : encodingResult =
            match source with
            | HeapReading(key, mo) -> x.HeapReading encCtx key mo typ source structFields name
            | ArrayIndexReading(hasDefaultValue, key, mo) ->
                x.ArrayIndexReading encCtx key hasDefaultValue mo typ source structFields name
            | VectorIndexReading(hasDefaultValue, key, mo) ->
                x.VectorIndexReading encCtx key hasDefaultValue mo typ source structFields name
            | StackBufferReading(key, mo) -> x.StackBufferReading encCtx key mo typ source structFields name
            | StaticsReading(key, mo) -> x.StaticsReading encCtx key mo typ source structFields name
            | StructFieldSource(structSource, field) -> x.StructReading encCtx structSource field typ structFields name
            | HeapAddressSource source ->
                assert(typ = addressType)
                x.EncodeSymbolicAddress encCtx source structFields name
            | _ -> x.CreateConstant name typ

    // ------------------------------- Decoding -------------------------------

        member private x.DecodeExpr op t (expr : Expr) =
            // TODO: bug -- decoding arguments with type of expression
            Expression (Operator op) (expr.Args |> Seq.map (x.Decode t) |> List.ofSeq) t

        member private x.DecodeBoolExpr op (expr : Expr) =
            x.DecodeExpr op typeof<bool> expr

        member private x.GetTypeOfBV (bv : BitVecExpr) =
            if bv.SortSize = 32u then typeof<int32>
            elif bv.SortSize = 64u then typeof<int64>
            elif bv.SortSize = 8u then typeof<int8>
            elif bv.SortSize = 16u then typeof<int16>
            else __unreachable__()

        member private x.DecodeConcreteHeapAddress (expr : Expr) : vectorTime =
            let addresses = encodingCache.heapAddresses
            let result = ref vectorTime.Empty
            let existing = addresses.TryGetValue(expr, result)
            match expr with
            | :? BitVecNum as expr when expr.Int64 = 0L -> VectorTime.zero
            | _ when existing -> result.Value
            | _ ->
                encodingCache.lastSymbolicAddress <- encodingCache.lastSymbolicAddress - 1
                let address = [encodingCache.lastSymbolicAddress]
                addresses.Add(expr, address)
                address

        member private x.DecodeSymbolicTypeAddress (expr : Expr) =
            let result = ref typeof<Void>
            if encodingCache.staticKeys.TryGetValue(expr, result) then result.Value
            else __notImplemented__()

        member private x.DecodeMemoryKey (reg : regionSort) (exprs : Expr array) =
            match reg with
            | HeapFieldSort field ->
                assert(exprs.Length = 1)
                let address = x.DecodeConcreteHeapAddress exprs[0] |> ConcreteHeapAddress
                ClassField(address, field)
            | StaticFieldSort field ->
                assert(exprs.Length = 1)
                let typ = x.DecodeSymbolicTypeAddress exprs[0]
                StaticField(typ, field)
            | ArrayIndexSort typ ->
                assert(exprs.Length >= 2)
                let heapAddress = x.DecodeConcreteHeapAddress exprs[0] |> ConcreteHeapAddress
                let indices = exprs |> Seq.tail |> Seq.map (x.Decode Types.IndexType) |> List.ofSeq
                ArrayIndex(heapAddress, indices, typ)
            | ArrayLengthSort typ ->
                assert(exprs.Length = 2)
                let heapAddress = x.DecodeConcreteHeapAddress exprs[0] |> ConcreteHeapAddress
                let index = x.Decode Types.IndexType exprs.[1]
                ArrayLength(heapAddress, index, typ)
            | ArrayLowerBoundSort typ ->
                assert(exprs.Length = 2)
                let heapAddress = x.DecodeConcreteHeapAddress exprs[0] |> ConcreteHeapAddress
                let index = x.Decode Types.IndexType exprs.[1]
                ArrayLowerBound(heapAddress, index, typ)
            | StackBufferSort key ->
                assert(exprs.Length = 1)
                let index = x.Decode typeof<int8> exprs.[0]
                StackBufferIndex(key, index)

        member private x.DecodeBv t (bv : BitVecNum) =
            match bv.SortSize with
            | 32u -> Concrete (convert bv.Int64 t) t
            | 64u -> Concrete (convert (uint64 bv.BigInteger) t) t
            | 16u -> Concrete (convert bv.Int t) t
            | 8u  -> Concrete (convert bv.Int t) t
            | _ -> __notImplemented__()

        member public x.Decode t (expr : Expr) =
            match expr with
            | :? BitVecNum as bv when Types.IsNumeric t -> x.DecodeBv t bv
            | :? BitVecNum as bv when not (Types.IsValueType t) ->
                let address = x.DecodeConcreteHeapAddress bv |> ConcreteHeapAddress
                HeapRef address t
            | :? BitVecExpr as bv when bv.IsConst ->
                if encodingCache.e2t.ContainsKey(expr) then encodingCache.e2t.[expr]
                else x.GetTypeOfBV bv |> Concrete expr.String
            | :? IntNum as i -> Concrete i.Int typeof<int>
            | :? RatNum as r -> Concrete (double(r.Numerator.Int) * 1.0 / double(r.Denominator.Int)) typeof<int>
            | _ ->
                if encodingCache.e2t.ContainsKey(expr) then encodingCache.e2t.[expr]
                elif expr.IsTrue then True
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

        member private x.WriteFields structure value = function
            | [field] -> Memory.WriteStructField structure field value
            | field::fields ->
                match structure with
                | {term = Struct(contents, _)} ->
                    let recurred = x.WriteFields contents[field] value fields
                    Memory.WriteStructField structure field recurred
                | _ -> internalfail "Expected structure, but got %O" structure
            | [] -> __unreachable__()

        member private x.WriteDictOfValueTypes (dict : IDictionary<'key, term ref>) (key : 'key) fields structureType value =
            match fields with
            | [] ->
                assert(not <| dict.ContainsKey key)
                dict.Add(key, ref value)
            | _ ->
                let structureRef = Dict.getValueOrUpdate dict key (fun () ->
                    Memory.DefaultOf structureType |> ref)
                structureRef.Value <- x.WriteFields structureRef.Value value fields

        member x.MkModel (m : Model) =
            let subst = Dictionary<ISymbolicConstantSource, term>()
            let stackEntries = Dictionary<stackKey, term ref>()
            let state = {Memory.EmptyState() with complete = true}
            // TODO: some of evaluated constants are written to model: most of them contain stack reading
            // TODO: maybe encode stack reading as reading from array?
            encodingCache.t2e |> Seq.iter (fun kvp ->
                match kvp.Key with
                | {term = Constant(_, StructFieldChain(fields, StackReading(key)), t)} as constant ->
                    let refinedExpr = m.Eval(kvp.Value.expr, false)
                    let decoded = x.Decode t refinedExpr
                    if decoded <> constant then
                        x.WriteDictOfValueTypes stackEntries key fields key.TypeOfLocation decoded
                | {term = Constant(_, (:? IMemoryAccessConstantSource as ms), _)} as constant ->
                    match ms with
                    | HeapAddressSource(StructFieldChain(fields, StackReading(key))) ->
                        let refinedExpr = m.Eval(kvp.Value.expr, false)
                        let t = if List.isEmpty fields then key.TypeOfLocation else (List.last fields).typ
                        let address = x.DecodeConcreteHeapAddress refinedExpr |> ConcreteHeapAddress
                        let value = HeapRef address t
                        x.WriteDictOfValueTypes stackEntries key fields key.TypeOfLocation value
                    | HeapAddressSource(StructFieldChain(_, (:? functionResultConstantSource as frs)))
                    | StructFieldChain(_, (:? functionResultConstantSource as frs)) ->
                        let refinedExpr = m.Eval(kvp.Value.expr, false)
                        let t = (frs :> ISymbolicConstantSource).TypeOfLocation
                        let term = x.Decode t refinedExpr
                        assert(not (constant = term) || kvp.Value.expr = refinedExpr)
                        if constant <> term then subst.Add(ms, term)
                    | _ -> ()
                | {term = Constant(_, :? IStatedSymbolicConstantSource, _)} -> ()
                | {term = Constant(_, source, t)} as constant ->
                    let refinedExpr = m.Eval(kvp.Value.expr, false)
                    let term = x.Decode t refinedExpr
                    assert(not (constant = term) || kvp.Value.expr = refinedExpr)
                    if constant <> term then subst.Add(source, term)
                | _ -> ())

            let frame = stackEntries |> Seq.map (fun kvp ->
                let key = kvp.Key
                let term = kvp.Value.Value
                let typ = TypeOf term
                (key, Some term, typ))
            Memory.NewStackFrame state None (List.ofSeq frame)

            let defaultValues = Dictionary<regionSort, term ref>()
            encodingCache.regionConstants |> Seq.iter (fun kvp ->
                let region, fields = kvp.Key
                let constant = kvp.Value
                let arr = m.Eval(constant, false)
                let typeOfLocation =
                    if fields.IsEmpty then region.TypeOfLocation
                    else (List.last fields).typ
                let rec parseArray (arr : Expr) =
                    if arr.IsConstantArray then
                        assert(arr.Args.Length = 1)
                        let constantValue =
                            if Types.IsValueType typeOfLocation then x.Decode typeOfLocation arr.Args.[0]
                            else
                                let addr = x.DecodeConcreteHeapAddress arr.Args[0] |> ConcreteHeapAddress
                                HeapRef addr typeOfLocation
                        x.WriteDictOfValueTypes defaultValues region fields region.TypeOfLocation constantValue
                    elif arr.IsDefaultArray then
                        assert(arr.Args.Length = 1)
                    elif arr.IsStore then
                        assert(arr.Args.Length >= 3)
                        parseArray arr.Args.[0]
                        let address = x.DecodeMemoryKey region arr.Args.[1..arr.Args.Length - 2]
                        let value =
                            if Types.IsValueType typeOfLocation then
                                x.Decode typeOfLocation (Array.last arr.Args)
                            else
                                let address = Array.last arr.Args |> x.DecodeConcreteHeapAddress |> ConcreteHeapAddress
                                HeapRef address typeOfLocation
                        let address = fields |> List.fold (fun address field -> StructField(address, field)) address
                        let states = Memory.Write state (Ref address) value
                        assert(states.Length = 1 && states.[0] = state)
                    elif arr.IsConst then ()
                    else internalfailf "Unexpected array expression in model: %O" arr
                parseArray arr)
            defaultValues |> Seq.iter (fun kvp ->
                let region = kvp.Key
                let constantValue = kvp.Value.Value
                Memory.FillRegion state constantValue region)

            state.startingTime <- [encodingCache.lastSymbolicAddress - 1]

            encodingCache.heapAddresses.Clear()
            state.model <- PrimitiveModel subst
            StateModel state


    let private ctx = new Context()
    let private builder = Z3Builder(ctx)

    type internal Z3Solver(timeoutMs : uint option) =
        let solver = ctx.MkSolver()

        do
            match timeoutMs with
            | Some timeoutMs ->
                let parameters = ctx.MkParams().Add("timeout", timeoutMs);
                solver.Parameters <- parameters
            | None -> ()

        interface ISolver with
            member x.CheckSat (encCtx : encodingContext) (q : term) : smtResult =
                Logger.printLog Logger.Trace "SOLVER: trying to solve constraints..."
                Logger.printLogLazy Logger.Trace "%s" (lazy q.ToString())
                try
                    try
                        let query = builder.EncodeTerm encCtx q
                        let assumptions = query.assumptions
                        let assumptions =
                            seq {
                                yield! (Seq.cast<_> assumptions)
                                yield query.expr
                            } |> Array.ofSeq

                        let result = solver.Check assumptions
                        match result with
                        | Status.SATISFIABLE ->
                            Logger.trace "SATISFIABLE"
                            let z3Model = solver.Model
                            let model = builder.MkModel z3Model
                            SmtSat { mdl = model }
                        | Status.UNSATISFIABLE ->
                            Logger.trace "UNSATISFIABLE"
                            SmtUnsat { core = Array.empty (*optCtx.UnsatCore |> Array.map (builder.Decode Bool)*) }
                        | Status.UNKNOWN ->
                            Logger.error $"Solver returned Status.UNKNOWN. Reason: {solver.ReasonUnknown}\n\
                                Expression: {assumptions |> Seq.map (fun a -> a.ToString()) |> Seq.toList}"

                            Logger.trace "UNKNOWN"
                            SmtUnknown solver.ReasonUnknown
                        | _ -> __unreachable__()
                    with
                    | :? EncodingException as e ->
                        Logger.printLog Logger.Info "SOLVER: exception was thrown: %s" e.Message
                        SmtUnknown (sprintf "Z3 has thrown an exception: %s" e.Message)
                finally
                    builder.Reset()

            member x.Assert encCtx (fml : term) =
                Logger.printLogLazy Logger.Trace "SOLVER: Asserting: %s" (lazy(fml.ToString()))
                let encoded = builder.EncodeTerm encCtx fml
                let encoded = List.fold (fun acc x -> builder.MkAnd(acc, x)) (encoded.expr :?> BoolExpr) encoded.assumptions
                solver.Assert(encoded)

            member x.SetMaxBufferSize size =
                builder.SetMaxBufferSize size

    let reset() =
        builder.Reset()
