namespace VSharp.Solver

open System
open System.Text
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

// ------------------------------- Path -------------------------------

    type private pathPart =
        | StructFieldPart of fieldId
        | PointerAddress
        | PointerOffset

    type private path =
        { parts : pathPart list }
        with
        member x.StructField fieldId =
            { x with parts = StructFieldPart fieldId :: x.parts }

        member x.PointerAddress() =
            assert(List.isEmpty x.parts)
            { x with parts = PointerAddress :: x.parts }

        member x.PointerOffset() =
            assert(List.isEmpty x.parts)
            { x with parts = PointerOffset :: x.parts }

        member x.TypeOfLocation with get() =
            assert(List.isEmpty x.parts |> not)
            match List.last x.parts with
            | PointerAddress -> addressType
            | PointerOffset -> typeof<int>
            | StructFieldPart fieldId -> fieldId.typ

        member x.ToAddress address =
            let convertToAddress (address, ptrPart) part =
                assert(Option.isNone ptrPart)
                match part with
                | StructFieldPart field -> (StructField(address, field), ptrPart)
                | PointerAddress
                | PointerOffset -> (address, Some part)
            List.fold convertToAddress (address, None) x.parts

        member x.Fold f acc =
            List.fold f acc x.parts

        member x.IsEmpty with get() =
            List.isEmpty x.parts

        static member Empty with get() =
            { parts = List.empty }

        override x.ToString() =
            let mutable sb = StringBuilder()
            for p in x.parts do
                sb <- sb.Append(p.ToString())
            sb.ToString()

    module private Path =

        let rec (|Path|_|) (src : ISymbolicConstantSource) =
            match src with
            | PointerOffsetSource(StructFieldChain(fields, s)) ->
                let path = path.Empty.PointerOffset()
                let path = path |> List.foldBack (fun f path -> path.StructField f) fields
                Path(path, s) |> Some
            | PointerAddressSource(StructFieldChain(fields, s)) ->
                let path = path.Empty.PointerAddress()
                let path = path |> List.foldBack (fun f path -> path.StructField f) fields
                Path(path, s) |> Some
            | StructFieldChain(fields, s) ->
                let path = path.Empty
                let path = path |> List.foldBack (fun f path -> path.StructField f) fields
                Path(path, s) |> Some
            | _ -> None

// ------------------------------- Cache -------------------------------

    type private encodingCache = {
        sorts : IDictionary<Type, Sort>
        e2t : IDictionary<Expr, term>
        t2e : IDictionary<term, encodingResult>
        heapAddresses : IDictionary<Expr, vectorTime>
        staticKeys : IDictionary<Expr, Type>
        regionConstants : Dictionary<regionSort * path, ArrayExpr>
        defaultValues : IDictionary<regionSort, HashSet<Expr[]>>
        mutable lastSymbolicAddress : int32
    } with
        member x.Get(term, encoder : unit -> Expr) =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = {expr = encoder(); assumptions = List.empty}
                x.e2t[result.expr] <- term
                x.t2e[term] <- result
                result)
        member x.Get(term, encoder : unit -> encodingResult) =
            Dict.tryGetValue2 x.t2e term (fun () ->
                let result = encoder()
                x.e2t[result.expr] <- term
                x.t2e[term] <- result
                result)

    let private freshCache () = {
        sorts = Dictionary<Type, Sort>()
        e2t = Dictionary<Expr, term>()
        t2e = Dictionary<term, encodingResult>()
        heapAddresses = Dictionary<Expr, vectorTime>()
        staticKeys = Dictionary<Expr, Type>()
        regionConstants = Dictionary<regionSort * path, ArrayExpr>()
        defaultValues = Dictionary<regionSort, HashSet<Expr[]>>()
        lastSymbolicAddress = 0
    }

// ------------------------------- Encoding: primitives -------------------------------

    type private Z3Builder(ctx : Context) =
        let mutable encodingCache = freshCache()
        let emptyState = Memory.EmptyState()
        let mutable maxBufferSize = 128

        let getMemoryConstant mkConst (typ : regionSort * path) =
            let result : ArrayExpr ref = ref null
            if encodingCache.regionConstants.TryGetValue(typ, result) then result.Value
            else
                let regConst = mkConst()
                encodingCache.regionConstants.Add(typ, regConst)
                regConst

        let getDefaultValues (regionSort : regionSort) =
            let result : HashSet<Expr[]> ref = ref null
            if encodingCache.defaultValues.TryGetValue(regionSort, result) then result.Value
            else
                let suitableKeys = HashSet<Expr[]>()
                encodingCache.defaultValues.Add(regionSort, suitableKeys)
                suitableKeys

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
            if Char.IsDigit id[0] then "_" + id else id

        member private x.AddressSort = ctx.MkBitVecSort(32u) :> Sort

        member private x.Type2Sort typ =
            Dict.getValueOrUpdate encodingCache.sorts typ (fun () ->
                match typ with
                | Bool -> ctx.MkBoolSort() :> Sort
                | typ when typ.IsEnum -> ctx.MkBitVecSort(numericBitSizeOf typ) :> Sort
                | typ when Types.isIntegral typ -> ctx.MkBitVecSort(numericBitSizeOf typ) :> Sort
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

        member private x.MkAnd(left, right) =
            match left, right with
            | _ when left = right -> left
            | True, _ -> right
            | _, True -> left
            | False, _ -> left
            | _, False -> right
            | _ -> ctx.MkAnd(left, right)

        member private x.MkOr(left, right) =
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

        member private x.MkOr ([<ParamArray>] elems : BoolExpr[]) =
            if Array.contains TrueExpr elems then TrueExpr
            else
                let nonFalseElems = Array.filter (fun elem -> elem <> FalseExpr) elems
                x.SimplifyOrElements nonFalseElems

        member private x.MkOr (elems : BoolExpr seq) =
            if Seq.contains TrueExpr elems then TrueExpr
            else
                let nonFalseElems = Seq.filter (fun elem -> elem <> FalseExpr) elems
                x.SimplifyOrElements nonFalseElems

        member private x.MkITE(cond : BoolExpr, thenExpr, elseExpr) : Expr =
            match cond, thenExpr, elseExpr with
            | True, _, _ -> thenExpr
            | False, _, _ -> elseExpr
            | _ when thenExpr = elseExpr -> thenExpr
            | _, (:? BitVecExpr as thenExpr), (:? BitVecExpr as elseExpr) ->
                let thenExpr, elseExpr = x.ExtendIfNeed (thenExpr, elseExpr) true
                ctx.MkITE(cond, thenExpr, elseExpr)
            | _ -> ctx.MkITE(cond, thenExpr, elseExpr)

// ------------------------------- Encoding: arithmetic simplifications -------------------------------

        member private x.ExtendIfNeed (x : BitVecExpr, y : BitVecExpr as args) isSigned =
            let difference = int x.SortSize - int y.SortSize
            if difference = 0 then args
            else
                let extend = if isSigned then ctx.MkSignExt else ctx.MkZeroExt
                if difference > 0 then x, extend(uint32 difference, y)
                else extend(uint32 -difference, x), y

        member private this.ChangeSize (x : BitVecExpr) size isSigned =
            let xSize = int x.SortSize
            let xDiff = size - xSize
            if xDiff = 0 then x
            elif xDiff > 0 then
                let extend = if isSigned then ctx.MkSignExt else ctx.MkZeroExt
                extend(uint32 xDiff, x)
            else
                let from = size - 1
                ctx.MkExtract(uint32 from, 0u, x)

        member private this.ChangeSizeIfNeed (x : BitVecExpr, y : BitVecExpr as args) typ =
            let size = numericBitSizeOf typ |> int
            let xSize = int x.SortSize
            let ySize = int y.SortSize
            if xSize = ySize && xSize = size then args
            else
                let isSigned = isSigned typ
                let x = this.ChangeSize x size isSigned
                let y = this.ChangeSize y size isSigned
                x, y

        member private x.MkEq(left : Expr, right : Expr) =
            match left, right with
            | _ when left = right -> TrueExpr
            | :? BitVecExpr as l, (:? BitVecExpr as r) ->
                x.ExtendIfNeed(l, r) true |> ctx.MkEq
            | _ -> ctx.MkEq(left, right)

        member private x.MkBVSGT(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> FalseExpr
            | _ -> x.ExtendIfNeed operands true |> ctx.MkBVSGT

        member private x.MkBVUGT(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> FalseExpr
            | _ -> x.ExtendIfNeed operands false |> ctx.MkBVUGT

        member private x.MkBVSGE(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> TrueExpr
            | _ -> x.ExtendIfNeed operands true |> ctx.MkBVSGE

        member private x.MkBVUGE(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> TrueExpr
            | _ -> x.ExtendIfNeed operands false |> ctx.MkBVUGE

        member private x.MkBVSLT(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> FalseExpr
            | _ -> x.ExtendIfNeed operands true |> ctx.MkBVSLT

        member private x.MkBVULT(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> FalseExpr
            | _ -> x.ExtendIfNeed operands false |> ctx.MkBVULT

        member private x.MkBVSLE(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> TrueExpr
            | _ -> x.ExtendIfNeed operands true |> ctx.MkBVSLE

        member private x.MkBVULE(left, right as operands) : BoolExpr =
            match left, right with
            | _ when left = right -> TrueExpr
            | _ -> x.ExtendIfNeed operands false |> ctx.MkBVULE

        member private x.MkBVAndT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVAND

        member private x.MkBVOrT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVOR

        member private x.MkBVOr operands : BitVecExpr =
            x.ExtendIfNeed operands false |> ctx.MkBVOR

        member private x.MkBVXorT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVXOR

        member private x.MkBVShlT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVSHL

        member private x.MkBVShl operands : BitVecExpr =
            x.ExtendIfNeed operands true |> ctx.MkBVSHL

        member private x.MkBVAShrT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVASHR

        member private x.MkBVLShrT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVLSHR

        member private x.MkBVLShr operands : BitVecExpr =
            x.ExtendIfNeed operands true |> ctx.MkBVLSHR

        member private x.MkBVAddT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVAdd

        member private x.MkBVAdd operands : BitVecExpr =
            x.ExtendIfNeed operands true |> ctx.MkBVAdd

        member private x.MkBVAddNoUnderflow operands : BoolExpr =
            x.ExtendIfNeed operands true |> ctx.MkBVAddNoUnderflow

        member private x.MkBVAddNoOverflow operands : BoolExpr =
            let left, right = x.ExtendIfNeed operands true
            ctx.MkBVAddNoOverflow(left, right, true)

        member private x.MkBVMulT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVMul

        member private x.MkBVMul operands : BitVecExpr =
            x.ExtendIfNeed operands true |> ctx.MkBVMul

        member private x.MkBVMulNoUnderflow operands : BoolExpr =
            x.ExtendIfNeed operands true |> ctx.MkBVMulNoUnderflow

        member private x.MkBVMulNoOverflow operands : BoolExpr =
            let left, right = x.ExtendIfNeed operands true
            ctx.MkBVMulNoOverflow(left, right, true)

        member private x.MkBVSubT typ operands : BitVecExpr =
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVSub

        member private x.MkBVSub operands : BitVecExpr =
            x.ExtendIfNeed operands true |> ctx.MkBVSub

        member private x.MkBVSDivT typ operands : BitVecExpr =
            assert(isSigned typ)
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVSDiv

        member private x.MkBVUDivT typ operands : BitVecExpr =
            assert(isUnsigned typ)
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVUDiv

        member private x.MkBVSRemT typ operands : BitVecExpr =
            assert(isSigned typ)
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVSRem

        member private x.MkBVURemT typ operands : BitVecExpr =
            assert(isUnsigned typ)
            x.ChangeSizeIfNeed operands typ |> ctx.MkBVURem

        member private x.Max (left : BitVecExpr) (right : BitVecExpr) =
            assert(left.SortSize = right.SortSize)
            x.MkITE(x.MkBVSGT(left, right), left, right) :?> BitVecExpr

        member private x.Min (left : BitVecExpr) (right : BitVecExpr) =
            assert(left.SortSize = right.SortSize)
            x.MkITE(x.MkBVSGT(left, right), right, left) :?> BitVecExpr

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
                x.EncodeMemoryAccessConstant encCtx name source path.Empty typ
            | _ -> x.CreateConstant name typ

// ------------------------------- Encoding: expression -------------------------------

        member private x.EncodeOperation encCtx operation args typ =
            match operation with
            | OperationType.BitwiseNot -> x.MakeUnary encCtx ctx.MkBVNot args
            | OperationType.BitwiseAnd -> x.MakeBinary encCtx (x.MkBVAndT typ) args
            | OperationType.BitwiseOr -> x.MakeBinary encCtx (x.MkBVOrT typ) args
            | OperationType.BitwiseXor -> x.MakeBinary encCtx (x.MkBVXorT typ) args
            // [NOTE] IL specifies: arguments of SHL, SHR, SHR.UN are (int32 and int32), (int64 and int32)
            // So it's needed to extend one of them for Z3
            | OperationType.ShiftLeft -> x.MakeBinary encCtx (x.MkBVShlT typ) args
            | OperationType.ShiftRight -> x.MakeBinary encCtx (x.MkBVAShrT typ) args
            | OperationType.ShiftRight_Un -> x.MakeBinary encCtx (x.MkBVLShrT typ) args
            | OperationType.LogicalNot -> x.MakeUnary encCtx x.MkNot args
            | OperationType.LogicalAnd -> x.MakeOperation encCtx x.MkAnd args
            | OperationType.LogicalOr -> x.MakeOperation encCtx x.MkOr args
            | OperationType.LogicalXor -> x.MakeOperation encCtx ctx.MkXor args
            | OperationType.Equal -> x.MakeBinary encCtx x.MkEq args
            | OperationType.NotEqual -> x.MakeBinary encCtx (x.MkNot << x.MkEq) args
            | OperationType.Greater -> x.MakeBinary encCtx x.MkBVSGT args
            | OperationType.Greater_Un -> x.MakeBinary encCtx x.MkBVUGT args
            | OperationType.GreaterOrEqual -> x.MakeBinary encCtx x.MkBVSGE args
            | OperationType.GreaterOrEqual_Un -> x.MakeBinary encCtx x.MkBVUGE args
            | OperationType.Less -> x.MakeBinary encCtx x.MkBVSLT args
            | OperationType.Less_Un -> x.MakeBinary encCtx x.MkBVULT args
            | OperationType.LessOrEqual -> x.MakeBinary encCtx x.MkBVSLE args
            | OperationType.LessOrEqual_Un -> x.MakeBinary encCtx x.MkBVULE args
            | OperationType.Add -> x.MakeBinary encCtx (x.MkBVAddT typ) args
            | OperationType.AddNoOvf ->
                let operation (l, r) =
                    x.MkAnd(x.MkBVAddNoUnderflow(l, r), x.MkBVAddNoOverflow(l, r))
                x.MakeBinary encCtx operation args
            | OperationType.Multiply -> x.MakeBinary encCtx (x.MkBVMulT typ) args
            | OperationType.MultiplyNoOvf ->
                let operation (l, r) =
                    x.MkAnd(x.MkBVMulNoUnderflow(l, r), x.MkBVMulNoOverflow(l, r))
                x.MakeBinary encCtx operation args
            | OperationType.Subtract -> x.MakeBinary encCtx (x.MkBVSubT typ) args
            | OperationType.Divide -> x.MakeBinary encCtx (x.MkBVSDivT typ) args
            | OperationType.Divide_Un -> x.MakeBinary encCtx (x.MkBVUDivT typ) args
            | OperationType.Remainder -> x.MakeBinary encCtx (x.MkBVSRemT typ) args
            | OperationType.Remainder_Un -> x.MakeBinary encCtx (x.MkBVURemT typ) args
            | OperationType.UnaryMinus -> x.MakeUnary encCtx ctx.MkBVNeg args
            | _ -> __unreachable__()

        member private x.ExtractOrExtend (expr : BitVecExpr) size =
            let exprSize = expr.SortSize
            if exprSize = size then expr
            elif exprSize > size then ctx.MkExtract(size - 1u, 0u, expr)
            else ctx.MkSignExt(size - exprSize, expr)

        member private x.ReverseBytes (expr : BitVecExpr) =
            let size = int expr.SortSize
            assert(size % 8 = 0)
            let bytes = List.init (size / 8) (fun byte -> ctx.MkExtract(uint ((byte + 1) * 8) - 1u, uint (byte * 8), expr))
            List.reduce (fun x y -> ctx.MkConcat(x, y)) bytes

        member private x.ComputeSliceBounds assumptions cuts termSortSize =
            assert(termSortSize % 8u = 0u && termSortSize > 0u)
            let zero = ctx.MkBV(0, termSortSize)
            let sizeExpr = ctx.MkBV(termSortSize / 8u, termSortSize)
            let addBounds (startByte, endByte, pos) (assumptions, startExpr, sizeExpr, position) =
                let assumptions = assumptions @ startByte.assumptions @ endByte.assumptions @ pos.assumptions
                let startByte = x.ExtractOrExtend (startByte.expr :?> BitVecExpr) termSortSize
                let endByte = x.ExtractOrExtend (endByte.expr :?> BitVecExpr) termSortSize
                let pos = x.ExtractOrExtend (pos.expr :?> BitVecExpr) termSortSize
                let startByte = x.MkBVSub(startByte, position)
                let endByte = x.MkBVSub(endByte, position)
                let left = x.Max startByte zero
                let right = x.Min endByte sizeExpr
                let sliceSize = x.MkBVSub(right, left)
                let newLeft = x.MkBVAdd(startExpr, left)
                let newRight = x.Min (x.MkBVAdd(newLeft, sliceSize)) sizeExpr
                let newPos = x.Max pos zero
                assumptions, newLeft, newRight, newPos
            List.foldBack addBounds cuts (assumptions, zero, sizeExpr, zero)

        member private x.EncodeBoolBitVec (b : BoolExpr) =
            assert(sizeof<bool> = sizeof<byte>)
            let trueBitVec = ctx.MkNumeral(1, x.Type2Sort typeof<byte>)
            let falseBitVec = ctx.MkNumeral(0, x.Type2Sort typeof<byte>)
            x.MkITE(b, trueBitVec, falseBitVec) :?> BitVecExpr

        member private x.EncodeCombine encCtx slices typ =
            let res = ctx.MkNumeral(0, x.Type2Sort typ) :?> BitVecExpr
            let window = res.SortSize
            let windowExpr = ctx.MkNumeral(window, x.Type2Sort(Types.IndexType)) :?> BitVecExpr
            let addOneSlice (res, assumptions) slice =
                let term, cuts =
                    match slice.term with
                    | Slice(term, cuts) ->
                        let slices = List.map (fun (s, e, pos) -> x.EncodeTerm encCtx s, x.EncodeTerm encCtx e, x.EncodeTerm encCtx pos) cuts
                        x.EncodeTerm encCtx term, slices
                    | _ -> x.EncodeTerm encCtx slice, List.empty
                let t =
                    match term.expr with
                    | :? BitVecExpr as bv -> bv
                    | :? BoolExpr as b -> x.EncodeBoolBitVec b
                    | _ -> internalfail $"EncodeCombine: unexpected slice term {term}"
                let assumptions = assumptions @ term.assumptions
                let termSize = t.SortSize
                let sizeExpr = ctx.MkBV(termSize, termSize)
                let assumptions, lByte, rByte, posByte = x.ComputeSliceBounds assumptions cuts termSize
                let lByte = x.ExtractOrExtend lByte termSize
                let rByte = x.ExtractOrExtend rByte termSize
                let posByte = x.ExtractOrExtend posByte termSize
                let lBit = x.MkBVMul(lByte, ctx.MkBV(8, termSize))
                let rBit = x.MkBVMul(rByte, ctx.MkBV(8, termSize))
                let posBit = x.MkBVMul(posByte, ctx.MkBV(8, termSize))
                let sliceSize = x.MkBVSub(rBit, lBit)
                let zero = ctx.MkBV(0, termSize)
                let intersects = x.MkBVSGT(sliceSize, zero)
                let term = x.ReverseBytes t
                let left = x.ExtractOrExtend lBit termSize
                let term = x.MkBVShl(term, left)
                let cutRight = x.ExtractOrExtend (x.MkBVSub(sizeExpr, rBit)) termSize
                let term = x.MkBVLShr(term, x.MkBVAdd(left, cutRight))
                let term =
                    if termSize > window then ctx.MkExtract(window - 1u, 0u, term)
                    else ctx.MkZeroExt(window - termSize, term)
                let changedTermSize = term.SortSize
                let w = x.ExtractOrExtend windowExpr changedTermSize
                let pos = x.ExtractOrExtend posBit changedTermSize
                let sliceSize = x.ExtractOrExtend sliceSize changedTermSize
                let shift = x.MkBVSub(w, x.MkBVAdd(pos, sliceSize))
                let part = x.MkBVShl(term, shift)
                let res = x.MkITE(intersects, x.MkBVOr(res, part), res) :?> BitVecExpr
                res, assumptions
            let result, assumptions = List.fold addOneSlice (res, List.empty) slices
            {expr = x.ReverseBytes result; assumptions = assumptions}

        member private x.EncodeExpression encCtx term op args typ =
            encodingCache.Get(term, fun () ->
                match op with
                | Operator operation ->
                    x.EncodeOperation encCtx operation args typ
                | Application sf ->
                    let decl = ctx.MkConstDecl(sf |> toString |> IdGenerator.startingWith, x.Type2Sort typ)
                    x.MakeOperation encCtx (fun x -> ctx.MkApp(decl, x)) args
                | Cast(Numeric t1, Numeric t2) when isReal t1 || isReal t2 ->
                    failToEncode "encoding real numbers is not implemented"
                | Cast(Numeric t1, Numeric t2) when isLessForNumericTypes t1 t2 ->
                    let expr = x.EncodeTerm encCtx (List.head args)
                    let difference = numericBitSizeOf t2 - numericBitSizeOf t1
                    let extend = if isUnsigned t2 then ctx.MkZeroExt else ctx.MkSignExt
                    {expr = extend(difference, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric t1, Numeric t2) when isLessForNumericTypes t2 t1 ->
                    let expr = x.EncodeTerm encCtx (List.head args)
                    let from = numericBitSizeOf t2 - 1u
                    {expr = ctx.MkExtract(from, 0u, expr.expr :?> BitVecExpr); assumptions = expr.assumptions}
                | Cast(Numeric t1, Numeric t2) when numericBitSizeOf t1 = numericBitSizeOf t2 ->
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

        member private x.EncodeSymbolicAddress encCtx (heapRefSource : ISymbolicConstantSource) path name =
            x.EncodeMemoryAccessConstant encCtx name heapRefSource path addressType

        // TODO: [style] get rid of accumulators
        member private x.GetRegionConstant (name : string) sort (path : path) (regionSort : regionSort) =
            let constName = $"{name} of {regionSort}, {path}"
            let mkConst () = ctx.MkConst(constName, sort) :?> ArrayExpr
            getMemoryConstant mkConst (regionSort, path)

        member private x.MemoryReading encCtx specializeWithKey keysAreMatch encodeKey inst (path : path) readKey mo =
            let updates = MemoryRegion.flatten mo
            let assumptions, readExpr = encodeKey readKey
            let readRegion = (readKey :> IMemoryKey<'a, 'b>).Region
            let readKeyInRegionGuard = (readKey :> IMemoryKey<'a,'b>).InRegionCondition readRegion
            let {assumptions = inRegionAssumptions} = x.EncodeTerm encCtx readKeyInRegionGuard
            let assumptions = inRegionAssumptions @ assumptions
            let inst, instAssumptions = inst readExpr
            let assumptions = instAssumptions @ assumptions
            let checkOneKey (updateKey, reg, value) (acc, assumptions) =
                // NOTE: we need constraints on right key, because path condition may contain it
                // EXAMPLE: a[k] = 1; if (k == 0 && a[i] == 1) {...}
                let matchAssumptions, keysAreMatch = keysAreMatch readKey updateKey reg
                // TODO: [style] auto append assumptions
                let assumptions = List.append assumptions matchAssumptions
                let readFieldIfNeed term path =
                    match path with
                    | StructFieldPart field ->
                        assert(IsStruct term)
                        Memory.ReadField emptyState term field
                    | PointerAddress ->
                        assert(IsPtr term)
                        Memory.ExtractAddress term
                    | PointerOffset ->
                        assert(IsPtr term)
                        Memory.ExtractPointerOffset term
                let value = path.Fold readFieldIfNeed value
                let valueExpr = specializeWithKey value readKey updateKey |> x.EncodeTerm encCtx
                let assumptions = List.append assumptions valueExpr.assumptions
                x.MkITE(keysAreMatch, valueExpr.expr, acc), assumptions
            let expr, assumptions = List.foldBack checkOneKey updates (inst, assumptions)
            encodingResult.Create(expr, assumptions)

        member private x.HeapReading encCtx key mo typ source path name =
            assert mo.defaultValue.IsNone
            let encodeKey (k : heapAddressKey) = x.EncodeTerm encCtx k.address |> toTuple
            let regionSort = GetHeapReadingRegionSort source
            let inst (k : Expr) =
                let sort = ctx.MkArraySort(x.Type2Sort addressType, x.Type2Sort typ)
                let array = x.GetRegionConstant name sort path regionSort
                let defaultValues = getDefaultValues regionSort
                defaultValues.Add(Array.singleton k) |> ignore
                let expr = ctx.MkSelect(array, k)
                expr, x.GenerateInstAssumptions expr typ
            let keysAreMatch read update updateRegion =
                let matchCondition = (read :> IMemoryKey<'a,'b>).MatchCondition update updateRegion
                let {expr = matchConditionEncoded; assumptions = encodeAssumptions } = x.EncodeTerm encCtx matchCondition
                encodeAssumptions, matchConditionEncoded :?> BoolExpr
            let specialize v _ _ = v
            let res = x.MemoryReading encCtx specialize keysAreMatch encodeKey inst path key mo
            match regionSort with
            | HeapFieldSort field when field = Reflection.stringLengthField -> x.GenerateLengthAssumptions res
            | _ -> res

        // Generating assumptions for instantiation
        member private x.GenerateInstAssumptions (inst : Expr) typ =
            if typ = addressType then
                // In case of symbolic address instantiation, adding assumption "inst < 0"
                let zero = ctx.MkNumeral(0, inst.Sort) :?> BitVecExpr
                let belowZero = x.MkBVSLE(inst :?> BitVecExpr, zero)
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
            if API.CharsArePretty then
                let expr = encodingResult.expr :?> BitVecExpr
                let assumptions = encodingResult.assumptions
                let left = ctx.MkBVSGT(expr, ctx.MkBV(32, expr.SortSize))
                let right = ctx.MkBVSLT(expr, ctx.MkBV(127, expr.SortSize))
                {expr = expr; assumptions = left :: right :: assumptions}
            else encodingResult

        member private x.ArrayReading encCtx specialize keysAreMatch encodeKey hasDefaultValue indices key mo typ source path name =
            assert mo.defaultValue.IsNone
            let inst (k : Expr[]) =
                let domainSort = x.Type2Sort addressType :: List.map (x.Type2Sort Types.IndexType |> always) indices |> Array.ofList
                let valueSort = x.Type2Sort typ
                if hasDefaultValue then x.DefaultValue valueSort, List.empty
                else
                    let regionSort = GetHeapReadingRegionSort source
                    let defaultValues = getDefaultValues regionSort
                    defaultValues.Add(k) |> ignore
                    let sort = ctx.MkArraySort(domainSort, valueSort)
                    let array = x.GetRegionConstant name sort path regionSort
                    let expr = ctx.MkSelect(array, k)
                    expr, x.GenerateInstAssumptions expr typ
            let res = x.MemoryReading encCtx specialize keysAreMatch encodeKey inst path key mo
            let res = if typ = typeof<char> then x.GenerateCharAssumptions res else res
            match GetHeapReadingRegionSort source with
            | ArrayLengthSort _ -> x.GenerateLengthAssumptions res
            | _ -> res

        member private x.ArrayIndexReading encCtx key hasDefaultValue mo typ source path name =
            let encodeKey = function
                | OneArrayIndexKey(address, indices) -> address :: indices |> x.EncodeTerms encCtx
                | RangeArrayIndexKey _ as key -> internalfail $"EncodeMemoryAccessConstant: unexpected array key {key}"
            let arraysEquality (left, right) =
                Seq.map2 (fun l r -> x.MkEq(l, r)) left right |> x.MkAnd
            let keysAreMatch read (update : heapArrayKey) updateRegion =
                let matchCondition = (read :> IMemoryKey<'a,'b>).MatchCondition update updateRegion
                let {expr = matchConditionEncoded; assumptions = assumptions} = x.EncodeTerm encCtx matchCondition
                assumptions, matchConditionEncoded :?> BoolExpr
            let indices =
                match key with
                | OneArrayIndexKey(_, indices) -> indices
                | _ -> internalfail $"EncodeMemoryAccessConstant: unexpected array key {key}"
            x.ArrayReading encCtx SpecializeWithKey keysAreMatch encodeKey hasDefaultValue indices key mo typ source path name

        member private x.VectorIndexReading encCtx (key : heapVectorIndexKey) hasDefaultValue mo typ source path name =
            let encodeKey (k : heapVectorIndexKey) =
                [|k.address; k.index|] |> x.EncodeTerms encCtx
            let keysAreEqual (left : Expr[], right : Expr[]) =
                x.MkAnd(x.MkEq(left[0], right[0]), x.MkEq(left[1], right[1]))
            let keysAreMatch read update updateRegion =
                let matchCondition = (read :> IMemoryKey<'a,'b>).MatchCondition update updateRegion
                let {expr = matchConditionEncoded; assumptions = assumptions} = x.EncodeTerm encCtx matchCondition
                assumptions, matchConditionEncoded :?> BoolExpr
            let specialize v _ _ = v
            x.ArrayReading encCtx specialize keysAreMatch encodeKey hasDefaultValue [key.index] key mo typ source path name

        member private x.StackBufferReading encCtx key mo typ source path name =
            assert mo.defaultValue.IsNone
            let encodeKey (k : stackBufferIndexKey) = x.EncodeTerm encCtx k.index |> toTuple
            let inst (k : Expr) =
                let regionSort = GetHeapReadingRegionSort source
                let sort = ctx.MkArraySort(x.Type2Sort addressType, x.Type2Sort typ)
                let array = x.GetRegionConstant name sort path regionSort
                let defaultValues = getDefaultValues regionSort
                defaultValues.Add(Array.singleton k) |> ignore
                let expr = ctx.MkSelect(array, k)
                expr, x.GenerateInstAssumptions expr typ
            let keysAreMatch read update updateRegion =
               let matchCondition = (read :> IMemoryKey<'a,'b>).MatchCondition update updateRegion
               let {expr = matchConditionEncoded; assumptions = assumptions} = x.EncodeTerm encCtx matchCondition
               assumptions, matchConditionEncoded :?> BoolExpr
            let specialize v _ _ = v
            x.MemoryReading encCtx specialize keysAreMatch encodeKey inst path key mo

        member private x.StaticsReading encCtx (key : symbolicTypeKey) mo typ source path (name : string) =
            assert mo.defaultValue.IsNone
            let updates = MemoryRegion.flatten mo
            let value = Seq.tryFind (fun (k, _, _) -> k = key) updates
            match value with
            | Some (_, _, v) -> x.EncodeTerm encCtx v
            | None ->
                let keyType = x.Type2Sort Types.IndexType
                let sort = ctx.MkArraySort(keyType, x.Type2Sort typ)
                let regionSort = GetHeapReadingRegionSort source
                let array = x.GetRegionConstant name sort path regionSort
                let encodedKey = ctx.MkConst(key.ToString(), keyType)
                encodingCache.staticKeys.Add(encodedKey, key.typ)
                let defaultValues = getDefaultValues regionSort
                defaultValues.Add(Array.singleton encodedKey) |> ignore
                {expr = ctx.MkSelect(array, encodedKey); assumptions = List.empty}

        member private x.StructReading encCtx (structSource : ISymbolicConstantSource) (field : fieldId) typ (path : path) name =
            let path = path.StructField field
            let res = x.EncodeMemoryAccessConstant encCtx name structSource path typ
            match field with
            | _ when field.declaringType = typeof<decimal> && field.name = "_flags" ->
                let expr = res.expr :?> BitVecExpr
                let lowerWord = ctx.MkExtract(15u, 0u, expr)
                let lowerIsZero = x.MkEq(lowerWord, ctx.MkBV(0, lowerWord.SortSize))
                let exp = ctx.MkExtract(23u, 16u, expr)
                let expSize = exp.SortSize
                let leftBound = x.MkBVUGE(exp, ctx.MkBV(0, expSize))
                let rightBound = x.MkBVULE(exp, ctx.MkBV(28, expSize))
                let expInBound = x.MkAnd(leftBound, rightBound)
                let upper = ctx.MkExtract(30u, 24u, expr)
                let upperIsZero = x.MkEq(upper, ctx.MkBV(0, upper.SortSize))
                { res with assumptions = lowerIsZero::expInBound::upperIsZero::res.assumptions }
            | _ -> res

        member private x.EncodeMemoryAccessConstant encCtx name (source : ISymbolicConstantSource) (path : path) typ : encodingResult =
            match source with
            | HeapReading(key, mo) -> x.HeapReading encCtx key mo typ source path name
            | ArrayIndexReading(hasDefaultValue, key, mo) ->
                x.ArrayIndexReading encCtx key hasDefaultValue mo typ source path name
            | VectorIndexReading(hasDefaultValue, key, mo) ->
                x.VectorIndexReading encCtx key hasDefaultValue mo typ source path name
            | StackBufferReading(key, mo) -> x.StackBufferReading encCtx key mo typ source path name
            | StaticsReading(key, mo) -> x.StaticsReading encCtx key mo typ source path name
            | StructFieldSource(structSource, field) -> x.StructReading encCtx structSource field typ path name
            | HeapAddressSource source ->
                assert(typ = addressType)
                x.EncodeSymbolicAddress encCtx source path name
            | PointerAddressSource source ->
                assert(typ = addressType)
                let path = path.PointerAddress()
                x.EncodeMemoryAccessConstant encCtx name source path addressType
            | PointerOffsetSource source ->
                assert(typ = typeof<int>)
                let path = path.PointerOffset()
                x.EncodeMemoryAccessConstant encCtx name source path typeof<int>
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
                let index = x.Decode Types.IndexType exprs[1]
                ArrayLength(heapAddress, index, typ)
            | ArrayLowerBoundSort typ ->
                assert(exprs.Length = 2)
                let heapAddress = x.DecodeConcreteHeapAddress exprs[0] |> ConcreteHeapAddress
                let index = x.Decode Types.IndexType exprs[1]
                ArrayLowerBound(heapAddress, index, typ)
            | StackBufferSort key ->
                assert(exprs.Length = 1)
                let index = x.Decode typeof<int8> exprs[0]
                StackBufferIndex(key, index)
            | BoxedSort typ ->
                let address = x.DecodeConcreteHeapAddress exprs[0] |> ConcreteHeapAddress
                BoxedLocation(address, typ)

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
            | _ when t = addressType ->
                x.DecodeConcreteHeapAddress expr |> ConcreteHeapAddress
            | _ when not (Types.IsValueType t) ->
                let address = x.DecodeConcreteHeapAddress expr |> ConcreteHeapAddress
                HeapRef address t
            | :? BitVecExpr as bv when bv.IsConst ->
                if encodingCache.e2t.ContainsKey(expr) then encodingCache.e2t[expr]
                else x.GetTypeOfBV bv |> Concrete expr.String
            | :? IntNum as i -> Concrete i.Int typeof<int>
            | :? RatNum as r -> Concrete (double(r.Numerator.Int) * 1.0 / double(r.Denominator.Int)) typeof<int>
            | _ ->
                if encodingCache.e2t.ContainsKey(expr) then encodingCache.e2t[expr]
                elif expr.IsTrue then True()
                elif expr.IsFalse then False()
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

        member private x.WriteByPath term value (path : pathPart list) =
            match path, term.term with
            | [PointerAddress], Ptr(HeapLocation(_, t), sightType, offset) ->
                Ptr (HeapLocation(value, t)) sightType offset
            | [PointerOffset], Ptr(HeapLocation(address, t), sightType, _) ->
                Ptr (HeapLocation(address, t)) sightType value
            | [StructFieldPart field], Struct _ ->
                Memory.WriteStructField term field value
            | StructFieldPart field :: parts, Struct(contents, _) ->
                let recurred = x.WriteByPath contents[field] value parts
                Memory.WriteStructField term field recurred
            | StructFieldPart _ :: _, _ -> internalfail $"WriteByPath: expected structure, but got {term}"
            | PointerOffset _ :: parts, _
            | PointerAddress _ :: parts, _ when List.isEmpty parts |> not ->
                internalfail $"WriteByPath: unexpected path {path}"
            | _ -> internalfail $"WriteByPath: unexpected term {term} and path {path}"

        member private x.WriteDictOfValueTypes (dict : IDictionary<'key, term ref>) (key : 'key) (path : path) t value =
            if path.IsEmpty then
                assert(not <| dict.ContainsKey key)
                dict.Add(key, ref value)
            else
                let term = Dict.getValueOrUpdate dict key (fun () ->
                    Memory.DefaultOf t |> ref)
                term.Value <- x.WriteByPath term.Value value path.parts

        member private x.WriteToPrimitiveModel (subst : IDictionary<ISymbolicConstantSource, term ref>) (m : Model) (path : path) source expr typ constant =
            let refinedExpr = m.Eval(expr, false)
            let t = if path.IsEmpty then typ else path.TypeOfLocation
            let term = x.Decode t refinedExpr
            assert(not (constant = term) || expr = refinedExpr)
            if constant <> term then
                x.WriteDictOfValueTypes subst source path typ term

        member private x.FillRegion state (m : Model) (regionSort : regionSort) constantValue =
            match regionSort with
            | HeapFieldSort field ->
                let suitableKeys = HashSet<IHeapAddressKey>()
                for expressions in getDefaultValues regionSort do
                    assert(Array.length expressions = 1)
                    let refinedAddress = m.Eval(expressions[0], false)
                    let address = x.DecodeConcreteHeapAddress refinedAddress |> ConcreteHeapAddress
                    suitableKeys.Add({address = address}) |> ignore
                let isSuitable = suitableKeys.Contains
                Memory.FillClassFieldsRegion state field constantValue isSuitable
            | StaticFieldSort typ ->
                let suitableKeys = HashSet<ISymbolicTypeKey>()
                for expressions in getDefaultValues regionSort do
                    assert(Array.length expressions = 1)
                    let refinedAddress = m.Eval(expressions[0], false)
                    let typ = x.DecodeSymbolicTypeAddress refinedAddress
                    suitableKeys.Add({typ = typ}) |> ignore
                let isSuitable = suitableKeys.Contains
                Memory.FillStaticsRegion state typ constantValue isSuitable
            | ArrayIndexSort arrayType ->
                let suitableAddresses = HashSet<term>()
                for expressions in getDefaultValues regionSort do
                    let refinedAddress = m.Eval(expressions[0], false)
                    let address = x.DecodeConcreteHeapAddress refinedAddress |> ConcreteHeapAddress
                    suitableAddresses.Add(address) |> ignore
                let isSuitable (key : IHeapArrayKey) =
                    match key with
                    | :? heapArrayKey as OneArrayIndexKey(address, _) -> suitableAddresses.Contains address
                    | _ -> false
                Memory.FillArrayRegion state arrayType constantValue isSuitable
            | ArrayLengthSort arrayType ->
                let suitableKeys = HashSet<IHeapVectorIndexKey>()
                for expressions in getDefaultValues regionSort do
                    assert(Array.length expressions = 2)
                    let refinedAddress = m.Eval(expressions[0], false)
                    let address = x.DecodeConcreteHeapAddress refinedAddress |> ConcreteHeapAddress
                    let index = m.Eval(expressions[1], false) |> x.Decode typeof<int>
                    suitableKeys.Add({address = address; index = index}) |> ignore
                let isSuitable = suitableKeys.Contains
                Memory.FillLengthRegion state arrayType constantValue isSuitable
            | ArrayLowerBoundSort arrayType ->
                let suitableKeys = HashSet<IHeapVectorIndexKey>()
                for expressions in getDefaultValues regionSort do
                    assert(Array.length expressions = 2)
                    let refinedAddress = m.Eval(expressions[0], false)
                    let address = x.DecodeConcreteHeapAddress refinedAddress |> ConcreteHeapAddress
                    let index = m.Eval(expressions[1], false) |> x.Decode typeof<int>
                    suitableKeys.Add({address = address; index = index}) |> ignore
                let isSuitable = suitableKeys.Contains
                Memory.FillLowerBoundRegion state arrayType constantValue isSuitable
            | StackBufferSort key ->
                let suitableKeys = HashSet<IStackBufferIndexKey>()
                for expressions in getDefaultValues regionSort do
                    assert(Array.length expressions = 1)
                    let index = m.Eval(expressions[0], false) |> x.Decode typeof<int8>
                    suitableKeys.Add({index = index}) |> ignore
                let isSuitable = suitableKeys.Contains
                Memory.FillStackBufferRegion state key constantValue isSuitable
            | BoxedSort typ ->
                let suitableKeys = HashSet<IHeapAddressKey>()
                for expressions in getDefaultValues regionSort do
                    assert(Array.length expressions = 1)
                    let refinedAddress = m.Eval(expressions[0], false)
                    let address = x.DecodeConcreteHeapAddress refinedAddress |> ConcreteHeapAddress
                    suitableKeys.Add({address = address}) |> ignore
                let isSuitable = suitableKeys.Contains
                Memory.FillBoxedRegion state typ constantValue isSuitable

        member x.MkModel (m : Model) =
            try
                let stackEntries = Dictionary<stackKey, term ref>()
                let state = {Memory.EmptyState() with complete = true}
                let primitiveModel = Dictionary<ISymbolicConstantSource, term ref>()

                for KeyValue(key, value) in encodingCache.t2e do
                    match key with
                    | {term = Constant(_, Path.Path(path, StackReading(key)), t)} as constant ->
                        let refinedExpr = m.Eval(value.expr, false)
                        let decoded = x.Decode t refinedExpr
                        if decoded <> constant then
                            x.WriteDictOfValueTypes stackEntries key path key.TypeOfLocation decoded
                    | {term = Constant(_, (:? IMemoryAccessConstantSource as ms), _)} as constant ->
                        match ms with
                        | HeapAddressSource(Path.Path(path, StackReading(key))) ->
                            let refinedExpr = m.Eval(value.expr, false)
                            let t = if path.IsEmpty then key.TypeOfLocation else path.TypeOfLocation
                            let address = x.DecodeConcreteHeapAddress refinedExpr |> ConcreteHeapAddress
                            let value = HeapRef address t
                            x.WriteDictOfValueTypes stackEntries key path key.TypeOfLocation value
                        | HeapAddressSource(Path.Path(path, (:? functionResultConstantSource as frs)))
                        | Path.Path(path, (:? functionResultConstantSource as frs)) ->
                            let t = (frs :> ISymbolicConstantSource).TypeOfLocation
                            x.WriteToPrimitiveModel primitiveModel m path frs value.expr t constant
                        | _ -> ()
                    | {term = Constant(_, :? IStatedSymbolicConstantSource, _)} -> ()
                    | {term = Constant(_, source, t)} as constant ->
                        x.WriteToPrimitiveModel primitiveModel m path.Empty source value.expr t constant
                    | _ -> ()

                let frame = stackEntries |> Seq.map (fun kvp ->
                    let key = kvp.Key
                    let term = kvp.Value.Value
                    let typ = TypeOf term
                    (key, Some term, typ))
                Memory.NewStackFrame state None (List.ofSeq frame)

                let pointers = Dictionary<address, term>()
                let defaultValues = Dictionary<regionSort, term ref>()
                for KeyValue(typ, constant) in encodingCache.regionConstants do
                    let region, path = typ
                    let arr = m.Eval(constant, false)
                    let typeOfLocation =
                        if path.IsEmpty then region.TypeOfLocation
                        else path.TypeOfLocation
                    let rec parseArray (arr : Expr) =
                        if arr.IsConstantArray then
                            assert(arr.Args.Length = 1)
                            let constantValue = x.Decode typeOfLocation arr.Args[0]
                            x.WriteDictOfValueTypes defaultValues region path region.TypeOfLocation constantValue
                        elif arr.IsDefaultArray then
                            assert(arr.Args.Length = 1)
                        elif arr.IsStore then
                            assert(arr.Args.Length >= 3)
                            parseArray arr.Args[0]
                            let address = x.DecodeMemoryKey region arr.Args[1..arr.Args.Length - 2]
                            let value = Array.last arr.Args |> x.Decode typeOfLocation
                            let address, ptrPart = path.ToAddress address
                            match ptrPart with
                            | Some kind ->
                                let exists, ptr = pointers.TryGetValue(address)
                                let ptr =
                                    if exists then ptr
                                    else Memory.DefaultOf address.TypeOfLocation
                                match kind, ptr.term with
                                | PointerAddress, Ptr(HeapLocation(_, t), s, o) ->
                                    pointers[address] <- Ptr (HeapLocation(value, t)) s o
                                | PointerOffset, Ptr(HeapLocation(a, t), s, _) ->
                                    pointers[address] <- Ptr (HeapLocation(a, t)) s value
                                | _ -> internalfail $"MkModel: unexpected path {kind}"
                            | None ->
                                let states = Memory.Write state (Ref address) value
                                assert(states.Length = 1 && states[0] = state)
                        elif arr.IsConst then ()
                        elif arr.IsQuantifier then
                            let quantifier = arr :?> Quantifier
                            let body = quantifier.Body
                            // This case decodes predicates, for example: \x -> x = 100, where 'x' is address
                            if body.IsApp && body.IsEq && body.Args.Length = 2 then
                                // Firstly, setting all values to 'false'
                                x.WriteDictOfValueTypes defaultValues region path region.TypeOfLocation (MakeBool false)
                                let address = x.DecodeMemoryKey region (Array.singleton body.Args[1])
                                let address, ptrPart = path.ToAddress address
                                assert(Option.isNone ptrPart)
                                // Secondly, setting 'true' value for concrete address from predicate
                                let states = Memory.Write state (Ref address) (MakeBool true)
                                assert(states.Length = 1 && states[0] = state)
                            else internalfailf "Unexpected quantifier expression in model: %O" arr
                        else internalfailf "Unexpected array expression in model: %O" arr
                    parseArray arr

                for KeyValue(address, ptr) in pointers do
                    let states = Memory.Write state (Ref address) ptr
                    assert(states.Length = 1 && states[0] = state)

                for KeyValue(regionSort, value) in defaultValues do
                    let constantValue = value.Value
                    x.FillRegion state m regionSort constantValue

                state.startingTime <- [encodingCache.lastSymbolicAddress - 1]

                encodingCache.heapAddresses.Clear()

                let subst = Dictionary<ISymbolicConstantSource, term>()
                for KeyValue(k, v) in primitiveModel do
                    subst.Add(k, v.Value)

                state.model <- PrimitiveModel subst
                StateModel state
            with e -> internalfail $"MkModel: caught exception {e}"


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
