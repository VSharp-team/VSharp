namespace VSharp.Solver

open System
open Microsoft.Z3
open System.Collections.Generic
open VSharp
open VSharp.TypeUtils
open VSharp.Core
open VSharp.Core.SolverInteraction
open Logger

module internal Z3 =
    [<Struct>]
    type OptionalBuilder =
        member __.Bind(opt, binder) = Option.bind binder opt
        member __.Return(value) = Some value
        member __.ReturnFrom(value) = value
        member __.Zero() = None
        member __.Using(resource : #System.IDisposable, binder) = let result = binder resource in resource.Dispose(); result
    let opt = OptionalBuilder()
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
        heapAddresses : IDictionary<Type * Expr, vectorTime>
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
        heapAddresses = Dictionary<Type * Expr, vectorTime>()
        staticKeys = Dictionary<Expr, Type>()
        regionConstants = Dictionary<regionSort * fieldId list, ArrayExpr>()
        lastSymbolicAddress = 0
    }

    // -----------------------Concrete memory -------------------------
    let rec unboxConcrete state defaults term =
        match Terms.TryTermToObj state (createObjOfType state defaults) term with                
        | Some o -> Some (o |> unbox)
        | None -> None

    and createObjOfType state (defaults : Dictionary<regionSort, term ref>) addr typ =
        let cm = state.concreteMemory
        try
            let unboxConcrete' t = unboxConcrete state defaults t
            let cha = addr |> ConcreteHeapAddress
            let result = ref (ref Nop)
            match typ with
            | ArrayType(_, SymbolicDimension _) -> ()
            | ArrayType(elemType, _) ->                 
                let eval address =
                    address |> Ref |> Memory.Read state |> unboxConcrete' |> Option.get
                let arrayType, (lengths : int array), (lowerBounds : int array) =
                    Memory.ReadArrayParams typ cha eval
                let length = Array.reduce ( * ) lengths
                if length > 128 then () // TODO: move magic number
                else
                    let arr = if (lowerBounds = null) then Array.CreateInstance(elemType, lengths)
                              else Array.CreateInstance(elemType, lengths, lowerBounds)
                    cm.Allocate addr arr
                    if defaults.TryGetValue(ArrayIndexSort arrayType, result) then
                        match result.Value.Value with
                            | {term = HeapRef({term = ConcreteHeapAddress(a)}, _)} ->
                                cm.AddDep a addr
                            | _ -> ()
                        let value =  result.Value.Value |> unboxConcrete' |> Option.get
                        Array.fill arr value
            | _ when typ = typeof<string> ->
                let arTyp = (typeof<char>, 1, true)
                let l : int = ClassField(cha, Reflection.stringLengthField) |> Ref
                                   |> Memory.Read state |> unboxConcrete' |> Option.get
                let contents = Array.init l (fun i -> ArrayIndex(cha, [MakeNumber i], arTyp) |> Ref
                                                      |> Memory.Read state |> unboxConcrete' |> Option.get) 
                cm.Allocate addr (String(contents) :> obj)
            | _ ->
                let o = Reflection.createObject typ
                cm.Allocate addr o
                Reflection.fieldsOf false typ |>
                    Seq.iter (fun (fid, _) ->
                        let region = HeapFieldSort fid
                        if defaults.TryGetValue(region, result) then
                            match result.Value.Value with
                            | {term = HeapRef({term = ConcreteHeapAddress(a)}, _)} ->
                                cm.AddDep a addr
                            | _ -> ()
                            let value = result.Value.Value |> unboxConcrete' |> Option.get
                            cm.WriteClassField addr fid value
                )
        with
        | :? MemberAccessException as e -> // Could not create an instance
            if cm.Contains addr then cm.Remove addr
            raise e 

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

        member x.Reset() =
            encodingCache <- freshCache()

        member x.SetMaxBufferSize size =
            maxBufferSize <- size

        member private x.ValidateId id =
            assert(not <| String.IsNullOrWhiteSpace id)
            if Char.IsDigit id.[0] then "_" + id else id

        member private x.AddressSort = ctx.MkBitVecSort(32u) :> Sort

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

        member private x.True = ctx.MkTrue()

        member private x.MkEq(left, right) =
            if left = right then x.True
            else ctx.MkEq(left, right)

        member x.MkAnd(left, right) =
            if left = x.True then right
            elif right = x.True then left
            else ctx.MkAnd(left, right)

        member private x.SimplifyAndElements (nonTrueElems : BoolExpr seq) =
            match nonTrueElems with
            | Seq.Empty -> x.True
            | Seq.Cons(head, tail) when Seq.isEmpty tail -> head
            | _ -> ctx.MkAnd(nonTrueElems)

        member x.MkAnd ([<ParamArray>] elems) =
            let nonTrueElems = Array.filter (fun elem -> elem <> x.True) elems
            x.SimplifyAndElements nonTrueElems

        member x.MkAnd elems =
            let nonTrueElems = Seq.filter (fun elem -> elem <> x.True) elems
            x.SimplifyAndElements nonTrueElems

        member private x.DefaultValue sort = ctx.MkNumeral(0, sort)
        member private x.EncodeConcreteAddress encCtx (address : concreteHeapAddress) =
            ctx.MkNumeral(encCtx.addressOrder.[address], x.Type2Sort addressType)
            // TODO: cache in heapAddresses?

        member private x.CreateConstant name typ =
            ctx.MkConst(x.ValidateId name, x.Type2Sort typ) |> encodingResult.Create

// ------------------------------- Encoding: common -------------------------------

        member public x.EncodeTerm encCtx (t : term) : encodingResult =
            let getResult () =
                let result =
                    match t.term with
                    | Concrete(obj, typ) -> x.EncodeConcrete encCtx obj typ
                    | Constant(name, source, typ) -> x.EncodeConstant encCtx name.v source typ
                    | Expression(op, args, typ) -> x.EncodeExpression encCtx t op args typ
                    | HeapRef(address, _) -> x.EncodeTerm encCtx address
                    | _ -> internalfailf "unexpected term: %O" t
                let typ = TypeOf t
                if typ.IsEnum then x.AddEnumAssumptions encCtx typ result
                else result
            encodingCache.Get(t, getResult)

        member private x.AddEnumAssumptions encCtx typ (encodingResult : encodingResult) =
            assert typ.IsEnum
            let expr = encodingResult.expr
            let values = Enum.GetValues typ |> System.Linq.Enumerable.OfType<obj>
            let createAssumption assumptions value =
                let assumptions', concrete = x.EncodeConcrete encCtx value typ |> toTuple
                ctx.MkEq(expr, concrete), assumptions @ assumptions'
            let options, assumptions = Seq.mapFold createAssumption encodingResult.assumptions values
            let enumAssumptions = ctx.MkOr options
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
                    | :? concreteHeapAddress as addr ->
                        assert(List.isEmpty addr |> not)
                        x.EncodeConcreteAddress encCtx addr
                    | _ -> __unreachable__()
                | _ -> __notImplemented__()
            encodingResult.Create expr

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
            | OperationType.AddNoOvf -> x.MakeBinary encCtx (fun (x, y) -> ctx.MkAnd(ctx.MkBVAddNoUnderflow(x, y), ctx.MkBVAddNoOverflow(x, y, true))) args
            | OperationType.Multiply -> x.MakeBinary encCtx ctx.MkBVMul args
            | OperationType.MultiplyNoOvf -> x.MakeBinary encCtx (fun (x, y) -> ctx.MkAnd(ctx.MkBVMulNoUnderflow(x, y), ctx.MkBVMulNoOverflow(x, y, true))) args
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
                let left = ctx.MkITE(ctx.MkBVSGT(startBit, zero), startBit, zero) :?> BitVecExpr
                let right = ctx.MkITE(ctx.MkBVSGT(sizeExpr, endBit), endBit, sizeExpr) :?> BitVecExpr
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
                let res = ctx.MkITE(intersects, ctx.MkBVOR(res, part), res) :?> BitVecExpr
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
                | Cast _ -> __notImplemented__()
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
            x.EncodeMemoryAccessConstant encCtx name heapRefSource structFields addressType

        member private x.KeyInVectorTimeIntervals encCtx (key : Expr) acc (region : vectorTime intervals) =
            let onePointCondition acc (y : vectorTime endpoint) =
                let bound = ctx.MkNumeral(encCtx.addressOrder.[y.elem], x.Type2Sort addressType) :?> BitVecExpr
                let condition =
                    match y.sort with
                    | endpointSort.OpenRight -> ctx.MkBVSLT(key :?> BitVecExpr, bound)
                    | endpointSort.ClosedRight -> ctx.MkBVSLE(key :?> BitVecExpr, bound)
                    | endpointSort.OpenLeft -> ctx.MkBVSGT(key :?> BitVecExpr, bound)
                    | endpointSort.ClosedLeft -> ctx.MkBVSGE(key :?> BitVecExpr, bound)
                    | _ -> __unreachable__()
                x.MkAnd(acc, condition)
            let intervalWithoutLeftZeroBound = region.points |> List.filter (fun ep -> VectorTime.less VectorTime.zero ep.elem)
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

        member private x.MemoryReading encCtx keyInRegion keysAreEqual encodeKey inst structFields left mo =
            let updates = MemoryRegion.flatten mo
            let assumptions, leftExpr = encodeKey left
            let leftInRegion = keyInRegion x.True left leftExpr
            let assumptions = leftInRegion :: assumptions
            let inst = inst leftExpr
            let checkOneKey (right, value) (acc, assumptions) =
                let rightAssumptions, rightExpr = encodeKey right
                // TODO: [style] auto append assumptions
                let assumptions = List.append assumptions rightAssumptions
                // NOTE: we need constraints on right key, because value may contain it
                let keysEquality = keysAreEqual(leftExpr, rightExpr)
                let keysAreMatch = keyInRegion keysEquality right rightExpr
                let readFieldIfNeed structTerm field =
                    assert(IsStruct structTerm)
                    Memory.ReadField emptyState structTerm field
                let value = List.fold readFieldIfNeed value structFields
                let valueExpr = x.EncodeTerm encCtx value
                let assumptions = List.append assumptions valueExpr.assumptions
                ctx.MkITE(keysAreMatch, valueExpr.expr, acc), assumptions
            let expr, assumptions = List.foldBack checkOneKey updates (inst, assumptions)
            encodingResult.Create(expr, assumptions)

        member private x.HeapReading encCtx key mo typ source structFields name =
            assert mo.defaultValue.IsNone
            let encodeKey (k : heapAddressKey) = x.EncodeTerm encCtx k.address |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort addressType, x.Type2Sort typ)
            let regionSort = GetHeapReadingRegionSort source
            let array = x.GetRegionConstant name sort structFields regionSort
            let inst (k : Expr) = ctx.MkSelect(array, k)
            let keyInRegion = x.HeapAddressKeyInRegion encCtx
            let res = x.MemoryReading encCtx keyInRegion x.MkEq encodeKey inst structFields key mo
            match regionSort with
            | HeapFieldSort field when field = Reflection.stringLengthField -> x.GenerateLengthAssumptions res
            | _ -> res

        // NOTE: temporary generating string with 0 <= length <= 20
        member private x.GenerateLengthAssumptions encodingResult =
            let expr = encodingResult.expr :?> BitVecExpr
            let assumptions = encodingResult.assumptions
            let lengthIsNonNegative = ctx.MkBVSGE(expr, ctx.MkBV(0, expr.SortSize))
            let assumptions = lengthIsNonNegative :: assumptions
            let assumptions =
                if maxBufferSize < 0 then assumptions
                else (ctx.MkBVSLE(expr, ctx.MkBV(maxBufferSize, expr.SortSize))) :: assumptions
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

        member private x.ArrayReading encCtx keyInRegion keysAreEqual encodeKey hasDefaultValue indices key mo typ source structFields name =
            assert mo.defaultValue.IsNone
            let domainSort = x.Type2Sort addressType :: List.map (x.Type2Sort Types.IndexType |> always) indices |> Array.ofList
            let valueSort = x.Type2Sort typ
            let inst (k : Expr[]) =
                if hasDefaultValue then x.DefaultValue valueSort
                else
                    let sort = ctx.MkArraySort(domainSort, valueSort)
                    let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
                    ctx.MkSelect(array, k)
            let res = x.MemoryReading encCtx keyInRegion keysAreEqual encodeKey inst structFields key mo
            let res = if typ = typeof<char> then x.GenerateCharAssumptions res else res
            match GetHeapReadingRegionSort source with
            | ArrayLengthSort _ -> x.GenerateLengthAssumptions res
            | _ -> res

        member private x.StackBufferReading encCtx key mo typ source structFields name =
            assert mo.defaultValue.IsNone
            let encodeKey (k : stackBufferIndexKey) = x.EncodeTerm encCtx k.index |> toTuple
            let sort = ctx.MkArraySort(x.Type2Sort addressType, x.Type2Sort typ)
            let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
            let inst (k : Expr) = ctx.MkSelect(array, k)
            x.MemoryReading encCtx x.stackBufferIndexKeyInRegion x.MkEq encodeKey inst structFields key mo

        member private x.StaticsReading encCtx (key : symbolicTypeKey) mo typ source structFields (name : string) =
            assert mo.defaultValue.IsNone
            let keyType = x.Type2Sort Types.IndexType
            let sort = ctx.MkArraySort(keyType, x.Type2Sort typ)
            let array = GetHeapReadingRegionSort source |> x.GetRegionConstant name sort structFields
            let updates = MemoryRegion.flatten mo
            let value = Seq.tryFind (fun (k, _) -> k = key) updates
            match value with
            | Some (_, v) -> x.EncodeTerm encCtx v
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
                let lowerIsZero = ctx.MkEq(lowerWord, ctx.MkBV(0, lowerWord.SortSize))
                let exp = ctx.MkExtract(23u, 16u, expr)
                let expSize = exp.SortSize
                let leftBound = ctx.MkBVUGE(exp, ctx.MkBV(0, expSize))
                let rightBound = ctx.MkBVULE(exp, ctx.MkBV(28, expSize))
                let expInBound = ctx.MkAnd(leftBound, rightBound)
                let upper = ctx.MkExtract(30u, 24u, expr)
                let upperIsZero = ctx.MkEq(upper, ctx.MkBV(0, upper.SortSize))
                { res with assumptions = lowerIsZero::expInBound::upperIsZero::res.assumptions }
            | _ -> res

        member private x.EncodeMemoryAccessConstant encCtx name (source : ISymbolicConstantSource) (structFields : fieldId list) typ : encodingResult =
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
                assert(typ = addressType)
                x.EncodeSymbolicAddress encCtx source structFields name
            | _ -> x.CreateConstant name typ

    // ------------------------------- Decoding -------------------------------

        member private x.DecodeExpr (cm : IConcreteMemory) op t (expr : Expr) =
            // TODO: bug -- decoding arguments with type of expression
            Expression (Operator op) (expr.Args |> Seq.map (x.Decode cm t) |> List.ofSeq) t

        member private x.DecodeBoolExpr (cm : IConcreteMemory) op (expr : Expr) =
            x.DecodeExpr cm op typeof<bool> expr

        member private x.GetTypeOfBV (bv : BitVecExpr) =
            if bv.SortSize = 32u then typeof<int32>
            elif bv.SortSize = 64u then typeof<int64>
            elif bv.SortSize = 8u then typeof<int8>
            elif bv.SortSize = 16u then typeof<int16>
            else __unreachable__()

        member private x.DecodeConcreteHeapAddress (cm : IConcreteMemory) typ (expr : Expr) : vectorTime =
            // TODO: maybe throw away typ?
            let result = ref vectorTime.Empty
            let checkAndGet key = encodingCache.heapAddresses.TryGetValue(key, result)
            let charArray = typeof<char[]>
            if expr :? BitVecNum && (expr :?> BitVecNum).Int64 = 0L then VectorTime.zero
            elif checkAndGet (typ, expr) then result.Value
            elif typ = typeof<string> && checkAndGet (charArray, expr) then
                // NOTE: storing most concrete type for string
                encodingCache.heapAddresses.Remove((charArray, expr)) |> ignore
                encodingCache.heapAddresses.Add((typ, expr), result.Value)
                // strings are filled symbolically
                if cm.Contains result.Value then cm.Remove result.Value
                result.Value
            elif typ = charArray && checkAndGet (typeof<string>, expr) then result.Value
            else
                encodingCache.lastSymbolicAddress <- encodingCache.lastSymbolicAddress - 1
                let addr = [encodingCache.lastSymbolicAddress]
                encodingCache.heapAddresses.Add((typ, expr), addr)
                addr

        member private x.DecodeSymbolicTypeAddress (expr : Expr) =
            let result = ref typeof<Void>
            if encodingCache.staticKeys.TryGetValue(expr, result) then result.Value
            else __notImplemented__()

        member private x.DecodeMemoryKey (cm : IConcreteMemory) (reg : regionSort) (exprs : Expr array) =
            let toType (elementType : Type, rank, isVector) =
                if isVector then elementType.MakeArrayType()
                else elementType.MakeArrayType(rank)
            match reg with
            | HeapFieldSort field ->
                assert(exprs.Length = 1)
                let address = exprs.[0] |> x.DecodeConcreteHeapAddress cm field.declaringType |> ConcreteHeapAddress
                ClassField(address, field)
            | StaticFieldSort field ->
                assert(exprs.Length = 1)
                let typ = x.DecodeSymbolicTypeAddress exprs.[0]
                StaticField(typ, field)
            | ArrayIndexSort typ ->
                assert(exprs.Length >= 2)
                let heapAddress = exprs.[0] |> x.DecodeConcreteHeapAddress cm (toType typ) |> ConcreteHeapAddress
                let indices = exprs |> Seq.tail |> Seq.map (x.Decode cm Types.IndexType) |> List.ofSeq
                ArrayIndex(heapAddress, indices, typ)
            | ArrayLengthSort typ ->
                assert(exprs.Length = 2)
                let heapAddress = exprs.[0] |> x.DecodeConcreteHeapAddress cm (toType typ) |> ConcreteHeapAddress
                let index = x.Decode cm Types.IndexType exprs.[1]
                ArrayLength(heapAddress, index, typ)
            | ArrayLowerBoundSort typ ->
                assert(exprs.Length = 2)
                let heapAddress = exprs.[0] |> x.DecodeConcreteHeapAddress cm (toType typ) |> ConcreteHeapAddress
                let index = x.Decode cm Types.IndexType exprs.[1]
                ArrayLowerBound(heapAddress, index, typ)
            | StackBufferSort key ->
                assert(exprs.Length = 1)
                let index = x.Decode cm typeof<int8> exprs.[0]
                StackBufferIndex(key, index)

        member private x.DecodeBv t (bv : BitVecNum) =
            match bv.SortSize with
            | 32u -> Concrete (convert bv.Int64 t) t
            | 64u -> Concrete (convert (uint64 bv.BigInteger) t) t
            | 16u -> Concrete (convert bv.Int t) t
            | 8u  -> Concrete (convert bv.Int t) t
            | _ -> __notImplemented__()

        member public x.Decode (cm: IConcreteMemory) t (expr : Expr) =
            match expr with
            | :? BitVecNum as bv when Types.IsNumeric t -> x.DecodeBv t bv
            | :? BitVecNum as bv when not (Types.IsValueType t) ->
                let address = x.DecodeConcreteHeapAddress cm t bv |> ConcreteHeapAddress
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
                elif expr.IsNot then x.DecodeBoolExpr cm OperationType.LogicalNot expr
                elif expr.IsAnd then x.DecodeBoolExpr cm OperationType.LogicalAnd expr
                elif expr.IsOr then x.DecodeBoolExpr cm OperationType.LogicalOr expr
                elif expr.IsEq then x.DecodeBoolExpr cm OperationType.Equal expr
                elif expr.IsBVSGT then x.DecodeBoolExpr cm OperationType.Greater expr
                elif expr.IsBVUGT then x.DecodeBoolExpr cm OperationType.Greater_Un expr
                elif expr.IsBVSGE then x.DecodeBoolExpr cm OperationType.GreaterOrEqual expr
                elif expr.IsBVUGE then x.DecodeBoolExpr cm OperationType.GreaterOrEqual_Un expr
                elif expr.IsBVSLT then x.DecodeBoolExpr cm OperationType.Less expr
                elif expr.IsBVULT then x.DecodeBoolExpr cm OperationType.Less_Un expr
                elif expr.IsBVSLE then x.DecodeBoolExpr cm OperationType.LessOrEqual expr
                elif expr.IsBVULE then x.DecodeBoolExpr cm OperationType.LessOrEqual_Un expr
                else __notImplemented__()

        member private x.WriteFields structure value = function
            | [field] -> Memory.WriteStructField structure field value
            | field::fields ->
                match structure with
                | {term = Struct(contents, _)} ->
                    let recurred = x.WriteFields contents.[field] value fields
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
            let cm = state.concreteMemory
            encodingCache.t2e |> Seq.iter (fun kvp ->
                match kvp.Key with
                | {term = Constant(_, StructFieldChain(fields, StackReading(key)), t)} as constant ->
                    let refinedExpr = m.Eval(kvp.Value.expr, false)
                    let decoded = x.Decode cm t refinedExpr
                    if decoded <> constant then
                        x.WriteDictOfValueTypes stackEntries key fields key.TypeOfLocation decoded
                | {term = Constant(_, (:? IMemoryAccessConstantSource as ms), _)} as constant ->
                    match ms with
                    | HeapAddressSource(StackReading(key)) ->
                        let refinedExpr = m.Eval(kvp.Value.expr, false)
                        let t = key.TypeOfLocation
                        let addr = refinedExpr |> x.DecodeConcreteHeapAddress cm t |> ConcreteHeapAddress
                        stackEntries.Add(key, HeapRef addr t |> ref)
                    | HeapAddressSource(:? functionResultConstantSource as frs) ->
                        let refinedExpr = m.Eval(kvp.Value.expr, false)
                        let t = (frs :> ISymbolicConstantSource).TypeOfLocation
                        let term = x.Decode cm t refinedExpr
                        assert(not (constant = term) || kvp.Value.expr = refinedExpr)
                        if constant <> term then subst.Add(ms, term)
                    | _ -> ()
                | {term = Constant(_, :? IStatedSymbolicConstantSource, _)} -> ()
                | {term = Constant(_, source, t)} as constant ->
                    let refinedExpr = m.Eval(kvp.Value.expr, false)
                    let term = x.Decode cm t refinedExpr
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
            let processRegionConstraints isSymbolyc (kvp : KeyValuePair<(regionSort * fieldId list), ArrayExpr>) =
                let constant = kvp.Value
                let arr = m.Eval(constant, false)
                let region, fields = kvp.Key
                let typeOfLocation =
                    if fields.IsEmpty then region.TypeOfLocation
                    else fields.Head.typ
                
                let getValueAndWrite (arr : Expr) addr =
                    let value =
                        if Types.IsValueType typeOfLocation then
                            x.Decode cm typeOfLocation (Array.last arr.Args)
                        else
                            let address = arr.Args |> Array.last |> x.DecodeConcreteHeapAddress cm typeOfLocation |> ConcreteHeapAddress
                            HeapRef address typeOfLocation
                    let states = Memory.Write state (Ref addr) value
                    assert(states.Length = 1 && states.[0] = state) 
                
                let writeHelper symbolicProcessor mixedProcessor addr =
                    // Strings and array metadata are written symbolically
                    match addr with
                    | _ when typeOfLocation = typeof<string> -> symbolicProcessor addr
                    | ArrayLength _
                    | ArrayLowerBound _ -> symbolicProcessor addr
                    | _ -> mixedProcessor addr // may contain symbolic and concrete writes
                
                let writeSymbolic arr = writeHelper (getValueAndWrite arr) (fun _ -> ())
                let writeMixed arr = writeHelper (fun _ -> ()) (getValueAndWrite arr)
                
                let rec parseArray (arr : Expr)  =
                    if arr.IsConstantArray && isSymbolyc then // defaults are written symbolically
                        assert(arr.Args.Length = 1)
                        let constantValue =
                            if Types.IsValueType typeOfLocation then x.Decode cm typeOfLocation arr.Args.[0]
                            else
                                let addr = x.DecodeConcreteHeapAddress cm typeOfLocation arr.Args.[0]
                                HeapRef (addr |> ConcreteHeapAddress) typeOfLocation
                        x.WriteDictOfValueTypes defaultValues region fields region.TypeOfLocation constantValue
                    else if arr.IsStore then
                        assert(arr.Args.Length >= 3)
                        parseArray arr.Args.[0]
                        let addr = x.DecodeMemoryKey cm region arr.Args.[1..arr.Args.Length - 2]
                        if isSymbolyc
                            then writeSymbolic arr addr
                            else writeMixed arr addr
                    elif arr.IsConst || arr.IsConstantArray then ()
                    else internalfailf "Unexpected array expression in model: %O" arr
                parseArray arr
            
            // Process symbolic writes
            encodingCache.regionConstants |> Seq.iter (processRegionConstraints true)
            defaultValues |> Seq.iter (fun kvp ->
                let region = kvp.Key
                let constantValue = kvp.Value.Value
                Memory.FillRegion state constantValue region)

            // Create default concretes
            encodingCache.heapAddresses |> Seq.iter (fun kvp ->
                let typ, _ = kvp.Key
                let addr = kvp.Value
                if VectorTime.less addr VectorTime.zero then
                    state.allocatedTypes <- PersistentDict.add addr (ConcreteType typ) state.allocatedTypes
                    if cm.Contains addr then ()
                    else
                        try
                            createObjOfType state defaultValues addr typ
                        with // if type cannot be allocated concretely, it will be stored symbolically
                        | :? MemberAccessException -> () // Could not create an instance
                        | :? ArgumentException -> ()     // Could not unbox concrete
                        | _ -> internalfail "Unexpected exception in object creation" 
            )

            // Process stores
            encodingCache.regionConstants |> Seq.iter (processRegionConstraints false)

            state.startingTime <- [encodingCache.lastSymbolicAddress - 1]
            state.model <- PrimitiveModel subst
            encodingCache.heapAddresses.Clear()
            StateModel(state, typeModel.CreateEmpty())


    let private ctx = new Context()
    let private builder = Z3Builder(ctx)

    type internal Z3Solver() =
//        let optCtx = ctx.MkOptimize()
        let optCtx = ctx.MkSolver()

//        let addSoftConstraints lvl =
//            let pathAtoms =
//                seq {
//                    for i in 0 .. min (levelAtoms.Count - 1) (Level.toInt lvl) do
//                        if levelAtoms.[i] <> null then
//                            yield! __notImplemented__() (* pathAtoms.[uint32(i)] *)
//                }
//            optCtx.Push()
//            let weight = 1u
//            let group = null
//            pathAtoms |> Seq.iter (fun atom -> optCtx.AssertSoft(atom, weight, group) |> ignore)
//            pathAtoms

        interface ISolver with
            member x.CheckSat (encCtx : encodingContext) (q : term) : smtResult =
                printLog Trace "SOLVER: trying to solve constraints..."
                printLogLazy Trace "%s" (lazy(q.ToString()))
                try
                    try
                        let query = builder.EncodeTerm encCtx q
                        let assumptions = query.assumptions
                        let assumptions =
                            seq {
                                yield! (Seq.cast<_> assumptions)
                                yield query.expr
                            } |> Array.ofSeq
    //                    let pathAtoms = addSoftConstraints q.lvl
                        let result = optCtx.Check assumptions
                        match result with
                        | Status.SATISFIABLE ->
                            trace "SATISFIABLE"
                            let z3Model = optCtx.Model
                            let model = builder.MkModel z3Model
                            SmtSat { mdl = model }
                        | Status.UNSATISFIABLE ->
                            trace "UNSATISFIABLE"
                            SmtUnsat { core = Array.empty (*optCtx.UnsatCore |> Array.map (builder.Decode Bool)*) }
                        | Status.UNKNOWN ->
                            trace "UNKNOWN"
                            SmtUnknown optCtx.ReasonUnknown
                        | _ -> __unreachable__()
                    with
                    | :? EncodingException as e ->
                        printLog Info "SOLVER: exception was thrown: %s" e.Message
                        SmtUnknown (sprintf "Z3 has thrown an exception: %s" e.Message)
                finally
                    builder.Reset()

            member x.Assert encCtx (fml : term) =
                printLogLazy Trace "SOLVER: Asserting: %s" (lazy(fml.ToString()))
                let encoded = builder.EncodeTerm encCtx fml
                let encoded = List.fold (fun acc x -> builder.MkAnd(acc, x)) (encoded.expr :?> BoolExpr) encoded.assumptions
                optCtx.Assert(encoded)

            member x.SetMaxBufferSize size =
                builder.SetMaxBufferSize size

    let reset() =
        builder.Reset()
