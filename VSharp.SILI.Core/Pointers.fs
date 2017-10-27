namespace VSharp.Core

open VSharp
open VSharp.Core.Common

module internal Pointers =
    let private underlyingPointerTypeSizeof mtd = term >> function // for `T* ptr` returns `sizeof(T)`
        | PointerTo typ -> makeNumber (Types.sizeOf typ) mtd
        | t -> internalfailf "Taking sizeof underlying type of not pointer type: %O" t

    type private SymbolicPointerDifference(pos: list<term * int>, neg: list<term * int>) =
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty
        member this.Pos = pos
        member this.Neg = neg

        override this.ToString() =
            let toString =
                List.map (function
                    | (t, 1) -> sprintf "%O" t.term
                    | (t, n) -> sprintf "%O * %O" n t.term)
                >> function
                    | [x] -> x
                    | ss -> sprintf "[%s]" <| join " + " ss
            let left = toString this.Pos
            let right = toString this.Neg
            match this.Pos, this.Neg with
            | [], [] -> internalfail "Empty Symbolic Pointer Difference"
            | _, [] -> left
            | [], _ -> sprintf "- %s" right
            | _ -> sprintf "{%s - %s}" left right

        static member empty = SymbolicPointerDifference([], [])

    let private makeSPDConst typ mtd spd = Terms.Constant mtd (IdGenerator.startingWith "ptrDifference-") spd typ

    let private (~-.) (spd: SymbolicPointerDifference) = SymbolicPointerDifference(spd.Neg, spd.Pos)

    let private (+.) (spd1: SymbolicPointerDifference) (spd2: SymbolicPointerDifference) : SymbolicPointerDifference =
        let collectUniqueAndMatching (x: list<'a * int>) (y: list<'a * int>) =
            let xsFromY, xUnique = List.partition (fun (u, _) -> List.exists (fun (v, _) -> v = u) y) x
            let ysFromX, yUnique = List.partition (fun (u, _) -> List.exists (fun (v, _) -> v = u) xsFromY) y
            let xsFromY = List.sortBy hash xsFromY
            let ysFromX = List.sortBy hash ysFromX
            xUnique, yUnique, xsFromY, ysFromX

        let termlistDiff x y =
            let xUnique, yUnique, xsFromY, ysFromX = collectUniqueAndMatching x y
            let xy = List.filterMap2 (fun (a, n) (_, m) -> if n = m then None else Some(a, n - m)) xsFromY ysFromX
            let ysFromX, xsFromY = List.mappedPartition (fun (t, n) -> if n < 0 then Some(t, -n) else None) xy
            (xUnique @ xsFromY, yUnique @ ysFromX)

        let termlistMerge x y =
            let xUnique, yUnique, xsFromY, ysFromX = collectUniqueAndMatching x y
            let xy = List.map2 (fun (a, n) (_, m) -> (a, n + m)) xsFromY ysFromX
            xUnique @ yUnique @ xy

        let (c, b) = termlistDiff spd2.Pos spd1.Neg
        let (a, d) = termlistDiff spd1.Pos spd2.Neg
        SymbolicPointerDifference(termlistMerge a c, termlistMerge b d)

    let private (|SymbolicPointerDifferenceT|_|) (scs: ISymbolicConstantSource) =
        match scs with
        | :? SymbolicPointerDifference as spd -> Some(spd.Pos, spd.Neg)
        | _ -> None

    let isNativeInt typ = typ = typedefof<nativeint> || typ = typedefof<unativeint>

    let private (|ConstantPtrT|_|) = term >> function
        | Constant(_, (SymbolicPointerDifferenceT _ as spd), tp) -> Some(spd :?> SymbolicPointerDifference, tp)
        | _ -> None

    let private addPtrToSymbolicPointerDifference (p: term) (spd: SymbolicPointerDifference) tp mtd k =
        let mkConst = makeSPDConst tp mtd
        match term p with
        | HeapRef _
        | StackRef _
        | StaticRef _ ->
            let spd = spd +. SymbolicPointerDifference([p, 1], [])
            let nil = makeNullRef (typeOf p) mtd
            match spd.Pos, spd.Neg with
            | [], [] -> k nil
            | [lastp, 1], [] -> k lastp
            | [lastp, 1], neg ->
                let indent =  SymbolicPointerDifference([], neg)
                k <| IndentedPtr lastp (mkConst indent) mtd
            | _ ->
                let indent = spd +. SymbolicPointerDifference([], [nil, 1])
                k <| IndentedPtr nil (mkConst indent) mtd
        | _ -> internalfailf "expected reference but got %O" p

    let locationEqual mtd addr1 addr2 =
        match typeOf addr1, typeOf addr2 with
        | Types.StringType, Types.StringType -> Strings.simplifyEquality mtd addr1 addr2
        | Numeric _, Numeric _ ->
            if addr1 = addr2 then makeTrue mtd
            elif isConcrete addr1 && isConcrete addr2 then makeFalse mtd
            else makeBinary OperationType.Equal addr1 addr2 false Bool mtd
        | ArrayType _, ArrayType _ -> Arrays.equalsArrayIndices mtd addr1 addr2
        | _ -> __notImplemented__()

    let comparePath mtd path1 path2 =
        if List.length path1 <> List.length path2 then
            makeFalse mtd
        else
            List.map2 (fun (x, _) (y, _) -> locationEqual mtd x y) path1 path2 |> conjunction mtd

    let rec simplifyReferenceEquality mtd x y k =
        simplifyGenericBinary "reference comparison" State.empty x y (fst >> k)
            (fun _ _ _ _ -> __unreachable__())
            (fun x y s k ->
                let k = withSnd s >> k
                match x.term, y.term with
                | _ when x = y -> makeTrue mtd |> k
                | HeapRef(xpath, _, xtgt, Reference _), HeapRef(ypath, _, ytgt, Reference _) ->
                    makeBool (xtgt = ytgt) mtd &&& comparePath mtd (NonEmptyList.toList xpath) (NonEmptyList.toList ypath) |> k
                | StackRef(key1, path1, None), StackRef(key2, path2, None) ->
                    makeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | StaticRef(key1, path1, None), StaticRef(key2, path2, None) ->
                    makeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | _ -> makeFalse mtd |> k)
            (fun x y state k -> simplifyReferenceEquality mtd x y (withSnd state >> k))

    let isNull mtd ptr =
        simplifyReferenceEquality mtd ptr (makeNullRef Null mtd) id

    let rec private simplifySPDUnaryMinus mtd y ischk tp s k =
        simplifySPDExpression mtd y s k (fun y s k ->
        let k = withSnd s >> k
        match y with
        | ConstantPtrT(spd, tp) -> k <| makeSPDConst tp mtd (-. spd)
        | _ -> k <| makeUnary OperationType.UnaryMinus y ischk (tp |?? typeOf y) mtd)

    and private simplifySPDAddition mtd l r ischk tp s k =
        simplifySPDExpression mtd l s k (fun l s k ->
        simplifySPDExpression mtd r s k (fun r s k ->
            let k1 = withSnd s >> k
            match l, r with
            | ConstantPtrT(spdX, _), ConstantPtrT(spdY, _) ->
                spdX +. spdY |> makeSPDConst tp mtd |> k1
            | _ -> k1 <| makeBinary OperationType.Add l r ischk tp mtd))

    and private simplifySPDExpression mtd y s k repeat =
        let k (y, s) = repeat y s k
        match y with
        | Add(left, right, ischk, tp) ->
            simplifySPDAddition mtd left right ischk tp s k
        | UnaryMinusT(arg, ischk, tp) ->
            simplifySPDUnaryMinus mtd arg ischk (Some tp) s k
        | Sub(left, right, ischk, tp) ->
            simplifySPDUnaryMinus mtd right ischk None s (fun (right, s) ->
            simplifySPDAddition mtd left right ischk tp s k)
        | _ -> k (y, s)

    let rec private simplifyPointerExpressionAddition mtd x y k =
        let shiftTyp = typeOf y
        let mkAdd l r = makeBinary OperationType.Add l r false shiftTyp mtd
        let rec collectSPDs expr summands spd k =
            match expr with
            | Add(left, right, _, _) ->
                collectSPDs left summands spd (fun (summands, spd) ->
                collectSPDs right summands spd k)
            | ConstantPtrT(spd', _) -> k (summands, spd +. spd')
            | _ -> k (expr :: summands, spd)
        collectSPDs y [] SymbolicPointerDifference.empty (fun (summands, spd) ->
        Cps.List.reduce mkAdd summands (fun y ->
        addPtrToSymbolicPointerDifference x spd shiftTyp mtd (fun x ->
        let x, y =
            match term x with
            | IndentedPtr(x, shift) -> x, mkAdd shift y
            | _ -> x, y
        k <| IndentedPtr x y mtd)))

    let rec private simplifyPointerAdditionGeneric mtd isNativeInt x y state k = // y must be normalized by Arithmetics!
        let simplifyIndentedPointerAddition x y s k =
            let multWithSizeOf t =
                if isNativeInt then t else Arithmetics.mul mtd t <| underlyingPointerTypeSizeof mtd x

            let simplifyRawPointerAddition x y s k = // x is not Indented Ptr
                let k1 = withSnd s >> k
                match y with
                | ConcreteT(zero, _) when CSharpUtils.Calculator.IsZero zero -> k1 x
                | ConstantPtrT(spd, tp) -> addPtrToSymbolicPointerDifference x spd tp mtd k1
                | Add _ -> simplifyPointerExpressionAddition mtd x y k1
                | _ ->
                    match term y with
                    | Expression _
                    | Concrete _
                    | Constant _ -> k1 <| IndentedPtr x y mtd
                    | _ -> internalfailf "expected primitive value but got: %O" y

            match term x with
            | IndentedPtr(p, shift) ->
                simplifySPDExpression mtd (Arithmetics.add mtd shift <| multWithSizeOf y) s k (simplifyRawPointerAddition p)
            | _ -> simplifySPDExpression mtd (multWithSizeOf y) s k (simplifyRawPointerAddition x)

        simplifyGenericBinary "add shift to pointer" state x y k
            (fun _ _ _ _ -> __unreachable__())
            simplifyIndentedPointerAddition
            (simplifyPointerAdditionGeneric mtd isNativeInt)

    let private simplifyIndentedReferenceAddition mtd isNativeInt state x y k =
        let x', y' = if Terms.isNumeric y then x, y else y, x
        simplifyPointerAdditionGeneric mtd isNativeInt x' y' state k

    let rec private simplifyPointerSubtractionGeneric mtd isNativeInt x y state k =
        let makeDiff p q =
            let tp = Numeric typedefof<int64>
            if p = q
            then makeNumber 0L mtd
            else
                SymbolicPointerDifference([p, 1], [q, 1])
                |> makeSPDConst tp mtd
        let divideBySizeof diff =
            if isNativeInt then diff else Arithmetics.div mtd diff <| underlyingPointerTypeSizeof mtd x
        let simplifyPointerDiffWithOffset p q offset s =
            simplifySPDExpression mtd (Arithmetics.add mtd (makeDiff p q) offset) s k (fun diff s k -> k (divideBySizeof diff, s))
        let simplifyIndentedPointerSubtraction x y s k =
            match term x, term y with
            | IndentedPtr(p, a), IndentedPtr(q, b) -> simplifyPointerDiffWithOffset p q (Arithmetics.sub mtd a b) s
            | IndentedPtr(p, a), _ -> simplifyPointerDiffWithOffset p y a s
            | _, IndentedPtr(q, b) -> simplifyPointerDiffWithOffset x q (Arithmetics.neg mtd b) s
            | _ -> k (divideBySizeof <| makeDiff x y, s)

        simplifyGenericBinary "pointer1 - pointer2" state x y k
            (fun _ _ _ _ -> __unreachable__())
            simplifyIndentedPointerSubtraction
            (simplifyPointerSubtractionGeneric mtd isNativeInt)

    let private simplifyIndentedReferenceSubtraction mtd isNativeInt state x y k =
        if isNumeric y
        then simplifyPointerAdditionGeneric mtd isNativeInt x (Arithmetics.neg mtd y) state k
        else simplifyPointerSubtractionGeneric mtd isNativeInt x y state k

    let simplifyBinaryOperation metadata op state x y targetType k =
        let isNativeInt = isNativeInt targetType

        match op with
        | OperationType.Subtract ->
            simplifyIndentedReferenceSubtraction metadata isNativeInt state x y k
        | OperationType.Add ->
            simplifyIndentedReferenceAddition metadata isNativeInt state x y k
        | OperationType.Equal -> simplifyReferenceEquality metadata x y (withSnd state >> k)
        | OperationType.NotEqual ->
            simplifyReferenceEquality metadata x y (fun e ->
            Propositional.simplifyNegation metadata e (withSnd state >> k))
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let add mtd x y =
        simplifyBinaryOperation mtd OperationType.Add State.empty x y (typeof<System.Void>.MakePointerType()) fst

    let sub mtd x y =
        simplifyBinaryOperation mtd OperationType.Subtract State.empty x y (typeof<System.Void>.MakePointerType()) fst

    let isPointerOperation op t1 t2 =
        let isNearlyPtr t = Types.isPointer t || Types.isReference t

        match op with
        | OperationType.Equal
        | OperationType.NotEqual -> isNearlyPtr t1 && isNearlyPtr t2
        | OperationType.Subtract -> isNearlyPtr t1 && (isNearlyPtr t2 || Types.isNumeric t2)
        | OperationType.Add ->
            (isNearlyPtr t1 && Types.isNumeric t2) || (isNearlyPtr t2 && Types.isNumeric t1)
        | _ -> false

    let rec topLevelLocation = Merging.map (function
        | {term = HeapRef(((a, _), []), _, _, _)} -> a
        | _ -> __notImplemented__())

    type HeapAddressExtractor() =
        inherit TermExtractor()
        override x.Extract t = topLevelLocation t

    let symbolicThisStackKey = "symbolic this on stack"

    let (|SymbolicThisOnStack|_|) = function
       | StackRef((name, token), path, typ) when symbolicThisStackKey.Equals(name) -> Some(SymbolicThisOnStack(token, path, typ))
       | _ -> None
