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

    let private makeSPDConst typ mtd spd = Terms.Constant mtd (IdGenerator.startingWith "ptrDifference-") spd typ

    let private (+.) (spd1: SymbolicPointerDifference) (spd2: SymbolicPointerDifference) : SymbolicPointerDifference =
        __notImplemented__()

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

    let private simplifySPDExpression mtd y s k repeat =
        __notImplemented__()

    let rec private simplifyPointerExpressionAddition mtd x y k =
        __notImplemented__()

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

    let simplifyBinaryOperation metadata op state x y targetType k =
        let isNativeInt = isNativeInt targetType

        match op with
        | OperationType.Subtract -> __notImplemented__()
        | OperationType.Add ->
            simplifyIndentedReferenceAddition metadata isNativeInt state x y k
        | OperationType.Equal -> simplifyReferenceEquality metadata x y (withSnd state >> k)
        | OperationType.NotEqual ->
            simplifyReferenceEquality metadata x y (fun e ->
            Propositional.simplifyNegation metadata e (withSnd state >> k))
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

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
