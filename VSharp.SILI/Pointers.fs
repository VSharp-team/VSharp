namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.Common

module internal Pointers =
    type private SymbolicPtrDiff(pos: list<Term * int>, neg: list<Term * int>) =
        inherit SymbolicConstantSource()
        let pos = pos
        let neg = neg
        member this.Pos = pos
        member this.Neg = neg

        static member add (spd1: SymbolicPtrDiff) (spd2: SymbolicPtrDiff) = // TODO: add ~> (+)
            let collectUniqueAndMatching (x: list<'a * int>) (y: list<'a * int>) =
                let xsFromY, xUnique = List.partition (fun (u, _) -> List.exists (fun (v, _) -> v = u) y) x
                let ysFromX, yUnique = List.partition (fun (u, _) -> List.exists (fun (v, _) -> v = u) xsFromY) y
                let xsFromY = List.sortBy (fun x -> x.GetHashCode()) xsFromY
                let ysFromX = List.sortBy (fun x -> x.GetHashCode()) ysFromX
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
            SymbolicPtrDiff(termlistMerge a c, termlistMerge b d)

    let private makeSPDConst spd = Terms.Constant (IdGenerator.startingWith "ptrDifference-") spd

    let private (~-) (spd: SymbolicPtrDiff) = SymbolicPtrDiff(spd.Neg, spd.Pos)

    let private (|SymbolicPtrDiffT|_|) (scs: SymbolicConstantSource) =
        match scs with
        | :? SymbolicPtrDiff as spd -> Some(spd.Pos, spd.Neg)
        | _ -> None

    let private (|ConstantPtr|_|) (t: TermNode) =
        match t with
        | Constant(s, (SymbolicPtrDiffT _ as spd), tp) -> Some(s, spd :?> SymbolicPtrDiff, tp)
        | _ -> None

    let private AddPtrToSymbolicPtrDiff (p: Term) (spd: SymbolicPtrDiff) mkConst mtd k =
        match term p with
        | HeapRef _
        | StackRef _
        | StaticRef _ ->
            let spd = SymbolicPtrDiff.add (SymbolicPtrDiff([p, 1], [])) spd
            let nil = MakeNullRef (TypeOf p) mtd
            match spd.Pos, spd.Neg with
            | [], [] -> k nil
            | [lastp, 1], [] -> k <| lastp
            | [lastp, 1], neg ->
                let indent = mkConst <| SymbolicPtrDiff([], neg)
                k <| IndentedRef lastp indent mtd
            | _ -> k <| IndentedRef nil (SymbolicPtrDiff([], [nil, 1]) |> SymbolicPtrDiff.add spd |> mkConst) mtd
        | _ -> internalfailf "expected reference but got %O" p

    let internal locationEqual mtd addr1 addr2 =
        match TypeOf addr1, TypeOf addr2 with
        | String, String -> Strings.simplifyEquality mtd addr1 addr2
        | Numeric _, Numeric _ -> Arithmetics.eq mtd addr1 addr2
        | ArrayType _, ArrayType _ -> Arrays.equalsArrayIndices mtd addr1 addr2 |> fst
        | _ -> __notImplemented__()

    let internal comparePath mtd path1 path2 =
        if List.length path1 <> List.length path2 then
            Terms.MakeFalse mtd
        else
            List.map2 (fun (x, _) (y, _) -> locationEqual mtd x y) path1 path2 |> conjunction mtd

    let rec internal simplifyReferenceEquality mtd x y k =
        simplifyGenericBinary "reference comparison" State.empty x y (fst >> k)
            (fun _ _ _ _ -> __unreachable__())
            (fun x y s k ->
                let k = withSnd s >> k in
                match x.term, y.term with
                | _ when x = y -> MakeTrue mtd |> k
                | HeapRef(xpath, _), HeapRef(ypath, _) ->
                    comparePath mtd (NonEmptyList.toList xpath) (NonEmptyList.toList ypath) |> k
                | StackRef(key1, path1), StackRef(key2, path2) ->
                    MakeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | StaticRef(key1, path1), StaticRef(key2, path2) ->
                    MakeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | _ -> MakeFalse mtd |> k)
            (fun x y state k -> simplifyReferenceEquality mtd x y (withSnd state >> k))

    let internal isNull mtd ptr =
        simplifyReferenceEquality mtd ptr (MakeNullRef Null mtd) id

    let rec private simplifySPDUnaryMinus mtd ischk y s k =
        simplifySPDExpressionGeneric mtd y s k (fun y s k ->
            let k = withSnd s >> k in
            match term y with
            | ConstantPtr(_, spd, tp) -> k <| makeSPDConst (- spd) tp mtd
            | _ -> k <| MakeUnary OperationType.UnaryMinus y ischk (TypeOf y) mtd)

    and private simplifySPDAddition mtd l r ischk tp s k =
        simplifySPDExpressionGeneric mtd l s k (fun l s k ->
        simplifySPDExpressionGeneric mtd r s k (fun r s k ->
            let k1 = withSnd s >> k
            match term l, term r with
            | ConstantPtr(nameX, spdX, _), ConstantPtr(nameY, spdY, _) ->
                match SymbolicPtrDiff.add spdX spdY with
                | SymbolicPtrDiffT([], []) -> k1 <| MakeNumber 0L mtd
                | spdSum -> k1 <| makeSPDConst spdSum tp mtd
            | _, _ -> k1 <| MakeBinary OperationType.Add l r ischk tp mtd))

    and private simplifySPDExpressionGeneric mtd y s k repeat =
        match term y with
        | Expression _ -> simplifySPDExpression mtd y s (fun (y, s) -> repeat y s k)
        | _ -> repeat y s k

    and private simplifySPDExpression mtd y s k =
        match y with
        | Add(left, right, ischk, tp) ->
            simplifySPDAddition mtd left right ischk tp s k
        | UnaryMinusT(arg, ischk, tp) ->
            simplifySPDUnaryMinus mtd ischk arg s k
        | Sub(left, right, ischk, tp) ->
            simplifySPDUnaryMinus mtd ischk right s (fun (right, s) ->
            simplifySPDAddition mtd left right ischk tp s k)
        | _ -> k (y, s)

    let rec private simplifyPointerExpressionAddition mtd x y k =
        __notImplemented__()

    let rec private simplifyPointerAdditionGeneric mtd x y state k = // y must be normalized by Arithmetics!
        let rec simplifyIndentedPointerAddition x y s k =
            let simplifyRawPointerAddition x y s k = // x is not Indented Ref
                let k1 = withSnd s >> k in
                match term y with
                | Concrete(zero, _) when CSharpUtils.Calculator.IsZero zero -> k1 x
                | ConstantPtr(_, spd, tp) ->
                    AddPtrToSymbolicPtrDiff x spd (fun spd -> makeSPDConst spd tp mtd) mtd k1
                | Expression(Operator(OperationType.Add, _), _, _) ->
                    simplifyPointerExpressionAddition mtd x y k1
                | Expression _
                | Concrete _
                | Constant _ -> k1 <| IndentedRef x y mtd
                | _ -> internalfailf "expected primitive value but got: %O" y

            match term x with
            | IndentedRef(p, shift) -> simplifyIndentedPointerAddition p (Arithmetics.add mtd shift y) s k
            | _ -> simplifySPDExpressionGeneric mtd y s k (simplifyRawPointerAddition x)

        simplifyGenericBinary "add shift to pointer" state x y k
            (fun _ _ _ _ -> __unreachable__())
            simplifyIndentedPointerAddition
            (simplifyPointerAdditionGeneric mtd)

    let private simplifyIndentedReferenceAddition mtd state x y k =
        let x', y' = if Terms.IsNumeric y then x, y else y, x in
        simplifyPointerAdditionGeneric mtd x' y' state k

    let internal simplifyBinaryOperation metadata op state x y k =
        match op with
        | OperationType.Subtract -> __notImplemented__()
        | OperationType.Add ->
            simplifyIndentedReferenceAddition metadata state x y k
        | OperationType.Equal -> simplifyReferenceEquality metadata x y (withSnd state >> k)
        | OperationType.NotEqual ->
            simplifyReferenceEquality metadata x y (fun e ->
            Propositional.simplifyNegation metadata e (withSnd state >> k))
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let internal isPointerOperation op t1 t2 =
        let inline isNearlyPtr t = Types.IsPointer t || Types.IsBottom t
        match op with
        | OperationType.Equal
        | OperationType.NotEqual ->
            isNearlyPtr t1 && isNearlyPtr t2
        | OperationType.Subtract -> isNearlyPtr t1 && (isNearlyPtr t2 || Types.IsNumeric t2)
        | OperationType.Add ->
            (isNearlyPtr t1 && Types.IsNumeric t2) || (isNearlyPtr t2 && Types.IsNumeric t1)
        | _ -> false
