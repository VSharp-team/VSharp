namespace VSharp.Core

open VSharp
open VSharp.Core.Common

module internal Pointers =
    let private underlyingPointerTypeSizeof mtd = term >> function // for `T* ptr` returns `sizeof(T)`
        | Ptr(_, _, typ, _) -> makeNumber mtd (Types.sizeOf typ)
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

    let private (|ConstantPtrT|_|) = term >> function
        | Constant(_, (SymbolicPointerDifferenceT _ as spd), tp) -> Some(spd :?> SymbolicPointerDifference, tp)
        | _ -> None

    let private shift mtd ptr shift k =
        match term ptr with
        | Ptr(tla, psl, typ, None) -> k <| IndentedPtr mtd tla psl typ shift
        | Ptr(tla, psl, typ, Some shift') -> k <| IndentedPtr mtd tla psl typ (Arithmetics.add mtd shift' shift)
        | _ -> __unreachable__()

    let private addPtrToSymbolicPointerDifference (p: term) (spd: SymbolicPointerDifference) tp mtd k =
        let mkConst = makeSPDConst tp mtd
        match term p with
        | Ptr _ ->
            let spd = spd +. SymbolicPointerDifference([p, 1], [])
            let nil = makeNullPtr mtd <| typeOf p
            match spd.Pos, spd.Neg with
            | [], [] -> k nil
            | [lastp, 1], [] -> k lastp
            | [lastp, 1], neg ->
                let indent =  SymbolicPointerDifference([], neg)
                shift mtd lastp (mkConst indent) k
            | _ ->
                let indent = spd +. SymbolicPointerDifference([], [nil, 1])
                shift mtd nil (mkConst indent) k
        | _ -> internalfailf "expected pointer but got: %O" p

    let private isZero mtd x =
        Arithmetics.simplifyEqual mtd x (makeZeroAddress mtd) id

    let isZeroAddress mtd x = fastNumericCompare mtd x (makeZeroAddress mtd)

    let private compareTopLevel mtd = function
        | RefNullAddress, RefNullAddress -> makeTrue mtd
        | RefTopLevelHeap(addr, _, _), RefNullAddress
        | RefNullAddress, RefTopLevelHeap(addr, _, _) -> isZeroAddress mtd addr
        | RefTopLevelHeap(addr1, _, _), RefTopLevelHeap(addr2, _, _) -> fastNumericCompare mtd addr1 addr2
        | RefTopLevelStatics typ1, RefTopLevelStatics typ2 -> typesEqual mtd typ1 typ2
        | RefTopLevelStack key1, RefTopLevelStack key2 -> makeBool mtd (key1 = key2)
        | _ -> makeFalse mtd

    let private equalPathSegment mtd x y =
        match x, y with
        | BlockField(field1, typ1), BlockField(field2, typ2) -> makeBool mtd (field1 = field2) &&& typesEqual mtd typ1 typ2
        | ArrayIndex(addr1, _), ArrayIndex(addr2, _) -> Arrays.equalsIndicesArrays mtd addr1 addr2
        | ArrayLowerBound x, ArrayLowerBound y
        | ArrayLength x, ArrayLength y -> fastNumericCompare mtd x y
        | _ -> makeFalse mtd

    let private comparePath mtd path1 path2 =
        if List.length path1 <> List.length path2 then
            makeFalse mtd
        else
            List.map2 (equalPathSegment mtd) path1 path2 |> conjunction mtd

    let rec simplifyReferenceEqualityk mtd x y k =
        simplifyGenericBinary "reference comparison" x y k
            (fun _ _ _ -> __unreachable__())
            (fun x y k ->
                match x.term, y.term with
                | _ when x = y -> makeTrue mtd |> k
                | Ref(topLevel1, path1), Ref(topLevel2, path2) -> compareTopLevel mtd (topLevel1, topLevel2) &&& comparePath mtd path1 path2 |> k
                | Ptr(topLevel1, path1, _, shift1), Ptr(topLevel2, path2, _, shift2) ->
                    let refeq = compareTopLevel mtd (topLevel1, topLevel2) &&& comparePath mtd path1 path2
                    match shift1, shift2 with
                    | None, None -> refeq |> k
                    | Some shift, None
                    | None, Some shift -> refeq &&& isZero mtd shift |> k
                    | Some shift1, Some shift2 -> refeq &&& Arithmetics.simplifyEqual mtd shift1 shift2 id |> k
                | _ -> makeFalse mtd |> k)
            (fun x y k -> simplifyReferenceEqualityk mtd x y k)

    let isNull mtd ptr =
        simplifyReferenceEqualityk mtd ptr (makeNullRef mtd) id

    let rec private simplifySPDUnaryMinus mtd y tp k =
        simplifySPDExpression mtd y k (fun y k ->
        match y with
        | ConstantPtrT(spd, tp) -> k <| makeSPDConst tp mtd (-. spd)
        | _ -> k <| makeUnary OperationType.UnaryMinus y (tp |?? typeOf y) mtd)

    and private simplifySPDAddition mtd l r tp k =
        simplifySPDExpression mtd l k (fun l k ->
        simplifySPDExpression mtd r k (fun r k ->
            match l, r with
            | ConstantPtrT(spdX, _), ConstantPtrT(spdY, _) ->
                spdX +. spdY |> makeSPDConst tp mtd |> k
            | _ -> k <| makeBinary OperationType.Add l r tp mtd))

    and private simplifySPDExpression mtd y k repeat =
        let k y = repeat y k
        match y with
        | Add(left, right, tp) ->
            simplifySPDAddition mtd left right tp k
        | UnaryMinusT(arg, tp) ->
            simplifySPDUnaryMinus mtd arg (Some tp) k
        | Sub(left, right, tp) ->
            simplifySPDUnaryMinus mtd right None (fun right ->
            simplifySPDAddition mtd left right tp k)
        | _ -> k y

    let rec private simplifyPointerExpressionAddition mtd x y k =
        let shiftTyp = typeOf y
        let mkAdd l r = makeBinary OperationType.Add l r shiftTyp mtd
        let rec collectSPDs expr summands spd k =
            match expr with
            | Add(left, right, _) ->
                collectSPDs left summands spd (fun (summands, spd) ->
                collectSPDs right summands spd k)
            | ConstantPtrT(spd', _) -> k (summands, spd +. spd')
            | _ -> k (expr :: summands, spd)
        collectSPDs y [] SymbolicPointerDifference.empty (fun (summands, spd) ->
        Cps.List.reduce mkAdd summands (fun y ->
        addPtrToSymbolicPointerDifference x spd shiftTyp mtd (fun x ->
        shift mtd x y k)))

    let rec private simplifyPointerAdditionGeneric mtd x y k = // y must be normalized by Arithmetics!
        let simplifyIndentedPointerAddition x y k =
            let multWithSizeOf t = Arithmetics.mul mtd t <| underlyingPointerTypeSizeof mtd x

            let simplifyRawPointerAddition x y k = // x is not Indented Ptr
                match y with
                | ConcreteT(zero, _) when CSharpUtils.Calculator.IsZero zero -> k x
                | ConstantPtrT(spd, tp) -> addPtrToSymbolicPointerDifference x spd tp mtd k
                | Add _ -> simplifyPointerExpressionAddition mtd x y k
                | _ ->
                    match term y with
                    | Expression _
                    | Concrete _
                    | Constant _ -> shift mtd x y k
                    | _ -> internalfailf "expected primitive value but got: %O" y

            match term x with
            | Ptr(tla, psl, typ, Some shift) ->
                // TODO: [2columpio]: you construct Ptr and immediately deconstruct it in pattern matching!
                simplifySPDExpression mtd (Arithmetics.add mtd shift <| multWithSizeOf y) k (simplifyRawPointerAddition <| Ptr mtd tla psl typ)
            | _ -> simplifySPDExpression mtd (multWithSizeOf y) k (simplifyRawPointerAddition x)

        simplifyGenericBinary "add shift to pointer" x y k
            (fun _ _ _ -> __unreachable__())
            simplifyIndentedPointerAddition
            (simplifyPointerAdditionGeneric mtd)

    let private simplifyIndentedReferenceAddition mtd x y k =
        let x', y' = if Terms.isNumeric y then x, y else y, x
        simplifyPointerAdditionGeneric mtd x' y' k

    let rec private simplifyPointerSubtractionGeneric mtd x y k =
        let makeDiff p q =
            let tp = Numeric (Id typedefof<int64>)
            if p = q
            then makeNumber mtd 0L
            else
                SymbolicPointerDifference([p, 1], [q, 1])
                |> makeSPDConst tp mtd
        let divideBySizeof diff = Arithmetics.div mtd diff <| underlyingPointerTypeSizeof mtd x
        let simplifyPointerDiffWithOffset p q offset =
            simplifySPDExpression mtd (Arithmetics.add mtd (makeDiff p q) offset) k (fun diff k -> k (divideBySizeof diff))
        let simplifyIndentedPointerSubtraction x y k =
            match term x, term y with
            // TODO: [columpio]: rewrite it in a more efficient way!
            | Ptr(tl1, path1, t1, Some a), Ptr(tl2, path2, t2, Some b) -> simplifyPointerDiffWithOffset (Ptr mtd tl1 path1 t1) (Ptr mtd tl2 path2 t2) (Arithmetics.sub mtd a b)
            | Ptr(tl, path, t, Some a), _ -> simplifyPointerDiffWithOffset (Ptr mtd tl path t) y a
            | _, Ptr(tl, path, t, Some b) -> simplifyPointerDiffWithOffset x (Ptr mtd tl path t) (Arithmetics.neg mtd b)
            | _ -> k (divideBySizeof <| makeDiff x y)

        simplifyGenericBinary "pointer1 - pointer2" x y k
            (fun _ _ _ -> __unreachable__())
            simplifyIndentedPointerSubtraction
            (simplifyPointerSubtractionGeneric mtd)

    let private simplifyIndentedReferenceSubtraction mtd x y k =
        if isNumeric y
        then simplifyPointerAdditionGeneric mtd x (Arithmetics.neg mtd y) k
        else simplifyPointerSubtractionGeneric mtd x y k

    let simplifyBinaryOperation metadata op x y k =

        match op with
        | OperationType.Subtract ->
            simplifyIndentedReferenceSubtraction metadata x y k
        | OperationType.Add ->
            simplifyIndentedReferenceAddition metadata x y k
        | OperationType.Equal -> simplifyReferenceEqualityk metadata x y k
        | OperationType.NotEqual ->
            simplifyReferenceEqualityk metadata x y (fun e ->
            Propositional.simplifyNegation metadata e k)
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let add mtd x y =
        simplifyBinaryOperation mtd OperationType.Add x y id

    let sub mtd x y =
        simplifyBinaryOperation mtd OperationType.Subtract x y id

    let isPointerOperation op t1 t2 =
        let isRefOrNull t = Types.concreteIsReferenceType t || Types.isNull t
        let isPtrOrNull t = Types.isPointer t || Types.isNull t

        match op with
        | OperationType.Equal
        | OperationType.NotEqual ->
            (isRefOrNull t1 && isRefOrNull t2) || (isPtrOrNull t1 && isPtrOrNull t2)
        | OperationType.Subtract -> Types.isPointer t1 && (Types.isPointer t2 || Types.isNumeric t2)
        | OperationType.Add ->
            (Types.isPointer t1 && Types.isNumeric t2) || (Types.isPointer t2 && Types.isNumeric t1)
        | _ -> false

    let topLevelLocation = Merging.guardedApply (term >> function
        | Ref(RefTopLevelHeap (a, _, _), [])
        | Ptr(RefTopLevelHeap (a, _, _), [], _, _) -> a
        | Ref(RefNullAddress, _)
        | Ptr(RefNullAddress, _, _, _) -> makeZeroAddress Metadata.empty
        | _ -> __notImplemented__())

    type HeapAddressExtractor() =
        inherit TermExtractor()
        override x.Extract t = topLevelLocation t

    let (|SymbolicThisOnStack|_|) = function
       | Ref(RefTopLevelStack(SymbolicThisKey token), path) -> Some(SymbolicThisOnStack(token, path))
       | _ -> None
