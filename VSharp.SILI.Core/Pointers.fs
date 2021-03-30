namespace VSharp.Core

open VSharp
open VSharp.Core.Common

module internal Pointers =
    let private underlyingPointerTypeSizeof = term >> function // for `T* ptr` returns `sizeof(T)`
        | Ptr(_, Void, _) -> makeNumber 1
        | Ptr(_, typ, _) -> makeNumber (Types.sizeOf typ)
        | t -> internalfailf "Taking sizeof underlying type of not pointer type: %O" t

    // TODO: delete this symbolic constant source, because the idea is wrong
    type private SymbolicPointerDifference(pos: list<term * int>, neg: list<term * int>) =
        interface ISymbolicConstantSource with
            override x.SubTerms = Seq.empty
            override x.Time = VectorTime.zero
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

    let private makeSPDConst typ spd = Terms.Constant (IdGenerator.startingWith "ptrDifference-") spd typ

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

    let private shift ptr shift k =
        match term ptr with
        | Ptr(addr, typ, None) -> k <| Ptr addr typ (Some shift)
        | Ptr(addr, typ, Some shift') -> k <| Ptr addr typ (Arithmetics.add shift' shift |> Some)
        | _ -> __unreachable__()

    let private addPtrToSymbolicPointerDifference (p: term) (spd: SymbolicPointerDifference) tp k =
        let mkConst = makeSPDConst tp
        match term p with
        | Ptr _ ->
            let spd = spd +. SymbolicPointerDifference([p, 1], [])
            let nil = makeNullPtr (typeOf p)
            match spd.Pos, spd.Neg with
            | [], [] -> k nil
            | [lastp, 1], [] -> k lastp
            | [lastp, 1], neg ->
                let indent =  SymbolicPointerDifference([], neg)
                shift lastp (mkConst indent) k
            | _ ->
                let indent = spd +. SymbolicPointerDifference([], [nil, 1])
                shift nil (mkConst indent) k
        | _ -> internalfailf "expected pointer but got: %O" p

    let private isZero x =
        Arithmetics.simplifyEqual x zeroAddress id

    let isZeroAddress x = fastNumericCompare x zeroAddress

    let rec private compareAddress = function
        | PrimitiveStackLocation k1, PrimitiveStackLocation k2 -> makeBool (k1 = k2)
        | ClassField(addr1, f1), ClassField(addr2, f2) -> if f1 = f2 then Arithmetics.simplifyEqual addr1 addr2 id else False
        | StructField(addr1, f1), StructField(addr2, f2) -> if f1 = f2 then compareAddress(addr1, addr2) else False
        | ArrayIndex(addr1, idcs1, t1), ArrayIndex(addr2, idcs2, t2) ->
            if t1 = t2 && idcs1.Length = idcs2.Length then
                Arithmetics.simplifyEqual addr1 addr2 id
            else False
        | StackBufferIndex(k1, i1), StackBufferIndex(k2, i2) -> makeBool (k1 = k2) &&& Arithmetics.simplifyEqual i1 i2 id
        | BoxedLocation(addr1, _), BoxedLocation(addr2, _) -> makeBool (addr1 = addr2)
        | ArrayLength _, ArrayLength _
        | ArrayLowerBound _, ArrayLowerBound _
        | StaticField _, StaticField _ -> __unreachable__()
        | _ -> False

    let rec simplifyReferenceEqualityk x y k =
        simplifyGenericBinary "reference comparison" x y k
            (fun _ _ _ -> __unreachable__())
            (fun x y k ->
                match x.term, y.term with
                | _ when x = y -> k True
                | Ref addr1, Ref addr2 -> compareAddress (addr1, addr2) |> k
                | Ptr(addr1, _, shift1), Ptr(addr2, _, shift2) ->
                    let addrEq =
                        match addr1, addr2 with
                        | Some addr1, Some addr2 -> compareAddress (addr1, addr2)
                        | None, None -> True
                        | _ -> False
                    match shift1, shift2 with
                    | None, None -> k addrEq
                    | Some shift, None
                    | None, Some shift -> addrEq &&& isZero shift |> k
                    | Some shift1, Some shift2 -> addrEq &&& Arithmetics.simplifyEqual shift1 shift2 id |> k
                | HeapRef(addr1, _), HeapRef(addr2, _) -> Arithmetics.simplifyEqual addr1 addr2 id |> k
                | _ -> False |> k)
            (fun x y k -> simplifyReferenceEqualityk x y k)

    let isNull heapReference =
        simplifyReferenceEqualityk heapReference nullRef id

    let rec private simplifySPDUnaryMinus y tp k =
        simplifySPDExpression y k (fun y k ->
        match y with
        | ConstantPtrT(spd, tp) -> k <| makeSPDConst tp (-. spd)
        | _ -> k <| makeUnary OperationType.UnaryMinus y (tp |?? typeOf y))

    and private simplifySPDAddition l r tp k =
        simplifySPDExpression l k (fun l k ->
        simplifySPDExpression r k (fun r k ->
            match l, r with
            | ConstantPtrT(spdX, _), ConstantPtrT(spdY, _) ->
                spdX +. spdY |> makeSPDConst tp |> k
            | _ -> k <| makeBinary OperationType.Add l r tp))

    and private simplifySPDExpression y k repeat =
        let k y = repeat y k
        match y with
        | Add(left, right, tp) ->
            simplifySPDAddition left right tp k
        | UnaryMinusT(arg, tp) ->
            simplifySPDUnaryMinus arg (Some tp) k
        | Sub(left, right, tp) ->
            simplifySPDUnaryMinus right None (fun right ->
            simplifySPDAddition left right tp k)
        | _ -> k y

    let rec private simplifyPointerExpressionAddition x y k =
        let shiftTyp = typeOf y
        let mkAdd l r = makeBinary OperationType.Add l r shiftTyp
        let rec collectSPDs expr summands spd k =
            match expr with
            | Add(left, right, _) ->
                collectSPDs left summands spd (fun (summands, spd) ->
                collectSPDs right summands spd k)
            | ConstantPtrT(spd', _) -> k (summands, spd +. spd')
            | _ -> k (expr :: summands, spd)
        collectSPDs y [] SymbolicPointerDifference.empty (fun (summands, spd) ->
        Cps.List.reduce mkAdd summands (fun y ->
        addPtrToSymbolicPointerDifference x spd shiftTyp (fun x ->
        shift x y k)))

    let rec private simplifyPointerAdditionGeneric x y k = // y must be normalized by Arithmetics!
        let simplifyIndentedPointerAddition x y k =
            let multWithSizeOf t = Arithmetics.mul t <| underlyingPointerTypeSizeof x

            let simplifyRawPointerAddition x y k = // x is not Indented Ptr
                match y with
                | ConcreteT(zero, _) when CSharpUtils.Calculator.IsZero zero -> k x
                | ConstantPtrT(spd, tp) -> addPtrToSymbolicPointerDifference x spd tp k
                | Add _ -> simplifyPointerExpressionAddition x y k
                | _ ->
                    match term y with
                    | Expression _
                    | Concrete _
                    | Constant _ -> shift x y k
                    | _ -> internalfailf "Pointer arithmetic: expected primitive value but got: %O" y

            match term x with
            | Ptr(addr, typ, Some shift) ->
                simplifySPDExpression (Arithmetics.add shift <| multWithSizeOf y) k (simplifyRawPointerAddition <| Ptr addr typ None)
            | _ -> simplifySPDExpression (multWithSizeOf y) k (simplifyRawPointerAddition x)

        simplifyGenericBinary "add shift to pointer" x y k
            (fun _ _ _ -> __unreachable__())
            simplifyIndentedPointerAddition
            simplifyPointerAdditionGeneric

    let private simplifyIndentedReferenceAddition x y k =
        let x', y' = if Terms.isNumeric y then x, y else y, x
        simplifyPointerAdditionGeneric x' y' k

    let rec private simplifyPointerSubtractionGeneric x y k =
        let makeDiff p q =
            let tp = Numeric (Id typedefof<int64>)
            if p = q
            then makeNumber 0L
            else
                SymbolicPointerDifference([p, 1], [q, 1])
                |> makeSPDConst tp
        let divideBySizeof diff = Arithmetics.div diff <| underlyingPointerTypeSizeof x
        let simplifyPointerDiffWithOffset p q offset =
            simplifySPDExpression (Arithmetics.add (makeDiff p q) offset) k (fun diff k -> k (divideBySizeof diff))
        let simplifyIndentedPointerSubtraction x y k =
            match term x, term y with
            // TODO: [columpio]: rewrite it in a more efficient way!
            | Ptr(addr1, t1, Some a), Ptr(addr2, t2, Some b) -> simplifyPointerDiffWithOffset (Ptr addr1 t1 None) (Ptr addr2 t2 None) (Arithmetics.sub a b)
            | Ptr(addr, t, Some a), _ -> simplifyPointerDiffWithOffset (Ptr addr t None) y a
            | _, Ptr(addr, t, Some b) -> simplifyPointerDiffWithOffset x (Ptr addr t None) (Arithmetics.neg b)
            | _ -> k (divideBySizeof <| makeDiff x y)

        simplifyGenericBinary "pointer1 - pointer2" x y k
            (fun _ _ _ -> __unreachable__())
            simplifyIndentedPointerSubtraction
            simplifyPointerSubtractionGeneric

    let private simplifyIndentedReferenceSubtraction x y k =
        if isNumeric y
        then simplifyPointerAdditionGeneric x (Arithmetics.neg y) k
        else simplifyPointerSubtractionGeneric x y k

    let simplifyBinaryOperation op x y k =
        match op with
        | OperationType.Subtract ->
            simplifyIndentedReferenceSubtraction x y k
        | OperationType.Add ->
            simplifyIndentedReferenceAddition x y k
        | OperationType.Equal -> simplifyReferenceEqualityk x y k
        | OperationType.NotEqual ->
            simplifyReferenceEqualityk x y (fun e ->
            Propositional.simplifyNegation e k)
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let add x y =
        simplifyBinaryOperation OperationType.Add x y id

    let sub x y =
        simplifyBinaryOperation OperationType.Subtract x y id

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
