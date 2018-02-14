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

    let private (|SymbolicPointerDifferenceT|_|) (scs: ISymbolicConstantSource) =
        match scs with
        | :? SymbolicPointerDifference as spd -> Some(spd.Pos, spd.Neg)
        | _ -> None

    let isNativeInt typ = typ = typedefof<nativeint> || typ = typedefof<unativeint>

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

    let simplifyBinaryOperation metadata op state x y targetType k =
        let isNativeInt = isNativeInt targetType

        match op with
        | OperationType.Add
        | OperationType.Subtract -> __notImplemented__()
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
