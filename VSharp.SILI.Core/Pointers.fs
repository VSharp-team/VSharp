namespace VSharp.Core

open VSharp
open VSharp.Core.Common

module internal Pointers =

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

    let simplifyBinaryOperation metadata op state x y k =
        match op with
        | OperationType.Add
        | OperationType.Subtract -> __notImplemented__()
        | OperationType.Equal -> simplifyReferenceEquality metadata x y (withSnd state >> k)
        | OperationType.NotEqual ->
            simplifyReferenceEquality metadata x y (fun e ->
            Propositional.simplifyNegation metadata e (withSnd state >> k))
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let isPointerOperation op t1 t2 =
        (Types.isPointer t1 || Types.isReference t1 || Types.isBottom t1) &&
        (Types.isPointer t2 || Types.isReference t2 || Types.isBottom t2) &&
        match op with
        | OperationType.Add
        | OperationType.Subtract
        | OperationType.Equal
        | OperationType.NotEqual -> true
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
