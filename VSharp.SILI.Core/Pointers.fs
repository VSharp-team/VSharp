namespace VSharp.Core

open VSharp
open VSharp.Core.Common

module internal Pointers =

    let locationEqual mtd addr1 addr2 =
        match TypeOf addr1, TypeOf addr2 with
        | String, String -> Strings.simplifyEquality mtd addr1 addr2
        | Numeric _, Numeric _ ->
            if addr1 = addr2 then MakeTrue mtd
            elif IsConcrete addr1 && IsConcrete addr2 then Terms.MakeFalse mtd
            else MakeBinary OperationType.Equal addr1 addr2 false Bool mtd
        | ArrayType _, ArrayType _ -> Arrays.equalsArrayIndices mtd addr1 addr2 |> fst
        | _ -> __notImplemented__()

    let comparePath mtd path1 path2 =
        if List.length path1 <> List.length path2 then
            Terms.MakeFalse mtd
        else
            List.map2 (fun (x, _) (y, _) -> locationEqual mtd x y) path1 path2 |> conjunction mtd

    let rec simplifyReferenceEquality mtd x y k =
        simplifyGenericBinary "reference comparison" State.empty x y (fst >> k)
            (fun _ _ _ _ -> __unreachable__())
            (fun x y s k ->
                let k = withSnd s >> k
                match x.term, y.term with
                | _ when x = y -> MakeTrue mtd |> k
                | HeapRef(xpath, _, None), HeapRef(ypath, _, None) ->
                    comparePath mtd (NonEmptyList.toList xpath) (NonEmptyList.toList ypath) |> k
                | StackRef(key1, path1, None), StackRef(key2, path2, None) ->
                    MakeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | StaticRef(key1, path1, None), StaticRef(key2, path2, None) ->
                    MakeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | _ -> MakeFalse mtd |> k)
            (fun x y state k -> simplifyReferenceEquality mtd x y (withSnd state >> k))

    let isNull mtd ptr =
        simplifyReferenceEquality mtd ptr (MakeNullRef Null mtd) id

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
        (Types.IsPointer t1 || Types.IsReference t1 || Types.IsBottom t1) &&
        (Types.IsPointer t2 || Types.IsReference t2 || Types.IsBottom t2) &&
        match op with
        | OperationType.Add
        | OperationType.Subtract
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false

    let rec topLevelLocation t =
        match t.term with
        | HeapRef(((a, _), []), _, _) -> a
        | Union gvs -> Merging.guardedMap topLevelLocation gvs
        | _ -> __notImplemented__()
