namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.Common

module internal Pointers =

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

    let internal simplifyBinaryOperation metadata op state x y k =
        match op with
        | OperationType.Add
        | OperationType.Subtract -> __notImplemented__()
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
