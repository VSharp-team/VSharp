namespace VSharp

open JetBrains.Decompiler.Ast

module internal Strings =

    let internal simplifyEquality x y =
        match x, y with
        | Concrete(x, String), Concrete(y, String) -> Terms.MakeBool((x :?> string) = (y :?> string))
        | _ -> __notImplemented__()

    let internal simplifyConcatenation x y =
        match (x, y) with
        | (Concrete(x, xt), Concrete(y, yt)) -> Terms.MakeConcrete (VSharp.CSharpUtils.Calculator.Add(x, y, typedefof<string>)) typedefof<string>
        | _ -> Terms.MakeBinary OperationType.Add x y false String

    let internal simplifyOperation op x y =
        match op with
        | OperationType.Add -> simplifyConcatenation x y
        | OperationType.Equal -> simplifyEquality x y
        | OperationType.NotEqual -> !! (simplifyEquality x y)
        | OperationType.NullCoalescing
        | _ -> __notImplemented__()

    let internal isStringOperation op t1 t2 =
        Types.IsString t1 && Types.IsString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.NullCoalescing -> true
        | _ -> false
