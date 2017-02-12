namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open VSharp.Core.Symbolic

module internal Strings =

    let internal simplifyConcatenation x y =
        match (x, y) with
        | (Concrete(x, xt), Concrete(y, yt)) -> Terms.MakeConcrete (VSharp.CSharpUtils.Calculator.Add(x, y, typedefof<string>)) typedefof<string>
        | _ -> Terms.MakeBinary OperationType.Add x y false String

    let internal simplifyOperation op x y =
        match op with
        | OperationType.Add -> simplifyConcatenation x y
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.NullCoalescing
        | _ -> raise(new System.NotImplementedException())

    let internal isStringOperation op t1 t2 =
        Types.IsString t1 && Types.IsString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.NullCoalescing -> true
        | _ -> false
