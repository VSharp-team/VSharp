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
