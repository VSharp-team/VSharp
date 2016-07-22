namespace VSharp.Core.Common

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft

module TypePrinter =

    let private __notImplemented__ = raise (new System.NotImplementedException())

    let printDeclaredType (ctx : Z3.Context) (declaredType : IDeclaredType) =
        match declaredType with
        | t when t.IsFloatOrDouble() -> ctx.MkRealSort() :> Z3.Sort
        | t when t.IsPredefinedNumeric() -> ctx.MkIntSort() :> Z3.Sort
        | t when t.IsBool() -> ctx.MkBoolSort() :> Z3.Sort
        | _ -> __notImplemented__

    let printType (ctx : Z3.Context) (typ : IType) =
        match typ with
        | :? IDeclaredType as declaredType -> printDeclaredType ctx declaredType
        | _ -> __notImplemented__

    let printExpressionType (ctx : Z3.Context) (expressionType : IExpressionType) =
        match expressionType with
        | :? IType as typ -> printType ctx typ
        | _ -> __notImplemented__
