namespace VSharp.Core.Common

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft
open System

module TypePrinter =

    let private __notImplemented__ = raise (new System.NotImplementedException())

    let printDeclaredType (ctx : Z3.Context) (declaredType : IDeclaredType) =
        match declaredType with
        | t when t.IsFloatOrDouble() -> ctx.MkRealSort() :> Z3.Sort
        | t when t.IsPredefinedNumeric() ->
            let infinite = VSharp.Core.Properties.Settings.InfiniteIntegers
            if infinite then ctx.MkIntSort() :> Z3.Sort
            else
                let clrName = t.GetPresentableName(CSharp.CSharpLanguage.Instance)
                // Calling Type.GetType and Marshal.SizeOf is safe because type filtered out to be good
                let size = Runtime.InteropServices.Marshal.SizeOf(Type.GetType(clrName))
                ctx.MkBitVecSort(Convert.ToUInt32(size)) :> Z3.Sort
        | t when t.IsBool() -> ctx.MkBoolSort() :> Z3.Sort
        | t when t.IsVoid() -> null
        | _ -> __notImplemented__

    let printType (ctx : Z3.Context) (typ : IType) =
        match typ with
        | :? IDeclaredType as declaredType -> printDeclaredType ctx declaredType
        | _ -> __notImplemented__

    let printExpressionType (ctx : Z3.Context) (expressionType : IExpressionType) =
        match expressionType with
        | :? IType as typ -> printType ctx typ
        | _ -> __notImplemented__
