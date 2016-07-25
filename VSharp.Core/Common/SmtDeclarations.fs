namespace VSharp.Core.Common

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft
open System.Collections.Generic
open VSharp.Core.Utils.IdGenerator

type SmtDeclarations() =
    let declared = new Dictionary<IDeclaredElement, Z3.AST>()
    let intermediate = new Dictionary<ITreeNode, Z3.AST>()
    let hasReturnType = new Dictionary<Z3.FuncDecl, bool>()
    let currentFunctions = new Stack<ICSharpParametersOwnerDeclaration>()

    let tempVarPrefix = "tr"
    let tempVarIndeces = new Dictionary<string, int>()
    let tempVars = new Dictionary<string, Z3.Expr>()

    member public this.Add(decl : IDeclaredElement, expr : Z3.Expr) = declared.Add(decl, expr)
    member public this.Add(node : ITreeNode, expr : Z3.Expr) = intermediate.Add(node, expr)
    member public this.Item
        with get(decl) = declared.[decl]
        and set (decl : IDeclaredElement) (value : Z3.AST) =
            if (decl :? IParametersOwner) && (value :? Z3.FuncDecl) then
                hasReturnType.[value :?> Z3.FuncDecl] <- not ((decl :?> IParametersOwner).ReturnType.IsVoid())
            declared.[decl] <- value
    member public this.Item with get(node) = intermediate.[node] and  set node value = intermediate.[node] <- value

    member public this.Has decl = declared.ContainsKey decl
    member public this.Has node = intermediate.ContainsKey node

    member public this.IsExpr decl = declared.[decl] :? Z3.Expr
    member public this.IsExpr node = intermediate.[node] :? Z3.Expr
    member public this.IsArithmetic decl = declared.[decl] :? Z3.ArithExpr
    member public this.IsArithmetic node = intermediate.[node] :? Z3.ArithExpr
    member public this.IsBoolean decl = declared.[decl] :? Z3.BoolExpr
    member public this.IsBoolean node = intermediate.[node] :? Z3.BoolExpr
    member public this.IsFunction decl = declared.[decl] :? Z3.FuncDecl
    member public this.IsFunction node = intermediate.[node] :? Z3.FuncDecl

    member public this.Expr decl = declared.[decl] :?> Z3.Expr
    member public this.Expr node = intermediate.[node] :?> Z3.Expr
    member public this.Arithmetic decl = declared.[decl] :?> Z3.ArithExpr
    member public this.Arithmetic node = intermediate.[node] :?> Z3.ArithExpr
    member public this.Boolean decl = declared.[decl] :?> Z3.BoolExpr
    member public this.Boolean node = intermediate.[node] :?> Z3.BoolExpr
    member public this.Function decl = declared.[decl] :?> Z3.FuncDecl
    member public this.Function node = intermediate.[node] :?> Z3.FuncDecl

    member public this.HasReturnType func = if not (hasReturnType.ContainsKey(func)) then false else hasReturnType.[func]

    member public this.ResetIntermediateVars = tempVarIndeces.Clear
    member public this.NewIntermediateVar(ctx : Z3.Context, sort : Z3.Sort) =
        let prefix = tempVarPrefix + sort.Name.ToString()
        let nextIndex =
            let result =
                if tempVarIndeces.ContainsKey(prefix) then
                    let temp = tempVarIndeces.[prefix]
                    tempVarIndeces.Remove(prefix) |> ignore
                    temp + 1
                else 0
            tempVarIndeces.Add(prefix, result)
            result

        let name = prefix + nextIndex.ToString()
        if not (tempVars.ContainsKey name) then
            tempVars.Add(name, ctx.MkConst(name, sort))
        tempVars.[name]

    member public this.EnterFunctionDeclaration = currentFunctions.Push
    member public this.LeaveFunctionDeclaration() = currentFunctions.Pop() |> ignore
    member public this.CurrentFunctionDeclaration = currentFunctions.Peek
