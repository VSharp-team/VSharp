namespace VSharp.Core.Common

open JetBrains.ReSharper.Psi.Tree
open Microsoft
open System.Collections.Generic

type SmtDeclarations() =
    let expressions = new Dictionary<string, Z3.Expr>()
    let functions = new Dictionary<string, Z3.FuncDecl>()
    let intermediate = new Dictionary<ITreeNode, Z3.Expr>()

    member public this.AddExpr(id : string, expr : Z3.Expr)  = expressions.Add(id, expr)
    member public this.AddExpr(node : ITreeNode, expr : Z3.Expr) = intermediate.Add(node, expr)
    member public this.Item with get(node) = intermediate.[node]
    member public this.Item with get(id) = expressions.[id]
    member public this.ArithmeticExpr id = expressions.[id] :?> Z3.ArithExpr
    member public this.ArithmeticExpr node = intermediate.[node] :?> Z3.ArithExpr

    member public this.AddFunc = functions.Add
    member public this.Function id = functions.[id]
