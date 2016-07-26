namespace VSharp.Core.Horn

open JetBrains.ReSharper.Daemon.CSharp
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft
open System.Collections.Generic

module Shit =
    let mutable shit = 0

type HornHighlighter(ctx : Z3.Context, fp : Z3.Fixedpoint) =

    let result = new List<HighlightingInfo>()
    let tempRelationPrefix = "tempCheck"

    let isUnsat (assertion : Z3.BoolExpr) =
        System.Console.WriteLine("Querying");
        System.Console.WriteLine(fp.ToString())
        let tempRel = ctx.MkFuncDecl(VSharp.Core.Utils.IdGenerator.startingWith tempRelationPrefix, [| |], ctx.MkBoolSort())
        fp.RegisterRelation(tempRel)
        let target = tempRel.Apply() :?> Z3.BoolExpr
        fp.AddRule(ctx.MkImplies(assertion, target))

        let status = fp.Query(target)
        System.Console.WriteLine("Status: " + status.ToString())
        if status.Equals(Z3.Status.UNKNOWN) then
            System.Console.WriteLine("Reason: " + fp.GetReasonUnknown())
        System.Console.WriteLine(fp.Assertions.ToString())
        status.Equals(Z3.Status.UNSATISFIABLE)

    member public this.checkIfStatement (statement : IIfStatement) (assertions : Assertions) (condition : Z3.BoolExpr) =
        let checkBranch (branch : ICSharpStatement) condition =
            if branch <> null then
                let unsat = isUnsat(assertions.With(condition).Print(ctx))
                System.Console.WriteLine()
                if unsat then
                    result.Add(new HighlightingInfo(branch.GetDocumentRange(), new Errors.UnreachableCodeWarning(new Util.TreeRange(branch, branch))))
                    System.Console.WriteLine("FOUND UNREACHABLE CODE!!!")
                    System.Console.WriteLine(branch.GetText())

        checkBranch statement.Then condition
        checkBranch statement.Else (ctx.MkNot condition)

    member public this.highlights() = result
