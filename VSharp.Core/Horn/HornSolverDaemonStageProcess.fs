namespace VSharp.Core.Horn

open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft
open System;

type HornSolverDaemonStageProcess(daemonProcess : IDaemonProcess, file : ICSharpFile) =
    interface IDaemonStageProcess with
        member x.DaemonProcess with get() = daemonProcess
        member x.Execute commiter =
            let processMethods = fun (declaration : IMethodDeclaration) ->
                Console.WriteLine(declaration.ToString())
                Console.WriteLine(declaration.Type)
                Console.WriteLine(declaration.GetText())
                Console.WriteLine("--------------------------\n")

                let cfg =
                    let list = [ ("AUTO_CONFIG", "true" ) ]
                    System.Linq.Enumerable.ToDictionary(list, fst, snd)
                use ctx = new Z3.Context(cfg)
                let test = HornPrinter.infiniteDomain
                Console.WriteLine("RESULT: " + test.ToString())
                let facade = new HornFacade(ctx, ctx.MkFixedpoint(), new VSharp.Core.Common.SmtDeclarations())
                let assertions = new Assertions()
                Console.WriteLine((HornPrinter.printFunctionDeclaration facade assertions declaration).ToString())

            let processor = new RecursiveElementProcessor<IMethodDeclaration>(new Action<_>(processMethods));
            file.ProcessDescendants(processor);


//---------------------------------------------------------------------------------------------                         
//            let cfg =
//                let list = [ ("AUTO_CONFIG", "true" ) ]
//                System.Linq.Enumerable.ToDictionary(list, fst, snd)
//            use ctx = new Z3.Context(cfg)
//
//            let fp = ctx.MkFixedpoint()
//            let intSort = ctx.IntSort :> Z3.Sort
//            let boolSort = ctx.BoolSort :> Z3.Sort
//            let n = ctx.MkBound(0u, intSort) :?> Z3.ArithExpr
//            let m = ctx.MkBound(1u, intSort) :?> Z3.ArithExpr
//            let p = ctx.MkBound(2u, intSort) :?> Z3.ArithExpr
//            let fib = ctx.MkFuncDecl("fib", [| intSort; intSort |], boolSort)
//            let fail = ctx.MkFuncDecl("fail", [||], boolSort)
//            let one = ctx.MkInt(1) :> Z3.ArithExpr
//            let two = ctx.MkInt(2) :> Z3.ArithExpr
//            fp.RegisterRelation(fib)
//            fp.RegisterRelation(fail)
//
//            //(=> (< n 2) (fib n 1))
//            let clause1 = ctx.MkImplies(ctx.MkLt(n, two), fib.[n, one] :?> Z3.BoolExpr)
//            //(=> (and (not (< n 2)) (fib (- n 1) m) (fib (- n 2) p)) (fib n (+ m p)))
//            let clause2 = ctx.MkImplies(ctx.MkAnd(ctx.MkNot(ctx.MkLt(n, two)), fib.[ctx.MkSub(n, one), m] :?> Z3.BoolExpr, fib.[ctx.MkSub(n, two), p] :?> Z3.BoolExpr), 
//                                            fib.[n, ctx.MkAdd(m, p)] :?> Z3.BoolExpr)
//
//            // (=> (fib n m) (> m 0))
//            let assertion = ctx.MkImplies(ctx.MkAnd(fib.[n, m] :?> Z3.BoolExpr, ctx.MkLt(m, one)), fail.Apply() :?> Z3.BoolExpr)
//
//            fp.AddRule(clause1)
//            fp.AddRule(clause2)
//            fp.AddRule(assertion)
//
//            let status = fp.Query(fail.Apply() :?> Z3.BoolExpr)
//            Console.WriteLine("status: {0}", status)
//            Console.WriteLine("answer: {0}", fp.GetAnswer())

//---------------------------------------------------------------------------------------------                         
//            let cfg =
//                let list = [ ("AUTO_CONFIG", "true" ) ]
//                System.Linq.Enumerable.ToDictionary(list, fst, snd)
//            use ctx = new Z3.Context(cfg)
//            let x = ctx.MkIntConst("x")
//            let y = ctx.MkIntConst("y")
//
//            let intSort = ctx.IntSort :> Z3.Sort
//            let f = ctx.MkFuncDecl("f", [| intSort; intSort |], intSort)
//            let g = ctx.MkFuncDecl("g", [| intSort |], intSort)
//            let n = f.[f.[g.[x], g.[g.[x]]], g.[g.[y]]]
//
//            System.Console.WriteLine(n)
//
//            let nn = n.Substitute([| g.[g.[x]]; g.[y] |],  [| y  :> Z3.Expr; ctx.MkAdd(x, ctx.MkInt(1)) :> Z3.Expr |] )
//
//            System.Console.WriteLine(nn)
//            System.Console.WriteLine(n.Substitute(g.[g.[x]], y))
