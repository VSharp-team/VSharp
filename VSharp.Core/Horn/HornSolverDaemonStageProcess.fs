namespace VSharp.Core.Horn

open JetBrains.Metadata.Reader.API
open JetBrains.ReSharper.Feature.Services.Daemon
open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft
open System;
open System.Reflection

type HornSolverDaemonStageProcess(daemonProcess : IDaemonProcess, file : ICSharpFile) =

    interface IDaemonStageProcess with
        member x.DaemonProcess with get() = daemonProcess
        member x.Execute commiter =
            if not daemonProcess.InterruptFlag then
                let processInvocationExpression (invocation : IInvocationExpression) =
                    let resolved = (invocation.InvokedExpression :?> IReferenceExpression).Reference.Resolve()
                    let meth = resolved.DeclaredElement :?> IMethod
                    let (term, env) = VSharp.Core.Symbolic.Interpreter.decompileAndReduceMethod meth
                    Console.WriteLine("=========== Results: ===========")
                    Console.WriteLine("SVM term: " + term.ToString())
                    Console.WriteLine("SVM environment: " + env.ToString())

                let processor = new RecursiveElementProcessor<IInvocationExpression>(new Action<_>(processInvocationExpression))
                file.ProcessDescendants(processor)




//                let cfg =
//                    let list = [ ("AUTO_CONFIG", "true" ) ]
//                    System.Linq.Enumerable.ToDictionary(list, fst, snd)
//                use ctx = new Z3.Context(cfg)
//                let fp = ctx.MkFixedpoint()
//                let parameters = ctx.MkParams()
//                parameters.Add("fixedpoint.pdr.utvpi", false)
//                fp.Parameters <- parameters
//                let facade = new HornFacade(ctx, fp, new VSharp.Core.Common.SmtDeclarations())
//
//                let processMethods = fun (declaration : IMethodDeclaration) ->
//                    let assertions = new Assertions()
//                    Console.WriteLine("----------------------------<Analyzing>--------------------------\n")
//                    Console.WriteLine(declaration.GetText())
//                    Console.WriteLine("--------------------------\n")
//
//                    Console.WriteLine((HornPrinter.printFunctionDeclaration facade assertions declaration).ToString())
//
//                let processor = new RecursiveElementProcessor<IMethodDeclaration>(new Action<_>(processMethods));
//                try
//                    file.ProcessDescendants(processor);
//                with
//                | _ ->
//
//                commiter.Invoke(new DaemonStageResult(facade.highlighter.highlights()))
