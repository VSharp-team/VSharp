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
            let cfg =
                let list = [ ("AUTO_CONFIG", "true" ) ]
                System.Linq.Enumerable.ToDictionary(list, fst, snd)
            use ctx = new Z3.Context(cfg)
            let fp = ctx.MkFixedpoint()
            let parameters = ctx.MkParams()
            parameters.Add("fixedpoint.pdr.utvpi", false)
            fp.Parameters <- parameters
            let facade = new HornFacade(ctx, fp, new VSharp.Core.Common.SmtDeclarations())

            let processMethods = fun (declaration : IMethodDeclaration) ->
                let assertions = new Assertions()
                Console.WriteLine(declaration.ToString())
                Console.WriteLine(declaration.Type)
                Console.WriteLine(declaration.GetText())
                Console.WriteLine("--------------------------\n")

                Console.WriteLine((HornPrinter.printFunctionDeclaration facade assertions declaration).ToString())

            let processor = new RecursiveElementProcessor<IMethodDeclaration>(new Action<_>(processMethods));
            file.ProcessDescendants(processor);
