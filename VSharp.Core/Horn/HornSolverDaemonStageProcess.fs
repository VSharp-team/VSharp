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
            if not daemonProcess.InterruptFlag then
                let processInvocationExpression (invocation : IInvocationExpression) =
                    System.Console.WriteLine("=============================")
                    System.Console.WriteLine("GOT HERE: " + invocation.InvokedExpression.ToString() + " ;; " + invocation.InvokedExpression.GetText())
                    let lifetime = JetBrains.DataFlow.Lifetimes.Define()
                    let options = new JetBrains.Decompiler.ClassDecompilerOptions(true)
                    let methodCollector = new JetBrains.Metadata.Utils.MethodCollectorStub()
                    let assemblyLoader = new JetBrains.Metadata.Reader.API.MetadataLoader(JetBrains.Metadata.Access.MetadataProviderFactory.DefaultProvider)
                    let resolved = (invocation.InvokedExpression :?> IReferenceExpression).Reference.Resolve()
                    let meth = (resolved.DeclaredElement :?> IMethod)
                    let isValid = (meth <> null) && ((meth.GetContainingType() <> null) && (meth.GetContainingType().GetClrName() <> null))
                    System.Console.WriteLine("BEFORE SUSP: " + resolved.Info.ToString())
                    let qualifiedTypeName = if isValid then meth.GetContainingType().GetClrName().FullName else "TempTest.Testbed"
                    let methodName = if isValid then meth.ShortName else "Add1"

                    let path = 
                        if isValid then
                            match meth.GetContainingType().Module with
                            | :? JetBrains.ReSharper.Psi.Modules.IAssemblyPsiModule as assemblyModule -> assemblyModule.Assembly.Location
                            | _ -> raise(new Exception("Shit happens"))
                         else JetBrains.Util.FileSystemPath.Parse(@"c:\dev\VSharp\TempTest\bin\Debug\TempTest.dll")

                    let metadataAssembly = assemblyLoader.LoadFrom(path, fun x -> true)
                    let rp = JetBrains.Util.FileSystemPath.Parse(@"C:\Program Files\Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5\")
                    metadataAssembly.ReferencedAssembliesNames |> Seq.iter (fun ass -> Console.WriteLine("Loaded from " + assemblyLoader.LoadFrom(JetBrains.Metadata.Utils.AssemblyNameMetadataExtensions.FindAssemblyFile(rp, ass), fun x -> true).Location.ToString()))
                    let metadataTypeInfo = metadataAssembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false)
                    System.Console.WriteLine("METADATA ASS: " + metadataAssembly.Location.FullPath + " " + metadataAssembly.IsResolved.ToString())
                    System.Console.WriteLine("TYPE: " + metadataTypeInfo.ToString())
                    let metadataMethod = metadataTypeInfo.GetMethods() |> Seq.pick (fun m -> if m.Name.Equals(methodName) then Some(m) else None)

                    let decompiler = new JetBrains.Decompiler.ClassDecompiler(lifetime.Lifetime, metadataAssembly, options, methodCollector)
                    let decompiledMethod = decompiler.Decompile(metadataTypeInfo, metadataMethod)
                    System.Console.WriteLine("DECOMPILED: " + JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
                    System.Console.WriteLine("DECOMPILED BODY: " + JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod.Body))
                    System.Console.WriteLine("NOW TRACING:")
                    VSharp.Core.Symbolic.Interpreter.dbg 0 decompiledMethod

                let processor = new RecursiveElementProcessor<IInvocationExpression>(new Action<_>(processInvocationExpression));
                file.ProcessDescendants(processor);




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
