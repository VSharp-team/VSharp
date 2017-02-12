using JetBrains.ReSharper.Feature.Services.Daemon;
using JetBrains.ReSharper.Psi;
using JetBrains.ReSharper.Psi.CSharp.Tree;
using System;

namespace VSharp
{
    internal sealed class DaemonStageProcess : IDaemonStageProcess
    {
        private readonly ICSharpFile _file;

        public DaemonStageProcess(IDaemonProcess daemonProcess, ICSharpFile file)
        {
            DaemonProcess = daemonProcess;
            _file = file;
        }

        public IDaemonProcess DaemonProcess { get; }

        public void Execute(Action<DaemonStageResult> committer)
        {
            if (DaemonProcess.InterruptFlag)
                return;

            var processInvocationExpression = new Action<IInvocationExpression>(invocation =>
            {
                var resolved = ((IReferenceExpression) invocation.InvokedExpression).Reference.Resolve();
                var meth = (IMethod) resolved.DeclaredElement;
                bool isValid = (meth != null) && (meth.GetContainingType() != null) &&
                               (meth.GetContainingType().GetClrName() != null);

                if (!isValid)
                    throw new ArgumentException("Something went wrong...");

                var qualifiedTypeName = meth.GetContainingType().GetClrName().FullName;
                //else (invocation.InvokedExpression :?> IReferenceExpression).GetExtensionQualifier().GetText()

                var methodName = meth.ShortName;

                var path =
                    ((JetBrains.ReSharper.Psi.Modules.IAssemblyPsiModule) meth.GetContainingType().Module).Assembly
                        .Location;

                Core.Symbolic.Interpreter.interpret(qualifiedTypeName, methodName, path);
            });

            var processor = new RecursiveElementProcessor<IInvocationExpression>(processInvocationExpression);
            _file.ProcessDescendants(processor);
        }
    }
}