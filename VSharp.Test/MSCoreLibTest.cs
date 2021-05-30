using System.Collections.Generic;
using NUnit.Framework;

using VSharp.Interpreter.IL;

namespace VSharp.Test
{
    // [TestFixture]
    public class MSCoreLibTest
    {
        // [Test]
        public static void Test()
        {
            var svm = new SVM(new MethodInterpreter(new BFSSearcher(200)));
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
            // svm.ConfigureSolver(new SmtSolverWrapper<Microsoft.Z3.AST>(new Z3Solver()));
            var assembly = typeof(int).Assembly;
            svm.Run(assembly, new List<string>());
        }
    }
}
