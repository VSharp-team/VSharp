using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;

using VSharp.Interpreter.IL;

namespace VSharp.Test
{
    [TestFixture]
    public class LibrariesTest
    {
        [Test]
        public static void MSCoreLibTest()
        {
            var svm = new SVM(new MethodInterpreter(new DummySearcher()));
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
            // svm.ConfigureSolver(new SmtSolverWrapper<Microsoft.Z3.AST>(new Z3Solver()));
            var assembly = typeof(int).Assembly;
            svm.Run(assembly, new List<string>());
        }

        [Test]
        public static void OpenSourceRiderTest()
        {
            var svm = new SVM(new MethodInterpreter(new ExceptionsExplorationSearcher()));
            svm.ConfigureSolver();
            var path = "../../../../rd/rd-net/Test.Lifetimes/bin/Debug/netcoreapp3.1/Test.Lifetimes.dll";
            var assembly = Assembly.LoadFrom(path);
            var testingMethodType = assembly.GetType("Test.Lifetimes.Lifetimes.LifetimeTest");
            var testingMethod = testingMethodType?.GetMethod("StackTrace1");
            svm.ExploreOne(testingMethod);
        }
    }
}
