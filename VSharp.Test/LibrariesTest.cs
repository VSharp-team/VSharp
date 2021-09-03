using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;
using NUnit.Framework;

using VSharp.Interpreter.IL;

namespace VSharp.Test
{
    [TestFixture]
    public class LibrariesTest
    {
        [Ignore("externs")]
        public static void MSCoreLibTest()
        {
            var svm = new SVM(new MethodInterpreter(200, new BFSSearcher(200)));            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
            // svm.ConfigureSolver(new SmtSolverWrapper<Microsoft.Z3.AST>(new Z3Solver()));
            var assembly = typeof(int).Assembly;
            svm.Run(assembly, new List<string>());
        }

        [Ignore("externs and exceptions")]
        public static void OpenSourceRiderTest()
        {
            var svm = new SVM(new MethodInterpreter(200, new BFSSearcher(200)));
            svm.ConfigureSolver();
            var path = "../../../../rd/rd-net/Test.Lifetimes/bin/Debug/netcoreapp3.1/Test.Lifetimes.dll";
            var assembly = Assembly.LoadFrom(path);
            var testingMethodType = assembly.GetType("Test.Lifetimes.Core.TestResult");
            var testingMethod = testingMethodType?.GetMethod("UnwrapStackTraceEasy");
            Stopwatch stopwatch = new Stopwatch();
            stopwatch.Start();
            string result = svm.ExploreOne(testingMethod);
            stopwatch.Stop();
            var ts = stopwatch.Elapsed;
            Console.WriteLine($"For method {testingMethod} got: {result}");
            Console.WriteLine("Elapsed Time is {0:00}:{1:00}:{2:00}.{3}", ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds);
        }
    }
}
