using System;
using System.Diagnostics;
using Test.Lifetimes.Lifetimes;
using VSharp.Interpreter.IL;
using VSharp.Test;
using VSharp.Test.Tests;

namespace VSharp.ExecuteTests
{
    class Program
    {
        static void Main(string[] args)
        {
            var options = new siliOptions(explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchMode.BFSMode), executionMode.ConcolicMode, 200);
            var svm = new SVM(options);
            svm.ConfigureSolver();
            // var testingMethod = typeof(Arithmetics).GetMethod("SumShifts"); // works
            // var testingMethod = typeof(Arithmetics).GetMethod("LogMethod4"); // TODO: internal calls
            // var testingMethod = typeof(Conversions).GetMethod("TestNarrowingConv"); // TODO: fork
            // var testingMethod = typeof(Conversions).GetMethod("Test1"); // works
            // var testingMethod = typeof(Unsafe).GetMethod("ReturnConst"); // works
            // var testingMethod = typeof(Unsafe).GetMethod("ReturnClass"); // works
            // var testingMethod = typeof(Unsafe).GetMethod("ReturnField"); // works
            // var testingMethod = typeof(Unsafe).GetMethod("WriteFieldSymbolic"); // works
            // var testingMethod = typeof(Unsafe).GetMethod("RetConcrete"); // works
            // var testingMethod = typeof(Unsafe).GetMethod("WriteFieldByRefSymbolic"); // works
            // var testingMethod = typeof(Lists).GetMethod("RankTest"); // TODO: support structs (call: argument of InitializeArray)
            // var testingMethod = typeof(Lists).GetMethod("CopyToConcreteToConcreteArray"); // TODO: wrong binop stack types (Struct and I), mb unsafe operations (IntPtr)
            // var testingMethod = typeof(Lists).GetMethod("SolverTestConcreteArray"); // TODO: support structs (call: argument of InitializeArray)
            var testingMethod = typeof(ClassesSimplePropertyAccess).GetMethod("TestProperty1"); // TODO: stfld intrumentation is wrong
            Stopwatch stopwatch = new Stopwatch();
            stopwatch.Start();
            var result = svm.ExploreOne(testingMethod);
            stopwatch.Stop();
            var ts = stopwatch.Elapsed;
            Console.WriteLine($"For method {testingMethod} got: {result}");
            Console.WriteLine("Elapsed Time is {0:00}:{1:00}:{2:00}.{3}", ts.Hours, ts.Minutes, ts.Seconds,
                ts.Milliseconds);
        }
    }
}
