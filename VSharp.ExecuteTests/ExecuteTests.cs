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
            // var testingMethod = typeof(Arithmetics).GetMethod("SumShifts"); // works correctly
            var testingMethod = typeof(Arithmetics).GetMethod("LogMethod4"); // works correctly
            // var testingMethod = typeof(Conversions).GetMethod("TestNarrowingConv"); // TODO: concolic got terminate command, but all did not give VSharp results (were symbolic instructions, but all concrete after; how concolic gives results?) #do
            // var testingMethod = typeof(Conversions).GetMethod("Test1"); // TODO: need concrete results from concolic
            // var testingMethod = typeof(Unsafe).GetMethod("ReturnConst"); // TODO: need to resolve stack addresses #do
            // var testingMethod = typeof(Unsafe).GetMethod("ReturnClass"); // TODO: invalid program, instrumentation is wrong (mb newobj) #do
            // var testingMethod = typeof(Lists).GetMethod("RankTest"); // TODO: invalid program, instrumentation is wrong #do
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
