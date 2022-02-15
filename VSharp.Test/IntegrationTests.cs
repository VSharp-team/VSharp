using System;
using System.IO;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using NUnit.Framework.Internal.Commands;
using VSharp.Interpreter.IL;
using VSharp.Solver;

namespace VSharp.Test
{
    public class TestSvmFixtureAttribute : NUnitAttribute, IFixtureBuilder
    {
        public IEnumerable<TestSuite> BuildFrom(ITypeInfo typeInfo)
        {
            var typ = new Utils.DummyTypeInfo(typeInfo.Type);
            yield return new NUnitTestFixtureBuilder().BuildFrom(typ, new Utils.DummyFilter());
        }
    }

    public class TestSvmAttribute : NUnitAttribute, IWrapTestMethod, ISimpleTestBuilder
    {
        private static SiliOptions _options = null;

        static TestSvmAttribute()
        {
            Trace.Listeners.Add(new Utils.DumpStackTraceListener());

            var ci = new CultureInfo("en-GB")
            {
                NumberFormat = {
                    PositiveInfinitySymbol = "Infinity",
                    NegativeInfinitySymbol = "-Infinity"
                }
            };
            Thread.CurrentThread.CurrentCulture = ci;

            Logger.ConfigureWriter(TestContext.Progress);
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
        }
        private int _expectedCoverage = 100;
        private uint _maxBoundForTest = 15u;


        public TestSvmAttribute() { }

        public TestSvmAttribute(int expectedCoverage)
        {
            _expectedCoverage = expectedCoverage;
        }

        public TestSvmAttribute(int expectedCoverage, uint maxBoundForTest)
        {
            _expectedCoverage = expectedCoverage;
            _maxBoundForTest = maxBoundForTest;
        }

        public virtual TestCommand Wrap(TestCommand command)
        {
            return new TestSvmCommand(command, _expectedCoverage, _maxBoundForTest);
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            private int _expectedCoverage;
            private uint _maxBoundForTest;
            public TestSvmCommand(TestCommand innerCommand, int expectedCoverage, uint maxBoundForTest) : base(innerCommand)
            {
                _expectedCoverage = expectedCoverage;
                _maxBoundForTest = maxBoundForTest;
            }

            private TestResult Explore(TestExecutionContext context)
            {
                Core.API.ConfigureSolver(SolverPool.mkSolver());
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                try
                {
                    _options = new SiliOptions(explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchMode.DFSMode), executionMode.SymbolicMode, _maxBoundForTest);
                    SILI explorer = new SILI(_options);
                    UnitTests unitTests = new UnitTests(Directory.GetCurrentDirectory());

                    Stopwatch.runMeasuringTime("total_interpretation", FuncConvert.FromAction(() =>
                    {
                        explorer.InterpretIsolated(methodInfo, unitTests.GenerateTest, unitTests.GenerateError,
                            _ => { }, e => throw e);
                    }));

                    if (unitTests.UnitTestsCount == 0 && unitTests.ErrorsCount == 0 && explorer.Statistics.IncompleteStates.Count == 0)
                    {
                        throw new Exception("No states were obtained! Most probably this is bug.");
                    }
                    explorer.Statistics.PrintStatistics(TestContext.Out);
                    TestContext.Out.WriteLine("Test results written to {0}", unitTests.TestDirectory.FullName);
                    unitTests.WriteReport(explorer.Statistics.PrintStatistics);
                    if (unitTests.UnitTestsCount != 0 || unitTests.ErrorsCount != 0)
                    {
                        TestContext.Out.WriteLine("Starting coverage tool...");
                        var coverageTool = new CoverageTool(unitTests.TestDirectory.FullName,
                            Directory.GetCurrentDirectory());
                        coverageTool.Run(unitTests.TestDirectory);
                        /*int coverage = coverageTool.GetCoverage(methodInfo);
                        if (coverage != _expectedCoverage)
                        {
                            context.CurrentResult.SetResult(ResultState.Failure,
                                "Incomplete coverage! Expected " + _expectedCoverage + ", but got " + coverage);
                        }
                        else
                        {
                            context.CurrentResult.SetResult(ResultState.Success);
                        }*/
                        
                        context.CurrentResult.SetResult(ResultState.Success);
                    }
                    else
                    {
                        context.CurrentResult.SetResult(ResultState.Success);
                    }
                    
                    Stopwatch.saveMeasurements(methodInfo.Name);
                    Stopwatch.clear();
                }
                catch (Exception e)
                {
                    // TODO: add more info
                    context.CurrentResult.SetResult(ResultState.Error, e.Message);
                }

                return context.CurrentResult;
            }

            public override TestResult Execute(TestExecutionContext context)
            {
                return Explore(context);
            }
        }

        private static NUnitTestCaseBuilder _builder = new NUnitTestCaseBuilder();

        public TestMethod BuildFrom(IMethodInfo method, NUnit.Framework.Internal.Test suite)
        {
            var defaultParameters = method.GetParameters().Select(
                parameter => TypeUtils.defaultOf(parameter.ParameterType)).ToArray();
            var parameters = new TestCaseParameters(defaultParameters);
            if (method.ReturnType.Type != typeof(void))
                parameters.ExpectedResult = null;
            return _builder.BuildTestMethod(method, suite, parameters);
        }
    }
}
