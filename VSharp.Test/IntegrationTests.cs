using System;
using System.IO;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading;
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

            uint maxBound = 15;
            Logger.ConfigureWriter(TestContext.Progress);
            _options = new SiliOptions(explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchMode.DFSMode), executionMode.SymbolicMode, maxBound);
            Core.API.ConfigureSolver(SolverPool.mkSolver());
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
        }
        private int _expectedCoverage = 100;


        public TestSvmAttribute() { }

        public TestSvmAttribute(int expectedCoverage)
        {
            _expectedCoverage = expectedCoverage;
        }

        public virtual TestCommand Wrap(TestCommand command)
        {
            return new TestSvmCommand(command, _expectedCoverage);
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            private int _expectedCoverage;
            public TestSvmCommand(TestCommand innerCommand, int expectedCoverage) : base(innerCommand)
            {
                _expectedCoverage = expectedCoverage;
            }

            private TestResult Explore(TestExecutionContext context)
            {
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                try
                {
                    SILI explorer = new SILI(_options);
                    Statistics statistics = new Statistics();
                    TestGenerator testGenerator = new TestGenerator(Directory.GetCurrentDirectory());
                    statistics.SetupBeforeMethod(methodInfo);

                    explorer.InterpretIsolated(methodInfo, testGenerator.GenerateTest, testGenerator.GenerateError, _ => { }, e => throw e);

                    if (testGenerator.UnitTestsCount == 0 && testGenerator.ErrorsCount == 0 &&
                        explorer.IncompleteStates.Count == 0)
                    {
                        throw new Exception("No states were obtained! Most probably this is bug.");
                    }
                    statistics.AddSucceededMethod(methodInfo);
                    explorer.GenerateReport(TestContext.Out);
                    TestContext.Out.WriteLine("Test results written to {0}", testGenerator.TestDirectory.FullName);
                    testGenerator.WriteReport(explorer.GenerateReport);
                    var coverageTool = new CoverageTool(testGenerator.TestDirectory.FullName, Directory.GetCurrentDirectory());
                    coverageTool.Run(testGenerator.TestDirectory);
                    int coverage = coverageTool.GetCoverage(methodInfo);
                    if (coverage != _expectedCoverage)
                    {
                        context.CurrentResult.SetResult(ResultState.Failure, "Incomplete coverage! Expected " + _expectedCoverage + ", but got " + coverage);
                    }
                    else
                    {
                        context.CurrentResult.SetResult(ResultState.Success);
                    }
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
