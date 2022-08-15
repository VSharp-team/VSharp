using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
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
    public enum SearchStrategy
    {
        DFS,
        BFS,
        ShortestDistance
    }

    public enum CoverageZone
    {
        Method,
        Class,
        Module
    }

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

        // NOTE: expected coverage field in TestSVM attribute indicates dotCover needs to be run:
        //       if it wasn't mentioned -- dotCover will not be started
        //       if it was -- dotCover will be started
        private readonly int? _expectedCoverage;
        private readonly uint _recThresholdForTest;
        private readonly int _timeout;
        private readonly bool _concolicMode;
        private readonly SearchStrategy _strat;
        private readonly CoverageZone _coverageZone;
        private readonly bool _guidedMode;

        public TestSvmAttribute(
            int expectedCoverage = -1,
            uint recThresholdForTest = 0u,
            int timeout = -1,
            bool concolicMode = false,
            bool guidedMode = true,
            SearchStrategy strat = SearchStrategy.BFS,
            CoverageZone coverageZone = CoverageZone.Class)
        {
            if (expectedCoverage < 0)
                _expectedCoverage = null;
            else if (expectedCoverage > 100)
                _expectedCoverage = 100;
            else _expectedCoverage = expectedCoverage;
            _recThresholdForTest = recThresholdForTest;
            _timeout = timeout;
            _concolicMode = concolicMode;
            _guidedMode = guidedMode;
            _strat = strat;
            _coverageZone = coverageZone;
        }

        public virtual TestCommand Wrap(TestCommand command)
        {
            var execMode = _concolicMode ? executionMode.ConcolicMode : executionMode.SymbolicMode;
            return new TestSvmCommand(
                command,
                _expectedCoverage,
                _recThresholdForTest,
                _timeout,
                _guidedMode,
                execMode,
                _strat,
                _coverageZone
            );
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            private readonly int? _expectedCoverage;
            private readonly uint _recThresholdForTest;
            private readonly int _timeout;
            private readonly executionMode _executionMode;
            private readonly searchMode _searchStrat;
            private readonly coverageZone _coverageZone;

            private readonly SearchStrategy _baseSearchStrat;
            private readonly CoverageZone _baseCoverageZone;
            
            public TestSvmCommand(
                TestCommand innerCommand,
                int? expectedCoverage,
                uint recThresholdForTest,
                int timeout,
                bool guidedMode,
                executionMode execMode,
                SearchStrategy strat,
                CoverageZone coverageZone) : base(innerCommand)
            {
                _baseCoverageZone = coverageZone;
                _baseSearchStrat = strat;
                
                _expectedCoverage = expectedCoverage;
                _recThresholdForTest = recThresholdForTest;
                _executionMode = execMode;
                _timeout = timeout;

                _searchStrat = strat switch
                {
                    SearchStrategy.DFS => searchMode.DFSMode,
                    SearchStrategy.BFS => searchMode.BFSMode,
                    SearchStrategy.ShortestDistance => searchMode.ShortestDistanceBasedMode,
                    _ => throw new ArgumentOutOfRangeException(nameof(strat), strat, null)
                };

                _coverageZone = coverageZone switch
                {
                    CoverageZone.Method => Interpreter.IL.coverageZone.MethodZone,
                    CoverageZone.Class => Interpreter.IL.coverageZone.ClassZone,
                    CoverageZone.Module => Interpreter.IL.coverageZone.ModuleZone,
                    _ => throw new ArgumentOutOfRangeException(nameof(coverageZone), coverageZone, null)
                };

                if (guidedMode)
                {
                    _searchStrat = searchMode.NewGuidedMode(_searchStrat);
                }
            }

            private TestResult Explore(TestExecutionContext context)
            {
                IStatisticsReporter reporter = null;
                    
                var csvReportPath = TestContext.Parameters["csvPath"];
                if (csvReportPath != null)
                {
                    reporter = new CsvStatisticsReporter(
                        csvReportPath,
                        "TestResults",
                        TestContext.Parameters["runId"] ?? "" 
                    );
                }

                Core.API.ConfigureSolver(SolverPool.mkSolver());
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                var stats = new TestStatistics(
                    methodInfo,
                    _searchStrat.IsGuidedMode,
                    _baseSearchStrat,
                    _baseCoverageZone
                );
                    
                try
                {
                    UnitTests unitTests = new UnitTests(Directory.GetCurrentDirectory());
                    _options = new SiliOptions(
                        explorationMode.NewTestCoverageMode(_coverageZone, _searchStrat),
                        _executionMode,
                        unitTests.TestDirectory,
                        _recThresholdForTest,
                        _timeout,
                        false
                    );
                    SILI explorer = new SILI(_options);

                    explorer.InterpretIsolated(methodInfo, unitTests.GenerateTest, unitTests.GenerateError, _ => { }, e => throw e);

                    if (unitTests.UnitTestsCount == 0 && unitTests.ErrorsCount == 0 && explorer.Statistics.IncompleteStates.Count == 0)
                    {
                        throw new Exception("No states were obtained! Most probably this is bug.");
                    }

                    explorer.Statistics.PrintDebugStatistics(TestContext.Out);
                    TestContext.Out.WriteLine("Test results written to {0}", unitTests.TestDirectory.FullName);
                    unitTests.WriteReport(explorer.Statistics.PrintDebugStatistics);

                    stats = stats with
                    {
                        SiliStatisticsDump = explorer.Statistics.DumpStatistics(),
                        TestsGenerated = unitTests.UnitTestsCount,
                        TestsOutputDirectory = unitTests.TestDirectory.FullName
                    };
                    
                    if (unitTests.UnitTestsCount != 0 || unitTests.ErrorsCount != 0)
                    {
                        TestContext.Out.WriteLine("Starting coverage tool...");
                        // NOTE: to disable coverage check TestResultsChecker's expected coverage should be null
                        //       to enable coverage check use _expectedCoverage
                        var testChecker = new TestResultsChecker(unitTests.TestDirectory,
                            Directory.GetCurrentDirectory(), _expectedCoverage);
                        if (testChecker.Check(methodInfo, out var actualCoverage))
                        {
                            context.CurrentResult.SetResult(ResultState.Success);
                            reporter?.Report(stats with { Coverage = actualCoverage });
                        }
                        else
                        {
                            context.CurrentResult.SetResult(ResultState.Failure, testChecker.ResultMessage);
                            reporter?.Report(stats with { Coverage = actualCoverage });
                        }
                    }
                    else
                    {
                        context.CurrentResult.SetResult(ResultState.Success);
                        reporter?.Report(stats);
                    }
                }
                catch (Exception e)
                {
                    context.CurrentResult.SetResult(ResultState.Error, e.Message, e.StackTrace);
                    reporter?.Report(stats with { Exception = e });
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
