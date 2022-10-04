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
        ShortestDistance,
        ContributedCoverage,
        Interleaved
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
        private const string CsvPathParameterName = "csvPath";
        private const string RunIdParameterName = "runId";
        private const string SearchStrategyParameterName = "searchStrategy";
        private const string ExpectedCoverageParameterName = "expectedCoverage";
        private const string TimeoutParameterName = "timeout";
        private const string ReleaseBranchesParameterName = "releaseBranches";

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
        private readonly bool _releaseBranches;

        public TestSvmAttribute(
            int expectedCoverage = -1,
            uint recThresholdForTest = 0u,
            int timeout = -1,
            bool concolicMode = false,
            bool guidedMode = true,
            bool releaseBranches = true,
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
            _releaseBranches = releaseBranches;
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
                _releaseBranches,
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
            private readonly bool _releaseBranches;
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
                bool releaseBranches,
                executionMode execMode,
                SearchStrategy strat,
                CoverageZone coverageZone) : base(innerCommand)
            {
                _baseCoverageZone = coverageZone;
                _baseSearchStrat = TestContext.Parameters[SearchStrategyParameterName] == null ?
                    strat : (SearchStrategy)Enum.Parse(typeof(SearchStrategy), TestContext.Parameters[SearchStrategyParameterName], true);

                _expectedCoverage = TestContext.Parameters[ExpectedCoverageParameterName] == null ?
                    expectedCoverage : int.Parse(TestContext.Parameters[ExpectedCoverageParameterName]);

                _recThresholdForTest = recThresholdForTest;
                _executionMode = execMode;

                _timeout = TestContext.Parameters[TimeoutParameterName] == null ?
                    timeout : int.Parse(TestContext.Parameters[TimeoutParameterName]);

                _releaseBranches = TestContext.Parameters[ReleaseBranchesParameterName] == null ?
                    releaseBranches : bool.Parse(TestContext.Parameters[ReleaseBranchesParameterName]);

                _searchStrat = _baseSearchStrat switch
                {
                    SearchStrategy.DFS => searchMode.DFSMode,
                    SearchStrategy.BFS => searchMode.BFSMode,
                    SearchStrategy.ShortestDistance => searchMode.ShortestDistanceBasedMode,
                    SearchStrategy.ContributedCoverage => searchMode.ContributedCoverageMode,
                    SearchStrategy.Interleaved => searchMode.NewInterleavedMode(searchMode.ShortestDistanceBasedMode, 1, searchMode.ContributedCoverageMode, 9),
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

                var csvReportPath = TestContext.Parameters[CsvPathParameterName];
                if (csvReportPath != null)
                {
                    reporter = new CsvStatisticsReporter(
                        csvReportPath,
                        "TestResults",
                        TestContext.Parameters[RunIdParameterName] ?? ""
                    );
                }

                Core.API.ConfigureSolver(SolverPool.mkSolver());
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                var stats = new TestStatistics(
                    methodInfo,
                    _searchStrat.IsGuidedMode,
                    _releaseBranches,
                    _timeout,
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
                        false,
                        _releaseBranches,
                        128
                    );
                    SILI explorer = new SILI(_options);
                    AssemblyManager.Load(methodInfo.Module.Assembly);

                    void GenerateTestAndCheckCoverage(UnitTest unitTest)
                    {
                        unitTests.GenerateTest(unitTest);

                        if (_expectedCoverage == null)
                        {
                            return;
                        }

                        var method = Application.getMethod(unitTest.Method);
                        var approximateCoverage = explorer.Statistics.GetApproximateCoverage(method);

                        if (approximateCoverage >= _expectedCoverage)
                        {
                            explorer.Stop();
                        }
                    }

                    void GenerateErrorAndCheckCoverage(UnitTest unitTest)
                    {
                        unitTests.GenerateError(unitTest);

                        if (_expectedCoverage == null)
                        {
                            return;
                        }

                        var method = Application.getMethod(unitTest.Method);
                        var approximateCoverage = explorer.Statistics.GetApproximateCoverage(method);

                        if (approximateCoverage >= _expectedCoverage)
                        {
                            explorer.Stop();
                        }
                    }

                    explorer.InterpretIsolated(methodInfo, GenerateTestAndCheckCoverage, GenerateErrorAndCheckCoverage, _ => { }, e => throw e);

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
                parameter => Reflection.defaultOf(parameter.ParameterType)).ToArray();
            var parameters = new TestCaseParameters(defaultParameters);
            if (method.ReturnType.Type != typeof(void))
                parameters.ExpectedResult = null;
            return _builder.BuildTestMethod(method, suite, parameters);
        }
    }
}
