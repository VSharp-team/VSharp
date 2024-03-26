using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.ExceptionServices;
using System.Threading;
using NUnit.Framework;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using NUnit.Framework.Internal.Commands;
using VSharp.Core;
using VSharp.CSharpUtils;
using VSharp.Explorer;
using VSharp.TestRenderer;

namespace VSharp.Test
{
    public enum SearchStrategy
    {
        DFS,
        BFS,
        ShortestDistance,
        RandomShortestDistance,
        ContributedCoverage,
        ExecutionTree,
        ExecutionTreeContributedCoverage,
        Interleaved
    }

    public enum CoverageZone
    {
        Method,
        Class,
        Module
    }

    public enum TestsCheckerMode
    {
        Run,
        RenderAndRun
    }

    public enum OsType
    {
        All,
        Windows,
        Unix
    }

    public enum FuzzerIsolation
    {
        Process
    }

    public enum ExplorationMode
    {
        Sili,
        Interleaving,
        Fuzzer
    }

    public class TestSvmFixtureAttribute : NUnitAttribute, IFixtureBuilder
    {
        public IEnumerable<TestSuite> BuildFrom(ITypeInfo typeInfo)
        {
            var typ = new Utils.DummyTypeInfo(typeInfo.Type);
            yield return new NUnitTestFixtureBuilder().BuildFrom(typ, new Utils.DummyFilter());
        }
    }

    [AttributeUsage(AttributeTargets.Class | AttributeTargets.Struct | AttributeTargets.Method, AllowMultiple = true)]
    public class IgnoreFuzzerAttribute: Attribute
    {
        public string Reason { get; init; }

        public IgnoreFuzzerAttribute(string reason)
        {
            Reason = reason;
        }
    }

    public sealed class TestSvmAttribute : NUnitAttribute, IWrapTestMethod, ISimpleTestBuilder
    {
        private const string CsvPathParameterName = "csvPath";
        private const string RunIdParameterName = "runId";
        private const string SearchStrategyParameterName = "searchStrategy";
        private const string ExpectedCoverageParameterName = "expectedCoverage";
        private const string TimeoutParameterName = "timeout";
        private const string SolverTimeoutParameterName = "solverTimeout";
        private const string ReleaseBranchesParameterName = "releaseBranches";

        private static SVMOptions _options;

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

            Logger.configureWriter(TestContext.Progress);
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
        }

        // NOTE: expected coverage field in TestSVM attribute indicates dotCover needs to be run:
        //       if it wasn't mentioned -- dotCover will not be started
        //       if it was -- dotCover will be started
        private readonly int? _expectedCoverage;
        private readonly uint _recThresholdForTest;
        private readonly int _timeout;
        private readonly int _solverTimeout;
        private readonly SearchStrategy _strat;
        private readonly CoverageZone _coverageZone;
        private readonly TestsCheckerMode _testsCheckerMode;
        private readonly bool _releaseBranches;
        private readonly bool _checkAttributes;
        private readonly bool _hasExternMocking;
        private readonly OsType _supportedOs;
        private readonly FuzzerIsolation _fuzzerIsolation;
        private readonly ExplorationMode _explorationMode;
        private readonly int _randomSeed;
        private readonly uint _stepsLimit;

        public TestSvmAttribute(
            int expectedCoverage = -1,
            uint recThresholdForTest = 1u,
            int timeout = -1,
            int solverTimeout = -1,
            bool releaseBranches = true,
            SearchStrategy strat = SearchStrategy.BFS,
            CoverageZone coverageZone = CoverageZone.Class,
            TestsCheckerMode testsCheckerMode = TestsCheckerMode.RenderAndRun,
            bool checkAttributes = true,
            bool hasExternMocking = false,
            OsType supportedOs = OsType.All,
            FuzzerIsolation fuzzerIsolation = FuzzerIsolation.Process,
            ExplorationMode explorationMode = ExplorationMode.Sili,
            int randomSeed = 0,
            uint stepsLimit = 0)
        {
            if (expectedCoverage < 0)
                _expectedCoverage = null;
            else if (expectedCoverage > 100)
                _expectedCoverage = 100;
            else _expectedCoverage = expectedCoverage;
            _recThresholdForTest = recThresholdForTest;
            _timeout = timeout;
            _solverTimeout = solverTimeout;
            _releaseBranches = releaseBranches;
            _strat = strat;
            _coverageZone = coverageZone;
            _testsCheckerMode = testsCheckerMode;
            _checkAttributes = checkAttributes;
            _hasExternMocking = hasExternMocking;
            _supportedOs = supportedOs;
            _fuzzerIsolation = fuzzerIsolation;
            _explorationMode = explorationMode;
            _randomSeed = randomSeed;
            _stepsLimit = stepsLimit;
        }

        public TestCommand Wrap(TestCommand command)
        {
            return new TestSvmCommand(
                command,
                _expectedCoverage,
                _recThresholdForTest,
                _timeout,
                _solverTimeout,
                _releaseBranches,
                _strat,
                _coverageZone,
                _testsCheckerMode,
                _checkAttributes,
                _hasExternMocking,
                _supportedOs,
                _fuzzerIsolation,
                _explorationMode,
                _randomSeed,
                _stepsLimit
            );
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            private readonly int? _expectedCoverage;
            private readonly uint _recThresholdForTest;
            private readonly int _timeout;
            private readonly int _solverTimeout;
            private readonly bool _releaseBranches;
            private readonly searchMode _searchStrat;
            private readonly coverageZone _coverageZone;

            private readonly SearchStrategy _baseSearchStrat;
            private readonly CoverageZone _baseCoverageZone;
            private readonly bool _renderTests;
            private readonly bool _checkAttributes;
            private readonly bool _hasExternMocking;
            private readonly OsType _supportedOs;
            private readonly fuzzerIsolation _fuzzerIsolation;
            private readonly ExplorationMode _explorationMode;
            private readonly int _randomSeed;
            private readonly uint _stepsLimit;

            private class Reporter: IReporter
            {
                private readonly UnitTests _unitTests;

                public Reporter(UnitTests unitTests)
                {
                    _unitTests = unitTests;
                }

                public void ReportFinished(UnitTest unitTest) => _unitTests.GenerateTest(unitTest);
                public void ReportException(UnitTest unitTest) => _unitTests.GenerateError(unitTest);
                public void ReportIIE(InsufficientInformationException iie) {}
                public void ReportInternalFail(Method method, Exception exn) => ExceptionDispatchInfo.Capture(exn).Throw();
                public void ReportCrash(Exception exn) => ExceptionDispatchInfo.Capture(exn).Throw();
            }

            public TestSvmCommand(
                TestCommand innerCommand,
                int? expectedCoverage,
                uint recThresholdForTest,
                int timeout,
                int solverTimeout,
                bool releaseBranches,
                SearchStrategy strat,
                CoverageZone coverageZone,
                TestsCheckerMode testsCheckerMode,
                bool checkAttributes,
                bool hasExternMocking,
                OsType supportedOs,
                FuzzerIsolation fuzzerIsolation,
                ExplorationMode explorationMode,
                int randomSeed,
                uint stepsLimit) : base(innerCommand)
            {
                _baseCoverageZone = coverageZone;
                _baseSearchStrat = TestContext.Parameters[SearchStrategyParameterName] == null ?
                    strat : (SearchStrategy)Enum.Parse(typeof(SearchStrategy), TestContext.Parameters[SearchStrategyParameterName], true);

                _expectedCoverage = TestContext.Parameters[ExpectedCoverageParameterName] == null ?
                    expectedCoverage : int.Parse(TestContext.Parameters[ExpectedCoverageParameterName]);

                _recThresholdForTest = recThresholdForTest;

                _timeout = TestContext.Parameters[TimeoutParameterName] == null ?
                    timeout : int.Parse(TestContext.Parameters[TimeoutParameterName]);

                _solverTimeout = TestContext.Parameters[SolverTimeoutParameterName] == null ?
                    solverTimeout : int.Parse(TestContext.Parameters[SolverTimeoutParameterName]);

                _releaseBranches = TestContext.Parameters[ReleaseBranchesParameterName] == null ?
                    releaseBranches : bool.Parse(TestContext.Parameters[ReleaseBranchesParameterName]);

                _searchStrat = _baseSearchStrat switch
                {
                    SearchStrategy.DFS => searchMode.DFSMode,
                    SearchStrategy.BFS => searchMode.BFSMode,
                    SearchStrategy.ShortestDistance => searchMode.ShortestDistanceBasedMode,
                    SearchStrategy.RandomShortestDistance => searchMode.RandomShortestDistanceBasedMode,
                    SearchStrategy.ContributedCoverage => searchMode.ContributedCoverageMode,
                    SearchStrategy.ExecutionTree => searchMode.ExecutionTreeMode,
                    SearchStrategy.ExecutionTreeContributedCoverage => searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1),
                    SearchStrategy.Interleaved => searchMode.NewInterleavedMode(searchMode.ShortestDistanceBasedMode, 1, searchMode.ContributedCoverageMode, 9),
                    _ => throw new ArgumentOutOfRangeException(nameof(strat), strat, null)
                };

                _coverageZone = coverageZone switch
                {
                    CoverageZone.Method => Explorer.coverageZone.MethodZone,
                    CoverageZone.Class => Explorer.coverageZone.ClassZone,
                    CoverageZone.Module => Explorer.coverageZone.ModuleZone,
                    _ => throw new ArgumentOutOfRangeException(nameof(coverageZone), coverageZone, null)
                };

                _renderTests = testsCheckerMode == TestsCheckerMode.RenderAndRun;
                _checkAttributes = checkAttributes;
                _hasExternMocking = hasExternMocking;
                _supportedOs = supportedOs;
                _fuzzerIsolation = fuzzerIsolation switch
                {
                    FuzzerIsolation.Process => Explorer.fuzzerIsolation.Process
                };
                _explorationMode = explorationMode;
                _randomSeed = randomSeed;
                _stepsLimit = stepsLimit;
            }

            private TestResult IgnoreTest(TestExecutionContext context)
            {
                context.CurrentResult.SetResult(ResultState.Skipped);
                return context.CurrentResult;
            }

            private bool RenderTests(
                TestExecutionContext context,
                TestStatistics statistics,
                MethodInfo exploredMethodInfo,
                IStatisticsReporter reporter,
                DirectoryInfo testsDir)
            {
                if (!_renderTests || _hasExternMocking) return true;

                var tests = testsDir.EnumerateFiles("*.vst");
                TestContext.Out.WriteLine("Starting tests renderer...");
                try
                {
                    Renderer.Render(tests, true, false, exploredMethodInfo.DeclaringType);
                }
                catch (UnexpectedExternCallException)
                {
                    // TODO: support rendering for extern mocks
                }
                catch (Exception e)
                {
                    context.CurrentResult.SetResult(ResultState.Failure,
                        $"Test renderer failed: {e}");
                    reporter?.Report(statistics with { Exception = e });
                    return false;
                }

                return true;
            }

            // For fixing Rider bug,
            // evaluating state in "Debugger -> Thread & Variables" throws System.ArgumentException
            private void EnforceLoadCore()
            {
                var x = API.Memory.EmptyIsolatedState();
                Logger.writeLine($"Enforcing load VSharp.Core {x}");
            }

            private void CheckCoverage(
                TestExecutionContext context,
                TestStatistics statistics,
                MethodInfo exploredMethodInfo,
                IStatisticsReporter reporter,
                DirectoryInfo testsDir
            )
            {
                // NOTE: to disable coverage check TestResultsChecker's expected coverage should be null
                //       to enable coverage check use _expectedCoverage
                bool success;
                var resultMessage = string.Empty;
                uint? actualCoverage;
                if (_expectedCoverage is {} expectedCoverage)
                {
                    TestContext.Out.WriteLine("Starting coverage tool...");
                    success =
                        TestResultChecker.Check(
                            testsDir,
                            exploredMethodInfo,
                            expectedCoverage,
                            out var coverage,
                            out resultMessage);
                    actualCoverage = (uint?)coverage;
                }
                else
                {
                    TestContext.Out.WriteLine("Starting tests checker...");
                    success = TestResultChecker.Check(testsDir);
                    actualCoverage = null;
                }
                if (success)
                    context.CurrentResult.SetResult(ResultState.Success);
                else
                    context.CurrentResult.SetResult(ResultState.Failure, resultMessage);
                reporter?.Report(statistics with { Coverage = actualCoverage });
            }

            private TestResult Explore(TestExecutionContext context)
            {
                EnforceLoadCore();
                if (_hasExternMocking && !ExternMocker.ExtMocksSupported)
                    return IgnoreTest(context);

                switch (_supportedOs)
                {
                    case OsType.Windows:
                        if (!OperatingSystem.IsWindows())
                            return IgnoreTest(context);
                        break;
                    case OsType.Unix:
                        if (!OperatingSystem.IsLinux() && !OperatingSystem.IsMacOS())
                            return IgnoreTest(context);
                        break;
                    case OsType.All:
                        break;
                }

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

                var fuzzingEnabled = _explorationMode switch
                {
                    ExplorationMode.Sili => false,
                    ExplorationMode.Interleaving => true,
                    ExplorationMode.Fuzzer => true,
                };

                var siliEnabled = _explorationMode switch
                {
                    ExplorationMode.Sili => true,
                    ExplorationMode.Interleaving => true,
                    ExplorationMode.Fuzzer => false,
                };

                var originMethodInfo = innerCommand.Test.Method.MethodInfo;
                var exploredMethodInfo = (MethodInfo) AssemblyManager.NormalizeMethod(originMethodInfo);
                var stats = new TestStatistics(
                    exploredMethodInfo,
                    _releaseBranches,
                    _timeout,
                    _baseSearchStrat,
                    _baseCoverageZone
                );

                try
                {
                    var unitTests = new UnitTests(Directory.GetCurrentDirectory());

                    _options = new SVMOptions(
                        explorationMode: explorationMode.NewTestCoverageMode(_coverageZone, _searchStrat),
                        recThreshold: _recThresholdForTest,
                        solverTimeout: _solverTimeout,
                        visualize: false,
                        releaseBranches: _releaseBranches,
                        maxBufferSize: 128,
                        prettyChars: true,
                        checkAttributes: _checkAttributes,
                        stopOnCoverageAchieved: _expectedCoverage ?? -1,
                        randomSeed: _randomSeed,
                        stepsLimit: _stepsLimit
                    );

                    var fuzzerOptions = new FuzzerOptions(
                        fuzzerIsolation.Process,
                        _coverageZone
                    );

                    var explorationModeOptions = _explorationMode switch
                    {
                        ExplorationMode.Sili => Explorer.explorationModeOptions.NewSVM(_options),
                        ExplorationMode.Interleaving => Explorer.explorationModeOptions.NewCombined(_options, fuzzerOptions),
                        ExplorationMode.Fuzzer => Explorer.explorationModeOptions.NewFuzzing(fuzzerOptions),
                        _ => throw new ArgumentOutOfRangeException($"StartExploration: unexpected exploration mode {_explorationMode}")
                    };

                    var explorationOptions = new ExplorationOptions(
                        timeout: _timeout == -1 ? TimeSpanBuilders.Infinite : TimeSpanBuilders.FromSeconds(_timeout),
                        outputDirectory: unitTests.TestDirectory,
                        explorationModeOptions: explorationModeOptions
                    );

                    using var explorer = new Explorer.Explorer(explorationOptions, new Reporter(unitTests));
                    explorer.StartExploration(
                        new [] { exploredMethodInfo },
                        global::System.Array.Empty<Tuple<MethodBase, EntryPointConfiguration>>()
                    );

                    if (siliEnabled && unitTests.UnitTestsCount == 0 && unitTests.ErrorsCount == 0 && explorer.Statistics.IncompleteStates.Count == 0)
                    {
                        throw new Exception("No states were obtained! Most probably this is bug.");
                    }

                    explorer.Statistics.PrintDebugStatistics(TestContext.Out);
                    TestContext.Out.WriteLine($"Test results written to {unitTests.TestDirectory.FullName}");
                    unitTests.WriteReport(explorer.Statistics.PrintDebugStatistics);

                    stats = stats with
                    {
                        SvmStatisticsDump = explorer.Statistics.DumpStatistics(),
                        TestsGenerated = unitTests.UnitTestsCount,
                        TestsOutputDirectory = unitTests.TestDirectory.FullName
                    };

                    if (fuzzingEnabled || siliEnabled && (unitTests.UnitTestsCount != 0 || unitTests.ErrorsCount != 0))
                    {
                        var testsDir = unitTests.TestDirectory;
                        var rendered = RenderTests(context, stats, exploredMethodInfo, reporter, testsDir);
                        if (!rendered) return context.CurrentResult;
                        CheckCoverage(context, stats, exploredMethodInfo, reporter, testsDir);
                    }
                    else
                    {
                        context.CurrentResult.SetResult(
                            _expectedCoverage > 0
                                ? ResultState.Failure
                                : ResultState.Success
                        );
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
