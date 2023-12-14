using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using VSharp.Interpreter.IL;
using VSharp.Explorer;

namespace VSharp.Test.Benchmarks;

[TestFixture, Explicit]
public class SearcherBenchmarks
{
    [Test]
    [TestCaseSource(typeof(VSharpTargets), nameof(VSharpTargets.LoanExam))]
    public void SearchersEvaluation(BenchmarkTarget target)
    {
        var executionTreeContributedCoverageMode = searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1);
        var stepsLimit = 40000u;
        TestContext.Progress.WriteLine("Running BFS...");
        var bfsResults = Benchmarks.Run(target, searchMode.BFSMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running DFS...");
        var dfsResults = Benchmarks.Run(target, searchMode.DFSMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Contributed coverage...");
        var contributedCoverageResults = Benchmarks.Run(target, searchMode.ContributedCoverageMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Shortest distance...");
        var shortestDistanceResults = Benchmarks.Run(target, searchMode.ShortestDistanceBasedMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Shortest distance (random)...");
        var randomShortestDistanceResults = Benchmarks.Run(target, searchMode.RandomShortestDistanceBasedMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Shortest distance (random, 2)...");
        var randomShortestDistanceResults2 = Benchmarks.Run(target, searchMode.RandomShortestDistanceBasedMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Execution tree interleaved...");
        var executionTreeResultsInterleaved = Benchmarks.Run(target, executionTreeContributedCoverageMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Execution tree interleaved (2)...");
        var executionTreeResultsInterleaved2 = Benchmarks.Run(target, executionTreeContributedCoverageMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Execution tree...");
        var executionTreeResults = Benchmarks.Run(target, executionTreeContributedCoverageMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0, calculateCoverage: true);
        TestContext.Progress.WriteLine("Running Execution tree (2)...");
        var executionTreeResults2 = Benchmarks.Run(target, executionTreeContributedCoverageMode, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100, calculateCoverage: true);
        var stats = new List<(string, BenchmarkResult)>
        {
            ("BFS", bfsResults),
            ("DFS", dfsResults),
            ("Contributed coverage", contributedCoverageResults),
            ("Shortest distance", shortestDistanceResults),
            ("Random shortest distance (1)", randomShortestDistanceResults),
            ("Random shortest distance (2)", randomShortestDistanceResults2),
            ("Execution tree int (1)", executionTreeResultsInterleaved),
            ("Execution tree int (2)", executionTreeResultsInterleaved2),
            ("Execution tree (1)", executionTreeResults),
            ("Execution tree (2)", executionTreeResults2)
        };
        Benchmarks.PrintStatisticsComparison(stats);
    }

    [Test]
    public void BizHawkLR35902IsCoveredWithExecutionTreeInterleavedSearcher([Values(0, 42, 73)] int randomSeed)
    {
        var executionTreeContributedCoverageMode = searchMode.NewInterleavedMode(searchMode.ExecutionTreeMode, 1, searchMode.ContributedCoverageMode, 1);
        var target = BizHawkTargets.LR35902().Single(t => t.Method.Name.Contains("ExecuteOne"));
        var stepsLimit = 30000u;
        var result = Benchmarks.Run(
            target,
            executionTreeContributedCoverageMode,
            stepsLimit: stepsLimit,
            releaseBranches: false,
            randomSeed: randomSeed,
            renderAndBuildTests: true,
            calculateCoverage: true
        );
        Assert.IsTrue(result.IsSuccessful);
        TestContext.Out.WriteLine($"Coverage: {result.Coverage}%");
        Assert.That(result.Coverage, Is.GreaterThan(90));
    }
}
