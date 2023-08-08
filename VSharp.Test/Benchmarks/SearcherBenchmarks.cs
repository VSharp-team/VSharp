using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;

namespace VSharp.Test.Benchmarks;

[TestFixture, Explicit]
public class SearcherBenchmarks
{
    [Test]
    [TestCaseSource(typeof(VSharpTargets), nameof(VSharpTargets.LoanExam))]
    public void SearchersEvaluation(BenchmarkTarget target)
    {
        var stepsLimit = 40000u;
        TestContext.Out.WriteLine("Running BFS...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.BFS, out var bfsResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running DFS...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.DFS, out var dfsResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Contributed coverage...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ContributedCoverage, out var contributedCoverageResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Shortest distance...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ShortestDistance, out var shortestDistanceResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Shortest distance (random)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.RandomShortestDistance, out var randomShortestDistanceResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Shortest distance (random, 2)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.RandomShortestDistance, out var randomShortestDistanceResults2, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100));
        TestContext.Out.WriteLine("Running Execution tree interleaved...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, out var executionTreeResultsInterleaved, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Execution tree interleaved (2)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, out var executionTreeResultsInterleaved2, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100));
        TestContext.Out.WriteLine("Running Execution tree...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTree, out var executionTreeResults, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Execution tree (2)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTree, out var executionTreeResults2, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100));
        var stats = new List<(string, Statistics, int)>
        {
            ("BFS", bfsResults, Benchmarks.GetMethodCoverage(target, bfsResults)),
            ("DFS", dfsResults, Benchmarks.GetMethodCoverage(target, dfsResults)),
            ("Contributed coverage", contributedCoverageResults, Benchmarks.GetMethodCoverage(target, contributedCoverageResults)),
            ("Shortest distance", shortestDistanceResults, Benchmarks.GetMethodCoverage(target, shortestDistanceResults)),
            ("Random shortest distance (1)", randomShortestDistanceResults, Benchmarks.GetMethodCoverage(target, randomShortestDistanceResults)),
            ("Random shortest distance (2)", randomShortestDistanceResults2, Benchmarks.GetMethodCoverage(target, randomShortestDistanceResults2)),
            ("Execution tree int (1)", executionTreeResultsInterleaved, Benchmarks.GetMethodCoverage(target, executionTreeResultsInterleaved)),
            ("Execution tree int (2)", executionTreeResultsInterleaved2, Benchmarks.GetMethodCoverage(target, executionTreeResultsInterleaved2)),
            ("Execution tree (1)", executionTreeResults, Benchmarks.GetMethodCoverage(target, executionTreeResults)),
            ("Execution tree (2)", executionTreeResults2, Benchmarks.GetMethodCoverage(target, executionTreeResults2)),
        };
        Benchmarks.PrintStatisticsComparison(stats);
    }

    [Test]
    public void BizHawkLR35902IsCoveredWithExecutionTreeInterleavedSearcher([Values(0, 42, 73)] int randomSeed)
    {
        var target = BizHawkTargets.LR35902().Single(t => t.Method.Name.Contains("ExecuteOne"));
        var stepsLimit = 30000u;
        Assert.True(
            Benchmarks.RunBenchmark(
                target,
                VSharp.SearchStrategy.ExecutionTreeContributedCoverage,
                out var statistics,
                stepsLimit: stepsLimit,
                releaseBranches: false,
                randomSeed: randomSeed,
                renderAndBuildTests: true
            )
        );
        var coverage = Benchmarks.GetMethodCoverage(target, statistics);
        TestContext.Out.WriteLine($"Coverage: {coverage}%");
        Assert.That(coverage, Is.GreaterThan(90));
    }
}
