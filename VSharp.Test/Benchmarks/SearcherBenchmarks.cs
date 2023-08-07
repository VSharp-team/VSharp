using System.Collections.Generic;
using NUnit.Framework;

namespace VSharp.Test.Benchmarks;

[TestFixture, Explicit]
public class ExecutionTreeSearcherBenchmarks
{
    [Test]
    [TestCaseSource(typeof(BizHawkTargets), nameof(BizHawkTargets.LR35902))]
    //[TestCaseSource(typeof(LifetimesTargets), nameof(LifetimesTargets.Collections))]
    //[TestCaseSource(typeof(LifetimesTargets), nameof(LifetimesTargets.Util))]
    public void SearchersEvaluation(BenchmarkTarget target)
    {
        var stepsLimit = 10000u;
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
        var stats = new List<(string, Statistics)>
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
            ("Execution tree (2)", executionTreeResults2),
        };
        Benchmarks.PrintStatisticsComparison(stats);
    }
}
