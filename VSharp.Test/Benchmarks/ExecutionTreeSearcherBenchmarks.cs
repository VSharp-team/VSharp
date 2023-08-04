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
    public void ExecutionTreeSearcherVsBfs(BenchmarkTarget target)
    {
        var stepsLimit = 10000u;
        /*TestContext.Out.WriteLine("Running BFS...");
        var bfsResults = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.BFS, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0);
        TestContext.Out.WriteLine("Running DFS...");
        var dfsResults = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.DFS, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0);
        TestContext.Out.WriteLine("Running Contributed coverage...");
        var contributedCoverageResults = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ContributedCoverage, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0);
        TestContext.Out.WriteLine("Running Shortest distance...");
        var shortestDistanceResults = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ShortestDistance, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0);
        TestContext.Out.WriteLine("Running Shortest distance (random)...");
        var randomShortestDistanceResults = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.RandomShortestDistance, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0);
        TestContext.Out.WriteLine("Running Shortest distance (random, 2)...");
        var randomShortestDistanceResults2 = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.RandomShortestDistance, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100);
        TestContext.Out.WriteLine("Running Execution tree interleaved...");*/
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, out var executionTreeResultsInterleaved, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        /*TestContext.Out.WriteLine("Running Execution tree interleaved (2)...");
        var executionTreeResultsInterleaved2 = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100);
        TestContext.Out.WriteLine("Running Execution tree...");
        var executionTreeResults = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTree, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0);
        TestContext.Out.WriteLine("Running Execution tree (2)...");
        MeasureProfiler.StartCollectingData();
        var executionTreeResults2 = Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTree, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100);
        MeasureProfiler.SaveData();*/
        var stats = new List<(string, Statistics)>
        {
            /*("BFS", bfsResults),
            ("DFS", dfsResults),
            ("Contributed coverage", contributedCoverageResults),
            ("Shortest distance", shortestDistanceResults),
            ("Random shortest distance (1)", randomShortestDistanceResults),
            ("Random shortest distance (2)", randomShortestDistanceResults2),*/
            ("Execution tree int (1)", executionTreeResultsInterleaved),
            /*("Execution tree int (2)", executionTreeResultsInterleaved2),
            ("Execution tree (1)", executionTreeResults),
            ("Execution tree (2)", executionTreeResults2),*/
        };
        Benchmarks.PrintStatisticsComparison(stats);
    }
    
    [Test, TestCaseSource(typeof(BizHawkTargets), nameof(BizHawkTargets.LR35902))]
    public void ExecutionTreeSearcherVsBfs2(BenchmarkTarget target)
    {
        var stepsLimit = 10000u;
        TestContext.Out.WriteLine("Running Execution tree interleaved...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, out var executionTreeResultsInterleaved, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 0));
        TestContext.Out.WriteLine("Running Execution tree interleaved (2)...");
        Assert.True(Benchmarks.RunBenchmark(target, VSharp.SearchStrategy.ExecutionTreeContributedCoverage, out var executionTreeResultsInterleaved2, stepsLimit: stepsLimit, releaseBranches: false, randomSeed: 100));
        var stats = new List<(string, Statistics)>
        {
            ("Execution tree int (1)", executionTreeResultsInterleaved),
            ("Execution tree int (2)", executionTreeResultsInterleaved2)
        };
        Benchmarks.PrintStatisticsComparison(stats);
    }
}
