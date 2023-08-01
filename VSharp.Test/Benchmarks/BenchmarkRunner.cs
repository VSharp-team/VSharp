#nullable enable
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using ConsoleTables;
using NUnit.Framework;

namespace VSharp.Test.Benchmarks;

internal static class BenchmarkRunner
{
    public static Statistics RunBenchmark(
        BenchmarkTarget target,
        VSharp.SearchStrategy searchStrategy,
        int timeoutS = -1,
        uint stepsLimit = 0,
        bool releaseBranches = true,
        int randomSeed = -1)
    {
        var options = new VSharpOptions(
            Timeout: timeoutS,
            SearchStrategy: searchStrategy,
            ReleaseBranches: releaseBranches,
            Verbosity: Verbosity.Warning,
            RandomSeed: randomSeed,
            StepsLimit: stepsLimit);
        Statistics statistics;

        if (target.Method is not null)
        {
            TestGenerator.CoverAndRun(target.Method, out statistics, options);
        }
        else if (target.Types.Count == 1)
        {
            TestGenerator.CoverAndRun(target.Types.Single(), out statistics, options);
        }
        else if (target.Types.Count > 1)
        {
            TestGenerator.CoverAndRun(target.Types, out statistics, options);
        }
        else
        {
            TestGenerator.CoverAndRun(target.Assembly, out statistics, options);
        }

        return statistics;
    }

    public static int CompareCoverageSteps(Statistics one, Statistics another)
    {
        var lastTestOfOne = one.GeneratedTestInfos.Last();
        var lastTestOfAnother = another.GeneratedTestInfos.Last();

        if (lastTestOfOne.Coverage < lastTestOfAnother.Coverage)
        {
            var closestToOne = another.GeneratedTestInfos.First(t => t.Coverage >= lastTestOfOne.Coverage);
            return lastTestOfOne.StepsCount.CompareTo(closestToOne.StepsCount);
        }
        
        var closestToAnother = one.GeneratedTestInfos.First(t => t.Coverage >= lastTestOfAnother.Coverage);
        return closestToAnother.StepsCount.CompareTo(lastTestOfAnother.StepsCount);
    }

    public static void PrintStatisticsComparison(List<(string Title, Statistics Stats)> statistics)
    {
        var infos = statistics.SelectMany(ts => ts.Stats.GeneratedTestInfos.Where(i => !i.IsError).Select(ti => (ts.Title, ti)))
            .OrderBy(ti => ti.Item2.StepsCount);

        var header = new List<string> { "" };
        header.AddRange(statistics.Select(ts => ts.Title));
        var totalStatsTable = new ConsoleTable(header.ToArray());
        
        var timeRow = new List<string> { "Elapsed time" };
        timeRow.AddRange(statistics.Select(ts => ts.Stats.TestGenerationTime.ToString()));
        totalStatsTable.AddRow(timeRow.ToArray());
        
        var stepsRow = new List<string> { "Steps count" };
        stepsRow.AddRange(statistics.Select(ts => ts.Stats.StepsCount.ToString()));
        totalStatsTable.AddRow(stepsRow.ToArray());
        
        var testsCountRow = new List<string> { "Tests generated" };
        testsCountRow.AddRange(statistics.Select(ts => ts.Stats.TestsCount.ToString()));
        totalStatsTable.AddRow(testsCountRow.ToArray());
        
        var errorsCountRow = new List<string> { "Errors found" };
        errorsCountRow.AddRange(statistics.Select(ts => ts.Stats.ErrorsCount.ToString()));
        totalStatsTable.AddRow(errorsCountRow.ToArray());

        totalStatsTable.Write();
        
        var testsStatsTableHeader = new List<string> { "Steps count" };
        testsStatsTableHeader.AddRange(statistics.Select(ts => ts.Title));
        var testsStatsTable = new ConsoleTable(testsStatsTableHeader.ToArray());
        
        foreach (var (title, info) in infos)
        {
            var row = new List<string> { info.StepsCount.ToString() };
            foreach (var (columnHeader, _) in statistics)
            {
                row.Add(title == columnHeader ? info.Coverage.ToString("0.##") : "");
            }

            testsStatsTable.AddRow(row.ToArray());
        }
        
        testsStatsTable.Write();
    }

    public static Assembly LoadBenchmarkAssembly(string suite, string dllFileName)
    {
        var dllPath = TestContext.Parameters["BenchmarkDllsPath"];
        if (dllPath is null)
        {
            throw new Exception("Cannot read dll directory path from test context parameters");
        }

        var assemblyPath = Path.Combine(dllPath, suite, $"{dllFileName}.dll");
        return AssemblyManager.LoadFromAssemblyPath(assemblyPath);
    }
}
