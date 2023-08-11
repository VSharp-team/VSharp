#nullable enable
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using ConsoleTables;
using NUnit.Framework;

namespace VSharp.Test.Benchmarks;

internal static class Benchmarks
{
    private static readonly string TestRunnerPath = typeof(TestRunner.TestRunner).Assembly.Location;
    private static readonly DirectoryInfo RenderedTestsDirectory = new(Path.Combine(Directory.GetCurrentDirectory(), "RenderedTests"));

    private static bool TryBuildGeneratedTests()
    {

        var testsDir = RenderedTestsDirectory.GetDirectories("*.Tests").Single();
        var info = new ProcessStartInfo
        {
            WorkingDirectory = testsDir.FullName,
            FileName = "dotnet",
            Arguments = "build"
        };
        var process = new Process();
        process.StartInfo = info;
        process.Start();
        process.WaitForExit();
        return process.ExitCode == 0;
    }

    public static bool RunBenchmark(
        BenchmarkTarget target,
        VSharp.SearchStrategy searchStrategy,
        out Statistics statistics,
        int timeoutS = -1,
        uint stepsLimit = 0,
        bool releaseBranches = true,
        int randomSeed = -1,
        bool renderAndBuildTests = false)
    {
        if (RenderedTestsDirectory.Exists)
        {
            Directory.Delete(RenderedTestsDirectory.FullName, true);
        }

        Directory.CreateDirectory(RenderedTestsDirectory.FullName);

        var options = new VSharpOptions(
            Timeout: timeoutS,
            SearchStrategy: searchStrategy,
            ReleaseBranches: releaseBranches,
            Verbosity: Verbosity.Warning,
            RandomSeed: randomSeed,
            StepsLimit: stepsLimit,
            RenderTests: renderAndBuildTests,
            RenderedTestsDirectory: RenderedTestsDirectory.FullName);

        bool testRunnerResult;

        if (target.Method is not null)
        {
            testRunnerResult = TestGenerator.CoverAndRun(target.Method, out statistics, options);
        }
        else if (target.Types.Count == 1)
        {
            testRunnerResult = TestGenerator.CoverAndRun(target.Types.Single(), out statistics, options);
        }
        else if (target.Types.Count > 1)
        {
            testRunnerResult = TestGenerator.CoverAndRun(target.Types, out statistics, options);
        }
        else
        {
            testRunnerResult = TestGenerator.CoverAndRun(target.Assembly, out statistics, options);
        }

        var renderResult = true;
        if (renderAndBuildTests)
        {
            renderResult = TryBuildGeneratedTests();
        }

        return statistics is { TestsCount: 0u, ErrorsCount: 0u } || (testRunnerResult && renderResult);
    }

    public static void PrintStatisticsComparison(List<(string Title, Statistics Stats, int coverage)> statistics)
    {
        var infos = statistics.SelectMany(ts =>
            ts.Stats.GeneratedTestInfos
                .Where(i => !i.IsError)
                .Select(ti => (ts.Title, ti)))
                .OrderBy(ti => ti.Item2.StepsCount
        );

        var header = new List<string> { "" };
        header.AddRange(statistics.Select(s => s.Title));
        var totalStatsTable = new ConsoleTable(header.ToArray());

        var timeRow = new List<string> { "Elapsed time" };
        timeRow.AddRange(statistics.Select(s => s.Stats.TestGenerationTime.ToString()));
        totalStatsTable.AddRow(timeRow.ToArray());

        var stepsRow = new List<string> { "Steps count" };
        stepsRow.AddRange(statistics.Select(s => s.Stats.StepsCount.ToString()));
        totalStatsTable.AddRow(stepsRow.ToArray());

        var testsCountRow = new List<string> { "Tests generated" };
        testsCountRow.AddRange(statistics.Select(s => s.Stats.TestsCount.ToString()));
        totalStatsTable.AddRow(testsCountRow.ToArray());

        var errorsCountRow = new List<string> { "Errors found" };
        errorsCountRow.AddRange(statistics.Select(s => s.Stats.ErrorsCount.ToString()));
        totalStatsTable.AddRow(errorsCountRow.ToArray());

        var coverageRow = new List<string> { "Total coverage (with tool)" };
        coverageRow.AddRange(statistics.Select(s => $"{s.coverage}%"));
        totalStatsTable.AddRow(coverageRow.ToArray());

        totalStatsTable.Write();

        var testsStatsTableHeader = new List<string> { "Steps count" };
        testsStatsTableHeader.AddRange(statistics.Select(ts => ts.Title));
        var testsStatsTable = new ConsoleTable(testsStatsTableHeader.ToArray());

        foreach (var (title, info) in infos)
        {
            var row = new List<string> { info.StepsCount.ToString() };
            foreach (var (columnHeader, _, _) in statistics)
            {
                row.Add(title == columnHeader ? info.Coverage.ToString("0.##") : "");
            }

            testsStatsTable.AddRow(row.ToArray());
        }

        testsStatsTable.Write();
    }

    public static int GetMethodCoverage(BenchmarkTarget target, Statistics result)
    {
        if (target.Method is null)
        {
            throw new Exception("Cannot get coverage of BenchmarkTarget without single method");
        }

        var runnerWithArgs = $"{TestRunnerPath} {result.OutputDir.FullName}";
        return CoverageRunner.CoverageRunner.RunAndGetCoverage(runnerWithArgs, result.OutputDir, target.Method);
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
