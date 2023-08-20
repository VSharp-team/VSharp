#nullable enable
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using ConsoleTables;
using NUnit.Framework;
using VSharp.Interpreter.IL;
using VSharp.TestRenderer;

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

    public static BenchmarkResult Run(
        BenchmarkTarget target,
        searchMode searchStrategy,
        int timeoutS = -1,
        uint stepsLimit = 0,
        bool releaseBranches = true,
        int randomSeed = -1,
        bool renderAndBuildTests = false,
        bool calculateCoverage = false)
    {
        if (target.Method is null)
        {
            throw new NotImplementedException("Running non single method benchmarks is not implemented yet");
        }

        if (RenderedTestsDirectory.Exists)
        {
            Directory.Delete(RenderedTestsDirectory.FullName, true);
        }

        Directory.CreateDirectory(RenderedTestsDirectory.FullName);

        var exploredMethodInfo = AssemblyManager.NormalizeMethod(target.Method);

        Logger.configureWriter(TestContext.Progress);
        Logger.currentLogLevel = Logger.Warning;

        var unitTests = new UnitTests(Directory.GetCurrentDirectory());
        var options = new SiliOptions(
            explorationMode: explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchStrategy),
            outputDirectory: unitTests.TestDirectory,
            recThreshold: 1,
            timeout: timeoutS,
            solverTimeout: -1,
            visualize: false,
            releaseBranches: releaseBranches,
            maxBufferSize: 128,
            checkAttributes: false,
            stopOnCoverageAchieved: -1,
            randomSeed: randomSeed,
            stepsLimit: stepsLimit
        );
        using var explorer = new SILI(options);

        explorer.Interpret(
            new[] { exploredMethodInfo },
            new Tuple<MethodBase, string[]>[] { },
            unitTests.GenerateTest,
            unitTests.GenerateError,
            (e) => TestContext.Progress.WriteLine($"[II] {e.Message}"),
            (m, e) => TestContext.Progress.WriteLine($"[ERROR] {m.Name}: {e}"),
            (e) => TestContext.Progress.WriteLine($"[CRASH] {e}")
        );

        var result = new BenchmarkResult(false, explorer.Statistics, unitTests, target);

        explorer.Statistics.PrintDebugStatistics(TestContext.Progress);
        TestContext.Progress.WriteLine($"Test results written to {unitTests.TestDirectory.FullName}");

        TestContext.Progress.WriteLine($"Generated tests count: {unitTests.UnitTestsCount}");
        TestContext.Progress.WriteLine($"Found errors count: {unitTests.ErrorsCount}");

        if (unitTests is { UnitTestsCount: 0, ErrorsCount: 0 })
        {
            return result with { IsSuccessful = true };
        }

        var testsDir = unitTests.TestDirectory;
        if (renderAndBuildTests)
        {
            var tests = testsDir.EnumerateFiles("*.vst");
            TestContext.Progress.WriteLine("Starting tests renderer...");
            try
            {
                Renderer.Render(tests, true, false, exploredMethodInfo.DeclaringType, outputDir: RenderedTestsDirectory);
            }
            catch (UnexpectedExternCallException)
            {
                // TODO: support rendering for extern mocks
            }
            catch (Exception e)
            {
                TestContext.Progress.WriteLine($"[RENDER ERROR]: {e}");
                return result;
            }

            if (!TryBuildGeneratedTests())
            {
                TestContext.Progress.WriteLine($"[BUILD]: Cannot build generated tests");
                return result;
            }
        }

        if (!TestRunner.TestRunner.ReproduceTests(unitTests.TestDirectory))
        {
            return result;
        }

        if (calculateCoverage)
        {
            return result with { IsSuccessful = true, Coverage = GetMethodCoverage(result) };
        }

        return result with { IsSuccessful = true };
    }

    public static void PrintStatisticsComparison(List<(string Title, BenchmarkResult Results)> titleToResults)
    {
        var infos = titleToResults.SelectMany(tr =>
                tr.Results.Statistics.GeneratedTestInfos
                    .Where(ti => !ti.isError)
                    .Select(ti => (tr.Title, ti)))
            .OrderBy(tti => tti.ti.coverage);

        var header = new List<string> { "" };
        header.AddRange(titleToResults.Select(s => s.Title));
        var totalStatsTable = new ConsoleTable(header.ToArray());

        var timeRow = new List<string> { "Elapsed time" };
        timeRow.AddRange(titleToResults.Select(tr => tr.Results.Statistics.CurrentExplorationTime.ToString()));
        totalStatsTable.AddRow(timeRow.ToArray());

        var stepsRow = new List<string> { "Steps count" };
        stepsRow.AddRange(titleToResults.Select(tr => tr.Results.Statistics.StepsCount.ToString()));
        totalStatsTable.AddRow(stepsRow.ToArray());

        var testsCountRow = new List<string> { "Tests generated" };
        testsCountRow.AddRange(titleToResults.Select(tr => tr.Results.Tests.UnitTestsCount.ToString()));
        totalStatsTable.AddRow(testsCountRow.ToArray());

        var errorsCountRow = new List<string> { "Errors found" };
        errorsCountRow.AddRange(titleToResults.Select(tr => tr.Results.Tests.ErrorsCount.ToString()));
        totalStatsTable.AddRow(errorsCountRow.ToArray());

        var coverageRow = new List<string> { "Total coverage (with tool)" };
        coverageRow.AddRange(titleToResults.Select(tr => $"{tr.Results.Coverage}%"));
        totalStatsTable.AddRow(coverageRow.ToArray());

        totalStatsTable.Write();

        var testsStatsTableHeader = new List<string> { "Steps count" };
        testsStatsTableHeader.AddRange(titleToResults.Select(tr => tr.Title));
        var testsStatsTable = new ConsoleTable(testsStatsTableHeader.ToArray());

        foreach (var (title, info) in infos)
        {
            var row = new List<string> { info.stepsCount.ToString() };
            foreach (var (columnHeader, _) in titleToResults)
            {
                row.Add(title == columnHeader ? info.coverage.ToString("0.##") : "");
            }

            testsStatsTable.AddRow(row.ToArray());
        }

        testsStatsTable.Write();
    }

    private static int GetMethodCoverage(BenchmarkResult result)
    {
        if (result.Target.Method is null)
        {
            throw new Exception("Cannot get coverage of BenchmarkTarget without single method");
        }

        var runnerWithArgs = $"{TestRunnerPath} {result.Tests.TestDirectory}";
        return CoverageRunner.CoverageRunner.RunAndGetCoverage(runnerWithArgs, result.Tests.TestDirectory, result.Target.Method);
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
