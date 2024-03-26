using System;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using NUnit.Framework;
using VSharp.CSharpUtils;
using VSharp.Explorer;
using VSharp.TestRenderer;

namespace VSharp.Test;

public static class FuzzerTests
{
    private static readonly string OutputDirectory = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}fuzzerTests";
    private static readonly DirectoryInfo OutputDirectoryInfo = new (OutputDirectory);

    private class FuzzerReporter : IReporter
    {
        public void ReportFinished(UnitTest test)
        {
            throw new Exception("Unreachable");
        }

        public void ReportException(UnitTest test)
        {
            throw new Exception("Unreachable");
        }

        public void ReportIIE(InsufficientInformationException iie)
        {
            throw new Exception("Unreachable");
        }

        public void ReportInternalFail(Method method, Exception exception)
        {
            throw new Exception($"InternalFail: {exception}");
        }

        public void ReportCrash(Exception exception)
        {
            throw new Exception($"Crash: {exception}");
        }
    }

    private static void PrepareOutputDirectory()
    {
        if (OutputDirectoryInfo.Exists)
        {
            OutputDirectoryInfo.Delete(recursive:true);
        }
        OutputDirectoryInfo.Create();
    }

    private static MethodBase[] LoadTestMethods()
    {
        return Assembly
                .GetExecutingAssembly()
                .GetTypes()
                .Where(x => x.GetCustomAttribute<TestSvmFixtureAttribute>() != null)
                .Where(x => x.GetCustomAttributes<IgnoreFuzzerAttribute>().Any() == false)
                .SelectMany(x => x.GetMethods())
                .Where(x => x.GetCustomAttribute<TestSvmAttribute>() != null)
                .Where(x => x.GetCustomAttributes<IgnoreFuzzerAttribute>().Any() == false)
                .Select(x => (MethodBase) x)
                .ToArray();
    }

    private static void RunFuzzer()
    {
        var fuzzerOptions = new FuzzerOptions(
            isolation:fuzzerIsolation.Process,
            coverageZone:coverageZone.ModuleZone
        );

        var explorerOptions = new ExplorationOptions(
            timeout:TimeSpanBuilders.Infinite,
            outputDirectory:OutputDirectoryInfo,
            explorationModeOptions:explorationModeOptions.NewFuzzing(fuzzerOptions)
        );

        var reporter = new FuzzerReporter();
        var explorer = new Explorer.Explorer(explorerOptions, reporter);
        var methodsToTest = LoadTestMethods();
        Logger.printLogString(Logger.Error, "Methods loaded");

        explorer.StartExploration(
            methodsToTest,
            global::System.Array.Empty<Tuple<MethodBase, EntryPointConfiguration>>()
        );
    }

    private static void ReproduceGeneratedTests()
    {
        var reproduced = TestRunner.TestRunner.ReproduceTests(OutputDirectoryInfo);
        if (!reproduced) throw new Exception("Reproducing tests failed");
    }

    [Test, Explicit]
    public static void Tests()
    {
        Logger.enableTag("Communication", Logger.Trace);
        PrepareOutputDirectory();
        RunFuzzer();
        ReproduceGeneratedTests();
    }
}
