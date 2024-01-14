using System.IO;
using System.Reflection;
using System.Diagnostics;
using VSharp.CoverageTool;
using VSharp.CSharpUtils;


namespace VSharp.Test;

public static class TestResultChecker
{
    private static readonly string TestRunnerPath = typeof(TestRunner.TestRunner).Assembly.Location;

    public static bool Check(DirectoryInfo testDir)
    {
        var info = new ProcessStartInfo
        {
            WorkingDirectory = testDir.FullName,
            FileName = DotnetExecutablePath.ExecutablePath,
            Arguments = $"{TestRunnerPath} {testDir.FullName}"
        };

        var proc = info.StartWithLogging(
            x => Logger.printLogString(Logger.Info, $"{x}"),
            x => Logger.printLogString(Logger.Error, $"{x}")
        );
        proc.WaitForExit();
        var success = proc.IsSuccess();

        if (!success)
            Logger.printLogString(Logger.Error, "TestRunner Check failed!");

        return success;
    }

    public static bool Check(
        DirectoryInfo testDir,
        MethodInfo methodInfo,
        int expectedCoverage,
        out int actualCoverage,
        out string resultMessage)
    {
        var runnerWithArgs = $"{TestRunnerPath} {testDir.FullName}";
        var coverageTool = new PassiveCoverageTool(testDir, methodInfo);
        var coverage = coverageTool.RunWithCoverage(runnerWithArgs);
        actualCoverage = coverage;
        resultMessage = string.Empty;

        if (expectedCoverage == coverage)
        {
            return true;
        }

        resultMessage = $"Incomplete coverage! Expected {expectedCoverage}, but got {coverage}";
        return false;
    }
}
