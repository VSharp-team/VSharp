using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.InteropServices;
using System.Diagnostics;
using static VSharp.CoverageRunner.CoverageRunner;
using static VSharp.TestRunner.TestRunner;

namespace VSharp.Test;

public static class TestResultChecker
{
    private static readonly string TestRunnerPath = typeof(TestRunner.TestRunner).Assembly.Location;

    public static bool Check(DirectoryInfo testDir)
    {
        var info = new ProcessStartInfo
        {
            WorkingDirectory = testDir.FullName,
            FileName = "dotnet",
            Arguments = $"{TestRunnerPath} {testDir.FullName}"
        };

        var success = RunWithLogging(info);

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
        var coverage = RunAndGetCoverage(runnerWithArgs, testDir, methodInfo);
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
