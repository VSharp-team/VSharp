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
    private static string testRunnerPath = typeof(TestRunner.TestRunner).Assembly.Location;

    public static bool Check(DirectoryInfo testDir)
    {
        var info = new ProcessStartInfo
        {
            WorkingDirectory = testDir.FullName,
            FileName = "dotnet",
            Arguments = $"{testRunnerPath} {testDir.FullName}",
            UseShellExecute = false,
            RedirectStandardInput = false,
            RedirectStandardOutput = true,
            RedirectStandardError = true
        };

        var proc = Process.Start(info);
        if (proc == null)
            throw new NullReferenceException("couldn't start dotnet process!");
        proc.WaitForExit();
        Logger.printLogString(Logger.Info, proc.StandardOutput.ReadToEnd());
        Logger.printLogString(Logger.Error, proc.StandardError.ReadToEnd());

        return proc.ExitCode == 0;
    }

    public static bool Check(
        DirectoryInfo testDir,
        MethodInfo methodInfo,
        int expectedCoverage,
        out int actualCoverage,
        out string resultMessage)
    {
        var runnerWithArgs = $"{testRunnerPath} {testDir.FullName}";
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
