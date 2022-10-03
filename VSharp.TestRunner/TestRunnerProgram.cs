using System;
using System.IO;
using System.CommandLine;
using System.CommandLine.Invocation;


namespace VSharp.TestRunner
{
    public enum SuitType
    {
        TestsOnly,
        ErrorsOnly,
        TestsAndErrors
    }

    internal static class TestRunnerProgram
    {
        private static int ShowUsage()
        {
            Console.Error.WriteLine("V# test runner tool. Accepts unit test in *.vst format, runs the target executable with the specified input data.\n" +
                                    "\n" +
                                    "Usage: {0} <test directory or *.vst file>", AppDomain.CurrentDomain.FriendlyName);
            return 2;
        }

        private static int ReproduceTests(string testPath, SuitType suitType, bool disableCheck)
        {
            bool checkResult = !disableCheck;
            if (Directory.Exists(testPath))
            {
                if (!checkResult) return ShowUsage();
                var dir = new DirectoryInfo(testPath);
                var result = TestRunner.ReproduceTests(dir, suitType);
                return result ? 0 : 1;
            }

            if (File.Exists(testPath))
            {
                var fi = new FileInfo(testPath);
                var result = TestRunner.ReproduceTest(fi, checkResult);
                return result ? 0 : 1;
            }

            return ShowUsage();
        }

        public static int Main(string[] args)
        {
            var testPathArgument =
                new Argument<string>("test-path", description: "Path to the tests (.vst)");
            var disableCheckOption =
                new Option("--disable-check", description: "Disables test result check");
            var suitOption =
                new Option<SuitType>(aliases: new[] { "--suit", "-s" },
                    () => SuitType.TestsAndErrors,
                    "Chooses which suits will be reproduced: test suits, error suits or both");

            var rootCommand = new RootCommand();

            rootCommand.AddArgument(testPathArgument);
            rootCommand.AddGlobalOption(suitOption);
            rootCommand.AddGlobalOption(disableCheckOption);

            rootCommand.Description = "V# test runner tool. Accepts unit test in *.vst format, runs the target executable with the specified input data.";

            rootCommand.Handler = CommandHandler.Create<string, SuitType, bool>((testPath, suit, disableCheck) =>
            {
                return ReproduceTests(testPath, suit, disableCheck);
            });

            return rootCommand.InvokeAsync(args).Result;
        }
    }
}
