using System;
using System.CommandLine;
using System.IO;
using System.Threading.Tasks;

namespace VSharp.TestRunner
{
    public enum SuiteType
    {
        TestsOnly,
        ErrorsOnly,
        TestsAndErrors
    }

    internal static class TestRunnerProgram
    {
        private static int ShowUsage()
        {
            Console.Error.WriteLine(
                "V# test runner tool. Accepts unit test in *.vst format, runs the target executable with the specified input data.\n" +
                "\n" +
                "Usage: {0} <test directory or *.vst file>", AppDomain.CurrentDomain.FriendlyName
            );
            return 2;
        }

        private static int ReproduceTests(string testPath, SuiteType suiteType, bool disableCheck, bool recursive)
        {
            bool checkResult = !disableCheck;
            if (Directory.Exists(testPath))
            {
                if (!checkResult) return ShowUsage();
                var dir = new DirectoryInfo(testPath);
                var result = TestRunner.ReproduceTests(dir, suiteType, recursive);
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
                new Option<bool>("--disable-check", description: "Disables test result check");
            var suiteOption =
                new Option<SuiteType>(aliases: new[] { "--suite", "-s" },
                    () => SuiteType.TestsAndErrors,
                    "Chooses which suites will be reproduced: test suites, error suites or both");
            var recursiveOption =
                new Option<bool>("--recursive", description: "Search for .vst files in subdirectories as well");

            var rootCommand = new RootCommand();

            rootCommand.AddArgument(testPathArgument);
            rootCommand.AddGlobalOption(suiteOption);
            rootCommand.AddGlobalOption(disableCheckOption);
            rootCommand.AddGlobalOption(recursiveOption);

            rootCommand.Description = "V# test runner tool. Accepts unit test in *.vst format, runs the target executable with the specified input data.";

            rootCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                context.ExitCode = ReproduceTests(
                    parseResult.GetValueForArgument(testPathArgument),
                    parseResult.GetValueForOption(suiteOption),
                    parseResult.GetValueForOption(disableCheckOption),
                    parseResult.GetValueForOption(recursiveOption));
            });

            return rootCommand.Invoke(args);
        }
    }
}
