using System;
using System.IO;

namespace VSharp.TestRunner
{
    internal static class TestRunnerProgram
    {
        private static int ShowUsage()
        {
            Console.Error.WriteLine("V# test runner tool. Accepts unit test in *.vst format, runs the target executable with the specified input data.\n" +
                                    "\n" +
                                    "Usage: {0} <test directory or *.vst file>", AppDomain.CurrentDomain.FriendlyName);
            return 2;
        }

        public static int Main(string[] args)
        {
            if (args.Length != 1)
            {
                return ShowUsage();
            }

            string path = args[0];
            if (Directory.Exists(path))
            {
                var dir = new DirectoryInfo(path);
                var result = TestRunner.ReproduceTests(dir);
                return result ? 0 : 1;
            }

            if (File.Exists(path))
            {
                var fi = new FileInfo(path);
                var result = TestRunner.ReproduceTest(fi);
                return result ? 0 : 1;
            }

            return ShowUsage();
        }
    }
}
