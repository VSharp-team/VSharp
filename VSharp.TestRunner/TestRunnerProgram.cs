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

        public static int Main(string[] args) // TODO: add another arg (disable check result) #do
        {
            bool checkResult = true;
            if (args.Length == 2)
            {
                if (!Boolean.TryParse(args[1], out checkResult))
                    return ShowUsage();
            }
            else if (args.Length < 1 || args.Length > 2)
            {
                return ShowUsage();
            }

            string path = args[0];
            if (Directory.Exists(path))
            {
                if (!checkResult) return ShowUsage();
                var dir = new DirectoryInfo(path);
                var result = TestRunner.ReproduceTests(dir);
                return result ? 0 : 1;
            }

            if (File.Exists(path))
            {
                var fi = new FileInfo(path);
                var result = TestRunner.ReproduceTest(fi, checkResult);
                return result ? 0 : 1;
            }

            return ShowUsage();
        }
    }
}
