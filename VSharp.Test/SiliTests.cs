using System;
using System.Diagnostics;
using System.IO;
using System.Text;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace VSharp.Test
{
    [TestClass]
    public sealed class SiliTests
    {
        [TestMethod]
        public void RunCSharpTests()
        {
            bool failed = false;
            var pathToTests = Path.Combine(System.IO.Path.GetFullPath(@"..\..\"), "testSources");
            var pathToGoldRes = Path.Combine(System.IO.Path.GetFullPath(@"..\..\"), "testResults");
            var tests = Directory.GetDirectories(pathToTests);
            foreach (var testDir in tests)
            {
                var libEntries = Directory.GetFiles(testDir);
                foreach (var lib in libEntries)
                {
                    if (lib.EndsWith("txt"))
                    {
                        continue;
                    }

                    var libName = Path.GetFileNameWithoutExtension(lib);
                    var testPath = Path.Combine(testDir, libName)  + "_test.txt";
                    var res = SVM.Run(System.Reflection.Assembly.LoadFile(lib));
                    File.WriteAllText(testPath, res);
                    var resPath = Path.Combine(pathToGoldRes, Path.GetFileName(testDir), libName) + "_res.txt";

                    if (File.Exists(resPath))
                    {
                        Process p = new Process();
                        p.StartInfo.UseShellExecute = false;
                        p.StartInfo.RedirectStandardOutput = true;
                        p.StartInfo.FileName = "FC";
                        p.StartInfo.Arguments = testPath + " " + resPath;
                        p.Start();
                        string output = p.StandardOutput.ReadToEnd();
                        p.WaitForExit();
                        Console.Write(output);
                        if (!output.Split('\n')[1].Contains("no differences encountered"))
                        {
                            failed = true;
                        }
                    }
                }
            }

            if (failed)
            {
                Assert.Fail();
            }
        }
    }
}
