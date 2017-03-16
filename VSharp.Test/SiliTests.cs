using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace VSharp.Test
{
    [TestClass]
    public sealed class SiliTests
    {
        private const string MethodSeparator = "METHOD: ";
        private const string ResultSeparator = "RESULT: ";
        private const string TestsDirectoryName = "Tests";
        private const string ExtensionOfIdealTestFile = ".gold";

        private void WriteResultToFile(string path, IDictionary<MethodInfo, string> result)
        {
            if (File.Exists(path))
            {
                File.Delete(path);
            }

            foreach (KeyValuePair<MethodInfo, string> keyValuePair in result)
            {
                string text = $"{MethodSeparator}{MethodInfoToString(keyValuePair.Key)}\n{ResultSeparator}{keyValuePair.Value}\n";
                File.AppendAllText(path, text);
            }
        }

        private string MethodInfoToString(MethodInfo methodInfo)
        {
            string parameters = string.Join(", ", methodInfo.GetParameters().Select(p => p.ParameterType.ToString()));
            return $"{methodInfo.ReturnType} {methodInfo.DeclaringType}.{methodInfo.Name}({parameters})";
        }

        [TestMethod]
        public void RunCSharpTests()
        {
            List<string> disabledTests = new List<string>
            {
                "Calculator"
                //, "Conditional"
                //, "Arithmetics"
                //, "Fibonacci"
                //, "Lambdas"
                //, "GCD"
            };

            bool failed = false;
            string pathToTests = Path.Combine(Path.GetFullPath(@"..\..\"), TestsDirectoryName);
            string[] tests = Directory.GetDirectories(pathToTests);
            foreach (string testDir in tests)
            {
                var libEntries = Directory.GetFiles(testDir);
                foreach (string lib in libEntries)
                {
                    if (lib.EndsWith(ExtensionOfIdealTestFile)) continue;
                    IDictionary<MethodInfo, string> testDictionary = SVM.Run(Assembly.LoadFile(lib), disabledTests);
                    string libName = Path.GetFileNameWithoutExtension(lib);
                    if (string.IsNullOrEmpty(libName))
                    {
                        Console.WriteLine("smth wrong happend with tested lib file");
                        Assert.Fail();
                    }

                    string resultPath = Path.Combine(testDir, libName) + ExtensionOfIdealTestFile;
                    // Uncomment this line to overwrite ideal results with current tests run
                    // WriteResultToFile(resultPath, testDictionary);
                    string resultText = "";
                    if (File.Exists(resultPath))
                    {
                        try
                        {
                            resultText = File.ReadAllText(resultPath);
                        }
                        catch (IOException e)
                        {
                            failed = true;
                            Console.WriteLine($"Can't open file with results. Exception: {e.Message}");
                        }
                    }

                    if (string.IsNullOrEmpty(resultText)) continue;
                    // todo split by regexp
                    IEnumerable<string> methodsWithResults = Regex.Split(resultText, $"^{MethodSeparator}", RegexOptions.Multiline).Where(s => !string.IsNullOrWhiteSpace(s));
                    var resultsDictionary = new Dictionary<string, string>();
                    foreach (string str in methodsWithResults)
                    {
                        IList<string> methodsAndResults = Regex.Split(str, $"^{ResultSeparator}", RegexOptions.Multiline).Where(s => !string.IsNullOrWhiteSpace(s)).ToList();
                        resultsDictionary.Add(methodsAndResults[0].Trim('\n', '\r'), methodsAndResults[1].Trim('\n', '\r'));
                    }

                    foreach (KeyValuePair<MethodInfo, string> keyValuePair in testDictionary)
                    {
                        string keyMethod = MethodInfoToString(keyValuePair.Key);
                        string actualValue;
                        if (resultsDictionary.TryGetValue(keyMethod, out actualValue))
                        {
                            if (string.Equals(actualValue, keyValuePair.Value)) continue;
                            failed = true;
                            Console.WriteLine($"{MethodSeparator}{keyMethod}\nEXPECTED: {actualValue}\nGOT: {keyValuePair.Value}");
                        }
                        else
                        {
                            failed = true;
                            Console.WriteLine($"Result does not contain this {keyMethod}");
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
