using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Linq;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class SiliTests
    {
        private const string MethodSeparator = "METHOD: ";
        private const string ResultSeparator = "RESULT: ";
        private const string TestsDirectoryName = "Tests";
        private const string IdealTestFilePattern = "*.gold";

        private void OverwriteIdealValues(string path, IDictionary<MethodInfo, string> result)
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

        private IDictionary<string, string> ParseIdealValues(string resultPath, System.Text.StringBuilder failReason)
        {
            string resultText = "";
            if (File.Exists(resultPath))
            {
                try
                {
                    resultText = File.ReadAllText(resultPath);
                }
                catch (IOException e)
                {
                    failReason.AppendFormat("Can't read gold file! Exception: {0}\n\n", e.Message);
                }
            }

            if (string.IsNullOrEmpty(resultText))
            {
                return null;
            }

            IEnumerable<string> methodsWithResults = Regex.Split(resultText, $"^{MethodSeparator}", RegexOptions.Multiline).Where(s => !string.IsNullOrWhiteSpace(s));
            var resultsDictionary = new Dictionary<string, string>();
            foreach (string str in methodsWithResults)
            {
                IList<string> methodsAndResults = Regex.Split(str, $"^{ResultSeparator}", RegexOptions.Multiline).Where(s => !string.IsNullOrWhiteSpace(s)).ToList();
                resultsDictionary.Add(methodsAndResults[0].Trim('\n', '\r'), methodsAndResults[1].Trim('\n', '\r'));
            }

            return resultsDictionary;
        }

        private IEnumerable<IDictionary<string, string>> ReadAllIdealValues(string testDir, System.Text.StringBuilder failReason)
        {
            var result = new List<IDictionary<string, string>>();
            var idealFiles = Directory.GetFiles(testDir, IdealTestFilePattern);
            foreach (string file in idealFiles)
            {
                IDictionary<string, string> values = ParseIdealValues(file, failReason);
                if (values != null)
                {
                    result.Add(values);
                }
            }
            return result;
        }

        [Test]
        public void RunCSharpTests()
        {
            var disabledTests = new List<string>
            {
                "Calculator"
                //, "Arithmetics"
                //, "Logics"
                //, "Conditional"
                //, "Fibonacci"
                //, "GCD"
                //, "Lambdas"
                //, "ClassesSimple"
                //, "StaticClass"
                //, "StaticMembers"
                //, "TryCatch"
            };

            var failReason = new System.Text.StringBuilder();
            string pathToTests = Path.Combine(Path.GetFullPath("."), "..", "..", TestsDirectoryName);
            string[] tests = Directory.GetDirectories(pathToTests);
            foreach (string testDir in tests)
            {
                var libEntries = Directory.GetFiles(testDir);
                foreach (string lib in libEntries)
                {
                    if (!lib.EndsWith(".dll", StringComparison.Ordinal))
                    {
                        continue;
                    }

                    IDictionary<MethodInfo, string> got = SVM.Run(Assembly.LoadFile(lib), disabledTests);

                    IEnumerable<IDictionary<string, string>> expected = ReadAllIdealValues(testDir, failReason);
                    if (expected.Count() == 0)
                    {
                        Assert.Fail($"Could not find or parse ideal values for {lib}");
                        break;
                    }

                    foreach (KeyValuePair<MethodInfo, string> keyValuePair in got)
                    {
                        string keyMethod = MethodInfoToString(keyValuePair.Key);
                        string actualValue = "";
                        if (expected.All(dict =>
                        {
                            if (dict.TryGetValue(keyMethod, out actualValue))
                            {
                                return !string.Equals(actualValue, keyValuePair.Value);
                            }
                            else
                            {
                                failReason.AppendFormat("Gold file does not contain ideal values for {0}!\n", keyMethod);
                                failReason.AppendFormat("FYI, got value\n{0}\n\n", keyValuePair.Value);
                                return true;
                            }
                        }))
                        {
                            failReason.AppendFormat("{0}{1}\nEXPECTED: {2}\nGOT:      {3}\n\n", MethodSeparator, keyMethod, actualValue, keyValuePair.Value);
                        }
                    }
                }
            }

            string fail = failReason.ToString();
            if (!string.IsNullOrEmpty(fail))
            {
                Assert.Fail(fail);
            }
        }
    }
}
