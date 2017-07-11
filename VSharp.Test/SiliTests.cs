using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading;
using NUnit.Framework;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class SiliTests
    {
        private const string MethodSeparator = "METHOD: ";
        private const string ResultSeparator = "RESULT: ";
        private const string TestsDirectoryName = "Tests";
        private const string IdealTestFileExtension = ".gold";

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
            string os = Environment.OSVersion.Platform.ToString();
            string goldFile = testDir + Path.DirectorySeparatorChar + os + IdealTestFileExtension;
            IDictionary<string, string> values = ParseIdealValues(goldFile, failReason);
            var result = new List<IDictionary<string, string>>();
            if (values != null)
                result.Add(values);
            return result;
        }

        private static string _currentTestDirectory = "";

        private static Assembly LoadFromTestFolder(object sender, ResolveEventArgs args)
        {
            // This handler is called only when the common language runtime tries to bind to the assembly and fails.
            string name = new AssemblyName(args.Name).Name;
            string additionalPath = _currentTestDirectory + Path.DirectorySeparatorChar + name + ".dll";
            if (File.Exists(additionalPath))
            {
                return Assembly.LoadFrom(additionalPath);
            }

            return null;
        }

        [Test]
        public void RunCSharpTests()
        {
            Thread.CurrentThread.CurrentCulture = new CultureInfo("en-GB");
            var ignoredLibs = new List<string>
            {
                //"VSharp.CSharpUtils.dll",
                "ChessDotNet.dll"
            };
            var ignoredTypes = new List<string>
            {
                "Calculator"
                , "AnonymousType"
                , "Arithmetics"
                , "Logics"
                , "Conditional"
                , "Fibonacci"
                , "GCD"
                , "Lambdas"
                , "ClassesSimple"
                , "StaticClass"
                , "StaticMembers"
                , "TryCatch"
                , "Lists"
                , "IKeeper"
                , "Bag"
                , "Generic"
                , "Celsius"
                , "Fahrenheit"
                , "INormalize"
                , "Coord"
                //, "Piece"
                , "Pawn"
                , "IPromotion"
                , "Employee"
                , "Helper"
            };

            var failReason = new System.Text.StringBuilder();
            string pathToTests = Path.Combine(Path.GetFullPath("."), "..", "..", TestsDirectoryName);
            string[] tests = Directory.GetDirectories(pathToTests);
            foreach (string testDir in tests)
            {
                string[] libEntries = Directory.GetFiles(testDir);
                foreach (string lib in libEntries)
                {
                    if (!lib.EndsWith(".dll", StringComparison.Ordinal) || ignoredLibs.Exists(i => lib.EndsWith(i)))
                    {
                        continue;
                    }

                    _currentTestDirectory = testDir;
                    AppDomain currentDomain = AppDomain.CurrentDomain;
                    currentDomain.AssemblyResolve += LoadFromTestFolder;

                    IDictionary<MethodInfo, string> got = SVM.Run(Assembly.LoadFile(lib), ignoredTypes);

                    IEnumerable<IDictionary<string, string>> expected = ReadAllIdealValues(testDir, failReason);
                    if (expected.Count() == 0)
                    {
                        Assert.Fail($"Could not find or parse ideal values for {lib}");
                        break;
                    }

                    foreach (KeyValuePair<MethodInfo, string> keyValuePair in got)
                    {
                        string method = MethodInfoToString(keyValuePair.Key);
                        string gotValue = keyValuePair.Value;
                        string expectedValue = "";
                        if (expected.All(dict =>
                        {
                            if (dict.TryGetValue(method, out expectedValue))
                            {
                                return !string.Equals(expectedValue, gotValue);
                            }

                            failReason.AppendFormat("Gold file does not contain ideal values for {0}!\n", method);
                            return true;
                        }))
                        {
                            failReason.AppendFormat("{0}{1}\nEXPECTED: {2}\nGOT:      {3}\n\n", MethodSeparator, method, expectedValue, gotValue);
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
