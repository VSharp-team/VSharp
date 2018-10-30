using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using NUnit.Framework;
using VSharp.Interpreter;

namespace VSharp.Test
{
    public class DumpStackTraceListener : TraceListener
    {
        public override void Write(string message)
        {
            Console.Write(message);
        }

        public override void WriteLine(string message)
        {
            Console.WriteLine(message);
        }

        public override void Fail(string message)
        {
            Fail(message, String.Empty);
        }

        public override void Fail(string message1, string message2)
        {
            Console.WriteLine("ASSERT FAILED");
            Console.WriteLine("{0}: {1}", message1, message2);
            Console.WriteLine("Stack Trace:");

            StackTrace trace = new StackTrace( true );
            StackFrame[] stackFrames = trace.GetFrames();
            if (stackFrames != null)
            {
                foreach (StackFrame frame in stackFrames)
                {
                    MethodBase frameClass = frame.GetMethod();
                    Console.WriteLine("  {2}.{3} {0}:{1}",
                        frame.GetFileName(),
                        frame.GetFileLineNumber(),
                        frameClass.DeclaringType,
                        frameClass.Name);
                }
            }
        }
    }

    [TestFixture]
    public sealed class SiliTests
    {
        private const string MethodSeparator = "METHOD: ";
        private const string ResultSeparator = "RESULT: ";
        private const string TestsDirectoryName = "Tests";
        private const string IdealTestFileExtension = ".gold";

//        private void OverwriteIdealValues(string path, IDictionary<MethodInfo, string> result)
//        {
//            if (File.Exists(path))
//            {
//                File.Delete(path);
//            }
//
//            foreach (KeyValuePair<MethodInfo, string> keyValuePair in result)
//            {
//                string text = $"{MethodSeparator}{MethodInfoToString(keyValuePair.Key)}\n{ResultSeparator}{keyValuePair.Value}\n";
//                File.AppendAllText(path, text);
//            }
//        }

        private string MethodInfoToString(MethodInfo methodInfo)
        {
            string parameters = string.Join(", ", methodInfo.GetParameters().Select(p => p.ParameterType.ToString()));
            return $"{methodInfo.ReturnType} {methodInfo.DeclaringType}.{methodInfo.Name}({parameters})";
        }

        private void PrepareSvm()
        {
            // Something like Propositional.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
            SVM.ConfigureSolver(new SmtSolverWrapper<Microsoft.Z3.AST>(new Z3Solver()));
        }

        private IDictionary<string, string> ParseIdealValues(string resultPath, StringBuilder failReason)
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
                resultsDictionary.Add(methodsAndResults[0].Trim('\n', '\r'), methodsAndResults[1].Replace("\r\n", "\n").Trim('\n', '\r'));
            }

            return resultsDictionary;
        }

        private IList<IDictionary<string, string>> ReadAllIdealValues(string testDir, StringBuilder failReason)
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
            Trace.Listeners.Add(new DumpStackTraceListener());

            CultureInfo ci = new CultureInfo("en-GB");
            ci.NumberFormat.PositiveInfinitySymbol = "Infinity";
            ci.NumberFormat.NegativeInfinitySymbol = "-Infinity";
            Thread.CurrentThread.CurrentCulture = ci;

            var ignoredLibs = new List<string>
            {
                //"VSharp.CSharpUtils.dll",
                "ChessDotNet.dll"
            };
            var ignoredTypes = new List<string>
            {
                "Calculator"
                , "AnonymousType"
//                , "Arithmetics"
//                , "Logics"
//                , "Conditional"
//                , "Fibonacci"
//                , "GCD"
                , "McCarthy91"
//                , "Lambdas"
//                , "ClassesSimple"
                , "ClassesSimpleHierarchy"
//                , "StaticClass"
//                , "StaticMembers"
//                , "TryCatch"
//                , "Lists"
//                , "Typecast"
//                , "Generic"
//                , "Strings"
//                , "Methods"
                , "Bag"
                , "Tree"
                , "Celsius"
                , "Fahrenheit"
                , "IPromotion"
                , "Employee"
                , "Helper"
                , "Array"
//                , "Unsafe"
            };

            var failReason = new StringBuilder();
            string pathToTests = Path.Combine(Path.GetFullPath("."), "..", "..", TestsDirectoryName);
            string[] tests = Directory.GetDirectories(pathToTests);
            PrepareSvm();
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

//                    string os = Environment.OSVersion.Platform.ToString();
//                    string goldFile = testDir + Path.DirectorySeparatorChar + os + IdealTestFileExtension;
//                    OverwriteIdealValues(goldFile, got);

                    IList<IDictionary<string, string>> expected = ReadAllIdealValues(testDir, failReason);
                    if (expected.Count == 0)
                    {
                        Assert.Fail($"Could not find or parse ideal values for {lib}");
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
