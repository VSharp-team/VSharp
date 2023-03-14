using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;
using static VSharp.TestExtensions.ObjectsComparer;

namespace VSharp.TestRunner
{
    public static class TestRunner
    {
        private static bool ReproduceTest(FileInfo fileInfo, SuiteType suiteType, bool checkResult, bool fileMode = false)
        {
            try
            {
                testInfo ti;
                using (FileStream stream = new FileStream(fileInfo.FullName, FileMode.Open, FileAccess.Read))
                {
                    ti = UnitTest.DeserializeTestInfo(stream);
                }
                AssemblyManager.SetDependenciesDirs(ti.extraAssemblyLoadDirs);
                UnitTest test = UnitTest.DeserializeFromTestInfo(ti, false);

                var method = test.Method;

                Console.Out.WriteLine("Starting reproducing {0} for method {1}", fileInfo.Name, method);
                if (!checkResult)
                    Console.Out.WriteLine("Result check is disabled");
                if (suiteType == SuiteType.TestsOnly)
                    Console.Out.WriteLine("Error reproducing is disabled");
                object[] parameters = test.Args ?? method.GetParameters()
                    .Select(t => FormatterServices.GetUninitializedObject(t.ParameterType)).ToArray();
                var ex = test.Exception;
                try
                {
                    object result;
                    string message = test.ErrorMessage;
                    var debugAssertFailed = message != null && message.Contains("Debug.Assert failed");
                    bool shouldInvoke = suiteType switch
                    {
                        SuiteType.TestsOnly => !test.IsError || fileMode,
                        SuiteType.ErrorsOnly => test.IsError || fileMode,
                        SuiteType.TestsAndErrors => !debugAssertFailed || fileMode,
                        _ => false
                    };
                    if (shouldInvoke)
                    {
                        result = method.Invoke(test.ThisArg, parameters);
                    }
                    else
                    {
                        Console.ForegroundColor = ConsoleColor.White;
                        Console.WriteLine("Test {0} ignored.", fileInfo.Name);
                        Console.ResetColor();
                        return true;
                    }
                    if (ex != null)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Test {0} failed! The expected exception {1} was not thrown",
                            fileInfo.Name, ex);
                        Console.ResetColor();
                        return false;
                    }
                    if (checkResult && !test.IsError && !CompareObjects(test.Expected, result))
                    {
                        // TODO: use NUnit?
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Test {0} failed! Expected {1}, but got {2}", fileInfo.Name,
                            test.Expected ?? "null",
                            result ?? "null");
                        Console.ResetColor();
                        return false;
                    }
                }
                catch (TargetInvocationException e)
                {
                    var exceptionExpected = e.InnerException != null && e.InnerException.GetType() == ex;
                    if (exceptionExpected || test.IsError && suiteType == SuiteType.TestsAndErrors && !fileMode) {
                        Console.ForegroundColor = ConsoleColor.Green;
                        Console.WriteLine("Test {0} throws the expected exception {1}!", fileInfo.Name, e.InnerException.GetType().FullName);
                        Console.ResetColor();
                    }
                    else if (e.InnerException != null && ex != null)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine("Test {0} throws {1} when the expected exception was {2}!", fileInfo.Name, e.InnerException, ex);
                        Console.ResetColor();
                        throw e.InnerException;
                    }
                    else throw;
                }
            }
            catch (Exception e)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.Error.WriteLine("Error ({0}): {1}", fileInfo.Name, e);
                Console.ResetColor();
                return false;
            }

            Console.ForegroundColor = ConsoleColor.Green;
            Console.Out.WriteLine("{0} passed!", fileInfo.Name);
            Console.ResetColor();

            return true;
        }

        public static bool ReproduceTest(FileInfo file, bool checkResult)
        {
            return ReproduceTest(file, SuiteType.TestsAndErrors, checkResult, true);
        }

        public static bool ReproduceTests(DirectoryInfo testsDir, SuiteType suiteType = SuiteType.TestsAndErrors, bool recursive = false)
        {
            var tests = testsDir.EnumerateFiles("*.vst", recursive ? SearchOption.AllDirectories : SearchOption.TopDirectoryOnly);
            var testsList = tests.ToList();

            if (testsList.Count == 0)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.Error.WriteLine("No *.vst tests found in {0}", testsDir.FullName);
                Console.ResetColor();
                return false;
            }

            var result = true;

            foreach (var testFileInfo in testsList)
            {
                result &= ReproduceTest(testFileInfo, suiteType, true);
            }

            return result;
        }
    }
}
