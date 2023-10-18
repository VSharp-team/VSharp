#nullable enable
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
        private static unsafe bool CheckResult(object? expected, object? got)
        {
            return (expected, got) switch
            {
                (Pointer x, Pointer y) => CompareObjects(Pointer.Unbox(x), Pointer.Unbox(y)),
                (null, Pointer y) => CompareObjects(null, Pointer.Unbox(y)),
                (Pointer x, null) => CompareObjects(Pointer.Unbox(x), null),
                _ => CompareObjects(expected, got)
            };
        }

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

                var methodName = Reflection.getFullMethodName(method);
                Console.Out.WriteLine($"Starting reproducing {fileInfo.Name} for method {methodName}");
                if (!checkResult)
                    Console.Out.WriteLine("Result check is disabled");
                if (suiteType == SuiteType.TestsOnly)
                    Console.Out.WriteLine("Error reproducing is disabled");
                object[] parameters = test.Args ?? method.GetParameters()
                    .Select(t => FormatterServices.GetUninitializedObject(t.ParameterType)).ToArray();
                var ex = test.Exception;
                try
                {
                    object? result;
                    var shouldInvoke = suiteType switch
                    {
                        SuiteType.TestsOnly => !test.IsError || fileMode,
                        SuiteType.ErrorsOnly => test.IsError || fileMode,
                        SuiteType.TestsAndErrors => !test.IsFatalError || fileMode,
                        _ => false
                    };
                    if (shouldInvoke)
                    {
                        test.ApplyExternMocks(fileInfo.Name);
                        result = method.Invoke(test.ThisArg, parameters);
                        test.ReverseExternMocks(); // reverses if ex was not thrown
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
                    if (checkResult && !test.IsError && !CheckResult(test.Expected, result))
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
                    test.ReverseExternMocks(); // reverses if ex was thrown
                    var exceptionExpected = e.InnerException != null && e.InnerException.GetType() == ex;
                    if (exceptionExpected || test.IsError && suiteType == SuiteType.TestsAndErrors && !fileMode) {
                        Console.ForegroundColor = ConsoleColor.Green;
                        var exceptionType = e.InnerException?.GetType().FullName;
                        Console.WriteLine($"Test {fileInfo.Name} throws the expected exception {exceptionType}!");
                        Console.ResetColor();
                    }
                    else if (e.InnerException != null && ex != null)
                    {
                        Console.ForegroundColor = ConsoleColor.Red;
                        Console.Error.WriteLine($"Test {fileInfo.Name} throws {e.InnerException} when the expected exception was {ex}!");
                        Console.ResetColor();
                        throw e.InnerException;
                    }
                    else throw;
                }
                finally
                {
                    test.ReverseExternMocks();
                }
            }
            catch (Exception e)
            {
                Console.ForegroundColor = ConsoleColor.Red;
                Console.Error.WriteLine($"Error ({fileInfo.FullName}): {e}");
                Console.ResetColor();
                return false;
            }

            Console.ForegroundColor = ConsoleColor.Green;
            Console.Out.WriteLine($"{fileInfo.Name} passed!");
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
