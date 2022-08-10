using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Serialization;

namespace VSharp.TestRunner
{
    public static class TestRunner
    {

        private static IEnumerable<string> _extraAssemblyLoadDirs;

        private static Assembly TryLoadAssemblyFrom(object sender, ResolveEventArgs args)
        {
            var existingInstance = AppDomain.CurrentDomain.GetAssemblies().FirstOrDefault(assembly => assembly.FullName == args.Name);
            if (existingInstance != null)
            {
                return existingInstance;
            }
            foreach (string path in _extraAssemblyLoadDirs)
            {
                string assemblyPath = Path.Combine(path, new AssemblyName(args.Name).Name + ".dll");
                if (!File.Exists(assemblyPath))
                    return null;
                Assembly assembly = Assembly.LoadFrom(assemblyPath);
                return assembly;
            }

            return null;
        }

        private static bool StructurallyEqual(object expected, object got)
        {
            Debug.Assert(expected != null && got != null && expected.GetType() == got.GetType());
            var flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance;
            var fields = expected.GetType().GetFields(flags);
            foreach (var field in fields)
            {
                if (!TypeUtils.isSubtypeOrEqual(field.FieldType, typeof(MulticastDelegate)) &&
                    !field.Name.Contains("threadid", StringComparison.OrdinalIgnoreCase) &&
                    !CompareObjects(field.GetValue(expected), field.GetValue(got)))
                {
                    return false;
                }
            }

            return true;
        }

        private static bool ContentwiseEqual(System.Array expected, System.Array got)
        {
            Debug.Assert(expected != null && got != null && expected.GetType() == got.GetType());
            if (expected.Rank != got.Rank)
                return false;
            for (int i = 0; i < expected.Rank; ++i)
                if (expected.GetLength(i) != got.GetLength(i) || expected.GetLowerBound(i) != got.GetLowerBound(i))
                    return false;
            var enum1 = expected.GetEnumerator();
            var enum2 = got.GetEnumerator();
            while (enum1.MoveNext() && enum2.MoveNext())
            {
                if (!CompareObjects(enum1.Current, enum2.Current))
                    return false;
            }
            return true;
        }

        private static bool CompareObjects(object expected, object got)
        {
            if (expected == null)
                return got == null;
            if (got == null)
                return false;
            var type = expected.GetType();
            if (type != got.GetType())
                return false;

            if (type == typeof(Pointer) || type.IsPrimitive || expected is string || type.IsEnum)
            {
                // TODO: compare double with epsilon?
                return got.Equals(expected);
            }

            if (expected is System.Array array)
                return ContentwiseEqual(array, got as System.Array);
            return StructurallyEqual(expected, got);
        }

        private static bool ReproduceTest(FileInfo fileInfo, SuitType suitType, bool checkResult, bool fileMode = false)
        {
            try
            {
                testInfo ti;
                using (FileStream stream = new FileStream(fileInfo.FullName, FileMode.Open, FileAccess.Read))
                {
                    ti = UnitTest.DeserializeTestInfo(stream);
                }
                _extraAssemblyLoadDirs = ti.extraAssemblyLoadDirs;
                UnitTest test = UnitTest.DeserializeFromTestInfo(ti);
                // _extraAssemblyLoadDirs = test.ExtraAssemblyLoadDirs;

                var method = test.Method;

                Console.Out.WriteLine("Starting test reproducing for method {0}", method);
                if (!checkResult)
                    Console.Out.WriteLine("Result check is disabled");
                if (suitType == SuitType.TestsOnly)
                    Console.Out.WriteLine("Error reproducing is disabled");
                object[] parameters = test.Args ?? method.GetParameters()
                    .Select(t => FormatterServices.GetUninitializedObject(t.ParameterType)).ToArray();
                var ex = test.Exception;
                try
                {
                    object result = null;
                    bool shouldInvoke = suitType switch
                    {
                        SuitType.TestsOnly => !test.IsError,
                        SuitType.ErrorsOnly => test.IsError,
                        SuitType.TestsAndErrors => true,
                        _ => false
                    };
                    if (shouldInvoke)
                        result = method.Invoke(test.ThisArg, parameters);
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
                    if (exceptionExpected || suitType == SuitType.TestsAndErrors && !fileMode) {
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
            AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;
            return ReproduceTest(file, SuitType.TestsAndErrors, checkResult);
        }

        public static bool ReproduceTests(DirectoryInfo testsDir, SuitType suitType = SuitType.TestsAndErrors)
        {
            AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;

            var tests = testsDir.EnumerateFiles("*.vst");
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
                result &= ReproduceTest(testFileInfo, suitType, true);
            }

            return result;
        }
    }
}
