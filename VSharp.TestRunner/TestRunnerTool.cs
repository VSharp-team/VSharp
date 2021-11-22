using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.Serialization;
using System.Reflection;
using System.Linq;

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

        private static bool ContentwiseEqual(Array expected, Array got)
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
            if (type.IsPrimitive || expected is string || type.IsEnum)
            {
                // TODO: compare double with epsilon?
                return got.Equals(expected);
            }

            if (expected is Array array)
                return ContentwiseEqual(array, got as Array);
            return StructurallyEqual(expected, got);
        }

        private static bool ReproduceTests(IEnumerable<FileInfo> tests, bool shouldReproduceError)
        {
            AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;

            foreach (FileInfo fi in tests)
            {
                try
                {
                    using (FileStream stream = new FileStream(fi.FullName, FileMode.Open, FileAccess.Read))
                    {
                        UnitTest test = UnitTest.Deserialize(stream);
                        _extraAssemblyLoadDirs = test.ExtraAssemblyLoadDirs;

                        var method = test.Method;

                        Console.WriteLine("Starting test reproducing for method {0}", method);
                        object[] parameters = test.Args ?? method.GetParameters()
                            .Select(t => FormatterServices.GetUninitializedObject(t.ParameterType)).ToArray();
                        var ex = test.Exception;
                        try
                        {
                            object result = null;
                            if (!test.IsError || shouldReproduceError)
                                result = method.Invoke(test.ThisArg, parameters);
                            if (ex != null)
                            {
                                Console.Error.WriteLine("Test {0} failed! The expected exception {1} was not thrown",
                                    fi.Name, ex);
                                return false;
                            }
                            if (test.ResultCheck && !CompareObjects(test.Expected, result))
                            {
                                // TODO: use NUnit?
                                Console.Error.WriteLine("Test {0} failed! Expected {1}, but got {2}", fi.Name,
                                    test.Expected ?? "null",
                                    result ?? "null");
                                return false;
                            }
                        }
                        catch (TargetInvocationException e)
                        {
                            if (e.InnerException != null && e.InnerException.GetType() == ex)
                                Console.WriteLine("Test {0} throws the expected exception!", fi.Name);
                            else if (e.InnerException != null && ex != null)
                            {
                                Console.Error.WriteLine("Test {0} throws {1} when the expected exception was {2}!", fi.Name, e.InnerException, ex);
                                throw e.InnerException;
                            }
                            else throw;
                        }
                    }
                }
                catch (Exception e)
                {
                    Console.Error.WriteLine("Error ({0}): {1}", fi.Name, e);
                    return false;
                }

                Console.WriteLine("{0} passed!", fi.Name);
            }

            return true;
        }

        public static bool ReproduceTest(FileInfo file)
        {
            return ReproduceTests(new[] {file}, true);
        }

        public static bool ReproduceTests(DirectoryInfo testsDir)
        {
            var tests = testsDir.EnumerateFiles("*.vst");
            var testsList = tests.ToList();
            if (testsList.Count > 0)
            {
                return ReproduceTests(testsList, false);
            }

            Console.Error.WriteLine("No *.vst tests found in {0}", testsDir.FullName);
            return false;
        }
    }
}
