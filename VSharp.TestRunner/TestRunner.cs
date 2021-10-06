using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Reflection;
using System.Linq;

namespace VSharp.TestRunner
{
    public static class Program
    {

        private static bool CompareResults(object expected, object got)
        {
            if (got == null)
                return expected == null;
            // TODO: compare double with epsilon?
            // TODO: compare bytewise reference types?
            return got.Equals(expected);
        }

        private static int ShowUsage()
        {
            Console.Error.WriteLine("V# test runner tool. Accepts unit test in *.vst format, runs the target executable with the specified input data.\n" +
                                    "\n" +
                                    "Usage: {0} <test directory or *.vst file>", AppDomain.CurrentDomain.FriendlyName);
            return 2;
        }

        public static int Main(string[] args)
        {
            if (args.Length != 1)
            {
                return ShowUsage();
            }

            string path = args[0];
            IEnumerable<FileInfo> tests;
            if (Directory.Exists(path))
            {
                var dir = new DirectoryInfo(path);

                tests = dir.EnumerateFiles("*.vst");
            } else if (File.Exists(path))
            {
                var fi = new FileInfo(path);
                tests = new List<FileInfo> {fi};
            }
            else
            {
                return ShowUsage();
            }

            bool atLeastOneTestFound = false;
            foreach (FileInfo fi in tests)
            {
                atLeastOneTestFound = true;
                try
                {
                    using (FileStream stream = new FileStream(fi.FullName, FileMode.Open, FileAccess.Read))
                    {
                        UnitTest test = UnitTest.Deserialize(stream);

                        var method = test.Method;
                        // TODO: support generic arguments?
                        bool hasThis = method.CallingConvention.HasFlag(CallingConventions.HasThis);

                        // TODO: support virtual methods by passing explicit declaring type?
                        object thisObj = hasThis && method.DeclaringType != null
                            ? FormatterServices.GetUninitializedObject(method.DeclaringType)
                            : null;
                        object[] parameters = test.Args ?? method.GetParameters()
                            .Select(t => FormatterServices.GetUninitializedObject(t.ParameterType)).ToArray();
                        object result = method.Invoke(thisObj, parameters);
                        if (!CompareResults(test.Expected, result))
                        {
                            // TODO: use NUnit?
                            Console.Error.WriteLine("Test {0} failed! Expected {1}, but got {2}", fi.Name, test.Expected ?? "null",
                                result ?? "null");
                            return 5;
                        }
                    }
                }
                catch (Exception e)
                {
                    Console.Error.WriteLine("Error: {0} {1}", e.Message, e.GetType().FullName);
                    return 2;
                }

                Console.WriteLine("Test passed!");
            }

            if (!atLeastOneTestFound)
            {
                Console.Error.WriteLine("No *.vst tests found in {0}", path);
            }

            return 0;
        }
    }
}