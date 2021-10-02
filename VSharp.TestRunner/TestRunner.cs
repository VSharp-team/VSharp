using System;
using System.Runtime.Serialization;
using System.Reflection;
using System.Linq;

namespace VSharp.TestRunner
{
    public static class Program
    {

        private static void ValidateProfilerEnvironment(Test test)
        {
            // TODO
            // if (test has no externals)
            return;
            if (Environment.GetEnvironmentVariable("CORCLR_ENABLE_PROFILING") != "1")
                throw new InvalidOperationException("IL rewriter environment was not set up! Please use the batch file runner instead of this executable.");
        }

        private static bool CompareResults(object expected, object got)
        {
            if (got == null)
                return expected == null;
            // TODO: compare double with epsilon?
            // TODO: compare bytewise reference types?
            return got.Equals(expected);
        }

        public static int Main(string[] args)
        {
            if (args.Length != 1)
            {
                Console.Error.WriteLine("V# test runner tool. Accepts unit test in *.vst format, runs the target executable with the specified input data.\n" +
                                  "\n" +
                                  "Usage: {0} test.vst", AppDomain.CurrentDomain.FriendlyName);
                return 2;
            }
            try
            {
                Test test = Test.Deserialize(args[0]);
                ValidateProfilerEnvironment(test);

                var method = test.Method;
                // TODO: support generic arguments?
                bool hasThis = method.CallingConvention.HasFlag(CallingConventions.HasThis);

                // TODO: support virtual methods by passing explicit declaring type?
                object thisObj = hasThis && method.DeclaringType != null ? FormatterServices.GetUninitializedObject(method.DeclaringType) : null;
                object[] parameters = test.Args ?? method.GetParameters().Select(t => FormatterServices.GetUninitializedObject(t.ParameterType)).ToArray();
                object result = method.Invoke(thisObj, parameters);
                if (!CompareResults(test.Expected, result))
                {
                    // TODO: use NUnit?
                    Console.Error.WriteLine("Test failed! Expected {0}, but got {1}", test.Expected, result);
                    return 5;
                }
            }
            catch (Exception e)
            {
                Console.Error.WriteLine("Error: {0} {1}", e.Message, e.GetType().FullName);
                return 2;
            }

            Console.WriteLine("Test passed!");
            return 0;
        }
    }
}