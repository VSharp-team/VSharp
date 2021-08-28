using System;
using System.Diagnostics;
using System.Reflection;

namespace VSharp.Test.Utils
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

            StackTrace trace = new StackTrace(true);
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
}