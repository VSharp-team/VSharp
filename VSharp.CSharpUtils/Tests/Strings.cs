using System;

namespace VSharp.CSharpUtils.Tests
{
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }

        public static string SymbolicString(string s)
        {
            var len = s.Length;
            return s;
        }

        public static int NullLength()
        {
            string s = null;
            return s.Length;
        }
    }
}
