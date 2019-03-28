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

        public static int GetConcreteHash()
        {
            String str = "sample string";
            return str.GetHashCode();
        }

        public static int GetSymbolicHash(string a)
        {
            return a.GetHashCode();
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

        public static string HopHeyCharArray(char[] a)
        {
            return new string(a);
        }
    }
}
