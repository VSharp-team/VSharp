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
    }
}
