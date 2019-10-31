using NUnit.Framework;
using System;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public sealed class Strings
    {
        // Expecting HeapRef on empty string
        [TestSvm]
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }

        [Ignore("Exceptions handling")]
        public static string SymbolicString(string s) // TODO: keys of MemoryCell are equal, but FQLs are not
        {
            var len = s.Length;
            return s;
        }

        [Ignore("Exceptions handling")]
        public static int NullLength()
        {
            string s = null;
            return s.Length;
        }

        [TestSvm]
        public static string HopHeyCharArray(char[] a)
        {
            return new string(a);
        }
    }
}
