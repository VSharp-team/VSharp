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
        public static string SymbolicString(string s)
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

        [TestSvm]
        public static string StringOfConcreteCharArray()
        {
            char[] a = new char[5] {'a', 'b', 'c', 'd', 'e'};
            string str = new string(a);
            return str;
        }

        [TestSvm]
        public static char GetCharOfString()
        {
            char[] a = new char[5] {'a', 'b', 'c', 'd', 'e'};
            string str = new string(a);
            return str[3];
        }

        [TestSvm]
        public static int StringOfNullCharArray()
        {
            char[] a = null;
            string str = new string(a);
            return str.Length;
        }
    }
}
