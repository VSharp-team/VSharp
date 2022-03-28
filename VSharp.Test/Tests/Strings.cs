using NUnit.Framework;
using System;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public sealed class Strings
    {
        [TestSvm]
        public static int ConcreteGetLength()
        {
            return "empty".Length;
        }

        [TestSvm]
        public static int SymbolicGetLength(string s)
        {
            return s.Length;
        }

        // Expecting HeapRef on empty string
        [TestSvm]
        public static string EmptyString(int n, int m)
        {
            return String.Empty;
        }

        [TestSvm]
        public static string SymbolicString(string s)
        {
            var len = s.Length;
            return s;
        }

        [TestSvm]
        public static int NullLength()
        {
            string s = null;
            return s.Length;
        }

        [TestSvm(100)]
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

        [TestSvm(75)]
        public static bool StringOfReplicatedChar()
        {
            string str = new string('s', 20);
            if (str[19] == 's')
                return true;
            return false;
        }

        [TestSvm(100)]
        public static bool SymbolicStringCharEq(string s)
        {
            return s[1] == 'A';
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

        [TestSvm]
        public static bool ConcreteStringToUpper()
        {
            string str = "C7";
            string upper = str.ToUpperInvariant();
            return upper == str;
        }

        [Ignore("constraints solving takes too much time")]
        public static bool SymbolicStringToUpper(char c)
        {
            string s = c + "c";
            return s.ToUpperInvariant()[1] == 'C';
        }
    }
}
