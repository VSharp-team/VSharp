using System;
using System.Text;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
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

        [TestSvm(81)]
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
        public static bool ConcatStrings(string s)
        {
            string str = "Some string";
            string newStr = s + str;
            return s == newStr[.. s.Length];
        }

        [TestSvm]
        public static int ConcatStrings1(string s)
        {
            string str = "Some string";
            string newStr = s + str;
            if (s.Length < 5 && s != newStr[.. s.Length])
                return -1;
            return 0;
        }

        [TestSvm]
        public static int ConcatStrings2(char c)
        {
            string str = "Some string";
            string newStr = c + str;
            if (newStr[0] != c)
                return -1;
            return 0;
        }

        [TestSvm]
        public static bool ConcreteStringToUpper()
        {
            string str = "C7";
            string upper = str.ToUpperInvariant();
            return upper == str;
        }

        [TestSvm(100)]
        public static int Contains(string str)
        {
            if (str.Contains("d8"))
                return 1;
            return 0;
        }

        [Ignore("takes too much time")]
        public static bool SymbolicStringToUpper(char c)
        {
            string s = c + "c";
            return s.ToUpperInvariant()[1] == 'C';
        }

        public enum Kind
        {
            First,
            Second,
            Third
        }

        public class ClassToString
        {
            public int X;
            public int Y;
            public Kind Kind;
        }

        [Ignore("need to fix Enum.ToString")]
        public static string StringFormat(ClassToString c)
        {
            return string.Format("{0}{1}{2}", c.Kind, c.X, c.Y);
        }

        [TestSvm(100, strat: SearchStrategy.ExecutionTreeContributedCoverage, randomSeed: 10)]
        public static string FormatUInt32(UInt32 x)
        {
            var ret = new StringBuilder();
            ret.Append($"{x:X4}");
            return ret.ToString();
        }

        [TestSvm(100)]
        public static string FormatInt(int x)
        {
            if (x > 10)
                return string.Format("int > 10, int = {0}", x);
            return string.Format("int <= 10, int = {0}", x);
        }

        [TestSvm(100)]
        public static string FormatInt1(int x)
        {
            return $"int = {x}";
        }

        [Ignore("fix composition with concrete memory regions")]
        public int LengthOfIntToString(int x)
        {
            if (x == 0)
                return x.ToString().Length;
            if (x > 0)
                return x.ToString().Length;
            return x.ToString().Length;
        }

        [TestSvm(100)]
        public static string StringFormat1(ClassToString c)
        {
            return string.Format("{0}: {1}, {2}: {3}, {4}: {5}", "Kind", c.Kind, "X", c.X, "Y", c.Y);
        }

        [TestSvm(100, strat: SearchStrategy.ShortestDistance)]
        public static string StringFormat2(ClassToString c)
        {
            if (c.Kind == Kind.First)
                return $"Kind: {c.Kind}, X: {c.X}, Y: {c.Y}";
            return $"{"Kind"}: {c.Kind}, {"X"}: {c.X + 20}, {"Y"}: {c.Y + 10}";
        }
    }
}
