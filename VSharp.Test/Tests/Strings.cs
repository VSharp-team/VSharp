using System;
using System.Collections;
using System.IO;
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

        [TestSvm]
        public static int CheckLength(string s)
        {
            if (s.Length < 5)
                return s[s.Length + 2];
            return 1;
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

        [TestSvm(100)]
        public static int StringEquals(string str1, string str2)
        {
            if (str1.Equals(str2))
                return 1;
            return -1;
        }

        [TestSvm(100)]
        public static int StringEquals1(string str1, string str2, StringComparison c)
        {
            if (str1.Equals(str2, c))
                return 1;
            return -1;
        }

        [TestSvm(100)]
        public static int StringEquals2(string str1, string str2, StringComparison c)
        {
            if (MemoryExtensions.Equals(str1, str2, c))
                return 1;
            return -1;
        }

        [TestSvm(83)]
        public static int Substring(string s)
        {
            if (s == s[.. s.Length])
                return 1;
            return -1;
        }

        [TestSvm(100)]
        public static int Substring2(string s1, string s2)
        {
            if (s2 == s1[.. s1.Length])
                return 1;
            return 0;
        }

        [TestSvm(100)]
        public static string Substring3(string[] args, int i)
        {
            var str = args[i].Trim();
            if (string.IsNullOrEmpty(str)) return string.Empty;

            var first = str[0];
            str = str.Substring(1);
            if (first != '-' && first != '/')
                return str;

            str = str.Substring(1);
            if (!string.IsNullOrEmpty(str) && str[0] == first && first == '-')
                str = str.Substring(1);
            return str;
        }

        [TestSvm(88)]
        public static int ConcatStrings(string s)
        {
            string str = "Some string";
            string newStr = s + str;
            if (s == newStr[.. s.Length])
                return 1;
            return -1;
        }

        [TestSvm(90)]
        public static int ConcatStrings1(string s)
        {
            string str = "Some string";
            string newStr = s + str;
            if (s.Length < 5 && s != newStr[.. s.Length])
                return -1;
            return 0;
        }

        [TestSvm(85)]
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

        [TestSvm(100)]
        public static int StartsWith(string str)
        {
            if (str.StartsWith("d8"))
                return 1;
            return 0;
        }

        [TestSvm(100)]
        public static int StartsWith1(string str1, string str2)
        {
            if (str1.StartsWith(str2))
            {
                if (str2.Length > 0)
                {
                    if (str1 != str2)
                        return 3;
                    return 2;
                }

                return 1;
            }

            return 0;
        }

        [TestSvm(100)]
        public static int WriteLineObject(string s)
        {
            var nre = new NullReferenceException(s);
            Console.WriteLine(nre);
            return 1;
        }

        [TestSvm(100)]
        public static int StreamReaderWriter(string s)
        {
            var stream = new MemoryStream();
            var writer = new StreamWriter(stream);
            writer.Write(s);
            writer.Flush();
            stream.Position = 0;
            var reader = new StreamReader(stream);
            if (reader.ReadToEnd() != s)
                return -1;
            return 1;
        }

        [Ignore("takes too much time")]
        public static int StreamReaderWriter1(string s)
        {
            var stream = new MemoryStream();
            var writer = new StreamWriter(stream);
            writer.Write(s);
            writer.Flush();
            stream.Position = 0;
            var reader = new StreamReader(stream);
            if (reader.ReadToEnd() != s && s != null)
                return -1;
            return 1;
        }

        [TestSvm(100)]
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

        [TestSvm(100)]
        public static string StringFormat(ClassToString c)
        {
            return string.Format("{0}{1}{2}", c.Kind, c.X, c.Y);
        }

        [Ignore("takes too much time")]
        public static Kind EnumTryParse(string s)
        {
            if (Enum.TryParse(typeof(Kind), s, out var res))
                return (Kind)res;

            throw new ArgumentException("invalid string");
        }

        [TestSvm(70)]
        public static Kind EnumTryParse1(Kind e)
        {
            if (Enum.TryParse(typeof(Kind), e.ToString(), out var res))
            {
                var enumResult = (Kind) res;
                if (enumResult != e)
                    throw new ArgumentException("invalid enum");

                return enumResult;
            }

            throw new ArgumentException("invalid string");
        }
        [TestSvm(75)]
        public static Kind ConcreteEnumTryParse()
        {
            if (Enum.TryParse(typeof(Kind), "First", out var res))
                return (Kind)res;

            throw new ArgumentException("invalid string");
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

        [Ignore("takes too much time")]
        public static string StringRegex(string str1, string str2)
        {
            var result = System.Text.RegularExpressions.Regex.Match(str1, str2);
            return result.Value;
        }

        [TestSvm(100, strat: SearchStrategy.ExecutionTreeContributedCoverage)]
        public static string StringRegex1(string str)
        {
            var result = System.Text.RegularExpressions.Regex.Match(str, ".*");
            return result.Value;
        }

        [Ignore("takes too much time")]
        public static int StringRegex2(string str)
        {
            var result = System.Text.RegularExpressions.Regex.Match(str, ".*");
            if (result.Value != str)
                return -1;
            return 1;
        }

        [TestSvm(100, strat: SearchStrategy.ExecutionTreeContributedCoverage)]
        public static string StringRegex3(string str)
        {
            if (string.IsNullOrEmpty(str))
                return string.Empty;
            var result = System.Text.RegularExpressions.Regex.Match(str, @"^(?<major>\d+)(\.(?<minor>\d+))?(\.(?<patch>\d+))?$");
            return result.Groups["major"].Value + result.Groups["minor"].Value + result.Groups["patch"].Value;
        }

        [TestSvm(100)]
        public static string StringRegex4()
        {
            var input = "7.3.7";
            var result = System.Text.RegularExpressions.Regex.Match(input, @"^(?<major>\d+)(\.(?<minor>\d+))?(\.(?<patch>\d+))?$");
            return result.Groups["major"].Value + result.Groups["minor"].Value + result.Groups["patch"].Value;
        }

        [TestSvm(100)]
        public static string StringRegex5()
        {
            var input = "kek";
            var result = System.Text.RegularExpressions.Regex.Match(input, "(?<fst>)");
            return result.Groups["fst"].Value;
        }

        [TestSvm(100)]
        public static bool StringHashtable()
        {
            var table = new Hashtable { { "fst", 1 } };
            return table.ContainsKey("fst");
        }

        [Ignore("fix strings comparison")]
        public static int StringGetHashCode(string str)
        {
            if (str == "abc")
            {
                if (str.GetHashCode() == "abc".GetHashCode())
                    return 1;
                return -1;
            }

            return 0;
        }

        [Ignore("takes too much time")]
        public static int StringGetHashCode1(string str)
        {
            if (str == "abc")
            {
                if (str.GetHashCode() == "abc".GetHashCode())
                    return 1;
                return -1;
            }

            return 0;
        }
        [TestSvm(100, strat: SearchStrategy.ExecutionTreeContributedCoverage)]
        public static int FromUtf16(char[] a)
        {
            var src = new ReadOnlySpan<char>(a);
            var dst = new Span<byte>(new byte[16]);
            System.Text.Unicode.Utf8.FromUtf16(src, dst, out var charsRead, out var bytesWritten);
            if (charsRead > 5 && bytesWritten > 5)
                return 1;
            return bytesWritten;
        }
    }
}
