using System.Text.RegularExpressions;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    public static class RegExImplementation
    {
        public static bool MatchStar(char c, string re, int repos, string text, int textpos)
        {
            do
            {
                if (MatchHere(re, repos, text, textpos))
                {
                    return true;
                }
            } while (textpos < text.Length && (text[textpos++] == c || c == '.'));

            return false;
        }

        public static bool MatchHere(string re, int repos, string text, int textpos)
        {
            if (repos >= re.Length)
                return textpos >= text.Length;
            if (repos + 1 < re.Length && re[repos + 1] == '*')
                return MatchStar(re[repos], re, repos + 2, text, textpos);
            if (re[repos] == '$' && repos + 1 >= re.Length)
                return textpos >= text.Length;
            if (textpos < text.Length && (re[repos] == '.' || re[repos] == text[textpos]))
                return MatchHere(re, repos + 1, text, textpos + 1);
            return false;
        }

        public static bool Match(string re, string text)
        {
            if (re[0] == '^')
                return MatchHere(re, 1, text, 0);
            int textpos = 0;
            do
            {
                if (MatchHere(re, 0, text, textpos))
                    return true;
            } while (textpos++ < text.Length);

            return false;
        }
    }

    [TestSvmFixture]
    public class RegExTest
    {
        [TestSvm(100)]
        public static string OwnImplementationTest(char c1, char c2, char c3, char c4, char c5, char c6)
        {
            string pattern = new string(new char[] {c1, c2, c3});
            string result = "";
            if (RegExImplementation.Match(pattern, "hello"))
            {
                result += "hello";
                if (!RegExImplementation.Match(pattern, "world"))
                {
                    result += " world";
                }
            }
            else
            {
                if (!RegExImplementation.Match(pattern, "nothing"))
                {
                    result += " nothing";
                }
            }

            return result;
        }

        [TestSvm(100)]
        public static bool OwnImplementationTest2(char c1, char c2, char c3, char c4, char c5, char c6)
        {
            string text = new string(new char[] {c1, c2, c3});
            return RegExImplementation.Match("kek", text);
        }

        public static int OwnImplementationTest3(char c1, char c2, char c3, char c4, char c5, char c6)
        {
            string text = new string(new char[] {c1, c2, c3});
            string text2 = new string(new char[] {c4, c5, c6});
            if (RegExImplementation.Match("yes", text))
            {
                if (RegExImplementation.Match (text2, text))
                {
                    return 42;
                }
                else
                {
                    return 100;
                }
            }

            return 0;
        }

        [TestSvm(100)]
        public static bool OwnImplementationTest4(string pattern)
        {
            return RegExImplementation.Match(pattern, "Hello");
        }

        // [TestSvm(100)]
        [Ignore("need more external method implementations")]
        public static MatchCollection SmallSystemImplementationTest()
        {
            Regex rx = new Regex(@"\b",
                RegexOptions.Compiled | RegexOptions.IgnoreCase);
            string text = "fox  ";
            MatchCollection matches = rx.Matches(text);
            return matches;
        }
    }
}
