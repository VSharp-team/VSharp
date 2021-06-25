using System.Text.RegularExpressions;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    public static class RegExImplementation
    {
        private static bool MatchStar(char c, string re, int repos, string text, int textpos)
        {
            do
            {
                if (MatchHere(re, repos, text, textpos))
                {
                    return true;
                }
            } while (textpos <= text.Length && (text[textpos++] == c || c == '.'));

            return false;
        }

        private static bool MatchHere(string re, int repos, string text, int textpos)
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
        [TestSvm]
        public static bool OwnImplementationTest(char c1, char c2, char c3, char c4, char c5, char c6)
        {
            string pattern = new string(new char[] {c1, c2, c3});
            return RegExImplementation.Match(pattern, "hello");
        }

        [Ignore("need more external method implementations")]
        public static MatchCollection SystemImplementationTest()
        {
            Regex rx = new Regex(@"\b(?<word>\w+)\s+(\k<word>)\b",
                RegexOptions.Compiled | RegexOptions.IgnoreCase);
            string text = "The the quick brown fox  fox jumps over the lazy dog dog.";
            MatchCollection matches = rx.Matches(text);
            return matches;
        }

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
