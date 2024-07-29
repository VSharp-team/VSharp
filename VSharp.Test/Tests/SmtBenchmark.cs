using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public sealed class SmtBenchmark
    {
        [TestSvm]
        public static int[] PrefixFunction(string s)
        {
            if (s.Length > 10)
            {
                return new int[0];
            }

            int[] p = new int[s.Length];
            p[0] = 0;

            for (int i = 1; i < s.Length; i++)
            {
                int k = p[i - 1];

                while (k > 0 && s[i] != s[k])
                    k = p[k - 1];

                if (s[i] == s[k])
                    k++;

                p[i] = k;
            }

            return p;
        }

        [TestSvm]
        public static bool ContainsSubstring(string source, string pattern)
        {
            string text = $"{source}#{pattern}";
            int[] p = PrefixFunction(text);

            for (int i = source.Length; i < p.Length; i++)
            {
                if (p[i] == pattern.Length)
                {
                    return true;
                }
            }

            return false;
        }
    }
}