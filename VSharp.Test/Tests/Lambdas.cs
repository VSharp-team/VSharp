using System;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public static class Lambdas
    {
        private static int Mult2(int n)
        {
            int result = 0;
            {
                Func<int, int> loop = null;
                loop = i =>
                {
                    if (i < n)
                    {
                        result += 2;
                        ++i;
                        return loop(i);
                    }
                    return result;
                };
                loop(0);
            }
            return result;
        }

        // Expecting 18
        [Ignore("reinterpretation")]
        public static int Always18()
        {
            return Mult2(9);
        }

        // Expecting always true
        [TestSvm]
        public static bool DoubleValue(int n, bool flag)
        {
            int a = 0, b = 0, c = 0;
            Action<int> assign;
            if (flag)
                assign = m =>
                {
                    a = m;
                    b = m;
                };
            else
                assign = m =>
                {
                    a = m;
                    c = m;
                };
            assign(n);
            return a + b + c == n + n;
        }

/*        public static int ForToLambdaDelegateSmokeTest(int n)
        {
            int s = 0;
            for (int i = 0; i < n; i++)
            {
                s += i * i;
            }
            return s;
        }

        public static int ForToLambdaDelegateNestedTest(int n)
        {
            int num = 0;
            for (int index1 = 0; index1 < n; ++index1)
            {
                for (int index2 = 0; index2 < index1; ++index2)
                    num += index1 * index2;
            }
            return num;
        }*/
    }
}
