using System;

namespace VSharp.CSharpUtils.Tests
{
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

        //public static int Always18()
        //{
        //    return Mult2(9);
        //}

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
    }
}
