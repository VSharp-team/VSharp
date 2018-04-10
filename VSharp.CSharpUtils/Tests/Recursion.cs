using System;

namespace VSharp.CSharpUtils.Tests
{
    public static class Fibonacci
    {
        private static int FibRec(int n)
        {
            if (n < 2)
                return 1;
            return FibRec(n - 1) + FibRec(n - 2);
        }

        public static int Fib2()
        {
            return FibRec(2);
        }

        public static int Fib5()
        {
            return FibRec(5);
        }

        private static int _a;
        private static int _b;
        private static int _c;

        private static void MutatingFib(int n)
        {
            if (n >= 2)
            {
                MutatingFib(n-1);
                int c = _a + _b;
                _a = _b;
                _b = c;
            }
            else
            {
                _a = 1;
                _b = 1;
            }
        }

        public static int FibUnbound(int n)
        {
            _c = 42;
            MutatingFib(n);
            return _a + _b + _c;
        }

    }

    public static class GCD
    {
        private static int GcdRec(int n, int m)
        {
            if (n > m)
                return GcdRec(m, n);
            if (n == 0)
                return m;
            return GcdRec(n, m - n);
        }

        public static int Gcd1()
        {
            return GcdRec(15, 4);
        }

        public static int Gcd15()
        {
            return GcdRec(30, 75);
        }
    }

    public static class McCarthy91
    {
        public static int McCarthy(int n)
        {
            return n > 100 ? n - 10 : McCarthy(McCarthy(n + 11));
        }

        public static void CheckMc91Safe(int x)
        {
            if (x <= 96 && McCarthy(x + 5) != 91)
            {
                throw new Exception();
            }
        }

        public static void CheckMc91Unsafe(int x)
        {
            if (x <= 97 && McCarthy(x + 5) != 91)
            {
                throw new Exception();
            }
        }
    }
}
