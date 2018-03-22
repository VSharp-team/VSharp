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
}
