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

        public static int Fib7()
        {
            return FibRec(7);
        }
    }
}
