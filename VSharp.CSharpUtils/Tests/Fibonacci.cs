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

//        private static int a = 0;
//        private static int b = 0;
//        private static int c = 0;
//
//        private static void MutatingFib(int n)
//        {
//            if (n >= 2)
//            {
//                MutatingFib(n-1);
//                int c = a + b;
//                a = b;
//                b = c;
//            }
//            else
//            {
//                a = 1;
//                b = 1;
//            }
//        }
//
//        public static int FibUnbound(int n)
//        {
//            c = 42;
//            MutatingFib(n);
//            MutatingFib(n + 1);
//            MutatingFib(n + 2);
//            return a + b + c;
//        }
    }
}
