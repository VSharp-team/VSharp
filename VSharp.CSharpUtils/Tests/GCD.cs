namespace VSharp.CSharpUtils.Tests
{
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
}
