namespace VSharp.CSharpUtils.Tests
{
    public sealed class Arithmetics
    {
        // 7 + n
        public static int ArithmeticsMethod1(int n, int m)
        {
            return -((n - m) + (m - n) + (1 + m + 2 + 0 - m + 4 + m) - (m + n)) + 14 + (n * (5 - 4) + (5 - 7 + m / m) * n) / m;
        }

        // 0
        public static int ArithmeticsMethod2(int a, int b)
        {
            return a + b - a - b;
        }

        // c - 11
        public static int ArithmeticsMethod3(int a, int b, int c)
        {
            return (a + b + c) - 1 * (a + b + 8) - 3;
        }

        // 6*n - 126826
        public static int ArithmeticsMethod4(int n, int m)
        {
            return (n + n + n + n + n + n - 2312) + m * m * m / (2 * n - n + 3 * n - 4 * n + m * m * m) - 124515;
        }

        // Expecting true
        public static bool IncrementsWorkCorrect(int x)
        {
            int xorig = x;
            x = x++;
            int x1 = x;
            x++;
            int x2 = x;
            x = ++x;
            int x3 = x;
            ++x;
            int x4 = x;
            return x1 == xorig & x2 == xorig + 1 & x3 == xorig + 2 & x4 == xorig + 3;
        }

        // Expecting true
        public static bool Decreasing(int x)
        {
            int x1 = x + 1;
            int x2 = x + 2;
            return x2 - x1 == 1;
        }

        public static int CheckedUnchecked(int x0, int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8, int x9)
        {
            return checked(x0 + unchecked(x1 + checked(x2 + x3 + x4)) + unchecked(x5 - x6 * x7));
        }

        // log(x)
        public static double LogMethod1(double x)
        {
            return System.Math.Log(x);
        }

        // log(x + y)
        public static double LogMethod2(double x, double y)
        {
            return System.Math.Log(x + y);
        }

        // 0
        public static double LogMethod3()
        {
            return System.Math.Log(1);
        }

        // log(1 + log(x))
        public static double LogMethod4(double x)
        {
            return System.Math.Log(1 + System.Math.Log(x));
        }

        // -Infinity
        public static double LogMethod5()
        {
            return System.Math.Log(0);
        }

        // NaN
        public static double LogMethod6()
        {
            return System.Math.Log(-1);
        }

        public static double LogMethod7(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return System.Math.Log(y);
        }

        // sqrt(x)
        public static double SqrtMethod1(double x)
        {
            return System.Math.Sqrt(x);
        }

        // 2
        public static double SqrtMethod2()
        {
            return System.Math.Sqrt(4);
        }

        public static double SqrtMethod3(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return System.Math.Sqrt(y);
        }

        // NaN
        public static double SqrtMethod4()
        {
            return System.Math.Sqrt(-1);
        }

        // 1
        public static double ExpMethod1()
        {
            return System.Math.Exp(0);
        }

        // exp(x)
        public static double ExpMethod2(double x)
        {
            return System.Math.Exp(x);
        }

        // 1
        public static double PowMethod1(double x)
        {
            return System.Math.Pow(x, 0);
        }

        // 1
        public static double PowMethod2(double x)
        {
            return System.Math.Pow(1, x);
        }

        // 25
        public static double PowMethod3()
        {
            return System.Math.Pow(5, 2);
        }

        //pow(x, y)
        public static double PowMethod4(double x, double y)
        {
            return System.Math.Pow(x, y);
        }

        public static double PowMethod5(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return System.Math.Pow(y, 2);
        }

        public static double PowMethod6(double x)
        {
            double y;
            double z;
            if (x >= 0) y = x;
            else y = -x;
            if (x >= 8) z = y;
            else z = x;
            return System.Math.Pow(y, z);
        }

        // x + y
        public static double PowMethod7(double x, double y)
        {
            return System.Math.Pow(x + y, 1);
        }
    }
}
