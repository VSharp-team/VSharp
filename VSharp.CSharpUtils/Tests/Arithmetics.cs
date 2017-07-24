using System;

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

        private static int CheckOverflow0(int x0, int x1)
        {
            return checked (2147483620 + x0 + 2147483620) + x1;
        }

        // Expecting overflow error
        public static int CheckOverflow1(int x1)
        {
            return CheckOverflow0(2147483620, 2147483620 + x1);
        }

        // Expecting overflow error
        public static int CheckOverflow2(int x1)
        {
            int x = 1000 * 1000 * 1000;
            int y = x;
            return checked ((x + y) * 2);
        }

        // Expecting Infinity + x1
        public static double CheckOverflow3(double x1)
        {
            double x = 1.5E+308;
            return checked ((x / 0.01) + x1);
        }

        // Expecting devide by zero error
        public static int CheckDivideByZeroException0(int x1)
        {
            int x = 255;
            int y = 0;
            return (x / y + x1);
        }

        // log(x)
        public static double LogMethod1(double x)
        {
            return Math.Log(x);
        }

        // log(x + y)
        public static double LogMethod2(double x, double y)
        {
            return Math.Log(x + y);
        }

        // 0
        public static double LogMethod3()
        {
            return Math.Log(1);
        }

        // log(1 + log(x))
        public static double LogMethod4(double x)
        {
            return Math.Log(1 + Math.Log(x));
        }

        // -Infinity
        public static double LogMethod5()
        {
            return Math.Log(0);
        }

        // NaN
        public static double LogMethod6()
        {
            return Math.Log(-1);
        }

        public static double LogMethod7(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Log(y);
        }

        // sqrt(x)
        public static double SqrtMethod1(double x)
        {
            return Math.Sqrt(x);
        }

        // 2
        public static double SqrtMethod2()
        {
            return Math.Sqrt(4);
        }

        public static double SqrtMethod3(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Sqrt(y);
        }

        // NaN
        public static double SqrtMethod4()
        {
            return Math.Sqrt(-1);
        }

        // 1
        public static double ExpMethod1()
        {
            return Math.Exp(0);
        }

        // exp(x)
        public static double ExpMethod2(double x)
        {
            return Math.Exp(x);
        }

        // 1
        public static double PowMethod1(double x)
        {
            return Math.Pow(x, Math.Log(1));
        }

        // 1
        public static double PowMethod2(double x)
        {
            return Math.Pow(1, x);
        }

        // 25
        public static double PowMethod3()
        {
            return Math.Pow(5, 2);
        }

        //pow(x, y)
        public static double PowMethod4(double x, double y)
        {
            return Math.Pow(x, y);
        }

        public static double PowMethod5(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Pow(y, 2);
        }

        public static double PowMethod6(double x)
        {
            double y;
            double z;
            if (x >= 0) y = x;
            else y = -x;
            if (x >= 8) z = y;
            else z = x;
            return Math.Pow(y, z);
        }

        // x + y
        public static double PowMethod7(double x, double y)
        {
            return Math.Pow(x + y, 1);
        }

        // pow(2, x)
        public static double PowMethod8(double x)
        {
            return Math.Pow(2, x);
        }

        // NaN
        public static double PowMethod9(double x)
        {
            return Math.Pow(Double.NaN, x);
        }

        // NaN
        public static double PowMethod10(double x, double y)
        {
            return Math.Pow(x, Math.Log(-1));
        }

        // 0
        public static double PowMethod11(double x)
        {
            return Math.Pow(0, x);
        }

        // 1
        public static double PowMethod12()
        {
            return Math.Pow(Double.PositiveInfinity, 0);
        }

        // 0
        public static double PowMethod13()
        {
            return Math.Pow(0, Double.PositiveInfinity);
        }

        // Infinity
        public static double PowMethod14(double x)
        {
            return Math.Pow(Double.PositiveInfinity, 5);
        }

        public static double PowMethod15(double x, double y)
        {
            return Math.Pow(Double.PositiveInfinity, x + y);
        }

        public static double PowMethod16(double x)
        {
            return Math.Pow(x, Double.NegativeInfinity);
        }

        // -Infinity
        public static double PowMethod17()
        {
            return Math.Pow(Double.NegativeInfinity, 9);
        }

        // 0
        public static double AcosMethod()
        {
            return Math.Acos(1);
        }

        // arcsin(x)
        public static double AsinMethod(double x)
        {
            return Math.Asin(x);
        }

        // arctan(x - y)
        public static double AtanMethod(double x, double y)
        {
            return Math.Atan(x - y);
        }

        // ceiling(x)
        public static double CeilingMethod(double x)
        {
            return Math.Ceiling(x);
        }

        // NaN
        public static double CosMethod()
        {
            return Math.Cos(Double.NaN);
        }

        // NaN
        public static double CoshMethod()
        {
            return Math.Cosh(Math.Log(0));
        }

        // floor(x)
        public static double FloorMethod(double x)
        {
            return Math.Floor(x);
        }

        // NaN
        public static double SinMethod()
        {
            return Math.Sin(Double.PositiveInfinity);
        }

        // NaN
        public static double TanMethod()
        {
            return Math.Tan(Double.NegativeInfinity);
        }

        public static double SinhMethod(double x)
        {
            double y;
            if (x > 0)
            {
                y = x;
            }
            else
            {
                y = -x;
            }

            return Math.Sinh(y);
        }

        // tanh(x)
        public static double TanhMethod(double x)
        {
            return Math.Tanh(x);
        }

        // 7
        public static double RoundMethod()
        {
            return Math.Round(6.7);
        }

        // abs(x)
        public static double AbsMethod(double x)
        {
            return Math.Abs(x);
        }

        // 5.9
        public static float AbsSingleMethod()
        {
            return Math.Abs(Convert.ToSingle(-5.9));
        }

        // NaN
        public static double Atan2Method1(double x)
        {
            return Math.Atan2(x, Double.NaN);
        }

        // NaN
        public static double Atan2Method2(double x)
        {
            return Math.Atan2(Double.PositiveInfinity, x);
        }

        // 0
        public static double Atan2Method3()
        {
            return Math.Atan2(1, Double.PositiveInfinity);
        }
    }
}
