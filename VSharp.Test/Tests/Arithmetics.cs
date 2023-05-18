using System;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    [Ignore("Need exceptions for all tests")]
    public sealed class Arithmetics_CIL
    {
        // [Ignore("unknown result")]
        [TestSvm]
        public static bool MultiplicationOfFloatsIsNotAssociative()
        {
            float a = 0.825402526103613f;
            float b = 0.909231618470155f;
            float c = 0.654626872695343f;
            float d = (a * b) * c;
            float e = a * (b * c);
            return d != e;
        }

        // [Ignore("unknown result")]
        [TestSvm]
        public static bool MultiplicationOfFloatsIsCommutativity()
        {
            float a = 0.825402526103613f;
            float b = 0.909231618470155f;
            return Math.Abs(a * b - b * a) <= Single.Epsilon;
        }

        // Overflow exception
        // [Ignore("Exceptions handling")]
        [TestSvm]
        public static int DivideWithOverflow()
        {
            int a = Int32.MinValue;
            int b = -1;
            return a / b;
        }

        // no exception
        [TestSvm]
        public static uint DivideWithoutOverflow(uint a)
        {
            int x = -1;
            uint y = (uint) x;
            return a / y;
        }

        // NaN
        [TestSvm]
        public static float DivideFloatOnZero(float a)
        {
            return a / 0;
        }

        // divide by zero exception
        // [Ignore("Exceptions handling")]
        [TestSvm]
        public static float DivideOnZero1()
        {
            int x = 8;
            int y = 0;
            return x / y;
        }

        // divide by zero exception
        // [Ignore("Exceptions handling")]
        [TestSvm]
        public static float DivideOnZero2()
        {
            uint x = 8;
            uint y = 0;
            return x / y;
        }

        // no exceptions
        [TestSvm]
        public static int Add(int a, int b)
        {
            return a + b;
        }

        // overflow exceptions possible
        [TestSvm]
        public static int Add_Checked(int a, int b)
        {
            return checked(a + b);
        }

        // no exceptions
        [TestSvm]
        public static uint Add_Unsigned(uint a, uint b)
        {
            return a + b;
        }

        // overflow exceptions possible
        [TestSvm]
        public static uint Add_Ovf_Un(uint a, uint b)
        {
            return checked(a + b);
        }

        // overflow exceptions possible
        [TestSvm]
        public static uint Mul_Ovf_Un(uint a, uint b)
        {
            return checked(a * b);
        }

        // overflow exceptions possible
        [TestSvm]
        public static int Mul_Ovf(int a, int b)
        {
            return checked(a * b);
        }

        [TestSvm]
        public static Int64 Mul_Ovf_64(Int64 a, Int64 b)
        {
            return checked(a * b);
        }

        [TestSvm]
        public static UInt64 Mul_Ovf_U64(UInt64 a, UInt64 b)
        {
            return checked(a * b);
        }

        [TestSvm]
        public static int Mul_No_OverFlow1()
        {
            int a = -1;
            int b = Int32.MaxValue;
            return Mul_Ovf(a, b);
        }

        // [Ignore("Exceptions handling")]
        [TestSvm]
        public static uint Mul_OverFlow1()
        {
            int a = -1;
            uint c = (UInt32) a;
            int b = Int32.MaxValue;
            uint d = (UInt32) b;
            return Mul_Ovf_Un(c, d);
        }

        [TestSvm]
        public static int Sub_Ovf(int a, int b)
        {
            return checked(a - b);
        }

        // [Ignore("Exceptions handling")]
        [TestSvm]
        public static int Sub_Overflow1()
        {
            int a = 0;
            int b = Int32.MinValue;
            return Sub_Ovf(a, b);
        }

        // [Ignore("Exceptions handling")]
        [TestSvm]
        public static int Sub_Overflow2()
        {
            int a = 1;
            int b = Int32.MinValue;
            return Sub_Ovf(a, b);
        }

        // [Ignore("Exceptions handling")]
        [TestSvm]
        public static int Sub_Overflow3()
        {
            int a = Int32.MinValue;
            int b = 1;
            return Sub_Ovf(a, b);
        }

        [TestSvm]
        public static int Sub_No_Overflow1()
        {
            int a = -1;
            int b = Int32.MinValue;
            return Sub_Ovf(a, b);
        }

        [TestSvm]
        public static int Sub_No_Overflow2()
        {
            int a = 0;
            int b = Int16.MinValue;
            return Sub_Ovf(a, b);
        }

        [TestSvm]
        public static int Sub_No_Overflow3()
        {
            int a = Int32.MaxValue;
            int b = Int32.MaxValue;
            return Sub_Ovf(a, b);
        }

        [TestSvm]
        public static int Sub_No_Overflow4()
        {
            int a = Int32.MinValue;
            int b = Int32.MinValue;
            return Sub_Ovf(a, b);
        }

        [TestSvm]
        public static uint Sub_Ovf_Un(uint a, uint b)
        {
            return checked(a - b);
        }

        [Ignore("Bug in Core: it assumes that a - b == a + (-b)")]
        public static uint Sub_Ovf_Un_NoOverflow1()
        {
            return Sub_Ovf_Un(5, 4);
        }

        [Ignore("Exceptions handling")]
        public static uint Sub_Ovf_Un_Overflow1()
        {
            return Sub_Ovf_Un(4, 5);
        }

        // if a = UInt32.MaxValue then a
        // else overflow
        [Ignore("Exceptions handling")]
        public static uint Sub_Ovf_Un_Overflow2(uint a)
        {
            return Sub_Ovf_Un(a, a + 1);
        }

        [TestSvm]
        public static int Add_sbyte_short(sbyte a, short b)
        {
            return checked(a + b);
        }

        [TestSvm]
        public static double Rem_Doubles(double a, double b)
        {
            return a % b;
        }

        [TestSvm]
        public static double Rem_Concrete_Doubles()
        {
            return Rem_Doubles(10.0, 6.0);
        }

        [TestSvm]
        public static double Rem_Always_A(double a)
        {
            return Rem_Doubles(a, double.PositiveInfinity);
        }

        [TestSvm]
        public static double Rem_Concrete_Doubles_0()
        {
            return Rem_Always_A(0.0);
        }

        [TestSvm]
        public static double Rem_Concrete_Double_Nan1()
        {
            return Rem_Always_A(double.PositiveInfinity);
        }

        [TestSvm]
        public static double Rem_Concrete_Double_Nan2()
        {
            return Rem_Always_A(double.NegativeInfinity);
        }

        [TestSvm]
        public static double Rem_Concrete_Double_Nan3()
        {
            return Rem_Doubles(double.MaxValue, 0.0);
        }

        [TestSvm]
        public static int Rem_Ints(int a, int b)
        {
            return a % b;
        }

        [Ignore("Exceptions handling")]
        public static int Rem_Ints_DivideOnZero(int a)
        {
            return Rem_Ints(a, 0);
        }

        [Ignore("Exceptions handling")]
        public static int Rem_Ints_Overflow()
        {
            return Rem_Ints(int.MinValue, -1);
        }

        [TestSvm]
        public static uint RemUn_Ints(uint a, uint b)
        {
            return a % b;
        }

        [Ignore("Exceptions handling")]
        public static uint RemUn_Ints_DivideOnZero(uint a)
        {
            return RemUn_Ints(a, 0);
        }

        [TestSvm]
        public static uint RemUn_Ints_No_Overflow()
        {
            int z = int.MinValue;
            int w = -1;

            uint a = (uint) z;
            uint b = (uint) (w);
            return RemUn_Ints(a, b);
        }
    }

    [TestSvmFixture]
    public sealed class Arithmetics
    {
        // 7 + n
        [Ignore("Exceptions handling")]
        public static int ArithmeticsMethod1(int n, int m)
        {
            return -((n - m) + (m - n) + (1 + m + 2 + 0 - m + 4 + m) - (m + n)) + 14 + (n * (5 - 4) + (5 - 7 + m / m) * n) / m;
        }

        // 0
        [TestSvm]
        public static int ArithmeticsMethod2(int a, int b)
        {
            return a + b - a - b;
        }

        // c - 11
        [TestSvm]
        public static int ArithmeticsMethod3(int a, int b, int c)
        {
            return (a + b + c) - 1 * (a + b + 8) - 3;
        }

        // 6*n - 126826
        [Ignore("Exceptions handling")]
        // [TestSvm]
        public static int ArithmeticsMethod4(int n, int m)
        {
            return (n + n + n + n + n + n - 2312) + m * m * m / (2 * n - n + 3 * n - 4 * n + m * m * m) - 124515;
        }

        // Expecting true
        [TestSvm]
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

        [TestSvm]
        public static int BigSum(int x)
        {
            return x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x;
        }

        [TestSvm]
        public static int SmallBigSum(int x)
        {
            return x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x;
        }

        [TestSvm]
        public static int BigSumCycle(int x)
        {
            int res = 0;
            for (int i = 0; i < 9; i++)
            {
                res += x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x;
            }
            return res;
        }

        // Expecting true
        [TestSvm]
        public static bool Decreasing(int x)
        {
            int x1 = x + 1;
            int x2 = x + 2;
            return x2 - x1 == 1;
        }

        [Ignore("Exceptions handling")]
        public static int CheckedUnchecked(int x0, int x1, int x2, int x3, int x4, int x5, int x6, int x7, int x8, int x9)
        {
            return checked(x0 + unchecked(x1 + checked(x2 + x3 + x4)) + unchecked(x5 - x6 * x7));
        }

        private static int CheckOverflow0(int x0, int x1)
        {
            return checked (2147483620 + x0 + 2147483620) + x1;
        }

        // Expecting overflow error
        [Ignore("Exceptions handling")]
        public static int CheckOverflow1(int x1)
        {
            return CheckOverflow0(2147483620, 2147483620 + x1);
        }

        // Expecting overflow error
        [Ignore("Exceptions handling")]
        public static int CheckOverflow2(int x1)
        {
            int x = 1000 * 1000 * 1000;
            int y = x;
            return checked ((x + y) * 2);
        }

        // Expecting Infinity + x1
        [Ignore("Incorrect result")]
        public static double CheckOverflow3(double x1)
        {
            double x = 1.5E+308;
            return checked ((x / 0.01) + x1);
        }

        // Expecting +Infinity
        [TestSvm]
        public static double CheckOverflow4()
        {
            double x = Double.MaxValue;
            double y = Double.MaxValue;
            return checked (x + y);
        }

        [TestSvm]
        public static long SumOfIntAndUint(int a, uint b)
        {
            return b + a;
        }

        // Expecting +Infinity
        [TestSvm]
        public static long CheckSumOfSingedAndUnsigned()
        {
            int x = 42;
            uint y = UInt32.MaxValue;
            return SumOfIntAndUint(x, y);
        }

        [TestSvm]
        public static long SumOfIntAndShort(int a, short b)
        {
            return b + a;
        }

        // Expecting +Infinity
        [TestSvm]
        public static long CheckSumOfIntAndShort()
        {
            int x = 42;
            short y = Int16.MaxValue;
            return SumOfIntAndShort(x, y);
        }

        // Expecting devide by zero error
        [Ignore("Exceptions handling")]
        public static int CheckDivideByZeroException0(int x1)
        {
            int x = 255;
            int y = 0;
            return (x / y + x1);
        }

        // Expecting 2000000000 + x1 + 2000000000
        [Ignore("Exceptions handling")]
        public static int CheckOrder(int x1)
        {
            int x = 2000000000;
            int y = x;
            return checked (x + x1 + y);
        }

        [Ignore("Exception handling in concolic")]
        public static int DivisionTest1(int a, int b)
        {
            return a / b;
        }

        // Expecting a
        [TestSvm]
        public static int ShiftLeftOnZero(int a)
        {
            return (a << 0) >> 0;
        }

        // Expecting 0.0
        [TestSvm]
        public static double ZeroShift(int a)
        {
            return 0 << a >> a;
        }

        // Expecting a << b
        [TestSvm]
        public static int DefaultShift(int a, int b)
        {
            return a << b;
        }

        // Expecting a << 32
        [TestSvm]
        public static Int64 SumShifts(Int64 a)
        {
            return (a << 31) + (a << 31);
        }

        // Expecting 0
        [TestSvm]
        public static Int32 ShiftSum(Int32 a)
        {
            return (a + a) << 31;
        }

        // Expecting a << 19
        [TestSvm]
        public static Int32 MultiplyOnShift1(Int16 a)
        {
            return (a << 17) * 4;
        }

        // Expecting a << 16
        [TestSvm]
        public static Int32 MultiplyOnShift2(Int16 a)
        {
            return (a << 14) * 4;
        }

        // Expecting 0
        [TestSvm]
        public static Int32 ShiftMultiplication(Int16 a)
        {
            return (a * 512) << 23;
        }

        // Expecting (a >> 20) / 1024
        [TestSvm]
        public static int ShiftDevision1(byte a)
        {
            return (a >> 20) / 1024;
        }

        // Expecting (a / 512) >> 12
        [TestSvm]
        public static int ShiftDevision2(int a)
        {
            return (a / 512) >> 12;
        }

        // Expecting 0
        [TestSvm]
        public static uint ShiftDevision3(uint a)
        {
            return (a >> 22) / 1024;
        }

        // Expecting a >> 41
        [TestSvm]
        public static ulong ShiftDevision4(ulong a)
        {
            return (a >> 31) / 1024;
        }

        [Ignore("Need to implement shr.un correctly")]
        public static uint ShrUn(int a)
        {
            uint b = (uint) a;
            return b >> 1;
        }

        [TestSvm]
        public static uint Shr(int a)
        {
            return (uint)(a >> 1);
        }

        // expecting 4294967295
        [TestSvm]
        public static uint ShrTest()
        {
            int a = -1;
            return Shr(a);
        }

        // expecting 2147483647
        [Ignore("Need to implement shr.un correctly")]
        public static uint ShrUnTest()
        {
            int a = -1;
            return ShrUn(a);
        }

        // Expecting 0
        [TestSvm]
        public static int ShiftSumOfShifts1(int a)
        {
            return ((a << 30) + (a << 30)) << 2;
        }

        // Expecting a << 34
        [TestSvm]
        public static long ShiftSumOfShifts2(long a)
        {
            return ((a << 31) + (a << 31)) << 2;
        }

        // Expecting -2147483648
        [TestSvm]
        public static int ConcreteShift()
        {
            return 2 << 30;
        }

        // Expecting 0
        [TestSvm]
        public static int MultiplyShifts1(int a, int c)
        {
            return ((a + a) << 14) * (c << 17);
        }

        // Expecting (a * c) << 29
        [TestSvm]
        public static int MultiplyShifts2(int a, int c)
        {
            return ((a + a) << 11) * (c << 17);
        }

        // Expecting (a << 6) / 4
        [TestSvm]
        public static int ShiftWithDivAndMul(int a)
        {
            return ((a * 16) << 2) / 4;
        }

        // Expecting (int64)(a >> 15 >> 18)
        [TestSvm]
        public static Int64 DoubleShiftRight(int a)
        {
            return (a >> 15) >> 18;
        }

        // log(x)
        [TestSvm]
        public static double LogMethod1(double x)
        {
            return Math.Log(x);
        }

        // log(x + y)
        [TestSvm]
        public static double LogMethod2(double x, double y)
        {
            return Math.Log(x + y);
        }

        // 0
        [TestSvm]
        public static double LogMethod3()
        {
            return Math.Log(1);
        }

        // log(1 + log(x))
        [TestSvm]
        public static double LogMethod4(double x)
        {
            return Math.Log(1 + Math.Log(x));
        }

        // -Infinity
        [TestSvm]
        public static double LogMethod5()
        {
            return Math.Log(0);
        }

        // NaN
        [TestSvm]
        public static double LogMethod6()
        {
            return Math.Log(-1);
        }

        [TestSvm(75)]
        public static double LogMethod7(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Log(y);
        }

        // sqrt(x)
        [TestSvm]
        public static double SqrtMethod1(double x)
        {
            return Math.Sqrt(x);
        }

        // 2
        [TestSvm]
        public static double SqrtMethod2()
        {
            return Math.Sqrt(4);
        }

        [TestSvm(75)]
        public static double SqrtMethod3(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Sqrt(y);
        }

        // NaN
        [TestSvm]
        public static double SqrtMethod4()
        {
            return Math.Sqrt(-1);
        }

        // 1
        [TestSvm]
        public static double ExpMethod1()
        {
            return Math.Exp(0);
        }

        // exp(x)
        [TestSvm]
        public static double ExpMethod2(double x)
        {
            return Math.Exp(x);
        }

        // 1
        [TestSvm]
        public static double PowMethod1(double x)
        {
            return Math.Pow(x, Math.Log(1));
        }

        // 1
        [TestSvm]
        public static double PowMethod2(double x)
        {
            return Math.Pow(1, x);
        }

        // 25
        [TestSvm]
        public static double PowMethod3()
        {
            return Math.Pow(5, 2);
        }

        //pow(x, y)
        [TestSvm]
        public static double PowMethod4(double x, double y)
        {
            return Math.Pow(x, y);
        }

        [TestSvm(76)]
        public static double PowMethod5(double x)
        {
            double y;

            if(x >= 0) y = x;
            else y = -x;

            return Math.Pow(y, 2);
        }

        [TestSvm(71)]
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
        [TestSvm]
        public static double PowMethod7(double x, double y)
        {
            return Math.Pow(x + y, 1);
        }

        // pow(2, x)
        [TestSvm]
        public static double PowMethod8(double x)
        {
            return Math.Pow(2, x);
        }

        // NaN
        [TestSvm]
        public static double PowMethod9(double x)
        {
            return Math.Pow(Double.NaN, x);
        }

        // NaN
        [TestSvm]
        public static double PowMethod10(double x, double y)
        {
            return Math.Pow(x, Math.Log(-1));
        }

        // 0
        [TestSvm]
        public static double PowMethod11(double x)
        {
            return Math.Pow(0, x);
        }

        // 1
        [TestSvm]
        public static double PowMethod12()
        {
            return Math.Pow(Double.PositiveInfinity, 0);
        }

        // 0
        [TestSvm]
        public static double PowMethod13()
        {
            return Math.Pow(0, Double.PositiveInfinity);
        }

        // Infinity
        [TestSvm]
        public static double PowMethod14(double x)
        {
            return Math.Pow(Double.PositiveInfinity, 5);
        }

        [TestSvm]
        public static double PowMethod15(double x, double y)
        {
            return Math.Pow(Double.PositiveInfinity, x + y);
        }

        [TestSvm]
        public static double PowMethod16(double x)
        {
            return Math.Pow(x, Double.NegativeInfinity);
        }

        // -Infinity
        [TestSvm]
        public static double PowMethod17()
        {
            return Math.Pow(Double.NegativeInfinity, 9);
        }

        // 0
        [TestSvm]
        public static double AcosMethod()
        {
            return Math.Acos(1);
        }

        // arcsin(x)
        [TestSvm]
        public static double AsinMethod(double x)
        {
            return Math.Asin(x);
        }

        // arctan(x - y)
        [TestSvm]
        public static double AtanMethod(double x, double y)
        {
            return Math.Atan(x - y);
        }

        // ceiling(x)
        [TestSvm]
        public static double CeilingMethod(double x)
        {
            return Math.Ceiling(x);
        }

        // NaN
        [TestSvm]
        public static double CosMethod()
        {
            return Math.Cos(Double.NaN);
        }

        // NaN
        [TestSvm]
        public static double CoshMethod()
        {
            return Math.Cosh(Math.Log(0));
        }

        // floor(x)
        [TestSvm]
        public static double FloorMethod(double x)
        {
            return Math.Floor(x);
        }

        // NaN
        [TestSvm]
        public static double SinMethod()
        {
            return Math.Sin(Double.PositiveInfinity);
        }

        // NaN
        [TestSvm]
        public static double TanMethod()
        {
            return Math.Tan(Double.NegativeInfinity);
        }

        [TestSvm(75)]
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
        [TestSvm]
        public static double TanhMethod(double x)
        {
            return Math.Tanh(x);
        }

        // 7
        [TestSvm]
        public static double RoundMethod()
        {
            return Math.Round(6.7);
        }

        // abs(x)
        [TestSvm]
        public static double AbsMethod(double x)
        {
            return Math.Abs(x);
        }

        [TestSvm]
        public static double AbsMethod2(double x)
        {
            double a = 42;
            if (x == 4)
                a = Math.Abs(x);
            return a;
        }

        // 5.9
        [Ignore("GetTypeFromHandle is not implemented #fix")]
        public static float AbsSingleMethod()
        {
            return Math.Abs(Convert.ToSingle(-5.9));
        }

        // NaN
        [TestSvm]
        public static double Atan2Method1(double x)
        {
            return Math.Atan2(x, Double.NaN);
        }

        // NaN
        [TestSvm]
        public static double Atan2Method2(double x)
        {
            return Math.Atan2(Double.PositiveInfinity, x);
        }

        // 0
        [TestSvm]
        public static double Atan2Method3()
        {
            return Math.Atan2(1, Double.PositiveInfinity);
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static void Mult(int x, int y)
        {
            int z = 0;
            while (x > 0)
            {
                z += y;
                x --;
            }

            if (x >= 0 && y >= 0 && z <= 0)
            {
                throw new Exception();
            }
        }

        [TestSvm(100)]
        public static int PossibleBug(int n) {
            if (n <= 0 && -n < 0) {
                throw new Exception("Possible Impossible bug");
            }
            return 42;
        }

        [TestSvm(85)]
        public static int ImpossibleBug(int n) {
            try {
                if (n <= 0 && checked(-n) < 0) {
                    throw new Exception("Possible Impossible bug");
                }
            } catch (OverflowException) {
                return 100;
            }

            return 42;
        }

        [TestSvm(100)]
        public static int DecimalTest(decimal sum)
        {
            return sum switch
            {
                <= 1_000_000 => 12,
                <= 5_000_000 => 14,
                <= 10_000_000 => 8,
                _ => throw new ArgumentOutOfRangeException()
            };
        }
    }
}
