//using System.Windows.Forms;

namespace VSharp.CSharpUtils.Tests
{
    public class Arithmetics
    {
        public static int Add7(int n, int m)
        {
            // var frm = new Form {Name = "Hello"};
            return -((n - m) + (m - n) + (1 + m + 2 + 0 - m + 4 + m) - (m + n)) + 14 + (n * (5 - 4) + (5 - 7 + m / m) * n) / m;
        }

        // 0
        public static int ArithmeticsMethod1(int a, int b)
        {
            return a + b - a - b;
        }

        // -a -b +c -19
        public static int ArithmeticsMethod2(int a, int b, int c)
        {
            return (a + b + c) - 2 * (a + b + 8) - 3;
        }


        // 6*n - 126826
        public static int SomeShit1(int n, int m)
        {
            return (n + n + n + n + n + n - 2312) + m * m * m / (2 * n - n + 3 * n - 4 * n + m * m * m) - 124515;
        }
    }
}