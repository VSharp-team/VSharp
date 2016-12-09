//using System.Windows.Forms;

namespace VSharp.CSharpUtils.Tests
{
    public class Arithmetics
    {
        public static int Add7(int n, int m)
        {
            // var frm = new Form {Name = "Hello"};
            return -((n - m) + (m - n) + (1 + m + 2 + 0 - m + 4 + m) - (m + n)) + 14 + (n * (5 - 4) + (5 - 7 + m/m) * n) / m;
        }
    }
}