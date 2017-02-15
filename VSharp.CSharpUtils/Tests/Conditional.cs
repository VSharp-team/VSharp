using System;

namespace VSharp.CSharpUtils.Tests
{
    public static class Conditional
    {
        private static int Max3(int x, int y, int z)
        {
            return Math.Max(x, Math.Max(y, z));
        }

        //public static bool IsMaxEven(int x, int y, int z)
        //{
        //    return Max3(2 * x, 2 * y, z)%2 == 0;
        //}

        private static int Mult2(int n)
        {
            int result = 0;
            for (int i = 0; i < n; ++i)
                result += 2;
            return result;
        }

        public static int Always18()
        {
            return Mult2(9);
        }
    }
}
