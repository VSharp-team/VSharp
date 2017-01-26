using System;

namespace VSharp.CSharpUtils.Tests
{
    public static class Branching
    {
        public static int Max3(int x, int y, int z)
        {
            return Math.Max(x, Math.Max(y, z));
        }

        public static bool IsMaxOdd(int x, int y, int z)
        {
            return Max3(2 * x, 2 * y, z)%2 == 0;
        }
    }
}
