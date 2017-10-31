using System;

namespace VSharp.CSharpUtils.Tests
{
    public unsafe class Unsafe
    {
        public static int ChangeThroughIndirection()
        {
            int x = 42;
            int z = 14;
            *&x = *&z;
            return x; // 14
        }
    }
}
