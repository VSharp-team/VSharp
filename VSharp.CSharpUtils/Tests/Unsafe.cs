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

        public static void StackAllocInitialize()
        {
            var s = stackalloc int[30];

            for (int i = 0; i < 3; i++)
            {
                var p = stackalloc char[20];
//                var c = *p;
            }

//            return *s; // for debug
        }

        public static void PtrDifference()
        {
            int x = 42;
            int y = 37;
            int* p = &x;
            int* q = &y;
            long d = p - q;
        }
    }
}
