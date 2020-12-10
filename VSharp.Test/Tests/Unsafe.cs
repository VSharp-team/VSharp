using System;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public unsafe class Unsafe
    {
        [TestSvm]
        public static int ChangeThroughIndirection()
        {
            int x = 42;
            int z = 14;
            *&x = *&z;
            return x; // 14
        }

        [TestSvm]
        public static int CharSizeOf()
        {
            return sizeof(char); // sizeof() = 2; Marshal.SizeOf() = 1; we should be 2
        }

        struct FixedSizedBuffer
        {
            public fixed char buf[20];
            public fixed bool bufs[29];
        }

        [TestSvm]
        public static int StrangeSizeOf()
        {
            return sizeof(FixedSizedBuffer); // sizeof() = 70; Marshal.SizeOf() = 72; we should behave like sizeof()
        }

        [TestSvm]
        public static int ReturnConst()
        {
            int x = 421234123;
            return *&x;
        }

        [TestSvm]
        public static int DoubleIndirection()
        {
            int x = 428999;
            int* p = &x;
            return **&p;
        }

        [Ignore("Insufficient information")]
        public static int ReturnIntFromIntPtr(int myFavouriteParameter)
        {
            var s = new IntPtr(&myFavouriteParameter);
            return *(int*) s.ToPointer();
        }

        [Ignore("Insufficient information")]
        public static void* CompilerHackLikePtrReturn(void* ptr)
        {
            var x = (IntPtr) ptr;
            return x.ToPointer();
        }

        // [Ignore("Not working, because of (p - q) might not be multiple of 8 #fix")]
        [TestSvm]
        public static int SimplePointerDifference(int x, double y)
        {
            int* p = &x;
            double* q = &y;
            long d = (double*) p - q;

            return * (int*) (q + d);
        }

        // [Ignore("Not working, because the idea of test is not right #fix")]
        [TestSvm]
        public static int PointerTriangle(int x, int y, int z)
        {
            int* px = &x;
            int* py = &y;
            int* pz = &z;

            long d1 = px - py;
            long d2 = py - pz;

            int* r = pz + d1 + d2;

            return *r; // x
        }
    }
}
