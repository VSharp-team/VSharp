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

        [TestSvm]
        public static IntPtr IntPtrZero()
        {
            return IntPtr.Zero;
        }

        [TestSvm]
        public static bool CreateIntPtrAndCheckEquals()
        {
            IntPtr ptr1 = new IntPtr(0);
            IntPtr ptr2 = new IntPtr(null);
            return ptr1 == ptr2;
        }

        [TestSvm]
        public static IntPtr IntPtrSum()
        {
            IntPtr ptr = new IntPtr(0);
            return ptr + 10;
        }

        private static unsafe bool Identity(int startValue)
        {
            void* nativeInt = (void*) startValue;
            int back = (int) nativeInt;
            return startValue == back;
        }

        [Ignore("Casting from pointer to number results in pointer, so we try to 'Ptr == 5'")]
        public static unsafe bool IdentityTest()
        {
            return Identity(5);
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

        [Ignore("Idea of symbolic pointer difference is wrong: (p - q) + q != p")]
        public static int SimplePointerDifference(int x, double y)
        {
            int* p = &x;
            double* q = &y;
            long d = (double*) p - q;

            return * (int*) (q + d);
        }

        [Ignore("Idea of symbolic pointer difference is wrong: (p - q) + q != p")]
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
