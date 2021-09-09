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

        private static bool Identity(int startValue)
        {
            void* nativeInt = (void*) startValue;
            int back = (int) nativeInt;
            return startValue == back;
        }

        [TestSvm]
        public static bool IdentityTest()
        {
            return Identity(5);
        }

        [TestSvm]
        public static int *AddressArithmetic()
        {
            int x = 428999;
            return &x + 1;
        }

        // Expected true
        [TestSvm]
        public static bool ArrayConcreteSafeRead1()
        {
            var array = new int[] {1, 2, 3};
            int result;
            fixed (int* ptr = &array[0])
            {
                result = *ptr;
            }

            if (result == 1)
                return true;
            else
                return false;
        }

        // Expected 2
        [TestSvm]
        public static bool ArrayConcreteSafeRead2()
        {
            var array = new int[] {1, 2, 3};
            int result;
            fixed (int* ptr = &array[0])
            {
                result = *(ptr + 1);
            }
            if (result == 2)
                return true;
            else
                return false;

        }

        [TestSvm]
        public static bool ArrayConcreteUnsafeRead()
        {
            var array = new int[] {1, 2, 3, 4, 5};
            long result;
            fixed (int* ptr = &array[0])
            {
                var ptr2 = (long*) ptr;
                result = *(ptr2 + 1);
            }

            if (result == 17179869187L)
                return true;
            else
                return false;

        }

        [TestSvm]
        public static bool ArraySymbolicSafeRead(int i)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            int result;
            fixed (int* ptr = &array[0])
            {
                result = *(ptr + i);
            }

            if (result == 3)
                return true;
            else
                return false;

        }

        [TestSvm]
        public static bool ArraySymbolicUnsafeRead(int i)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            long result;
            fixed (int* ptr = &array[0])
            {
                var ptr2 = (long*) ptr;
                result = *(ptr2 + i);
            }

            if (result == 17179869187L)
                return true;
            else
                return false;

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
