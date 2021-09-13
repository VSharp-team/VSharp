using System;
using System.Runtime.InteropServices;
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

            if (result != 3 && i == 2)
                return false;
            else
                return true;

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

            if (result != 17179869187L && i == 1)
                return false;
            else
                return true;

        }

        [TestSvm]
        public static bool ArraySymbolicUnsafeRead2(int i)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            long result;
            fixed (int* ptr = &array[0])
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = (long*) (ptr2 + i);
                result = *ptr3;
            }

            if (result != 216172782147338240L && i == 1)
                return false;
            else
                return true;
        }

        [TestSvm]
        public static bool ArrayConcreteSafeWrite()
        {
            var array = new int[] {1, 2, 3, 4, 5};
            fixed (int* ptr = &array[0])
            {
                *ptr = 10;
            }

            if (array[0] == 10)
                return true;
            else
                return false;
        }

        [TestSvm]
        public static bool ArrayConcreteUnsafeWrite()
        {
            var array = new int[] {1, 2, 3, 4, 5};
            fixed (int* ptr = &array[0])
            {
                var ptr2 = (long*) ptr;
                *ptr2 = 17179869187L;
            }

            if (array[0] == 3)
                return true;
            else
                return false;
        }

        [TestSvm]
        public static bool ArraySymbolicUnsafeWrite(int i)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            fixed (int* ptr = &array[0])
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = (long*) (ptr2 + i);
                *ptr3 = 17179869187L;
            }

            if (i == 0 && array[0] == 1)
                return false;
            else
                return true;
        }

        [TestSvm]
        public static bool ArraySymbolicUnsafeWrite2(int i)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            var test = new int[] {1, 2, 3, 4, 5};
            fixed (int* ptr = &array[0])
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = (long*) (ptr2 + i);
                *ptr3 = 216172782147338240L;
            }

            if ((array[0] != test[0] || array[1] != test[1] || array[2] != test[2] || array[3] != test[3] || array[4] != test[4]) && i == 1)
                return false;
            else
                return true;
        }

        [StructLayout(LayoutKind.Sequential)]
        public class SequentialClass
        {
            public int x;
            public int y;

            public SequentialClass(int x, int y)
            {
                this.x = x;
                this.y = y;
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        struct SequentialStruct
        {
            public int x;
            public int y;

            public SequentialStruct(int x, int y)
            {
                this.x = x;
                this.y = y;
            }
        }

        [StructLayout(LayoutKind.Explicit)]
        struct ExplicitStruct
        {
            [FieldOffset(0)]
            public int x;
            [FieldOffset(1)]
            public int y;

            public ExplicitStruct(int x, int y)
            {
                this.y = y;
                this.x = x;
            }
        }

        [StructLayout(LayoutKind.Explicit)]
        class ExplicitClassWithStructsInside
        {
            [FieldOffset(3)]
            public ExplicitStruct x;
            [FieldOffset(5)]
            public SequentialStruct y;

            public ExplicitClassWithStructsInside(ExplicitStruct x, SequentialStruct y)
            {
                this.x = x;
                this.y = y;
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        class SequentialClassWithStructsInside
        {
            public SequentialStruct x;
            public SequentialStruct y;

            public SequentialClassWithStructsInside(SequentialStruct x, SequentialStruct y)
            {
                this.x = x;
                this.y = y;
            }
        }

        [TestSvm]
        public static bool StructInsideArraySymbolicUnsafeRead(int i)
        {
            var array = new [] {new SequentialStruct(1, 2), new SequentialStruct(3, 4), new SequentialStruct(5, 6)};
            long result;
            fixed (SequentialStruct* ptr = &array[0])
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = (long*) (ptr2 + i);
                result = *ptr3;
            }

            if (i == 1 && result != 216172782147338240L)
                return false;
            else
                return true;
        }

        [TestSvm]
        public static bool StructInsideArraySymbolicUnsafeRead2(int i)
        {
            var array = new [] {new ExplicitStruct(1, 2), new ExplicitStruct(3, 4), new ExplicitStruct(5, 6)};
            long result;
            fixed (ExplicitStruct* ptr = &array[0])
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = (long*) (ptr2 + i);
                result = *ptr3;
            }

            if (i == 1 && result != 216172782113783808L)
                return false;
            else
                return true;
        }

        [TestSvm]
        public static bool ClassSymbolicUnsafeRead(int i)
        {
            var x = new ExplicitStruct(1, 2);
            var y = new SequentialStruct(3, 4);
            var c = new ExplicitClassWithStructsInside(x, y);
            long result;
            fixed (ExplicitStruct* ptr = &c.x)
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = (long*) (ptr2 + i);
                result = *ptr3;
            }

            if (i == 1 && result != 4398046511872L)
                return false;
            else
                return true;
        }

        [TestSvm]
        public static bool ClassSymbolicUnsafeRead2(int i)
        {
            var x = new SequentialStruct(1, 2);
            var y = new SequentialStruct(3, 4);
            var c = new SequentialClassWithStructsInside(x, y);
            long result;
            fixed (SequentialStruct* ptr = &c.x)
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = ptr2 + i;
                var ptr4 = (long*) ptr3;
                result = *ptr4;
            }

            if (i == 1 && result != 216172782113783808L)
                return false;
            else
                return true;
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
