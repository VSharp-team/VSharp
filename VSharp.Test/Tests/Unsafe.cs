using System;
using System.Runtime.InteropServices;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
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

        public class A
        {
            public int x;
        }

        [TestSvm]
        public static A ReturnClass()
        {
            var a = new A();
            return a;
        }

        [TestSvm]
        public static int ReturnField()
        {
            var a = new A();
            return a.x;
        }

        public static void WriteByRefSymbolic(ref int x, int y)
        {
            x = y;
        }

        [TestSvm]
        public static int WriteFieldByRefSymbolic(int y)
        {
            var a = new A();
            WriteByRefSymbolic(ref a.x, y);
            return a.x;
        }

        [TestSvm]
        public static int RetConcrete()
        {
            return 10;
        }

        [TestSvm]
        public static int WriteFieldSymbolic(int y)
        {
            var a = new A();
            a.x = y;
            return a.x;
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

        [Ignore("Need to create IntPtr struct")]
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

        [Ignore("Need to create IntPtr struct")]
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

        [Ignore("need to create case for ptr inside term2obj(use ptrRepr)")]
        public static int *AddressArithmetic()
        {
            int x = 428999;
            return &x + 1;
        }

        // Expected true
        [TestSvm(90)]
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
        [TestSvm(90)]
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

        [TestSvm(91)]
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

        [TestSvm(92)]
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

        [TestSvm(93)]
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

        [TestSvm(93)]
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

        [TestSvm(91)]
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

        [TestSvm(92)]
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

        [TestSvm(94)]
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

        [TestSvm(97)]
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

        [StructLayout(LayoutKind.Explicit)]
        public class ExplicitClass
        {
            [FieldOffset(3)]
            public int x;
            [FieldOffset(4)]
            public int y;

            public ExplicitClass(int x, int y)
            {
                this.x = x;
                this.y = y;
            }
        }

        [StructLayout(LayoutKind.Sequential)]
        public struct SequentialStruct
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

        [TestSvm(95)]
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

        [TestSvm(95)]
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

        [TestSvm(94)]
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

        [TestSvm(94)]
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

            if (i == 1 && result != 216172782147338240L)
                return false;
            else
                return true;
        }

        [TestSvm(92)]
        public static bool ClassSymbolicReadZeroBetweenFields(int i)
        {
            var c = new ExplicitClass(1, 2);
            byte result;
            fixed (int* ptr = &c.x)
            {
                var ptr2 = (byte*) ptr;
                var ptr3 = ptr2 + i;
                result = *ptr3;
            }
            if (i == -1 && result != 0)
                return false;
            else
                return true;
        }

        [TestSvm(87)]
        // TODO: minimize combine term #do
        public static bool ClassWriteSafeOverlappingFields(int i, int j)
        {
            var c = new ExplicitClass(i, j);
            c.y = 42;
            c.x = -1000;
            if (c.y == 16777212)
                return true;
            return false;
        }

        [TestSvm(96)]
        public static bool StructInsideArraySymbolicUnsafeWrite(int i, SequentialStruct v)
        {
            var array = new [] {new SequentialStruct(i, i), new SequentialStruct(i, i), new SequentialStruct(i, i)};
            fixed (SequentialStruct* ptr = &array[0])
            {
                var ptr2 = (int*) ptr;
                var ptr3 = ptr2 + i;
                var ptr4 = (SequentialStruct*) ptr3;
                *ptr4 = v;
            }

            if (i == 1 && (array[0].y != v.x || array[1].x != v.y))
                return false;
            else
                return true;
        }

        [Ignore("Incorrect result (unsafe reading for strings)")]
        public static bool StringAsSpanSymbolicRead(int i)
        {
            var s = "best string";
            long result;
            fixed (char* ptr = &s.AsSpan()[0])
            {
                var ptr2 = (int*) ptr;
                var ptr3 = ptr2 + i;
                var ptr4 = (long*) ptr3;
                result = *ptr4;
            }

            if (i == 3 && result != 30962698417209460L)
                return false;
            else
                return true;
        }

        [TestSvm(93)]
        public static bool StringSymbolicRead(int i)
        {
            var s = "best string";
            long result;
            fixed (char* ptr = &s.GetPinnableReference())
            {
                var ptr2 = (int*) ptr;
                var ptr3 = ptr2 + i;
                var ptr4 = (long*) ptr3;
                result = *ptr4;
            }

            if (i == 3 && result != 30962698417209460L)
                return false;
            else
                return true;
        }

        [TestSvm(82)]
        public static bool StringSymbolicRead2(int i)
        {
            var s = "best string";
            long result;
            fixed (char* ptr = s)
            {
                var ptr2 = (int*) ptr;
                var ptr3 = ptr2 + i;
                var ptr4 = (long*) ptr3;
                result = *ptr4;
            }

            if (i == 3 && result != 30962698417209460L)
                return false;
            else
                return true;
        }

        [Ignore("Support unsafe reading of string length")]
        public static int StringLengthUnsafeRead(int i)
        {
            var s = "best string";
            int result;
            fixed (char* ptr = s)
            {
                int* p = (int*) ptr;
                result = p[-1];
            }

            return result;
        }

        [Ignore("Incorrect result")]
        public static bool StringSymbolicWrite(int i)
        {
            var s = "best string";
            fixed (char* ptr = &s.GetPinnableReference())
            {
                var ptr2 = (int*) ptr;
                var ptr3 = ptr2 + i;
                var ptr4 = (long*) ptr3;
                *ptr4 = 30962698417209460L;
            }

            if (i == 3 && (s[0] != 'b' || s[1] != 'e' || s[2] != 's' || s[3] != 't' || s[4] != ' ')) // s != "best string"
                return false;
            else
                return true;
        }

        public class ClassA
        {
            public int x;
        }

        public class ClassB
        {
            public int y;
        }

        [TestSvm]
        public static int UnsafeAs1()
        {
            var b = new ClassA();
            var a = System.Runtime.CompilerServices.Unsafe.As<ClassA, ClassB>(ref b);
            a.y = 10;
            return b.x;
        }

        [TestSvm]
        public static int UnsafeAs2()
        {
            var b = new ClassA[]{ new ClassA(), new ClassA(), new ClassA() };
            var a = System.Runtime.CompilerServices.Unsafe.As<ClassA[], ClassB[]>(ref b);
            b[0].x = 42;
            return a[0].y;
        }

        [TestSvm(86)]
        public static int UnsafeAs3()
        {
            var b = new ClassA[]{ new ClassA(), new ClassA(), new ClassA() };
            var a = System.Runtime.CompilerServices.Unsafe.As<ClassA[], ClassB[]>(ref b);
            a[0] = new ClassB();
            return 1;
        }

        [TestSvm(100)]
        public static int UnsafeAs4(object o)
        {
            if (o is ClassA)
            {
                var a = o as ClassA;
                a.x = 42;
                var b = System.Runtime.CompilerServices.Unsafe.As<ClassA, ClassB>(ref a);
                return b.y;
            }

            if (o is ClassB)
            {
                var b = o as ClassB;
                b.y = 12;
                var a = System.Runtime.CompilerServices.Unsafe.As<ClassB, ClassA>(ref b);
                return a.x;
            }
            return 331;
        }

        [TestSvm]
        public static long UnsafeAs5()
        {
            var b = new long[]{ 0, 1, 2 };
            var a = System.Runtime.CompilerServices.Unsafe.As<long[], int[]>(ref b);
            a[0] = 10;
            return b[0];
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

        public static void SafeCopy(int[] source, int sourceOffset, int[] target,
            int targetOffset, int count)
        {
            for (int i = 0; i < count; i++)
            {
                target[targetOffset + i] = source[sourceOffset + i];
            }
        }

        public static void UnsafeCopy(int[] source, int sourceOffset, int[] target,
            int targetOffset, int count)
        {
            fixed (int* pSource = source, pTarget = target)
            {
                byte* pSourceByte = (byte*)pSource;
                byte* pTargetByte = (byte*)pTarget;
                var sourceOffsetByte = sourceOffset * sizeof(int);
                var targetOffsetByte = targetOffset * sizeof(int);
                // Copy the specified number of bytes from source to target.
                for (int i = 0; i < count * sizeof(int); i++)
                {
                    pTargetByte[targetOffsetByte + i] = pSourceByte[sourceOffsetByte + i];
                }
            }
        }

        [TestSvm(100)]
        public static bool CheckUnsafeArrayCopy(int offset1, int offset2, int value)
        {
            var length = 5;
            if (offset1 < 0 || offset1 >= length || offset2 < 0 || offset2 >= length)
                throw new ArgumentException("wrong offsets");

            // Create three arrays of the same length
            int[] array1 = new int[length];
            int[] array2 = new int[length];
            int[] array3 = new int[length];

            // Filling first array
            for (int i = 0; i < length; ++i)
            {
                array1[i] = i + value;
            }

            // Copy first array to second via unsafe code
            UnsafeCopy(array1, 0, array2, 0, length - offset1);

            // Copy first array to third via simple 'for'
            SafeCopy(array1, 0, array3, 0, length - offset2);

            // Checking equality of second and third arrays
            for (int i = 0; i < length; i++)
            {
                if (array2[i] != array3[i])
                    return false;
            }

            return true;
        }
    }
}
