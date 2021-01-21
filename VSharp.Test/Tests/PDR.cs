using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Dynamic;
using System.IO;
using NUnit.Framework;
using NUnit.Framework.Internal;
using VSharp.Test.Tests.Methods;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public static class PDR
    {
        [TestSvmFixture]
        public static class WithStaticMembersUsedInCFA
        {
            public static int x = 100;

            [TestSvm]
            public static int Increment()
            {
                ++x;
                return x;
            }

            [TestSvm]
            public static int Mutate(int y)
            {
                x = y;
                return x;
            }

            [TestSvm]
            public static int MutateAndIncrement()
            {
                Mutate(100);
                Increment();
                return x;
            }

            [TestSvm]
            public static int IncrementAndMutate()
            {
                Increment();
                Mutate(100);
                return x;
            }

        }

        [TestSvm]
        public static Action<int> CreateLambda(bool flag)
        {
            int a = 0, b = 0, c = 0;
            Action<int> assign;
            if (flag)
                assign = m =>
                {
                    a = m;
                    b = m;
                };
            else
                assign = m =>
                {
                    a = m;
                    c = m;
                };
            return assign;
        }

        [TestSvm]
        public static int CallIncrementOutside()
        {
            return WithStaticMembersUsedInCFA.Increment();
        }


        private static int ReturnConstant() => 17;

        [Ignore("Forward exploration does not handle recursion now")]
        public static int CasualFactorial(int x)
        {
            if (x <= 0) return 1;
            return x * CasualFactorial(x - 1);
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static int CasualFactorial1()
        {
            return CasualFactorial(1);
        }

        [TestSvm]
        public static void NewarrAllocatesArrayWithConcreteAddress()
        {
            int[] array = new int[10];
            ReturnConstant();
            array[5] = 42;
        }

        [TestSvm]
        public static int NewarrAllocatesArrayWithConcreteAddress2()
        {
            int[] array = new int[10];
            ReturnConstant();
            array[5] = 42;
            return array[5];
        }

        [TestSvm]
        public static int NewarrAllocatesArrayWithConcreteAddress3()
        {
            int[] array = new int[10] {0, 1, 2, 3, 4, 42, 6, 7, 8, 9};
            ReturnConstant();
            return array[5];
        }

        [TestSvm]
        public static ClassWithOneField NewObjWithBranching(bool f)
        {
            ClassWithOneField classWithOneField = new ClassWithOneField();
            if (f)
            {
                classWithOneField.x = 42;
            } else
            {
                classWithOneField.x = 456;
            }
            return classWithOneField;
        }

        [TestSvm]
        public static int NewarrWithBranching(bool f)
        {
            int[] array = new int[10];
            if (f)
            {
                array[0] = 42;
            } else
            {
                array[0] = 456;
            }
            return array[0];
        }

        private static int Return100()
        {
            int x;
            return 100;
        }

        [TestSvm]
        public static int TestPopStackWithReservedVariable()
        {
            return Return100();
        }

        [TestSvm]
        public static void NewObj1()
        {
            object c = new ClassWithOneField();
            for (int i = 0; i < 2; i++)
            {
                var x = new int[32];
            }

            var y = new double[32];
            var z = new double[32];
        }

        [TestSvm]
        public static int TestCompositionWhenCallDividesBlocksAndSomeInformationIsPropagated()
        {
            object c = new ClassWithOneField();
            for (int i = 0; i < 2; i++)
            {
                var x = new int[32];
            }

            var y = new double[32];
            var z = new double[32];
            ReturnConstant();

            return y.Length + z.Length;
        }

        [TestSvm]
        public static void SequentialNewObjs()
        {
            new object();
            new object();
        }

        [TestSvm]
        public static void NewObjInLoop()
        {
            for (int i = 0; i < 5; ++i)
            {
                new object();
            }
        }

        public static void SequentialNewClassesWithOneField()
        {
            new ClassWithOneField();
            new ClassWithOneField();
            new ClassWithOneField();
            new ClassWithOneField();
        }

        [TestSvm]
        public static void NewObjInLoop1()
        {
            object c = new ClassWithOneField();
            SequentialNewClassesWithOneField();
            for (int i = 0; i < 5; ++i)
            {
                new object();
            }

            if (c is string)
            {
                new object();
            }
        }

        public class ClassWithOneField
        {
            public int x;
        }

        private static int ReadFieldOfCLass(ClassWithOneField classWithOneField)
        {
            return classWithOneField.x;
        }

        [TestSvm]
        public static int NewObjAndDup()
        {
            ClassWithOneField classWithOneField = new ClassWithOneField();
            classWithOneField.x = 42;
            ReturnConstant();
            return classWithOneField.x;
        }

        [Ignore("Exceptions handling")]
        public static void AlwaysNullReferenceException()
        {
            ReadFieldOfCLass(null);
        }

        private static int Abs(int x)
        {
            if (x > 0) return x;
            return -x;
        }

        private static int PositivePart(int x)
        {
            if (x >= 0) return x;
            return 0;
        }

        [TestSvm]
        public static bool AbsPpEqualsAbs(int x)
        {
            int y = Abs(x);
            int z = PositivePart(y);
            return y == z;
        }

        private static int F(int x)
        {
            return x * 2;
        }

        [TestSvm]
        public static int FunctionsComposition(int x)
        {
            return F(F(x) + F(F(F(F(x)))));
        }

        public class ClassWithOneStaticField
        {
            public static int x;
        }

        public class ClassWithStaticConstructor
        {
            public static int X;

            static ClassWithStaticConstructor()
            {
                X = 100;
            }
        }

        [TestSvm]
        public static void DoubleInitializeObjects()
        {
            new object();
            new object();
        }

        public static void AllocateObject()
        {
            new object();
        }

        [TestSvm]
        public static void DoubleAllocateViaCall()
        {
            AllocateObject();
            AllocateObject();
        }

        [TestSvm]
        public static int MultipleInitializeStaticFieldsInCfa(int a)
        {
            ClassWithOneStaticField.x = a;
            if (a > 10)
                ClassWithOneStaticField.x = 20;
            return ClassWithOneStaticField.x;
        }

        // expecting 1042
        [Ignore("Static constructor of ``ClassWithStaticConstructor'' is always called while CFA-construction")]
        public static int StaticConstructorShouldBeCalledOnce(bool f)
        {
            ClassWithStaticConstructor.X = 42;
            if (f)
            {
                ClassWithStaticConstructor.X = ClassWithStaticConstructor.X + 1000;
            }
            else
            {
                ClassWithStaticConstructor.X = ClassWithStaticConstructor.X + 1000;
            }

            return ClassWithStaticConstructor.X;
        }


        // expecting 5x
        [TestSvm]
        public static int TestForCycle_5x(int x)
        {
            int sum = 0;
            for (int i = 0; i < 5; i++)
            {
                sum += x;
            }

            return sum;
        }

        [TestSvm]
        public static int TestConcreteArray(int x)
        {
            var res = 0;
            var a = new int[3] {1, 2, 3};
            a[2] = x;
            var len = a.Length;
            var lb = a.GetLowerBound(0);
            if (len == 3 && lb == 0)
                res = 1;
            return res;
        }


        // expecting 1
        [TestSvm]
        public static int TestAllocatedType_1()
        {
            VirtualC c = new VirtualC();
            ReturnConstant();
            if (c is IVirtual)
            {
                return 1;
            }

            return -1;
        }

        // expecting 1
        [TestSvm]
        public static int TestAllocatedType_2()
        {
            VirtualC c = new VirtualC();
            if (c is IVirtual)
            {
                return 1;
            }

            return -1;
        }

        private static int IsIVirtual(object obj)
        {
            if (obj is IVirtual)
            {
                return 1;
            }

            return -1;
        }

        // expecting 1
        [TestSvm]
        public static int TestAllocatedType_3()
        {
            VirtualC c = new VirtualC();
            return IsIVirtual(c);
        }

        // expecting f -> 100; !f -> 42
        [TestSvm]
        public static int TestAllocatedType_4(bool f)
        {
            ClassWithOneField classWithOneField;
            int result = 0;
            if (f)
            {
                classWithOneField = new ClassWithOneField();
                classWithOneField.x = 100;
                result = classWithOneField.x;
            }
            else
            {
                result = 42;
            }

            return result;
        }

        // expecting 20
        [TestSvm]
        public static int TestLengths_1()
        {
            int[,] array = new int[4,5];
            ReturnConstant();
            return array.Length;
        }

        // expecting 20
        [TestSvm]
        public static int TestLengths_2()
        {
            int[,] array = new int[4,5];
            return array.Length;
        }

        // expecting 15
        [TestSvm]
        public static int TestLengths_3()
        {
            int[,] array = new int[4,5];
            ReturnConstant();
            array = new int[15, 1];
            return array.Length;
        }

        // expecting 15
        [TestSvm]
        public static int TestLengths_4()
        {
            int[,] array = new int[4,5];
            array = new int[15, 1];
            return array.Length;
        }

        // expecting 100
        [TestSvm]
        public static int TestLengths_5()
        {
            Array array = new int[4,5];
            ReturnConstant();
            array = new int[100];
            return array.Length;
        }

        // expecting f -> 100, !f -> 20
        [Ignore("Insufficient information is correct result")]
        public static int TestLengths_6(bool f)
        {
            Array array = new int[4, 5];
            if (f)
            {
                array = new int[100];
            }

            return array.Length;
        }

        // expecting 0
        [TestSvm]
        public static int TestLowerBound_1()
        {
            Array array = new int[4,5];
            ReturnConstant();
            array = new int[100];
            return array.GetLowerBound(0);
        }

        // expecting 0
        [TestSvm]
        public static int TestLowerBound_2(bool f)
        {
            Array array = new int[100];
            int result = array.GetLowerBound(0);;
            if (f)
            {
                array = new int[4, 5];
                result = array.GetLowerBound(1);
            }

            return result;
        }


        [Ignore("Exceptions handling")]
        public static int ThrowException(int x)
        {
            throw new Exception();
        }

        [TestSvm]
        public static int qweqwe(int x, int y) {
            //A[] array = new A[15];

            return checked(x + y);
        }

        [TestSvm]
        public static int SmokeTest() {
            return 100500;
        }

        [TestSvm]
        public static int SimpleIf(int x)
        {
            if (x >= 0) return x;
            return -x;
        }

        [TestSvm]
        public static int TernaryIf(int x)
        {
            return x >= 0 ? x : -x;
        }

        public class DummyException : Exception
        {
            private int _x;

            public DummyException()
            {
                _x = 42;
            }

            public int X
            {
                get => _x;
                set => _x = value;
            }

            public void Inc()
            {
                _x++;
            }
        }

        public static bool CommonFilter(String message, bool res)
        {
            Console.WriteLine(message);
            return res;
        }

        [Ignore("Exceptions handling")]
        public static void CheckFinallyOrderForNestedTryBlocks2()
        {
            try
            {
                try
                {
                    throw new DummyException();
                }
                catch (DummyException)
                {
                    Console.WriteLine("catch (DummyException)");
                }
                finally
                {
                    Console.WriteLine("Inner finally");
                }

                Console.WriteLine("Continue in outer TRY-BLOCK");
            }
            catch (Exception) when (CommonFilter("CommonFilter", false))
            {
                Console.WriteLine("catch (Exception e) when (CommonFilter(e))");
            }
            catch (Exception)
            {
                Console.WriteLine("catch (Exception)");
            }
            finally
            {
                Console.WriteLine("Outer finally");
            }
        }

        [TestSvm]
        public static int Gotos1(int x)
        {
            if (x <= 10)
            {
                goto labelB;
            }

            labelA:
            x += 5;
            if (x >= 100)
            {
                goto exit;
            }

            labelB:
            x -= 2;
            if (x % 5 == 0)
            {
                goto labelA;
            }

            exit:
            return x;
        }

        [Ignore("Need to implement byref: IO.TextWriter is passed by ref")]
        public static int DivWithHandlers(int x, int y)
        {
            int ans = 0;
            try
            {
                try
                {
                    ans = x / y;
                }
                catch (DummyException)
                {
                    Console.WriteLine("DummyException");
                }
                catch (FileNotFoundException)
                {
                    Console.WriteLine("FileNotFoundException");
                }
                finally
                {
                    Console.WriteLine("Inner finally");
                }
            }
            catch (Exception)
            {
                Console.WriteLine("Exception");
            }
            finally
            {
                Console.WriteLine("Outer finally");
            }

            return ans;
        }

        public struct A
        {
            public int x;
            public A(int value)
            {
                x = value;
            }

            public int GetX()
            {
                return x;
            }
            public void SetX(int newX)
            {
                x = newX;
            }
        }

        public class ClassWithStructInside
        {
            private A _a;

            public ClassWithStructInside(int n)
            {
                A localA;
                localA = new A(n);
                _a = localA;
            }

            public int GetN()
            {
                return _a.x;
            }
        }


        public class ClassWithLotsOFFields
        {
            public A _a;
            public ClassWithLotsOFFields _next;
            public double _pi = 3.14;
            public int _fortyTwo = 42;
            public int _forty;
            public Array _array;
            public Action _action;

            public ClassWithLotsOFFields()
            {
                _forty = 40;
            }
        }

        [TestSvm]
        public static ClassWithLotsOFFields TestDefaultFieldsAllocation()
        {
            return new ClassWithLotsOFFields();
        }

        [TestSvm]
        public static ClassWithLotsOFFields TestNext()
        {
            ClassWithLotsOFFields classWithLotsOfFields = new ClassWithLotsOFFields();
            return classWithLotsOfFields._next;
        }

        [TestSvm]
        public static int CheckInvalidCfaConstruction(bool f)
        {
            ClassWithLotsOFFields c = new ClassWithLotsOFFields();
            if (f)
            {
                c._forty = 40;
            }
            else
            {
                c._forty = 100;
            }

            if (c._forty == 40)
            {
                return 40;
            }

            if (c._forty == 100)
            {
                return 100;
            }

            return 100500;
        }


        [TestSvm]
        public static int TestForty()
        {
            ClassWithLotsOFFields classWithLotsOfFields = new ClassWithLotsOFFields();
            return classWithLotsOfFields._forty;
        }

        [TestSvm]
        public static int TestForty_WithSet0()
        {
            ClassWithLotsOFFields classWithLotsOfFields = new ClassWithLotsOFFields();
            classWithLotsOfFields._forty = 0;
            return classWithLotsOfFields._forty;
        }

        public struct StructWithStructInside2
        {
            private A _a;

            public StructWithStructInside2(int n)
            {
                _a = new A(n);
            }

            public int GetN()
            {
                return _a.x;
            }
        }

        [TestSvm]
        public static StructWithStructInside2 Test1_StructWithStructInside2()
        {
            return new StructWithStructInside2();
        }

        [TestSvm]
        public static StructWithStructInside2 Test2_StructWithStructInside2(int n)
        {
            return new StructWithStructInside2(n);
        }

        [TestSvm]
        public static StructWithStructInside2 Test3_StructWithStructInside2()
        {
            return new StructWithStructInside2(42);
        }


        public class ClassWithClassInside
        {
            private ClassWithOneField _classWithOneField;

            public ClassWithClassInside(int n)
            {
                ClassWithOneField local = new ClassWithOneField();
                local.x = n;
                _classWithOneField = local;
            }
        }

        [TestSvm]
        public static ClassWithStructInside CreateClassThatHasStructInside(int n)
        {
            return new ClassWithStructInside(n);
        }

        [TestSvm]
        public static bool SameN(int n)
        {
            ClassWithStructInside c = new ClassWithStructInside(n);
            return n == c.GetN();
        }

        [TestSvm]
        public static ClassWithClassInside CreateClassThatHasClassInside(int n)
        {
            return new ClassWithClassInside(n);
        }

        [TestSvm]
        public static int CreateStructViaNewobj(int n)
        {
            int res1 = new A(n).GetX();
            return res1 + n;
        }

        public static A W2(int x, int y) {
            A[] array = new A[15];
            array[0] = new A();

            return array[0];
        }

        [TestSvm]
        public static int TestWithHandlers(int x, int y) {
            //A[] array = new A[15];
            int addition = 1;
            try
            {
                return x / y;
            }
            catch (OverflowException e)
            {
                return addition + 100500;
            }
            catch (DivideByZeroException e) when (x == 100)
            {
                return addition + 90;
            }
            finally
            {
                addition++;
            }

            return checked(x + y);
        }

        // expecting 111
        [TestSvm]
        public static int TestWithNestedFinallyHandlers(int x, int y)
        {
            int addition = 1;
            try  {}
            finally
            {
                try { }
                finally
                {
                    addition += 10;
                }
                addition += 100;
            }

            return addition;
        }

        public static void MutateField(ClassWithOneField a)
        {
            a.x = 42;
        }

        [TestSvm]
        public static void SymbolicHeapRefIsNotIdempotent(ClassWithOneField a, ClassWithOneField b) {
            var x = a;
            a = b;
            MutateField(x);
        }

        [TestSvm]
        public static int TestThatEffectsDoNotSpreadToDifferentBasicBlocks(int x)
        {
            x = x * x;
            return F(x + 42);
        }

        private static int Foooo(int x)
        {
            return x + 42;
        }

        public class Aaaa
        {
            public int a;
        }

        [TestSvm]
        public static int TestArgComposition(Aaaa c)
        {
            c.a = c.a * c.a;
            c.a = Foooo(c.a);
            return c.a;
        }

        private static A Set900ForStruct(A a)
        {
            a.x = 900;
            return a;
        }

        [TestSvm]
        public static A StructIsAllocatedInTemporaryLocalVariableBeforeCallingCtor()
        {
            A a = new A(17);
            return Set900ForStruct(a);
        }

        private static int F(int n, ref int x) {
            int m = n;
            if (n == 0) {
                x = 42;
                return 0;
            }
            return (n == 10 ? F(n-1, ref m) : F(n-1, ref x)) + m;
        }

        private static T[] EnsureConcreteTypeForArrayCreation<T>(int size)
        {
            return new T[size];
        }

        [TestSvm]
        public static void EnsureConcreteType_1()
        {
            int[] array = EnsureConcreteTypeForArrayCreation<int>(10);
            array[9] = 42;
        }

        // expecting 87
        [Ignore("Byref type is not implemented!")]
        public static int MutateStackValueFromPreviousFrame()
        {
            int x = 0;
            return F(10, ref x);
        }

        [TestSvm]
        public static int CheckOperationalStackBalance(int x)
        {
            int y = 2 * x;
            if (x > 0)
            {
                y++;
            }
            else
            {
                int z = -x - 5;
                y = z + 1;
            }
            return y;
        }

        [TestSvm]
        public static double AddressesBecomeComplicated(double x)
        {
            double y;

            if (x >= 0)
            {
                y = x;
            }
            else
            {
                y = -x;
            }

            ReturnConstant();
            var obj3 = new object();
            var obj4 = new object();
            var obj5 = new object();

            return Math.Log(y);
        }

        private static UnboxAny.A CreateA()
        {
            return new UnboxAny.A();
        }

        public class ClassWithStaticField1
        {
            public static ClassWithOneField ClassWithOneField = new ClassWithOneField();
        }

        private static ClassWithOneField CreateC()
        {
            ClassWithOneField c = new ClassWithOneField();
            c.x = 42;
            return c;
        }

        [TestSvm]
        public static ClassWithOneField Test100500()
        {
            ClassWithOneField c = CreateC();
            c.x = 100500;
            return c;
        }


        // [TestSvm]
        [Ignore("Barrier: no static cctor initialization during CFA-construction and assumptions-Engine")]
        public static void PropagateAllocatedTypes()
        {
            UnboxAny.A a1 = CreateA();
            ClassWithOneField c2 = ClassWithStaticField1.ClassWithOneField;
        }


        private static object BoxT<T>(T t) where T : class
        {
            return t;
        }

        // [TestSvm]
        [Ignore("stackKey type substitution should be done")]
        public static object BoxReferenceType_1()
        {
            PDR.ClassWithOneField a = new PDR.ClassWithOneField();
            a.x = 123;
            return BoxT(a);
        }

        // [TestSvm]
        [Ignore("stackKey type substitution should be done")]
        public static object BoxReferenceType_2(bool f)
        {
            PDR.ClassWithOneField a = null;
            if (f)
            {
                return BoxT(a);
            }

            return new PDR.ClassWithOneField();
        }

        public class ClassWithCCtor
        {
            public static object _f = new PDR.ClassWithOneField();
        }

        // [Ignore("Can't execute static cctor of System.Type, because of MemoryRegion.write's __notImplemented__()")]
        [TestSvm]
        public static object CallStaticCtor()
        {
            return ClassWithCCtor._f;
        }

        [Ignore("internalfail \"byref type is not implemented!\"")]
        public static NullReferenceException CreateNullReferenceException()
        {
            return new NullReferenceException();
        }

        [Ignore("internalfail \"byref type is not implemented!\"")]
        public static OverflowException CreateOverflowException()
        {
            return new OverflowException();
        }
    }
}
