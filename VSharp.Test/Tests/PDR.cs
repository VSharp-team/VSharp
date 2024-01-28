using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.Dynamic;
using System.IO;
using NUnit.Framework;
using NUnit.Framework.Internal;
using VSharp.Test;
#pragma warning disable CS0168

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class PDR
    {
        [TestSvmFixture]
        [IgnoreFuzzer("Need static field support")]
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

        [TestSvm(100)]
        public static int NRE_TEST(ClassWithOneField c)
        {
            return c.x;
        }


        [Ignore("Tests generation with lambdas is not implemented")]
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

        // [TestSvm]
        public static int factAgain(int n)
        {
            if (n <= 0) return 1;
            return n * factAgain(n - 1);
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
        public static object CreateClassViaNewobj()
        {
            return new object();
        }

        [TestSvm]
        public static int NewarrAllocatesArrayWithConcreteAddress3()
        {
            int[] array = new int[10] {0, 1, 2, 3, 4, 42, 6, 7, 8, 9};
            ReturnConstant();
            return array[5];
        }

        [TestSvm(100)]
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

        [TestSvm(100)]
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

        [TestSvm(90)]
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

        [TestSvm(100)]
        public static int ReadFieldOfCLass1(ClassWithOneField a, ClassWithOneField b)
        {
            var c1 = new ClassWithOneField();
            c1.x = 42;
            var c2 = new ClassWithOneField();
            c2.x = 12;
            var c3 = new ClassWithOneField();
            c3.x = 7;
            a.x = b.x * 2;
            if (a.x > 127)
                b.x = a.x * 2;
            return a.x;
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

        private static void TestRefClass(ref ClassWithOneField c)
        {
            c = new ClassWithOneField();
        }

        [TestSvm]
        public static int TestRefClass()
        {
            var c = new ClassWithOneField() {x = 56};
            TestRefClass(ref c);
            return c.x;
        }

        private static void TestRefClass2(ref object c)
        {
            ClassWithOneField c1 = (ClassWithOneField) c;
            c1.x = 100;
        }

        private static void TestRefClass1(ref object c)
        {
            ClassWithOneField c1 = (ClassWithOneField) c;
            c1.x = 100;
            c = new object();
        }

        [TestSvm]
        public static int TestRefClass1()
        {
            var c = new ClassWithOneField() {x = 56};
            object o = c;
            TestRefClass1(ref o);
            return c.x;
        }

        [TestSvm]
        public static int LdelemaTest1()
        {
            var array = new ClassWithOneField[] {new ClassWithOneField(){x = 56}, new ClassWithOneField(){x = 42}};
            TestRefClass(ref array[0]);
            return array[0].x;
        }

        // expecting System.ArrayTypeMismatchException
        [TestSvm(76)]
        public static void LdelemaTest2(bool f)
        {
            var array = new ClassWithOneField[] {new ClassWithOneField(){x = 56}, new ClassWithOneField(){x = 42}};
            if (f)
            {
                object[] objectArray = array;
                TestRefClass2(ref objectArray[0]);
            }
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

        [TestSvm(100)]
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
        [TestSvm(75)]
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
        [TestSvm(66)]
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
        [TestSvm(100)]
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
        [TestSvm(100)]
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
        [TestSvm(100)]
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

        public static int D(int arg)
        {
            return arg + 1;
        }

        public static int X(int arg)
        {
            return D(arg + 20);
        }

        public static int G(int arg)
        {
            return X(arg + 300);
        }

        [TestSvm]
        public static int BreakCallSitesComposition()
        {
            int x = X(0);
            int g = G(0);

            return x + g;
        }

        [TestSvm(100)]
        public static int BreakCallSitesCompositionRecursion(int n)
        {
            int sum = D(n);
            int restSum = 0;
            if (n > 0)
            {
                restSum = BreakCallSitesCompositionRecursion(n - 1);
            }

            return sum + restSum;
        }

        [TestSvm(100)]
        public static int BreakCallSitesCompositionCycle(int n)
        {
            int sum = 0;
            while (n >= 0)
            {
                sum += D(n);
                n--;
            }

            return sum;
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

        [TestSvm]
        public static int CheckFinallyOrderForNestedTryBlocks2()
        {
            var res = 0;
            try
            {
                try
                {
                    throw new DummyException();
                }
                catch (DummyException)
                {
                    res += 1;
                }
                finally
                {
                    res += 2;
                }

                res += 3;
            }
            catch (Exception) when (CommonFilter("CommonFilter", false))
            {
                res += 4;
            }
            catch (Exception)
            {
                res += 5;
            }
            finally
            {
                res += 6;
            }

            return res;
        }

        [TestSvm(100)]
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

        [TestSvm(92)]
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

        // Hypothetical situation with reservedVariable's peaks:
        // left state: only one frame {funcId = RecursiveF; entries = n = 7, localVariable with peaks 1; isEffect = false}
        // right state: 4 frames
        // {funcId = RecursiveF; entries = n = 3, localVariable with peaks 4; isEffect = false}
        // {funcId = RecursiveF; entries = n = 4, localVariable with peaks 3; isEffect = false}
        // {funcId = RecursiveF; entries = n = 5, localVariable with peaks 2; isEffect = false}
        // {funcId = RecursiveF; entries = n = 6, localVariable with peaks 1; isEffect = false}

        // Result state: 5 frames
        // {funcId = RecursiveF; entries = n = 3, localVariable with peaks 5; isEffect = false}
        // {funcId = RecursiveF; entries = n = 4, localVariable with peaks 4; isEffect = false}
        // {funcId = RecursiveF; entries = n = 5, localVariable with peaks 3; isEffect = false}
        // {funcId = RecursiveF; entries = n = 6, localVariable with peaks 2; isEffect = false}
        // {funcId = RecursiveF; entries = n = 7, localVariable with peaks 1; isEffect = false}

        [Ignore("Forward exploration does not handle recursion now")]
        public static int RecursiveF(int n)
        {
            if (n > 0)
                return RecursiveF(n - 1) + n;

            int localVariable = n * n;
            return localVariable;
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
        public static uint TestConversions(uint x)
        {
            return x;
        }

        [TestSvm(100)]
        public static long Conv_Ovf_short_int(long a)
        {
            return checked((int) a);
        }


        [TestSvm(100)]
        public static long Many_Conversions(short a)
        {
            long tmp = a;
            tmp = (short) Conv_Ovf_short_int(tmp);
            tmp = (byte) Conv_Ovf_short_int(tmp);
            tmp = (long) Conv_Ovf_short_int(tmp);
            tmp = (short) Conv_Ovf_short_int(tmp);
            tmp = (byte) Conv_Ovf_short_int(tmp);
            tmp = (long) Conv_Ovf_short_int(tmp);
            return tmp;
        }

        [TestSvm]
        public static long SumOfIntAndUint(int a, uint b)
        {
            return b + a;
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

        [TestSvm]
        public static object CallStaticCtor()
        {
            return ClassWithCCtor._f;
        }

        [TestSvm]
        public static int ByRefTest1(ref int x)
        {
            return x;
        }

        [TestSvm]
        public static void ByRefTest2(out int x)
        {
            x = 10;
        }

        [TestSvm]
        public static int ByRefTest2Call(int x)
        {
            x = 42;
            ByRefTest2(out x);
            return x;
        }

        [TestSvm]
        public static int ByRefTest3(in int x)
        {
            return x;
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
