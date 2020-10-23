using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using NUnit.Framework.Internal;

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

        [TestSvm]
        public static int CasualFactorial(int x)
        {
            if (x <= 0) return 1;
            return x * CasualFactorial(x - 1);
        }

        [TestSvm]
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
        public static void NewObjInLoop()
        {
            object c = new ClassWithOneField();
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

        [TestSvm]
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

        // [TestSvm]
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

        [TestSvm]
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

        [TestSvm]
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





        // [TestSvm]
        // public static int CheckOperationalStackBalance(int x)
        // {
        //     int y = 2 * x;
        //     if (x > 0)
        //     {
        //         y++;
        //     }
        //     else
        //     {
        //         int z = -x - 5;
        //         y = z + 1;
        //     }
        //
        //     return y;
        // }
    }
}