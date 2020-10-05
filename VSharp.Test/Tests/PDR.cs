using System;
using System.Runtime.InteropServices;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using NUnit.Framework.Internal;

namespace VSharp.Test.Tests
{
    // [TestSvmFixture]
    public static class PDR
    {
        private static int ReturnConstant() => 17;

        [TestSvm]
        public static void NewarrAllocatesArrayWithConcreteAddress()
        {
            int[] array = new int[10];
            ReturnConstant();
            array[5] = 42;
        }
        private class ClassWithOneField
        {
            public int x;
        }

        private static int ReadFieldOfCLass(ClassWithOneField classWithOneField)
        {
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

        // [TestSvm]
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

        // [TestSvm]
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