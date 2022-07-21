using System;
using NUnit.Framework;
using VSharp.Test;
#pragma warning disable CS0162

namespace IntegrationTests
{
    [TestSvmFixture]
    public class ExceptionsControlFlow
    {
        [Ignore("need deep copy for concrete memory or concolic")]
        public static int TestWithHandlers(int x, int y) {
            //A[] array = new A[15];
            int addition = 1;
            try
            {
                return x / y;
            }
            catch (OverflowException)
            {
                return addition + 100500;
            }
            catch (DivideByZeroException) when (x == 100)
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

        [TestSvm(100)]
        public static int TryWith2Leaves(bool f)
        {
            int res = 0;
            try
            {
                if (f)
                 return 100;
            }
            finally
            {
                res = 42;
            }

            res++;
            return res;
        }

        private static int Always42() => 42;
        private static int Always84() => Always42() * 2;

        // expecting 111111
        [Ignore("FilterHandler support")]
        public static int FilterInsideFinally(bool f)
        {
            int globalMemory = 0;
            try
            {
                globalMemory++;
            }
            finally
            {
                try
                {
                    globalMemory += 10;
                    throw new Exception();
                }
                catch (Exception) when ((globalMemory += 100) > 50 && f && Always42() == 42)
                {
                    globalMemory += 1000;
                }

                globalMemory += 10000;
            }

            globalMemory += 100000;
            return globalMemory;
        }


        [TestSvm(79)]
        public static int NestedTryCatchFinally()
        {
            int globalMemory = 0;
            try
            {
                try
                {
                    throw new Exception();
                }
                catch (Exception)
                {
                    globalMemory = 42;
                }
                finally
                {
                    globalMemory++;
                }
            }
            catch (Exception)
            {
                globalMemory = 12;
            }
            finally
            {
                globalMemory++;
            }
            return globalMemory;
        }

        [TestSvm(100)]
        public static int CallInsideFinally(bool f)
        {
            int res = 0;
            try
            {
                res += Always42();
            }
            finally
            {
                if (f)
                {
                    try
                    {
                        res += Always42();
                    }
                    finally
                    {
                        res += Always84();
                    }
                }
            }

            return res;
        }


    }
}
