using System;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class ExceptionsControlFlow
    {
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

        [TestSvm]
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

        private static int Alway42() => 42;
        private static int Alway84() => Alway42() * 2;

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
                catch (Exception) when ((globalMemory += 100) > 50 && f && Alway42() == 42)
                {
                    globalMemory += 1000;
                }

                globalMemory += 10000;
            }

            globalMemory += 100000;
            return globalMemory;
        }


        [TestSvm]
        public static int CallInsideFinally(bool f)
        {
            int res = 0;
            try
            {
                res += Alway42();
            }
            finally
            {
                if (f)
                {
                    try
                    {
                        res += Alway42();
                    }
                    finally
                    {
                        res += Alway84();
                    }
                }
            }

            return res;
        }


    }
}