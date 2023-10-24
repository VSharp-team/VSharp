using System;
using System.Diagnostics;
using NUnit.Framework;
using VSharp.Test;

#pragma warning disable CS0162

namespace IntegrationTests
{
    [TestSvmFixture]
    public class ExceptionsControlFlow
    {
        [TestSvm] //[Ignore("need deep copy for concrete memory or concolic")]
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

        [TestSvm(100, strat: SearchStrategy.DFS)]
        public static int CatchRuntimeException(int x, int y)
        {
            try
            {
                return x / y;
            }
            catch (DivideByZeroException)
            {
                return -42;
            }
        }

        [TestSvm(100, strat: SearchStrategy.DFS)]
        public static int SimpleFilterBlock(int x, int y)
        {
            try
            {
                return x / y;
            }
            catch (DivideByZeroException) when(x == 42)
            {
                return -42;
            }
        }

        private static bool ThrowNullReference()
        {
            throw new NullReferenceException();
        }
        
        [TestSvm(65, strat: SearchStrategy.DFS)]
        public static int ExceptionInsideFilter(int x, int y)
        {
            var a = 0;
            try
            {
                return x / y;
            }
            catch (DivideByZeroException e) when (ThrowNullReference())
            {
                a = 3;
            }
            finally
            {
                a = 5;
            }

            return a;
        }

        [TestSvm(100)]
        public static int ReturnMinWithAssert(int x, int y)
        {
            Debug.Assert(x <= y);
            return x;
        }

        [TestSvm(100)]
        public static int TestWithAssert(int x, int y)
        {
            if (x < 0)
            {
                Debug.Assert(x <= y);
            }
            else
            {
                x = y;
                Debug.Assert(x > y);
            }
            return x;
        }

        // expecting 111
        [TestSvm(100)]
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
        
        [TestSvm(66, strat: SearchStrategy.DFS)]
        public static void FilterOrder(int x)
        {
            bool ThrowException()
            {
                throw new NullReferenceException();
            }
            
            var a = 0;
            try
            {
                try
                {
                    a = 1;
                    throw new Exception();
                }
                catch (Exception) when (a == 1 && (x & 0b0001) == 0)
                {
                    a = 3;
                }
                catch (Exception) when (a == 1 && (x & 0b0010) == 0)
                {
                    ThrowException();
                }
            }
            catch (Exception) when ((x & 0b0100) == 0 && ThrowException())
            {
                a = 4;
            }
            catch (Exception) when (a == 3 && (x & 0b1000) == 0)
            {
                ThrowException();
            }
        }
        
        [TestSvm(78, strat: SearchStrategy.DFS)]
        public static void TwoFilters(int x)
        {
            bool ThrowException()
            {
                throw new NullReferenceException();
            }
            
            var a = 0;

            try
            { 
                a = 1;
                throw new Exception();
            }
            catch (Exception) when (a == 1 && (x & 0b0001) == 0)
            {
                a = 3;
            }
            catch (Exception) when (a == 1 && (x & 0b0010) == 0)
            {
                ThrowException();
            }
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
        [TestSvm(94, strat: SearchStrategy.DFS)]
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


        [TestSvm(84)]
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

        public static int ConcreteThrow()
        {
            throw new NullReferenceException("Null reference!");
        }

        [TestSvm(63)]
        public static int ConcreteThrowInCall()
        {
            try
            {
                ConcreteThrow();
            }
            catch (Exception)
            {
                return 1;
            }

            return 2;
        }

        [TestSvm(100, strat: SearchStrategy.DFS)]
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
