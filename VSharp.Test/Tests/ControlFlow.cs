using System;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    // unrolling recursion cycles
    public class ControlFlow
    {
        [TestSvm(100)]
        public static int SwitchWithSequentialCases(int x)
        {
            switch (x)
            {
                case 0:
                    return 1;
                case 1:
                    return 11;
                case 2:
                    return 101;
                case 3:
                    return 1001;
                case 4:
                    return 10001;
                default:
                    return -1;
            }
        }

        [TestSvm(100)]
        public static int ForSimple(int x)
        {
            int sum = 0;
            for (int i = 1; i <= x; i++)
            {
                sum += i;
            }
            return sum;
        }

        // x * (x + 1) * (x + 2) / 6
        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static int NestedFors2(int x)
        {
            int sum = 0;
            for (int i = 1; i <= x; i++)
            {
                for (int j = 1; j <= i; j++)
                {
                    sum += j;
                }
            }

            return sum;
        }

        // x > 10 && x >= 95 => x + 5
        // x > 10 && x < 95 && (x + 3) != 0 (mod 5) => x + 3
        // x > 10 && x < 95 && (x + 3) == 0 (mod 5) && (x + 8) >= 100 => x + 8
        // x > 10 && x < 95 && (x + 3) == 0 (mod 5) && (x + 8) < 100 => x + 6
        // x <= 10 && (x - 2) != 0 (mod 5) => x - 2
        // x <= 10 && (x - 2) == 0 (mod 5) => x + 1  //(x + 3) < 100 && (x + 1) != 0 (mod 5)
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
        public static int Gotos2(int x)
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

            if (x > 50)
            {
                x *= 3;
                goto labelA;
            }

            labelB:
            x -= 2;
            if (x % 5 == 0)
            {
                x *= 2;
                goto labelA;
            }

            if (x % 5 == 1)
            {
                goto labelB;
            }

            exit:
            return x;
        }

        [Ignore("Infinite cycle")]
        public static int InfiniteCycle1(int x)
        {
            label:
            x += 10;

            goto label;
        }

        [Ignore("Takes too much time")]
        public static int GotosWithinSwitch(int x)
        {
            switch (x)
            {
                case 0:
                    x += 2;
                    x *= 3;
                    goto case 2;

                case 1:
                    x *= 10;
                    goto case 5;

                case 2:
                    x %= 50;
                    goto case 4;

                case 4:
                    x += 34;
                    if (x > 50)
                    {
                        goto case 5;
                    }
                    else
                    {
                        goto case 2;
                    }

                case 5:
                    x -= 15;
                    goto default;

                default:
                    if (x == 28)
                    {
                        break;
                    }
                    else
                    {
                        x += 100;
                        goto case 0;
                    }
            }

            return x;
        }


        [TestSvm(100)]
        public static int GotoWithinWhile(int x)
        {
            int i = 2 * x;

            while (i >= 0 && i < 100)
            {
                if (i == 42)
                {
                    x += 1000;
                    break;
                }

                if (i == 84)
                {
                    x += 10000;
                    goto l1;
                }

                x += i % 2;
                i++;
            }

            x++;

            l1:
            return x;
        }

        [TestSvm(100)]
        public static int AcyclicGotos(int x)
        {
            if (x > 100)
            {
                if (x < 1000)
                {
                    goto l2;
                }
                goto l3;
            }

            if (x < 42)
            {
                goto l1;
            }

            goto l2;

            l1:
            x *= 10;
            goto exit;

            l2:
            x += 100;
            goto  exit;

            l3:
            x /= 1000;

            exit:
            return x;
        }


        [Ignore("Takes too much time")]
        public static int GotoOutwardsNestedCycle(int x)
        {
            int sum = 42;
            for (int i = 1; i < x; i++)
            {
                sum += i;
                for (int j = 0; j < i; j++)
                {
                    sum--;
                    if (x == 42)
                    {
                        goto exit;
                    }
                }
            }

            exit:
            x += sum;

            return x;
        }

        // NOTE: this test works fine with configuration Debug and Release, but DebugTailRec
        //       doesn't work, because dotnet generates non optimized IL, so there appears
        //       target on unreachable code, and test is explored infinitely, so setting timeout
        [TestSvm(100, timeout: 10, strat: SearchStrategy.BFS)]
        public static int ForsWithContinueAndBreak(int x)
        {
            int sum = 0;
            for (int i = 0; i < x; i++)
            {
                for (int j = 0; j < i; j++)
                {
                    if (j == 15)
                    {
                        break;
                    }

                    sum++;
                }

                if (2 * i % 100 == 45)
                {
                    continue;
                }

                sum++;
            }

            return sum;
        }

        [TestSvm(100)]
        public static int ForWithBreak(int x)
        {
            int sum = 0;
            for (int i = 0; i < x; i++)
            {
                if (i == 12)
                {
                    sum += 42;
                    break;
                }

                sum++;
            }

            return sum;
        }

        [TestSvm(100)]
        public static int SequentialIfsHard(int x)
        {
            if (2 * x == 50)
            {
                x += 100;
            }

            if (x % 7 == 5)
            {
                x *= 2;
            }

            if (x - 12 >= 0)
            {
                x++;
            }

            return x;
        }

        [TestSvm(100)]
        public static int SequentialIfsSimple(int x)
        {
            int res =  0;
            if (2 * x == 50)
            {
                res += 100;
            }

            if (x % 7 == 5)
            {
                res *= 2;
            }

            if (x - 12 >= 0)
            {
                res++;
            }

            return res;
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static int NestedForsSimple(int x)
        {
            int res = 0;
            for (int i = 0; i < x; i++)
            {
                for (int j = 0; j < x; j++)
                {
                    res++;
                }
            }

            return res;
        }

        [Ignore("Looping due to non-terminating paths")]
        public static int NestedForsHard(int x)
        {
            for (int i = 0; i < x; i++)
            {
                x++;
                for (int j = 0; j < i; j++)
                {
                    i++;
                }
            }

            return x;
        }

        [TestSvm(100)]
        public static int BinarySearch(int value, int[] a)
        {
            var l = a.GetLowerBound(0);
            var r = a.GetLength(0);
            while (l < r)
            {
                var mid = (l + r) / 2;
                int element = a[mid];
                if (element == value)
                    return mid;
                if (element < value)
                    r = mid - 1;
                l = mid + 1;
            }

            return -1;
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static int BinarySearch(int[] a, int x, int lo, int hi)
        {
            if (a == null) throw new ArgumentException("a == null");

            if (lo < 0) throw new ArgumentException("lo < 0");
            if (lo > hi) throw new ArgumentException("lo > hi");

            var m = lo + (hi - lo) / 2;

            while (lo < hi)
                if (a[m] == x)
                    return m;
                else if (a[m] > x)
                    hi = m;
                else
                    lo = m + 1;

            return -1;
        }

        private static int F(int x)
        {
            int sum = 0;
            for (int i = 1; i <= x; i++)
            {
                sum += i;
            }
            return sum;
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        public static int IncomingCallGraphEdgesTest(int x)
        {
            if (F(x) > 0 & F(x + 1) > 0)
            {
                return 1;
            }

            return 0;
        }

        [TestSvm]
        public static int CycleWith3EntryPoints(int x)
        {
            if (x == 1)
                goto lab1;
            if (x == 2)
                goto lab2;
            goto lab3;

            lab1:
            x += 100500;
            goto lab4;

            lab2:
            x *= 10;
            goto lab4;

            lab3:
            x++;

            lab4:
            if (x == 123)
                goto lab1;
            if (x == 234)
                goto lab2;
            if (x == 345)
                goto lab3;

            return x;
        }

        [TestSvm(100)]
        public static int DivideWithException(int x)
        {
            try
            {
                return 100 / x;
            }
            catch (DivideByZeroException e)
            {
                Console.WriteLine(e.Message);
                return 0;
            }
        }

        [TestSvm(90)]
        public static int TryInsideCatch(int x, string s)
        {
            // some code outside try block
            x++;

            try
            {
                x = s.Length;
            }
            catch (NullReferenceException)
            {
                try
                {
                    x = 100 / x;
                }
                catch (Exception e) when (e is DivideByZeroException)
                {
                    x = 1;
                }
            }

            return x;
        }

        [TestSvm(90, strat:SearchStrategy.DFS)]
        public static int TryInsideFinally(int x, string s)
        {
            x += 2;
            try
            {
                x = s.Length;
            }
            finally
            {
                try
                {
                    x = 100 / x;
                }
                catch (Exception e) when (e is DivideByZeroException)
                {
                    x = 1;
                }
            }

            return x;
        }

        [TestSvm(100)]
        public static int TryInsideWhile(int x)
        {
            int y = x;
            int res = -1;
            while (y > 0)
            {
                y--;
                try
                {
                    res = 100 / y;
                }
                catch (DivideByZeroException)
                {
                    res = 100;
                }
            }

            return res;
        }

        [TestSvm(80)]
        public static int RethrownInstructionIsNotCaughtByNextHandler(int x, string s)
        {
            try
            {
                x = s.Length / x;
            }
            catch (NullReferenceException nre)
            {
                x += nre.HResult;
                throw;
            }
            catch (DivideByZeroException divideByZeroException)
            {
                x += 42 + divideByZeroException.HResult;
            }
            catch
            {
                x += 43;
            }

            return x;
        }

        [TestSvm(100)]
        public static int CantCatchExplicitExceptionFromCatch(int x, string s)
        {
            try
            {
                x = s.Length / x;
            }
            catch (NullReferenceException nre)
            {
                throw new DivideByZeroException("Exception was thrown explicitly " + nre.Message);
            }
            catch (DivideByZeroException divideByZeroException)
            {
                x = 42 + divideByZeroException.HResult;
            }

            return x;
        }

        public static bool FilterWithException(NullReferenceException nre)
        {
            throw new DivideByZeroException("Exception was thrown explicitly");
        }

        [TestSvm(60)]
        public static int ExceptionThrownInFilter(int x, string s)
        {
            Console.WriteLine("Method: ExceptionThrownInFilter");
            try
            {
                x = s.Length;
            }
            catch (NullReferenceException nre) when (FilterWithException(nre))
            {
                x += nre.HResult;
            }
            catch (NullReferenceException nre)
            {
                x += nre.HResult;
                return x + 1;
            }

            return x;
        }

        public static bool FilterWithException1(string s)
        {
            var x = 0;
            try
            {
                x += s.Length;
            }
            catch (NullReferenceException nre) when (FilterWithException(nre))
            {
                return true;
            }

            return x > 0;
        }

        [TestSvm(57)]
        public static int ExceptionThrownInFilter1(int x, string s)
        {
            try
            {
                x = s.Length;
            }
            catch (NullReferenceException nre) when (FilterWithException1(s))
            {
                x += nre.HResult;
            }
            catch (NullReferenceException nre)
            {
                x += nre.HResult;
                return x + 1;
            }

            return x;
        }

        public static bool FilterWithTryCatch(NullReferenceException nre)
        {
            try
            {
                return 100 / nre.Message.Length < 5;
            }
            catch
            {
                Console.WriteLine("Exception occured in FilterWithTryCatch");
                return true;
            }
        }

        [TestSvm(72)]
        public static int FilterCanHaveExceptionHandler(int x, string s)
        {
            var nullReferenceExceptionWithEmptyMessage = new NullReferenceException("");
            try
            {
                x = s.Length;
            }
            catch (NullReferenceException nre) when (FilterWithTryCatch(nullReferenceExceptionWithEmptyMessage))
            {
                x += 1 + nre.HResult;
            }
            catch (NullReferenceException nre)
            {
                x += nre.HResult;
            }

            return x;
        }

        [TestSvm(62)]
        public static int OnlyFilter(int x, string s)
        {
            try
            {
                x = s.Length;
            }
            catch (NullReferenceException nre) when (nre.Message == "")
            {
                x += nre.HResult;
            }

            return x;
        }

        public static int FunctionWithFinallyThrowingException()
        {
            try
            {
                throw new SystemException("Explicitly thrown");
            }
            finally
            {
                Console.WriteLine("Finally was executed");
            }
        }

        [TestSvm(77)]
        public static int FunctionWithHandler(int x)
        {
            try
            {
                x = FunctionWithFinallyThrowingException();
            }
            catch (SystemException)
            {
                x = 123;
            }

            return x;
        }

        [TestSvm(80)]
        public static int NestedTryBlocks(int x, string s)
        {
            try
            {
                try
                {
                    x = 100 / s.Length;
                }
                catch (NullReferenceException nre)
                {
                    x += 1;
                }
                catch (Exception e) when (e is DivideByZeroException && s.Length > 0)
                {
                    x += 2;
                }
                finally
                {
                    x++;
                }
            }
            catch (Exception)
            {
                x += 1000;
            }

            return x;
        }

        [TestSvm(100)]
        public static int TryCatchFinally(int x, string s)
        {
            var res = 0;
            try
            {
                x = 100 / s.Length;
                res += 1;
            }
            catch (NullReferenceException nre)
            {
                res += nre.HResult;
            }
            finally
            {
                x++;
                res += 2;
            }

            return res + x;
        }

        [TestSvm(100)]
        public static int ReturnFromTryWithFinally(int x, string s)
        {
            var res = 0;
            try
            {
                x = 100 / s.Length;
                res += 1;
            }
            finally
            {
                res += 2;
            }

            return res + x;
        }

        [TestSvm(100)]
        public static int ManyFinallyBlocks(int x, string s)
        {
            var res = 0;
            try
            {
                try
                {
                    x = 100 / s.Length;
                    res += 1;
                }
                finally
                {
                    res += 2;
                }

            }
            catch (Exception)
            {
                res += 3;
            }
            finally
            {
                res += 4;
            }

            return x + res;
        }

        [TestSvm(90)]
        public static int TryCatchFilter(int x, string s)
        {
            try
            {
                x = 100 / s.Length;
            }
            catch (NullReferenceException)
            {
                return 0;
            }
            catch (DivideByZeroException) when (x == 56)
            {
                return 1;
            }
            return x;
        }

        [TestSvm(53)]
        public static int ExceptionInsideFilter(int x, string s)
        {
            try
            {
                x = 100 / s.Length;
            }
            catch (NullReferenceException) when (s.Length == 0)
            {
                return -1;
            }
            return x;
        }

        [TestSvm(100)]
        public int CycleReturningValueAndException(int x)
        {
            int sum = 0;
            for (int i = 0; i < x; i++)
            {
                if (i == 42)
                    throw new Exception("42");
                if (2 * i + x == 42)
                {
                    return 42;
                }

                sum++;
            }

            return sum;
        }
    }
}
