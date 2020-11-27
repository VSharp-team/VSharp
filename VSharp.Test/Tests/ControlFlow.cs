using System;
using System.Collections.Generic;
using System.Reflection.Emit;
using System.Runtime.InteropServices.ComTypes;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    // unrolling recursion cycles
    public class ControlFlow
    {
        [TestSvm]
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

        [TestSvm]
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
        [TestSvm]
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

//        // x > 10 && x >= 95 => x + 5
//        // x > 10 && x < 95 && (x + 3) != 0 (mod 5) => x + 3
//        // x > 10 && x < 95 && (x + 3) == 0 (mod 5) && (x + 8) >= 100 => x + 8
//        // x > 10 && x < 95 && (x + 3) == 0 (mod 5) && (x + 8) < 100 => x + 6
//        // x <= 10 && (x - 2) != 0 (mod 5) => x - 2
//        // x <= 10 && (x - 2) == 0 (mod 5) => x + 1  //(x + 3) < 100 && (x + 1) != 0 (mod 5)
//        [TestSvm]
//        public static int Gotos1(int x)
//        {
//            if (x <= 10)
//            {
//                goto labelB;
//            }
//
//            labelA:
//                x += 5;
//                if (x >= 100)
//                {
//                    goto exit;
//                }
//
//            labelB:
//                x -= 2;
//                if (x % 5 == 0)
//                {
//                    goto labelA;
//                }
//
//            exit:
//            return x;
//        }
//
//        [TestSvm]
//        public static int Gotos2(int x)
//        {
//            if (x <= 10)
//            {
//                goto labelB;
//            }
//
//            labelA:
//            x += 5;
//            if (x >= 100)
//            {
//                goto exit;
//            }
//
//            if (x > 50)
//            {
//                x *= 3;
//                goto labelA;
//            }
//
//            labelB:
//            x -= 2;
//            if (x % 5 == 0)
//            {
//                x *= 2;
//                goto labelA;
//            }
//
//            if (x % 5 == 1)
//            {
//                goto labelB;
//            }
//
//            exit:
//            return x;
//        }

//        [TestSvm]
//        public static int InfiniteCycle1(int x)
//        {
//            label:
//            x += 10;
//
//            goto label;
//        }

//        [TestSvm]
//        public static int GotosWithinSwitch(int x)
//        {
//            switch (x)
//            {
//                case 0:
//                    x += 2;
//                    x *= 3;
//                    goto case 2;
//
//                case 1:
//                    x *= 10;
//                    goto case 5;
//
//                case 2:
//                    x %= 50;
//                    goto case 4;
//
//                case 4:
//                    x += 34;
//                    if (x > 50)
//                    {
//                        goto case 5;
//                    }
//                    else
//                    {
//                        goto case 2;
//                    }
//
//                case 5:
//                    x -= 15;
//                    goto default;
//
//                default:
//                    if (x == 28)
//                    {
//                        break;
//                    }
//                    else
//                    {
//                        x += 100;
//                        goto case 0;
//                    }
//            }
//
//            return x;
//        }


        [TestSvm]
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

        [TestSvm]
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


//        [TestSvm]
//        public static int GotoOutwardsNestedCycle(int x)
//        {
//            int sum = 42;
//            for (int i = 1; i < x; i++)
//            {
//                sum += i;
//                for (int j = 0; j < i; j++)
//                {
//                    sum--;
//                    if (x == 42)
//                    {
//                        goto exit;
//                    }
//                }
//            }
//
//            exit:
//            x += sum;
//
//            return x;
//        }

        [TestSvm]
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

        [TestSvm]
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

        [TestSvm]
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

        [TestSvm]
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

        [TestSvm]
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

        [TestSvm]
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

//        [TestSvm]
//        public static int CycleWith3EntryPoints(int x)
//        {
//            if (x == 1)
//                goto lab1;
//            if (x == 2)
//                goto lab2;
//            goto lab3;
//
//            lab1:
//            x += 100500;
//            goto lab4;
//
//            lab2:
//            x *= 10;
//            goto lab4;
//
//            lab3:
//            x++;
//
//            lab4:
//            if (x == 123)
//                goto lab1;
//            if (x == 234)
//                goto lab2;
//            if (x == 345)
//                goto lab3;
//
//            return x;
//        }

//        public static int DivideWithException(int x)
//        {
//            try
//            {
//                return 100 / x;
//            }
//            catch (DivideByZeroException e)
//            {
//                Console.WriteLine(e.Message);
//                return 0;
//            }
//        }

//        public static int TryInsideCatch(int x, string s)
//        {
//            // some code outside try block
//            x++;
//
//            try
//            {
//                x = s.Length;
//            }
//            catch (NullReferenceException)
//            {
//                try
//                {
//                    x = 100 / x;
//                }
//                catch (Exception e) when (e is DivideByZeroException)
//                {
//                    x = 1;
//                }
//            }
//
//            return x;
//        }
//
//
//
//        public static int TryWithFinally(int x, string s)
//        {
//
//            x += 2;
//            try
//            {
//                x = s.Length;
//            }
//            finally
//            {
//                try
//                {
//                    x = 100 / x;
//                }
//                catch (Exception e) when (e is DivideByZeroException)
//                {
//                    x = 1;
//                }
//            }
//
//            return x;
//        }
//
//
//
//        public static int TryInsideWhile(int x)
//        {
//            int y = x;
//            int res = -1;
//            while (y > 0)
//            {
//                y--;
//                try
//                {
//                    res = 100 / y;
//                }
//                catch (DivideByZeroException)
//                {
//                    res = 100;
//                }
//            }
//
//            return res;
//        }
//
//
//        public static int RethrownInstructionIsNotCaughtByNextHandler(int x, string s)
//        {
//            Console.WriteLine("Method: CatchImplicitExceptionFromCatch");
//            try
//            {
//                x = s.Length / x;
//            }
//            catch (NullReferenceException nre)
//            {
//                Console.WriteLine($"Got NullReferenceException = {nre.Message}");
//                throw;
//            }
//            catch (DivideByZeroException divideByZeroException)
//            {
//                Console.WriteLine($"Got DivideByZeroException = {divideByZeroException.Message}");
//                x = 42;
//            }
//            catch
//            {
//                Console.WriteLine($"Got Exception");
//                x = 43;
//            }
//
//            return x;
//        }
//
//
//        public static int CantCatchExplicitExceptionFromCatch(int x, string s)
//        {
//            Console.WriteLine("Method: CatchExplicitExceptionFromCatch");
//            try
//            {
//                x = s.Length / x;
//            }
//            catch (NullReferenceException nre)
//            {
//                Console.WriteLine($"Got NullReferenceException = {nre.Message}");
//                throw new DivideByZeroException("Exception was thrown explicitly");
//            }
//            catch (DivideByZeroException divideByZeroException)
//            {
//                Console.WriteLine($"Got DivideByZeroException = {divideByZeroException.Message}");
//                x = 42;
//            }
//
//
//            Console.WriteLine("Code without exceptions");
//
//            return x;
//        }
//
//        public static bool FilterWithException(NullReferenceException nre)
//        {
//            throw new DivideByZeroException("Exception was thrown explicitly");
//        }
//
//        public static int ExceptionThrownInFilter(int x, string s)
//        {
//            Console.WriteLine("Method: ExceptionThrownInFilter");
//            try
//            {
//                x = s.Length;
//            }
//            catch (NullReferenceException nre) when (FilterWithException(nre))
//            {
//                Console.WriteLine($"Got NullReferenceException = {nre.Message}");
//            }
//            catch (NullReferenceException nre)
//            {
//                Console.WriteLine($"Got NullReferenceException = {nre.Message}");
//            }
//
//            Console.WriteLine("Code without exceptions");
//            return x;
//        }
//
//
//        public static bool FilterWithTryCatch(NullReferenceException nre)
//        {
//            try
//            {
//                return (100 / nre.Message.Length) < 5;
//            }
//            catch
//            {
//                Console.WriteLine("Exception occured in FilterWithTryCatch");
//                return true;
//            }
//        }
//
//        public static int FilterCanHaveExceptionHandler(int x, string s)
//        {
//            var nullReferenceExceptionWithEmptyMessage = new NullReferenceException("");
//            Console.WriteLine("Method: FilterCanNotHaveExceptionHandler");
//            try
//            {
//                x = s.Length;
//            }
//            catch (NullReferenceException nre) when (FilterWithTryCatch(nullReferenceExceptionWithEmptyMessage))
//            {
//                Console.WriteLine($"Got NullReferenceException in catch with filter\n{nre.Message}");
//            }
//            catch (NullReferenceException nre)
//            {
//                Console.WriteLine($"Got NullReferenceException in catch\n{nre.Message}");
//            }
//
//            return x;
//        }
//
//        public static int OnlyFilter(int x, string s)
//        {
//            try
//            {
//                x = s.Length;
//            }
//            catch (NullReferenceException nre) when(nre.Message == "")
//            {
//                x = 123;
//                Console.WriteLine($"Got NullReferenceException in catch\n{nre.Message}");
//            }
//
//            return x;
//        }
//
//        public static int FunctionWithFinallyThrowingException()
//        {
//            try
//            {
//                throw new SystemException("Explicitly thrown");
//            }
//            finally
//            {
//                Console.WriteLine("Finally was executed");
//            }
//        }
//
//        public static int FunctionWithHandler(int x)
//        {
//            try
//            {
//                x = FunctionWithFinallyThrowingException();
//            }
//            catch (SystemException)
//            {
//                x = 123;
//                Console.WriteLine($"Got SystemException");
//            }
//
//            return x;
//        }
//
//        public static int NestedTryBlocks(int x, string s)
//        {
//            try
//            {
//                try
//                {
//                    x = 100 / s.Length;
//                }
//                catch (NullReferenceException nre)
//                {
//                    Console.WriteLine(nre);
//                }
//                catch (Exception e) when (e is DivideByZeroException && s.Length > 0)
//                {
//                    Console.WriteLine(e.Message);
//                }
//                finally
//                {
//                    x++;
//                }
//            }
//            catch (Exception)
//            {
//                x = 1000;
//                Console.WriteLine();
//            }
//
//            return x;
//        }
//
//
//        public static int TryCatchFinally(int x, string s)
//        {
//            try
//            {
//                x = 100 / s.Length;
//            }
//            catch (NullReferenceException nre)
//            {
//                Console.WriteLine(nre);
//            }
//            finally
//            {
//                x++;
//            }
//
//            return x;
//        }
//
//        public static int ReturnFromTryWithFinally(int x, string s)
//        {
//            try
//            {
//                x = 100 / s.Length;
//                return x;
//            }
//            finally
//            {
//                Console.WriteLine("Finally");
//            }
//        }
//
//
//        public static int ManyFinallyBlocks(int x, string s)
//        {
//            try
//            {
//                try
//                {
//                    x = 100 / s.Length;
//                }
//                finally
//                {
//                    Console.WriteLine("Inner finally");
//                }
//
//            }
//            catch (System.Exception)
//            {
//                Console.WriteLine("Catch");
//            }
//            finally
//            {
//                Console.WriteLine("Outer Finally");
//            }
//
//            return x;
//        }
//
//        public static int TryCatchFilter(int x, string s)
//        {
//            try
//            {
//                x = 100 / s.Length;
//            }
//            catch (NullReferenceException)
//            {
//                Console.WriteLine("Catch {0}", 1);
//            }
//            catch (DivideByZeroException) when (x == 56)
//            {
//                Console.WriteLine("Filter {0}", 0);
//            }
//            return x;
//        }
//
//
//        public int CycleReturningValueAndException(int x)
//        {
//            int sum = 0;
//            for (int i = 0; i < x; i++)
//            {
//                if (i == 42)
//                    throw new Exception("42");
//                if (2 * i + x == 42)
//                {
//                    return 42;
//                }
//
//                sum++;
//            }
//
//            return sum;
//        }
    }
}
