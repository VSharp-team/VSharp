using System;
using NUnit.Framework;
using VSharp.Test;
#pragma warning disable CS0219

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class Conditional
    {
        private static int Max3(int x, int y, int z)
        {
            return Math.Max(x, Math.Max(y, z));
        }

        [TestSvm(100)]
        public static bool IsMaxEven(int x, int y, int z)
        {
            return Max3(2 * x, 2 * y, z) % 2 == 0;
        }

        private static int Mult2(int n)
        {
            int result = 0;
            for (int i = 0; i < n; ++i)
                result += 2;
            return result;
        }

        [TestSvm]
        public static int Always18()
        {
            return Mult2(9);
        }

//        private static int AlwaysN(int n)
//        {
//            for (int i = 0; i < n; i++)
//            {
//                if (i > 6)
//                {
//                    return n + 1;
//                }
//                if (i > 5)
//                {
//                    break;
//                }
//                if (i <= 5)
//                {
//                    continue;
//                }
//                n = 100;
//            }
//            return n;
//        }
//
//        public static int EnsureAlwaysN(int n)
//        {
//            return AlwaysN(n);
//        }

        private static int FirstEvenGreaterThen(int n)
        {
            for (int i = 0; i >= 0; i += 1)
            {
                if (i > n & i % 2 == 0)
                    return i;
            }
            return 100500;
        }

        [TestSvm]
        public static int FirstEvenGreaterThen7()
        {
            return FirstEvenGreaterThen(7);
        }

        // It's not a problem, that we got <VOID> < 5 or smth like that, because some path conditions are not achievable from program.
        // In case of TestSwitch method, we got <VOID> from dereferencing of not assigned variable.
        [TestSvm]
        public static bool TestSwitch(char c)
        {
            int result;
            switch (c)
            {
                case 'A':
                    result = 1;
                    break;
                case 'B':
                    result = 2;
                    break;
                case 'C':
                    result = 3;
                    break;
                case 'D':
                    result = 4;
                    break;
                case 'T':
                    throw new ArgumentException("Hey! Gimme number!");
                case 'R':
                    return false;
                default:
                    result = 0;
                    break;
            }
            return result < 5;
        }

        public class NewBool
        {
            public bool BoolValue;

            public bool ThrowException()
            {
                throw new NotImplementedException();
            }
        }

        [TestSvm]
        public static int ExceptionInCondition1(NewBool nb)
        {
            int n = 0;
            if (nb.BoolValue)
            {
                n = 67;
                return 42;
            }
            else
            {
                n = 56;
                return 56;
            }
        }

        [TestSvm]
        public static int ExceptionInCondition2(NewBool nb)
        {
            return nb.BoolValue ? 42 : 56;
        }

        [TestSvm]
        public static int ExceptionInCondition3(NewBool nb)
        {
            if (nb.ThrowException())
            {
                return 42;
            }
            else
            {
                return 56;
            }
        }

        [TestSvm(100)]
        public static int DeclareAfterReturn(bool flag, bool f, int x)
        {
            if (f)
            {
                if (flag)
                    return 42;
                int y = x + x;
                return x + x + y;
            }
            return x;
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static int PreconditionLoop(int n)
        {
            var num = 0;
            while (num < n)
            {
                num += 1;
            }

            return num; // n > 0 ? n : 0
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static int PostconditionLoop(int n)
        {
            var num = n;
            do
            {
                num -= 1;
            } while (num > 0);

            return num; // n > 1 ? 0 : n - 1
        }
    }
}
