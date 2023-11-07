using System;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class Lambdas
    {
        private static int Mult2(int n)
        {
            int result = 0;
            {
                Func<int, int> loop = null;
                loop = i =>
                {
                    if (i < n)
                    {
                        result += 2;
                        ++i;
                        return loop(i);
                    }
                    return result;
                };
                loop(0);
            }
            return result;
        }

        // Expecting 18
        [TestSvm]
        public static int Always18()
        {
            return Mult2(9);
        }

        // Expecting always true
        [TestSvm(100)]
        public static bool DoubleValue(int n, bool flag)
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
            assign(n);
            return a + b + c == n + n;
        }

        public static bool CheckIsLambda<T>(object o)
        {
            return o is Action<int>;
        }

        [TestSvm]
        public static bool LambdaAsObjectIsLambda(bool flag)
        {
            Action<int> nop;
            nop = x => {};
            object o = nop;
            return CheckIsLambda<Action<int>>(o);
        }

        public static bool FunctionInsideFunc(int x)
        {
            return x > 0;
        }

        [TestSvm(100)]
        public static bool FuncFromFunction(int x)
        {
            var func = new Func<int, bool>(FunctionInsideFunc);
            return func.Invoke(x);
        }

        [TestSvm(100)]
        public static bool FuncFromLambda(int x)
        {
            var func = new Func<int, bool>(i => i <= 0);
            return func.Invoke(x);
        }

        [Ignore("exception handling")]
        public static Action<int> NullLambdaInvoke()
        {
            Action<int> nullAction = null;
            nullAction(42);
            return nullAction;
        }

        [TestSvm(100)]
        public static bool SymbolicLambdaInvoke(Func<int, bool> f)
        {
            return f.Invoke(42);
        }

        [TestSvm]
        public static bool ConcreteLambdaInvokeSymbolic()
        {
            Func<int, bool> f = i => i < 0;
            return SymbolicLambdaInvoke(f);
        }

        [Ignore("exception handling")]
        public static bool NullLambdaInvokeSymbolic()
        {
            Func<int, bool> f = null;
            return SymbolicLambdaInvoke(f);
        }

/*        public static int ForToLambdaDelegateSmokeTest(int n)
        {
            int s = 0;
            for (int i = 0; i < n; i++)
            {
                s += i * i;
            }
            return s;
        }

        public static int ForToLambdaDelegateNestedTest(int n)
        {
            int num = 0;
            for (int index1 = 0; index1 < n; ++index1)
            {
                for (int index2 = 0; index2 < index1; ++index2)
                    num += index1 * index2;
            }
            return num;
        }*/
    }
}
