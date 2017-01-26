using System;

namespace Test
{
    public sealed class Test01
    {
        //private int Fib1(int n)
        //{
        //    if (n < 2)
        //        return 1;
        //    else
        //        return Fib1(n - 1) + Fib1(n - 2);
        //}

        //private int Fib2(int n)
        //{
        //    if (n < 2)
        //        return 1;
        //    return Fib1(n - 1) + Fib2(n - 2);
        //}

        //private int Fib3(int n)
        //{
        //    return n < 2 ? 1 : Fib3(n - 1) + Fib3(n - 2);
        //}

        public void DoSomething(int n)
        {
            //int n = new Random().Next(10);
            //int fib1 = Fib1(n);
            //int fib2 = Fib2(n);
            ////int fib3 = Fib3(n);
            //if (fib1 <= 0 || fib2 <= 0 /*|| fib3 <= 0*/)
            VSharp.CSharpUtils.Tests.Arithmetics.Add7(35, 42);
            //Math.Max(2, 1);
            VSharp.CSharpUtils.Tests.Branching.IsMaxOdd(n, 2 * n, 3 * n);
            //else
            //Console.WriteLine("We do not actually need 3 lines above");
        }
    }
}
