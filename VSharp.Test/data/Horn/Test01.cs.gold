using System;

namespace Test
{
    public sealed class Test01
    {
        private int Fib(int n)
        {
            return n < 2 ? 1 : Fib(n - 1) + Fib(n - 2);
        }

        public void DoSomething()
        {
            int n = new Random().Next(10);
            int fib = Fib(n);
            if (fib <= 0)
                Console.WriteLine("Goodbye cruel world :(");
            else
                Console.WriteLine("We do not actually need 3 lines above :)");
        }
    }
}
