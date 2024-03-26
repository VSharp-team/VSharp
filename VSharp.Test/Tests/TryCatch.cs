using System;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public class TryCatch
    {
        private void CheckPositiveAndOdd(int n)
        {
            if (n == 0)
            {
                throw new ArgumentException("Argument should not be zero!");
            }

            if (n <= 0)
            {
                throw new InvalidOperationException("Hmm.. negative numbers are also not allowed!");
            }

            if (n % 2 == 0)
            {
                throw new Exception("Not odd!");
            }
        }

        [TestSvm(100)]
        public bool MakeOdd(int n)
        {
            try
            {
                CheckPositiveAndOdd(n);
            }
            catch (ArgumentException)
            {
                n = 1;
            }
            catch (InvalidOperationException) when (n <= 0)
            {
                n = -n + 1;//return MakeOdd(-n);
            }
            catch
            {
                n++;
            }
            return n % 2 == 1;
        }

        [TestSvm]
        public static int ThrowNull(int n)
        {
            try
            {
                throw null;
            }
            catch (NullReferenceException e)
            {
                return e.HResult;
            }
        }

        public class MyDispose : IDisposable
        {
            public int[] X_field;

            public MyDispose(int[] x)
            {
                X_field = x;
            }

            public virtual void Dispose()
            {
                X_field[0] = 10;
            }
        }

        public class AnotherDisposable : MyDispose
        {
            public AnotherDisposable(int[] x) : base(x)
            {}
        }

        public class AnotherDisposable1 : MyDispose
        {
            public AnotherDisposable1(int[] x) : base(x)
            {}

            public override void Dispose()
            {
                X_field[0] = 20;
            }
        }

        public class YetAnotherDisposable1 : MyDispose
        {
            public YetAnotherDisposable1(int[] x) : base(x)
            {}

            public new virtual void Dispose()
            {
                X_field[0] = 30;
            }
        }

        public class YetAnotherDisposable2 : MyDispose, IDisposable
        {
            public YetAnotherDisposable2(int[] x) : base(x)
            {}

            public new virtual void Dispose()
            {
                X_field[0] = 30;
            }
        }

        [TestSvm]
        public static int UsingTest()
        {
            var myDispose = new MyDispose(new []{ 57 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 67
        }

        [TestSvm]
        public static int UsingTestWithInheritance()
        {
            var myDispose = new AnotherDisposable(new []{ 57 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 67
        }

        [TestSvm]
        public static int UsingTestWithInheritance1()
        {
            var myDispose = new AnotherDisposable1(new []{ 57 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 77
        }

        [TestSvm]
        public static int AnotherUsingTestWithInheritance1()
        {
            var myDispose = new YetAnotherDisposable1(new []{ 57 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 67
        }

        [TestSvm]
        public static int AnotherUsingTestWithInheritance2()
        {
            var myDispose = new YetAnotherDisposable2(new []{ 57 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 87
        }

        public sealed class NotPositive : Exception
        {
        }

        private int Fact(int n)
        {
            if (n <= 0)
                throw new NotPositive();
            try
            {
                return n * Fact(n - 1);
            }
            catch (NotPositive)
            {
                return 1;
            }
        }

        [TestSvm(100, strat: SearchStrategy.BFS)]
        [IgnoreFuzzer("Need StackOverflow handling")]
        public int FactResult(int n)
        {
            try
            {
                return Fact(n);
            }
            catch (NotPositive)
            {
                return 0;
            }
        }

        [Ignore("handle recursion")]
        [IgnoreFuzzer("Need StackOverflow handling")]
        public int CheckFactSafe(int n)
        {
            try
            {
                return Fact(n);
            }
            catch (NotPositive) when (n <= 0)
            {
                return 0;
            }
        }

        [Ignore("handle recursion")]
        [IgnoreFuzzer("Need StackOverflow handling")]
        public int CheckFactUnsafe(int n)
        {
            try
            {
                return Fact(n);
            }
            catch (NotPositive) when (n < 0)
            {
                return 0;
            }
        }
    }
}
