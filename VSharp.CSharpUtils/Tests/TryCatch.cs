using System;

namespace VSharp.CSharpUtils
{
    public class TryCatch
    {
        public int SafeFunc(int n)
        {
            try
            {
                return 1;
            }
            catch
            {
                throw new Exception();
            }
        }

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

        public class YetAnotherDisposable : MyDispose
        {
            public YetAnotherDisposable(int[] x) : base(x)
            {}

            public virtual void Dispose()
            {
                X_field[0] = 66;
            }
        }

        public static int UsingTest()
        {
            var myDispose = new MyDispose(new []{ 57 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 67
        }

        public static int UsingTestWithInheritance()
        {
            var myDispose = new AnotherDisposable(new []{ 57 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 67
        }

/*        public static int AnotherUsingTestWithInheritance()
        {
            var myDispose = new YetAnotherDisposable(new []{ 67 });
            int num;
            using (myDispose)
                num = myDispose.X_field[0];
            return num + myDispose.X_field[0]; // 77
        }*/

//        public sealed class NotPositive : Exception
//        {
//        }
//
//        private int Fact(int n)
//        {
//            if (n <= 0)
//                throw new NotPositive();
//            try
//            {
//                return n * Fact(n - 1);
//            }
//            catch (NotPositive)
//            {
//                return 1;
//            }
//        }
//
//        public int CheckFactSafe(int n)
//        {
//            try
//            {
//                return Fact(n);
//            }
//            catch (NotPositive) when (n <= 0)
//            {
//                return 0;
//            }
//        }
//
//        public int CheckFactUnsafe(int n)
//        {
//            try
//            {
//                return Fact(n);
//            }
//            catch (NotPositive) when (n < 0)
//            {
//                return 0;
//            }
//        }
    }
}
