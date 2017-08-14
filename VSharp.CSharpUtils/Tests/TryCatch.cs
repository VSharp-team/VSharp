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

        public static int throwNull(int n)
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
