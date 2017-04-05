using System;

namespace VSharp.CSharpUtils
{
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
                return MakeOdd(-n);
            }
            catch
            {
                n++;
            }
            return n % 2 == 1;
        }
    }
}
