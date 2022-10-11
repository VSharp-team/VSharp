using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public static class Byrefs
    {
        [TestSvm(100)]
        public static bool RefTest1(ref int n)
        {
            if (n + 1 > 0)
            {
                n = n + 10;
                if (n > 100)
                {
                    return true;
                }

                return false;
            }

            return false;
        }

        [TestSvm(100)]
        public static int RefTest2(int n)
        {
            var result = RefTest1(ref n);
            if (result)
            {
                return n;
            }

            return -n;
        }

        [TestSvm(100)]
        public static int OutTest1(out int n, int m)
        {
            if (m > 0)
            {
                n = 1;
            }
            else if (m < 0)
            {
                n = -1;
            }
            else
            {
                n = 0;
            }

            return n;
        }

        [TestSvm(100)]
        public static int OutTest2(int m)
        {
            var result = OutTest1(out var n, m);

            if (result > 0)
            {
                return n;
            }

            return -n;
        }

        [TestSvm(100)]
        public static int InTest1(in int n)
        {
            if (n > 0)
            {
                return 1;
            }
            else if (n < 0)
            {
                return -1;
            }

            return 0;
        }

        [TestSvm(100)]
        public static int InTest2(in int n)
        {
            if (InTest1(n) >= 0)
            {
                return n;
            }

            return -n;
        }

        [TestSvm(100)]
        public static int RefTest3(ref int n, ref int m)
        {
            if (n == m)
            {
                return 0;
            }

            return 1;
        }
    }

    [TestSvmFixture]
    public struct ByrefsStruct
    {
        [TestSvm(100)]
        public bool RefTest1(ref int n)
        {
            if (n + 1 > 0)
            {
                n = n + 10;
                if (n > 100)
                {
                    return true;
                }

                return false;
            }

            return false;
        }
    }
}
