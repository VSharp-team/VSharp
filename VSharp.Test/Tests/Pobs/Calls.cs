using VSharp.Test;

namespace IntegrationTests;

public class Calls
{
    [TestSvmFixture]
    public class NestedCalls
    {
        [TestSvm(100)]
        public static int F1(int x)
        {
            if (x > 0)
            {
                return F2(5);
            }

            return F2(-x);
        }

        [TestSvm(85)]
        public static int F2(int x)
        {
            switch (x)
            {
                case 5:
                    return F3(11);
                case 7:
                    return F3(6);
                default:
                    return F3(x + x + 1);
            }
        }

        public static int F3(int x)
        {
            return F4(x, 2);
        }

        public static int F4(int x, int y)
        {
            if ((x + y) % 2 == 0)
            {
                throw null;
            }

            return 42;
        }
    }

    [TestSvmFixture]
    public class Recursion
    {
        public static int F(int x)
        {
            int sum = 0;
            for (int i = 0; i < x; i++)
            {
                if (x != 7)
                {
                    sum += G(x);
                }
                else
                {
                    sum++;
                }
            }

            return sum;
        }

        public static int G(int x)
        {
            int y = F(x + 1);
            throw null;
        }

        [TestSvm(88)]
        public static int TrickyCycle(int x, int y)
        {
            int sum = 0;
            if (y > 5)
            {
                for (int i = 0; i < y; i++)
                {
                    sum += TrickyCycle(x, y - 1);
                }

                return sum;
            }

            for (int i = 0; i < x; i++)
            {
                if (x != 7)
                {
                    sum += G(x);
                }
                else
                {
                    sum++;
                }
            }

            return sum;
        }
    }
}
