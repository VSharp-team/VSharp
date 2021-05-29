namespace VSharp.Test.Tests.Pobs
{
    public class Calls
    {
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




        }



    }
}