namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class ConstraintIndependenceSandbox
    {
        [TestSvm]
        public static int Sandbox(int a, int b)
        {
            if (a != b)
            {
                if (a > 100)
                {
                    if (b < 200)
                    {
                        return 1;
                    }
                    else
                    {
                        return 2;
                    }
                }
                else
                {
                    if (b < 150)
                    {
                        return 3;
                    }
                    else
                    {
                        return 4;
                    }
                }
            }
            else
            {
                return 5;
            }
        }
    }
}