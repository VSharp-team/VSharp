using VSharp.Test;

namespace IntegrationTests;

[TestSvmFixture]
public class RecursiveObjects
{
    public class A
    {
        private B _b;

        public void SetB(B b)
        {
            _b = b;
        }
    }

    public class B
    {
        private A _a;

        public void SetA(A a)
        {
            _a = a;
        }
    }

    [TestSvm]
    public static A RecObject(int n)
    {
        if (n > 0)
        {
            return null;
        }

        var a = new A();
        var b = new B();
        a.SetB(b);
        b.SetA(a);
        return a;
    }
}
