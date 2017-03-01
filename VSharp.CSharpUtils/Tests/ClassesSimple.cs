namespace VSharp.CSharpUtils.Tests
{
    internal class A
    {
        private B _b = new B { _c = new C { _n = 13 } };
        private int _intField = 100500;

        public A()
        {
            _intField = 100501;
        }

        public A(int n)
        {
            _b._c._n = n;
            _b._c.SetN(n);
        }

        public void IncN()
        {
            _b.SetN(_b.GetN() + 1);
        }

        public void DecN()
        {
            _b.SetN(_b.GetN() - 1);
        }

        public int GetN()
        {
            return this._b.GetN();
        }
    }

    internal struct B
    {
        public C _c;

        public int GetN()
        {
            return _c._n;
        }

        public void SetN(int n)
        {
            _c._n = n;
        }
    }

    public struct C
    {
        public int _n;

        public void SetN(int n)
        {
            _n = n;
        }
    }

    public static class ClassesSimple
    {
        public static bool Test1(int n)
        {
            A a = new A(n);
            a.IncN();
            a.DecN();
            return n == a.GetN();
        }
    }
}
