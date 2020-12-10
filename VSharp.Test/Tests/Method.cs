using NUnit.Framework;
using VSharp.Test.Tests.Typecast;

namespace VSharp.Test.Tests.Methods
{
    public interface IVirtual
    {
        int F();
    }

    [TestSvmFixture]
    public class VirtualB
    {
        [TestSvm]
        public virtual int F()
        {
            return 8;
        }
    }

    [TestSvmFixture]
    public class VirtualD : VirtualB
    {
        private new int F()
        {
            return 44;
        }
    }

    [TestSvmFixture]
    public class VirtualE : VirtualD
    {
        [TestSvm]
        public override int F()
        {
            return 9;
        }
    }

    [TestSvmFixture]
    public class VirtualC : VirtualB, IVirtual
    {
        [TestSvm]
        public new virtual int F()
        {
            return 7;
        }

        int IVirtual.F()
        {
            return 71;
        }

        public virtual int F<T>()
        {
            return 7;
        }
    }

    [TestSvmFixture]
    public class VirtualG : VirtualC
    {
        [TestSvm]
        public override int F()
        {
            return 66;
        }
    }

    [TestSvmFixture]
    public class VirtualH : VirtualC
    {
        [TestSvm]
        public override int F()
        {
            return 7777777;
        }

        public override int F<T>()
        {
            return -7777777;
        }
    }

    public class VirtualI<T> : VirtualC
    {
        public override int F()
        {
            return 111;
        }

        public override int F<_>()
        {
            return -111;
        }
    }

    public class VirtualJ<T> : VirtualI<T>
    {
        public override int F()
        {
            return 5555;
        }

        public virtual int F<T, U>()
        {
            return base.F<U>();
        }
    }

    public class VirtualK : VirtualI<double>
    {
        public override int F()
        {
            return 6666;
        }

        public virtual int F<T, U>()
        {
            return base.F<U>() + 10000;
        }
    }


    [TestSvmFixture]
    public static class VirtualMethod
    {
        [Ignore("Calling virtual method is not implemented")]
        public static IMovable MakeVirtualMove(Pawn p, Coord c)
        {
            p?.MakeMove(c);
            return p;
        }

        [Ignore("Calling virtual method is not implemented")]
        public static IMovable MakeInterfaceMove(IMovable p, Coord c)
        {
            return p?.MakeMove(c);
        }

        [Ignore("Calling virtual method is not implemented")]
        public static IMovable MakeConcreteMove(Coord c)
        {
            var p = new Piece(0, 0);
            return p.MakeMove(c);
        }

        [Ignore("Exceptions handling")]
        public static IMovable MakeNullMove(Coord c)
        {
            IMovable p = null;
            return p.MakeMove(c);
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall(IVirtual a)
        {
            if (a == null) return 0;
            if (a is VirtualC)
            {
                return ((VirtualC) a).F();
            }

            return ((IVirtual) a).F();
        }

        public class A {
            public virtual int F() {
                return 1;
            }
        }

        public class B : A {
            public override int F() {
                return 2;
            }
        }

        public class C : B {
            public override int F() {
                return 3;
            }
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall1(VirtualC a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F();
            }

            return ((IVirtual) a).F();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall2(VirtualB a)
        {
            if (a == null) return 0;
            if (a is VirtualE)
            {
                return ((VirtualE) a).F();
            }

            return a.F();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall3(VirtualG a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualB) a).F();
            }

            return a.F();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall4(VirtualH a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F();
            }

            return ((IVirtual) a).F();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall5(VirtualH a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F<int>();
            }

            return ((IVirtual) a).F();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall6(VirtualI<double> a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F<int>();
            }

            return ((IVirtual) a).F();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall7(VirtualJ<float> a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F<float>();
            }

            return a.F<float, int>();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int VirtualCall8(VirtualK a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F<float>();
            }

            return a.F<float, int>();
        }

        [Ignore("Exceptions handling")]
        public static int VirtualCall9(A a)
        {
            if (a is B)
            {
                return ((B) a).F();
            }
            return a.F();
        }

        [Ignore("Calling virtual method is not implemented")]
        public static int CheckSightTypeWorksCorrect(C c)
        {
            if (c == null) return 0;
            A a = (A) c;
            return a.F();
        }
    }
}
