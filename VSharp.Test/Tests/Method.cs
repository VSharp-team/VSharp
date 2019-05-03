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
    public static class VirtualMethod
    {
        [TestSvm]
        public static IMovable MakeVirtualMove(Pawn p, Coord c)
        {
            p?.MakeMove(c);
            return p;
        }

        [TestSvm]
        public static IMovable MakeInterfaceMove(IMovable p, Coord c)
        {
            return p?.MakeMove(c);
        }

        [TestSvm]
        public static IMovable MakeConcreteMove(Coord c)
        {
            var p = new Piece(0, 0);
            return p.MakeMove(c);
        }

        [TestSvm]
        public static int VirtualCall(IVirtual a)
        {
            if (a == null) return 0;
            if (a is VirtualC)
            {
                return ((VirtualC) a).F();
            }

            return ((IVirtual) a).F();
        }

        [TestSvm]
        public static int VirtualCall1(VirtualC a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F();
            }

            return ((IVirtual) a).F();
        }

//        public static int VirtualCall2(VirtualB a) //TODO: Ticket
//        {
//            if (a == null) return 0;
//            if (a is VirtualE)
//            {
//                return ((VirtualE) a).F();
//            }
//
//            return a.F();
//        }

        [TestSvm]
        public static int VirtualCall3(VirtualG a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualB) a).F();
            }

            return a.F();
        }
    }
}
