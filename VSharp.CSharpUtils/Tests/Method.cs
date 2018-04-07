using VSharp.CSharpUtils.Tests.Typecast;

namespace VSharp.CSharpUtils.Tests.Methods
{
    public interface IVirtual
    {
        int F();
    }

    public class VirtualB
    {
        public virtual int F()
        {
            return 8;
        }
    }

    public class VirtualD : VirtualB
    {
        private new int F()
        {
            return 44;
        }
    }

    public class VirtualE : VirtualD
    {
        public override int F()
        {
            return 9;
        }
    }

    public class VirtualC : VirtualB, IVirtual
    {
        public new virtual int F()
        {
            return 7;
        }

        int IVirtual.F()
        {
            return 71;
        }
    }

    public class VirtualG : VirtualC
    {
        public override int F()
        {
            return 66;
        }
    }

    public static class VirtualMethod
    {
        public static IMovable MakeVirtualMove(Pawn p, Coord c)
        {
            p?.MakeMove(c);
            return p;
        }

        public static IMovable MakeInterfaceMove(IMovable p, Coord c)
        {
            return p?.MakeMove(c);
        }

        public static IMovable MakeConcreteMove(Coord c)
        {
            var p = new Piece(0, 0);
            return p.MakeMove(c);
        }

        public static int VirtualCall(IVirtual a)
        {
            if (a == null) return 0;
            if (a is VirtualC)
            {
                return ((VirtualC) a).F();
            }

            return ((IVirtual) a).F();
        }

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
