using VSharp.CSharpUtils.Tests.Typecast;

namespace VSharp.CSharpUtils.Tests.VirtualMethods
{
    public interface IVirtual
    {
        int F();
    }

    public class VirtualB
    {
        public int F()
        {
            return 8;
        }
    }

    public class VirtualC : VirtualB, IVirtual
    {
        public new int F()
        {
            return 7;
        }

        int IVirtual.F()
        {
            return 71;
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

        public static int VirtualCal2(VirtualC a, int n)
        {
            if (a == null) return 0;
            if (n > 10)
            {
                return ((VirtualC) a).F();
            }

            return ((IVirtual) a).F();
        }
    }
}
