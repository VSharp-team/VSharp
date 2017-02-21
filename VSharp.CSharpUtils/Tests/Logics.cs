namespace VSharp.CSharpUtils.Tests
{
    public sealed class Logics
    {
        private static bool AlwaysTrue()
        {
            return true;
        }

        private static bool AlwaysFalse()
        {
            return false;
        }

        private static bool Negate(bool a)
        {
            return !a;
        }

        public static bool Negation1()
        {
            return !true;
        }

        public static bool Negation2(bool b)
        {
            return !b;
        }

        //public static bool Negation3(bool b)
        //{
        //    b = !b;
        //    return !b;
        //}

        public static bool Negation4(bool b)
        {
            return Negation2(!b);
        }

        public static bool Negation5(bool b)
        {
            return Negation2(Negation2(b));
        }

        public static bool And1(bool a, bool b)
        {
            return a & AlwaysTrue();
        }

        public static bool And2(bool a, bool b)
        {
            return AlwaysFalse() & AlwaysTrue();
        }

        public static bool And3(bool a, bool b)
        {
            return AlwaysFalse() & AlwaysFalse();
        }

        public static bool And4(bool a, bool b)
        {
            return AlwaysTrue() & AlwaysTrue();
        }

        public static bool And5(bool a, bool b)
        {
            return a & (b & Negate(a));
        }

        public static bool And6(bool a, bool b)
        {
            return (a & b) & (b & Negate(a));
        }

        public static bool And7(bool a, bool b)
        {
            return a & b & b & Negate(a) & b & b;
        }

        public static bool And8(bool a, bool b)
        {
            return a & b & b & b & b;
        }

        public static bool And9(bool a, bool b, bool c)
        {
            return (a & b & c) & (Negate(a) & b & Negate(c));
        }

        public static bool Or1(bool a, bool b)
        {
            return a | AlwaysTrue();
        }

        public static bool Or2(bool a, bool b)
        {
            return AlwaysFalse() | AlwaysTrue();
        }

        public static bool Or3(bool a, bool b)
        {
            return AlwaysFalse() | AlwaysFalse();
        }

        public static bool Or4(bool a, bool b)
        {
            return AlwaysTrue() | AlwaysTrue();
        }

        public static bool Or5(bool a, bool b)
        {
            return a | (b | Negate(a));
        }

        public static bool Or6(bool a, bool b)
        {
            return (a | b) | (b | Negate(a));
        }

        public static bool Or7(bool a, bool b)
        {
            return a | b | b | Negate(a) | b | b;
        }

        public static bool Or8(bool a, bool b)
        {
            return a | b | b | b | b;
        }

        public static bool Or9(bool a, bool b, bool c)
        {
            return (a | b | c) | (Negate(a) | b | Negate(c));
        }

        public static bool Or10(bool a, bool b, bool c)
        {
            return (a | b) | (a | b) | (a | b) | (a | b) | (a | b);
        }

        public static bool AndOr0(bool a, bool b, bool c)
        {
            return (AlwaysFalse() & AlwaysTrue()) & (AlwaysFalse() | AlwaysTrue());
        }

        // b & c
        public static bool AndOr1(bool a, bool b, bool c)
        {
            return b & (Negate(b) | c);
        }

        // !b & c
        public static bool AndOr2(bool a, bool b, bool c)
        {
            return Negate(b) & (b | c);
        }

        // b | c
        public static bool AndOr3(bool a, bool b, bool c)
        {
            return b | (Negate(b) & c);
        }

        // !b | c
        public static bool AndOr4(bool a, bool b, bool c)
        {
            return Negate(b) | (b & c);
        }

        public static bool AndOr5(bool a, bool b, bool c)
        {
            return (a & b) & (Negate(b) | c);
        }
  
    }
}