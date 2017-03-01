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
        //a
        public static bool And1(bool a, bool b)
        {
            return a & AlwaysTrue();
        }

        //false
        public static bool And2(bool a, bool b)
        {
            return AlwaysFalse() & AlwaysTrue();
        }

        //false
        public static bool And3(bool a, bool b)
        {
            return AlwaysFalse() & AlwaysFalse();
        }

        //true
        public static bool And4(bool a, bool b)
        {
            return AlwaysTrue() & AlwaysTrue();
        }

        //false
        public static bool And5(bool a, bool b)
        {
            return a & (b & Negate(a));
        }

        //false
        public static bool And6(bool a, bool b)
        {
            return (a & b) & (b & Negate(a));
        }

        //false
        public static bool And7(bool a, bool b)
        {
            return a & b & b & Negate(a) & b & b;
        }

        //a&b
        public static bool And8(bool a, bool b)
        {
            return a & b & b & b & b;
        }

        //false
        public static bool And9(bool a, bool b, bool c)
        {
            return (a & b & c) & (Negate(a) & b & Negate(c));
        }

        //true
        public static bool Or1(bool a, bool b)
        {
            return a | AlwaysTrue();
        }

        //true
        public static bool Or2(bool a, bool b)
        {
            return AlwaysFalse() | AlwaysTrue();
        }

        //false
        public static bool Or3(bool a, bool b)
        {
            return AlwaysFalse() | AlwaysFalse();
        }

        //true
        public static bool Or4(bool a, bool b)
        {
            return AlwaysTrue() | AlwaysTrue();
        }

        //true
        public static bool Or5(bool a, bool b)
        {
            return a | (b | Negate(a));
        }

        //true
        public static bool Or6(bool a, bool b)
        {
            return (a | b) | (b | Negate(a));
        }

        //true
        public static bool Or7(bool a, bool b)
        {
            return a | b | b | Negate(a) | b | b;
        }

        //a|b
        public static bool Or8(bool a, bool b)
        {
            return a | b | b | b | b;
        }

        //true
        public static bool Or9(bool a, bool b, bool c)
        {
            return (a | b | c) | (Negate(a) | b | Negate(c));
        }

        //a|b
        public static bool Or10(bool a, bool b, bool c)
        {
            return (a | b) | (a | b) | (a | b) | (a | b) | (a | b);
        }

        //false
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

        //a&b&c
        public static bool AndOr5(bool a, bool b, bool c)
        {
            return (a & b) & (Negate(b) | c);
        }

        //(!a&b)|(a&c)
        public static bool AndOr6(bool a, bool b, bool c)
        {
            return (Negate(a) & b) | (a & c);
        }

        //((a | b) & (a | c)) ?? dnf - ol, but cnf is shorter
        public static bool AndOr7(bool a, bool b, bool c)
        {
            return (a | b) & (a | c);
        }

        //((a & b) | (a & c)) ?? dnf - ol, but cnf is shorter
        public static bool AndOr8(bool a, bool b, bool c)
        {
            return (a & b) | (a & c);
        }

        //((b | a) & (a | c))
        public static bool AndOr9(bool a, bool b, bool c)
        {
            return (b | a) & (a | c);
        }

        //((b & a) | (a & c))
        public static bool AndOr10(bool a, bool b, bool c)
        {
            return (b & a) | (a & c);
        }

        //((!a) | (!c) | (!b) | d)
        public static bool AndOr11(bool a, bool b, bool c, bool d)
        {
            return (a & b & c & d) | !(a & b & c);
        }

        //
        public static bool AndOr12(bool a, bool b, bool c, bool d)
        {
            return ((!a & b) | (a & c)) & !a;
        }

        // true
        public static bool AndOr13(bool a, bool b, bool c, bool d)
        {
            return !a & b | !(a & b) | b;
        }

        // (b & (!a))
        public static bool AndOr14(bool a, bool b, bool c, bool d)
        {
            return (a | b) & (!a | b) & (!a | !b);
        }

        // (((!a) | (!b)) & c)
        public static bool AndOr15(bool a, bool b, bool c, bool d)
        {
            return !(a & b | !c);
        }

        // (c | a | (!b))
        public static bool AndOr16(bool a, bool b, bool c, bool d)
        {
            return a | !(b & !c) | !(!a | b | !c);
        }

        // (((a & (!b)) | (c & a)) | (a & ((!b) | (!c)))) : it's equal to a, but this result is ok
        public static bool AndOr17(bool a, bool b, bool c, bool d)
        {
            return a & !b | a & b & c | a & !b & c | a & !(b & c);
        }

        // ((((a & (!b)) | c) & ((!a) | b)) | (!c))
        public static bool AndOr18(bool a, bool b, bool c, bool d)
        {
            return (a & !b | c) & (!a | b) | !c;
        }

        // (b & a)
        public static bool AndOr19(bool a, bool b, bool c, bool d)
        {
            return a & b & (!a & c | !(!(a & b) & c) | c & d);
        }

        // a
        public static bool AndOr20(bool a, bool b, bool c, bool d)
        {
            return a & b | a & !b & c | !b & a & !c | a & !c;
        }

        // ((!a) & b)
        public static bool AndOr21(bool a, bool b, bool c, bool d)
        {
            return ((a | b) & !a) | (!(a | b) & !!a);
        }

        // ((!b) & a)
        public static bool AndOr22(bool a, bool b, bool c, bool d)
        {
            return !((a & b) | (!a & !b)) & (a | !b);
        }

        // (b & (!a))
        public static bool AndOr23(bool a, bool b, bool c, bool d)
        {
            return (a | b) & (!a | b) & (!a | !b);
        }

        // ((((!c) | a) & ((!a) | b)) & ((!c) | (!a) | (!b)))
        public static bool AndOr24(bool a, bool b, bool c, bool d)
        {
            return (a | b | !c) & (a | !b | !c) & (!a | b | c) & (!a | b | !c) & (!a | !b | !c);
        }

        // a
        public static bool AndOr25(bool a, bool b, bool c, bool d)
        {
            bool e = true;
            return a | (a & b & c & d & e);
        }
        
    }
}