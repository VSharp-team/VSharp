using System;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public class Enums
    {
        public enum Color
        {
            Red,
            Blue,
            Yellow
        }

        public enum NonZeroEnum
        {
            First = 1,
            Second = 2
        }

        [TestSvm(100)]
        public static int Color2Int(Color c)
        {
            switch (c)
            {
                case Color.Blue: return 500;
                case Color.Red: return 10;
                default: return 42;
            }
        }

        [TestSvm(100)]
        public static int SymbolicColor2Int(Color c, int v)
        {
            Color x = (Color) v;
            if (c == x)
            {
                return 42;
            }

            return 100;
        }

        [TestSvm(100)]
        public static int CheckIsEnum(Color c)
        {
            var res = 0;
            if (c is Enum)
            {
                res += 10;
            }

            object o = c;
            if (o is Enum)
            {
                res += 42;
            }

            if (o is Color)
            {
                res += 11;
            }

            return res;
        }

        [TestSvm(100)]
        public static string EnumToString(Color c)
        {
            return string.Format("c == Color.Blue, c.ToString() == {0}", Color.Blue);
        }

        [TestSvm(100)]
        public static string EnumToString1(Color c)
        {
            if (c == Color.Blue)
                return string.Format("c == Color.Blue, c.ToString() == {0}", c);
            return string.Format("c != Color.Blue, c.ToString() == {0}", c);
        }
        [Ignore("GetType() is not implemented")]
        public static Type GetEnumType(Color c)
        {
            return c.GetType();
        }

        [TestSvm(100)]
        public static int NonZeroEnumTest(NonZeroEnum e)
        {
            return (int) e;
        }
    }
}
