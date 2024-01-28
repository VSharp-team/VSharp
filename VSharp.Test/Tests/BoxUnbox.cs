using NUnit.Framework;
using VSharp.Test;
#pragma warning disable CS0414

namespace IntegrationTests
{
    [TestSvmFixture]
    public class UnboxAny
    {
        public class A
        {
            private int x;

            public A()
            {
                x = 123;
            }
        }

        public struct B
        {
            private double x;

            public B(double z)
            {
                x = z;
            }
        }

        private static bool IsCast<T>(object o)
        {
            return o is T;
        }

        private static T AsCast<T> (object o) where T : class
        {
            return o as T;
        }

        private static T Cast<T> (object o)
        {
            return (T) o;
        }

        [TestSvm]
        public static B UnboxAny1()
        {
            var b = new B(5);
            return Cast<B>(b);
        }

        [TestSvm]
        public static object UnboxAny2()
        {
            var b = new B(5);
            return Cast<A>(b);
        }

        [TestSvm]
        public static object UnboxAny3()
        {
            var a = new A();
            return Cast<B>(a);
        }

        [TestSvm]
        public static object UnboxAny4()
        {
            var a = new A();
            return Cast<A>(a);
        }

        [TestSvm]
        public static uint[] UnboxAny5()
        {
            var a = new int[] {1, 2, 3};
            return Cast<uint[]>(a);
        }

        [TestSvm]
        public static int[] UnboxAny6()
        {
            var a = new uint[] {1, 2, 3};
            return Cast<int[]>(a);
        }
    }

    [TestSvmFixture]
    public class BoxUnbox
    {
        [TestSvm]
        public static object BoxInteger(int x)
        {
            object obj = x;
            return obj;
        }

        [TestSvm(100)]
        public static object TrickyBox(int x)
        {
            if (x == 5)
            {
                return x;
            }
            return 42;
        }

        [TestSvm]
        public static object Box7()
        {
            int? x = 7;
            return x;
        }

        [TestSvm]
        public static object BoxNullable(int? x)
        {
            return x;
        }

        [TestSvm]
        public static bool AlwaysNull()
        {
            return BoxNullable(null) == null;
        }

        private static bool AlwaysTrueForNullable(int? x)
        {
            object obj = x;
            int? y = (int?) obj;
            return x == y;
        }

        [TestSvm]
        public static bool True1()
        {
            return AlwaysTrueForNullable(null);
        }

        [TestSvm]
        public static bool True2()
        {
            int? x = 55;

            return AlwaysTrueForNullable(x);
        }

        [TestSvm]
        public static bool True3()
        {
            int x = 42;
            object obj = x;
            int y = (int) obj;
            return x == y;
        }
    }

    [TestSvmFixture]
    public class UnboxGeneric<T>
    {
        [TestSvm]
        public static T Cast(object o)
        {
            return (T) o;
        }
    }

    [TestSvmFixture]
    [IgnoreFuzzer("(Known bug) Last generated test is incorrect")]
    public class BoxUnboxWithGeneric<G, T, U, V>
        where G : IVirtual
        where T : class, IVirtual
        where U : struct, IVirtual
    {
        [TestSvm]
        public static object BoxValueOrReference(G t)
        {
            object o = t;
            return o;
        }

        [TestSvm]
        public static object BoxReference(T t)
        {
            object o = t;
            return o;
        }

        [TestSvm]
        public static object BoxValue(U t)
        {
            object o = t;
            return o;
        }
    }
}
