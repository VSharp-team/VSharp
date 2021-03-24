using System;
using NUnit.Framework;

namespace VSharp.Test.Tests
{
    internal class StaticClass
    {
        private static int GetInitialVal()
        {
            return 100500;
        }

        private static int _a = _b + GetInitialVal();
        private static int _b = _a + 1;

        public static int GetA()
        {
            return _a;
        }

        static StaticClass()
        {
            _a += 100;
            throw new Exception();
        }
    }

    public static class ClassWithStaticCctor
    {
        public static PDR.A _a1 = new PDR.A(90);
        public static PDR.A _a2;
        public static PDR.A _a3;
        public static PDR.ClassWithOneField _classWithOneField1 = new PDR.ClassWithOneField();
        public static PDR.ClassWithOneField _classWithOneField2;
        public static PDR.ClassWithOneField _classWithOneField3; // expecting null
        public const PDR.ClassWithOneField _classWithOneField4 = null;
        public const int _fortyTwo_int = 42;
        public const short _fortyTwo_short = 42;
        public const ushort _fortyTwo_ushort = 42;
        public const byte _fortyTwo_byte = 42;
        public const sbyte _fortyTwo_sbyte = 42;
        public const char _fortyTwo_char = (char)_fortyTwo_sbyte;
        public const double _pi_double = 3.14;
        public const float _pi_float = 3.14f;
        public static double _e;
        public static Enums.Color c1 = Enums.Color.Blue;
        public const Enums.Color c2 = Enums.Color.Yellow;
        public static Enums.Color c3;

        static ClassWithStaticCctor()
        {
            _a2 = new PDR.A(190);
            _classWithOneField2 = new PDR.ClassWithOneField();
            _classWithOneField2.x = 78;
        }

        public static void Init() { }
    }

    [TestSvmFixture]
    public static class StaticClassTests
    {
        [Ignore("Exceptions handling")]
        public static bool CheckStatic()
        {
            var t = typeof(StaticClass);
            try
            {
                return StaticClass.GetA() == 100600;
            }
            catch (Exception)
            {
                return t.ToString() == "VSharp.Test.Tests.StaticClass";
            }
        }


        // expecting 0
        [TestSvm]
        public static int InitStaticCctor()
        {
            ClassWithStaticCctor.Init();
            return ClassWithStaticCctor._a3.x;
        }
    }
}