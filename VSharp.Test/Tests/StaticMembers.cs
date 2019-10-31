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

    [TestSvmFixture]
    public static class Program
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
    }
}
