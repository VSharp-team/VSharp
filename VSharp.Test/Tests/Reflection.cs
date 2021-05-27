using System;
using System.Collections.Generic;

namespace VSharp.Test.Tests
{
    [TestSvmFixture]
    public class Reflection
    {
        [TestSvm]
        public static string TestGetTypeName(object o)
        {
            var handle = Type.GetTypeHandle(o);
            var type = Type.GetTypeFromHandle(handle);
            return type.Name;
        }

        [TestSvm]
        public static EqualityComparer<String> TestEqualityComparer()
        {
            return EqualityComparer<String>.Default;
        }
    }
}
