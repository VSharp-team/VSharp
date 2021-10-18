using System;
using System.Collections.Generic;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
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

        [Ignore("use different equality comparers for different types")]
        public static EqualityComparer<String> TestEqualityComparer()
        {
            return EqualityComparer<String>.Default;
        }
    }
}
