using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public sealed class IOFile
    {
        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static string ReadFromEmptyFname()
        {
            var s1 = System.IO.File.ReadAllText("");
            return s1;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static bool ReadTwiceFromCorrectPath()
        {
            var s1 = System.IO.File.ReadAllText("/etc/inputrc");
            var s2 = System.IO.File.ReadAllText("/etc/inputrc");
            return s1 == s2;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static bool ReadTwiceFromIncorrectPath()
        {
            var s1 = System.IO.File.ReadAllText("aVeryStrangeFileName");
            var s2 = System.IO.File.ReadAllText("42");
            return s1 == s2;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static int ReadToArray2()
        {
            var s1 = System.IO.File.ReadAllLines("/etc/inputrc");
            return s1.Length;
        }

        // [TestSvm(hasExternMocking: true)]
        [Ignore("shims are not supported for .NET 7")]
        public static bool ReadToArray()
        {
            var s1 = System.IO.File.ReadAllLines("aVeryStrangeFileName");
            return s1.Length == 0;
        }
    }
}
