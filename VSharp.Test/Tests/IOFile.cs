using System;
using System.Linq;
using NUnit.Framework;
using VSharp.Test;

namespace IntegrationTests
{
    [TestSvmFixture]
    public sealed class IOFile
    {
        [TestSvm]
        public static string readFromEmptyFname()
        {
            var s1 = System.IO.File.ReadAllText("");
            return s1;
        }

        [TestSvm]
        public static bool readTwiceFromCorrectPath()
        {
            var s1 = System.IO.File.ReadAllText("/etc/inputrc");
            var s2 = System.IO.File.ReadAllText("/etc/inputrc");
            return s1 == s2;
        }

        [TestSvm]
        public static bool readTwiceFromIncorrectPath()
        {
            var s1 = System.IO.File.ReadAllText("aVeryStrangeFileName");
            var s2 = System.IO.File.ReadAllText("42");
            return s1 == s2;
        }

        [TestSvm]
        public static int readToArray2()
        {
            var s1 = System.IO.File.ReadAllLines("/etc/inputrc");
            return s1.Length;
        }

        [TestSvm]
        public static bool readToArray()
        {
            var s1 = System.IO.File.ReadAllLines("aVeryStrangeFileName");
            return s1.Length == 0;
        }
    }
}
