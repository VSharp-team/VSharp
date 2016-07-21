using NUnit.Framework;
using VSharp.Core.Utils;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class UtilsTests
    {
        [Test]
        public void IdGeneratorTest1()
        {
            string v1 = IdGenerator.newId();
            string v2 = IdGenerator.newId();
            string f1 = IdGenerator.startingWith("Foo");
            string v3 = IdGenerator.startingWith("");
            string b1 = IdGenerator.startingWith("Bar");
            string f2 = IdGenerator.startingWith("Foo");
            Assert.AreEqual(v1, "v#!1");
            Assert.AreEqual(v2, "v#!2");
            Assert.AreEqual(v3, "v#!3");
            Assert.AreEqual(f1, "Foo1");
            Assert.AreEqual(f2, "Foo2");
            Assert.AreEqual(b1, "Bar1");
        }

        [Test]
        public void IdGeneratorTest2()
        {
            string v4 = IdGenerator.newId();
            string v5 = IdGenerator.newId();
            string f3 = IdGenerator.startingWith("Foo");
            string v6 = IdGenerator.startingWith("");
            string b2 = IdGenerator.startingWith("Bar");
            string f4 = IdGenerator.startingWith("Foo");
            Assert.AreEqual(v4, "v#!4");
            Assert.AreEqual(v5, "v#!5");
            Assert.AreEqual(v6, "v#!6");
            Assert.AreEqual(f3, "Foo3");
            Assert.AreEqual(f4, "Foo4");
            Assert.AreEqual(b2, "Bar2");
        }
    }
}
