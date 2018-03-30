using NUnit.Framework;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class UtilsTests
    {
        [Test]
        public void IdGeneratorTest1()
        {
            IdGenerator.reset();
            string v1 = IdGenerator.newId();
            string v2 = IdGenerator.newId();
            string f1 = IdGenerator.startingWith("Foo");
            string v3 = IdGenerator.startingWith("");
            string f2 = IdGenerator.startingWith("Foo");
            // Collision Foo11 should not happen!
            string f3 = IdGenerator.startingWith("Foo1");
            Assert.AreEqual(v1, "v#!1");
            Assert.AreEqual(v2, "v#!2");
            Assert.AreEqual(v3, "v#!3");
            Assert.AreEqual(f1, "Foo1");
            Assert.AreEqual(f2, "Foo2");
            Assert.AreEqual(f3, "Foo1!!1");
        }

        [Test]
        public void IdGeneratorTest2()
        {
            string v4 = IdGenerator.newId();
            string v5 = IdGenerator.newId();
            string f3 = IdGenerator.startingWith("Foo");

            IdGenerator.reset();
            string v1 = IdGenerator.startingWith("");
            string f1 = IdGenerator.startingWith("Foo");
            string f2 = IdGenerator.startingWith("Foo");
            Assert.AreEqual(v4, "v#!4");
            Assert.AreEqual(v5, "v#!5");
            Assert.AreEqual(f3, "Foo3");
            Assert.AreEqual(v1, "v#!1");
            Assert.AreEqual(f1, "Foo1");
            Assert.AreEqual(f2, "Foo2");

            IdGenerator.restore();
            string v6 = IdGenerator.startingWith("");
            string f4 = IdGenerator.startingWith("Foo");
            string f5 = IdGenerator.startingWith("Foo");
            Assert.AreEqual(v6, "v#!6");
            Assert.AreEqual(f4, "Foo4");
            Assert.AreEqual(f5, "Foo5");
        }
    }
}
