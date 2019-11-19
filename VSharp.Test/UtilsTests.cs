using System;
using NUnit.Framework;
using VSharp.CSharpUtils;

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

        public interface INothing {}
        public class Nothing : INothing {}

        public class GenericParametersKeeper<A, B, C, D, E, F, G, H, I, J>
            where A : B
            where B : J
            where C : struct
            where D : E, F
            where E : I
            where F : J, new()
            where G : H
            where H : Nothing
            where I : class
            where J : INothing
        {}

        private Type[] GetGenericArguments()
        {
            var constructedType = typeof(GenericParametersKeeper<Nothing, Nothing, int, Nothing, object, Nothing, Nothing, Nothing, object, Nothing>);
            return constructedType.GetGenericTypeDefinition().GetGenericArguments();
        }

        [Test]
        public void IsReferenceTypeParameterTest()
        {
            // Testing "IsReferenceTypeParameter" function
            var genericArguments = GetGenericArguments();
            Assert.AreEqual(false, TypeUtils.isReferenceTypeParameter(genericArguments[0]));
            Assert.AreEqual(false, TypeUtils.isReferenceTypeParameter(genericArguments[1]));
            Assert.AreEqual(false, TypeUtils.isReferenceTypeParameter(genericArguments[2]));
            Assert.AreEqual(true, TypeUtils.isReferenceTypeParameter(genericArguments[3]));
            Assert.AreEqual(true, TypeUtils.isReferenceTypeParameter(genericArguments[4]));
            Assert.AreEqual(false, TypeUtils.isReferenceTypeParameter(genericArguments[5]));
            Assert.AreEqual(true, TypeUtils.isReferenceTypeParameter(genericArguments[6]));
            Assert.AreEqual(true, TypeUtils.isReferenceTypeParameter(genericArguments[7]));
            Assert.AreEqual(true, TypeUtils.isReferenceTypeParameter(genericArguments[8]));
            Assert.AreEqual(false, TypeUtils.isReferenceTypeParameter(genericArguments[9]));
        }

        [Test]
        public void IsValueTypeParameterTest()
        {
            // Testing "IsValueTypeParameter" function
            var genericArguments = GetGenericArguments();
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[0]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[1]));
            Assert.AreEqual(true, TypeUtils.isValueTypeParameter(genericArguments[2]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[3]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[4]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[5]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[6]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[7]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[8]));
            Assert.AreEqual(false, TypeUtils.isValueTypeParameter(genericArguments[9]));
        }

        private class Program
        {
            public static T G<T>(T x)
            {
                return x;
            }

            public static T F<T>(T x)
            {
                return x;
            }
        }

        [Test]
        public void TestGenericHashes()
        {
            var ts = typeof(Program).GetMethods();
            var f = ts[0].ReturnType;
            var g = ts[1].ReturnType;
            Assert.AreNotEqual(f, g);
            Assert.AreNotEqual(f.GetDeterministicHashCode(), g.GetDeterministicHashCode());
        }
    }
}
