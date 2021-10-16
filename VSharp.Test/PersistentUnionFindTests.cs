using NUnit.Framework;
using System;
using System.Collections.Generic;
using static VSharp.PersistentUnionFind;

namespace VSharp.Test
{
    [TestFixture]
    public sealed class PersistentUnionFindTests
    {
        private const string foo = "foo";
        private const string bar = "bar";
        private const string baz = "baz";

        private pUnionFind<string> stringUnionFind;
        private pUnionFind<int> intUnionFind;

        [SetUp]
        public void SetUp()
        { 
            stringUnionFind = empty<string>();
            intUnionFind = empty<int>();
        }

        [Test]
        public void SingleElementSetsParentsTest()
        {
            Add(ref stringUnionFind, foo);
            Add(ref stringUnionFind, bar);
            var fooParent = find(foo, stringUnionFind);
            var barParent = find(bar, stringUnionFind);
            Assert.AreEqual(foo, fooParent);
            Assert.AreEqual(bar, barParent);
        }

        [Test]
        public void ElementsOfUnionHaveSameParentTest1()
        {
            Add(ref stringUnionFind, foo);
            Add(ref stringUnionFind, bar);
            Add(ref stringUnionFind, baz);
            Union(foo, bar, ref stringUnionFind);
            Union(foo, baz, ref stringUnionFind);
            var fooParent = find(foo, stringUnionFind);
            var barParent = find(bar, stringUnionFind);
            var bazParent = find(baz, stringUnionFind);
            Assert.AreEqual(fooParent, barParent);
            Assert.AreEqual(barParent, bazParent);
        }
        
        [Test]
        public void ElementsOfUnionHaveSameParentTest2()
        {
            Add(ref intUnionFind, 1);
            Add(ref intUnionFind, 2);

            for (var i = 3; i <= 100; ++i)
            {
                Add(ref intUnionFind, i);
                Union(i, 2 - i % 2, ref intUnionFind);
            }

            var parent1 = find(1, intUnionFind);
            var parent2 = find(2, intUnionFind);
            
            for (var i = 1; i <= 100; ++i)
            {
                var actualParent = find(i, intUnionFind);
                var expectedParent = i % 2 == 0 ? parent2 : parent1;
                Assert.AreEqual(expectedParent, actualParent);
            }

            Union(21, 54, ref intUnionFind);
            
            var unionParent = find(1, intUnionFind);
            
            for (var i = 1; i <= 100; ++i)
            {
                var actualParent = find(i, intUnionFind);
                Assert.AreEqual(unionParent, actualParent);
            }
        }

        [Test]
        public void SubsetTest()
        {
            Add(ref stringUnionFind, foo);
            Add(ref stringUnionFind, bar);
            Add(ref stringUnionFind, baz);
            Union(foo, bar, ref stringUnionFind);
            var fooBarSubset = new List<string>(toSeq(subset(foo, stringUnionFind)));
            var bazSubset = new List<string>(toSeq(subset(baz, stringUnionFind)));
            var expectedFooBarSubset = new List<string> { foo, bar };
            var expectedBazSubset = new List<string> { baz };
            Assert.That(fooBarSubset, Is.EquivalentTo(expectedFooBarSubset));
            Assert.That(bazSubset, Is.EquivalentTo(expectedBazSubset));
        }

        [Test]
        public void FindForNonexistentElementThrowsTest()
        {
            Add(ref stringUnionFind, foo);
            Assert.Throws<InvalidOperationException>(() => find(bar, stringUnionFind));
        }
        
        [Test]
        public void TryFindTest()
        {
            Add(ref stringUnionFind, foo);
            var found = tryFind(foo, stringUnionFind).Value;
            Assert.AreEqual(foo, found);
        }
        
        [Test]
        public void TryFindForNonexistentElementReturnsNoneTest()
        {
            Add(ref stringUnionFind, foo);
            Assert.Throws<NullReferenceException>(() => _ = tryFind(bar, stringUnionFind).Value);
        }
        
        [Test]
        public void AddExistentElementDoesNothingTest()
        {
            Add(ref stringUnionFind, foo);
            Add(ref stringUnionFind, foo);
            var actualElements = new List<string>(toSeq(stringUnionFind));
            var expectedElements = new List<string> { foo };
            Assert.That(actualElements, Is.EquivalentTo(expectedElements));
        }

        private void Add<T> (ref pUnionFind<T> unionFind, T element)
        {
            unionFind = add(unionFind, element);
        }
        
        private void Union<T> (T one, T another, ref pUnionFind<T> unionFind)
        {
            unionFind = union(one, another, unionFind);
        }
    }
}
