using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using VSharp.Test;

#pragma warning disable CS0108, CS0114, CS0649

namespace IntegrationTests
{
    [TestSvmFixture]
    public class Sets
    {
        [TestSvm]
        public bool ContainsConcreteTest(HashSet<int> set)
        {
            return set.Contains(42);
        }

        [TestSvm]
        public bool ContainsConcreteTest2(int item)
        {
            var set = new HashSet<int>() { 4 };

            return set.Contains(item);
        }

        [TestSvm]
        public bool ContainsConcreteTest3(int item)
        {
            var set = new HashSet<int>() { 4 };
            set.Add(item);

            return set.Contains(item);
        }

        [TestSvm]
        public bool ContainsSymbolicTest(HashSet<int> set, int item)
        {
            return set.Contains(item);
        }

        [TestSvm]
        public HashSet<byte> AddConcreteTest(HashSet<byte> set)
        {
            set.Add(42);

            return set;
        }

        [TestSvm]
        public HashSet<byte> AddConcreteTest2(byte item)
        {
            var set = new HashSet<byte>();
            set.Add(42);

            return set;
        }

        [TestSvm]
        public HashSet<char> AddSymbolicTest(HashSet<char> set, char item)
        {
            set.Add(item);

            return set;
        }

        [TestSvm]
        public HashSet<char> RemoveSymbolicTest(HashSet<char> set, char item)
        {
            set.Remove(item);

            return set;
        }

        // TODO: construct symbolic set with elements from region in test generator
        /*[TestSvm]
        public bool RemoveConcreteTest(HashSet<char> set)
        {
            var isRemoved = set.Remove('a');

            if (isRemoved)
            {
                return true;
            }

            return false;
        }*/

        [TestSvm]
        public bool RemoveConcreteTest2(long item)
        {
            var set = new HashSet<long>() { 1, 2, 3, 4 };

            return set.Remove(item);
        }

        [TestSvm]
        public bool CommonSymbolicTest(HashSet<char> set, char item, char item2)
        {
            set.Add(item);
            set.Add(item2);
            set.Remove(item);

            return set.Contains(item);
        }

        [TestSvm]
        public bool CommonSymbolicTest2(HashSet<char> set, char item, char item2)
        {
            set.Add(item);
            set.Add(item2);
            set.Remove(item);

            if (set.Contains(item))
            {
                return false;
            }

            return true;
        }

        [TestSvm]
        public bool CommonSymbolicTest3(HashSet<char> set, char item, char item2)
        {
            set.Add(item);
            set.Add(item2);
            set.Remove('z');

            return set.Contains(item);
        }

        [TestSvm]
        public int CountSymbolicTest(HashSet<long> set)
        {
            return set.Count;
        }

        [TestSvm]
        public bool CountSymbolicTest2(HashSet<long> set, long item)
        {
            set.Add(item);

            if (set.Count < 1)
            {
                return false;
            }

            return true;
        }

        [TestSvm]
        public int CountSymbolicTest3(long item)
        {
            var set = new HashSet<long>() { 1, 2, 4 };

            set.Add(item);
            set.Add(item);

            return set.Count;
        }

        [TestSvm]
        public int CountSymbolicTest4(HashSet<long> set, long item)
        {
            set.Add(item);
            set.Add(item);
            set.Add(item);

            return set.Count;
        }

        [TestSvm]
        public bool CountSymbolicTest5(long item)
        {
            var set = new HashSet<long>() { 3 };
            set.Add(item);

            if (set.Count > 1)
            {
                return true;
            }

            return false;
        }
    }
}
