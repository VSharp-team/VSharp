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
    public class SymbolicLists
    {
        [TestSvm]
        public bool ConcreteTest(int index)
        {
            var l = new List<char>() { 'a', 'b', 'c' };

            l[index] = 'd';

            if (l[index] == 'd')
            {
                return true;
            }

            return false;
        }

        [TestSvm]
        public bool ConcreteTest2(int index)
        {
            var l = new List<char>() { 'a', 'b', 'c' };

            if (l[index] == 'a')
            {
                return true;
            }

            return false;
        }

        [TestSvm]
        public bool ConcreteTest3(int index)
        {
            var l = new List<char>() { 'a', 'b', 'c' };

            l.RemoveAt(index);

            if (l[index] == 'b')
            {
                return true;
            }

            return false;
        }

        [TestSvm]
        public bool ConcreteTest4(int remove, int insert)
        {
            var l = new List<char>() { 'a', 'b', 'c' };

            l.Insert(insert, 'd');
            l.RemoveAt(remove);

            if (l[2] == 'd')
            {
                return true;
            }

            return false;
        }

        [TestSvm]
        public bool ConcreteTest5(char item)
        {
            var l = new List<char>() { 'a', 'b', 'c' };

            if (l.IndexOf(item) == 1)
            {
                return true;
            }

            return false;
        }

        [TestSvm]
        public bool ConcreteTest6(char item)
        {
            var l = new List<char>() { 'a', 'b', 'c' };

            l.Remove(item);

            if (l.Count > 2)
            {
                return true;
            }

            return false;
        }

        [TestSvm]
        public int ConcreteMemoryTest(int i)
        {
            var l = new List<int>();
            l.Add(1);
            l.Add(2);
            l.Add(i);
            if (l[2] != i)
                return -1;
            return 1;
        }
    }
}
