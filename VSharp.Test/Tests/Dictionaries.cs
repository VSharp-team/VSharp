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
    public class Dictionaries
    {
        [TestSvm]
        public int SymbolicInitialize(char a, int b)
        {
            var dict = new Dictionary<char, int>()
            {
                {a, 1},
                {'b', 2},
                {'c', b}
            };
            return dict['a'];
        }

        [TestSvm]
        public int SymbolicInitialize2(Dictionary<int, int> d)
        {
            d[1] = 1;
            return d[1];
        }

        [TestSvm]
        public int SymbolicInitialize3(Dictionary<int, int> d, int a, int b)
        {
            d[a] = b;
            d[2] = 2;
            return d[a];
        }

        [TestSvm]
        public float Mutate(long i)
        {
            var dict = new Dictionary<long, float>()
            {
                {1L, 1f},
                {2L, 2f},
                {3L, 3f},
                {4L, 4f}
            };
            dict[1L] = 40f;
            dict[i] = 10f;
            return dict[3L];
        }

        [TestSvm]
        public float Mutate2(Dictionary<long, float> dict, long i)
        {
            dict[i] = 10f;
            return dict[3L];
        }

        [TestSvm]
        public int SymbolicWriteAfterConcreteWrite(int k)
        {
            var dict = new Dictionary<int, int>();
            dict[2] = 42;
            dict[k] = 12;
            return dict[2];
        }

        [TestSvm]
        public int SymbolicWriteAfterConcreteWrite3(Dictionary<int, int> dict, int k)
        {
            dict[2] = 42;
            dict[k] = 12;
            return dict[2];
        }

        [TestSvm]
        public Dictionary<byte, decimal> RetDictionary(bool flag1, bool flag2)
        {
            var dict = new Dictionary<byte, decimal>();
            if (flag1)
            {
                dict[1] = 42;
            }
            else if (flag2)
            {
                dict[1] = 89;
            }

            return dict;
        }

        /*[TestSvm]
        public int SymbolicWriteAfterConcreteWrite2(Dictionary<int, int> d, int k)
        {
            d[2] = 42;
            d[k] = 12;
            return d[2];
        }

        [TestSvm]
        public int SolverTestDictionaryKey(Dictionary<int, int> d, int x)
        {
            d[1] = 12;
            d[x] = 12;
            if (x != 10)
            {
                d[10] = 42;
            }

            var res = 0;
            if (d[x] == 12)
            {
                res = 1;
            }

            return res;
        }*/

        // TODO: see test generator's TODOs
        /*[TestSvm]
        public static int TestConnectionBetweenKeysAndValues(Dictionary<int, int> d, int i, int j)
        {
            int x = d[i];
            int y = d[j];
            int res = 0;
            if (i == j && x != y)
                res = 1;
            return res;
        }*/

        [TestSvm]
        public static int LastRecordReachability(Dictionary<int, string> a, Dictionary<int, string> b, int i, string s)
        {
            a[i] = "1";
            b[1] = s;
            if (b[1] != s)
            {
                // unreachable
                return -1;
            }

            return 0;
        }

        [TestSvm]
        public static bool DictionarySymbolicUpdate(int i)
        {
            var dict = new Dictionary<int, int>()
            {
                {1, 1},
                {2, 2},
                {3, 3},
                {4, 4},
                {5, 5}
            };
            dict[i] = 10;
            if (i == 0 && dict[0] != 10)
                return false;
            else
                return true;
        }

        [TestSvm]
        public static bool DictionarySymbolicUpdate2(int i)
        {
            var dict = new Dictionary<int, int>()
            {
                {1, 1},
                {2, 2},
                {3, 3},
                {4, 4},
                {5, 5}
            };
            dict[i] = 10;
            dict[0] = 12;
            if (i == 0 && dict[0] != 12)
                return false;
            else
                return true;
        }

        // TODO: see test generator's TODOs
        /*[TestSvm]
        public bool CountSymbolicTest(Dictionary<char, int> dict)
        {
            if (dict.Count > 0)
            {
                return true;
            }
            else
            {
                return false;
            }
        }

        [TestSvm]
        public bool CountSymbolicTest2(Dictionary<char, int> dict, char key, int item)
        {
            dict.Add(key, item);

            if (dict.Count < 1)
            {
                return false;
            }

            return true;
        }

        [TestSvm]
        public bool CountSymbolicTest3(short key, long item)
        {
            var dict = new Dictionary<short, long>()
            {
                { 1, 1 },
                { 2, 2 },
                { 3, 3 }
            };

            dict.Add(key, item);
            dict[key] = item;

            if (dict.Count > 3)
            {
                return false;
            }
            else
            {
                return true;
            }
        }*/

        [TestSvm]
        public int CountSymbolicTest4(Dictionary<short, long> dict, short key, long item)
        {
            dict[key] = item;
            dict[key] = item;
            dict[key] = item;

            return dict.Count;
        }
    }
}
