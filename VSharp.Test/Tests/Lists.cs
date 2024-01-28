using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using NUnit.Framework;
using VSharp.Test;

#pragma warning disable CS0108, CS0114, CS0649

namespace IntegrationTests
{
//    public class ListNode
//    {
//        public int Key;
//        public ListNode Next;
//    }

    [TestSvmFixture]
    public class Lists
    {
//        public void IncN(ListNode l, int n)
//        {
//            if (l == null || n == 0)
//                return;
//            l.Key += 1;
//            IncN(l.Next, n - 1);
//        }
//
//        public int DerefIncN(ListNode l, ListNode p)
//        {
//            if (l == null || p == null)
//            {
//                return 100500;
//            }
//            IncN(l, 10);
//            return p.Key;
//        }
//
//        public ListNode IncConcreteList(int n)
//        {
//            var l3 = new ListNode { Key = 30, Next = null };
//            var l2 = new ListNode { Key = 20, Next = l3 };
//            var l1 = new ListNode { Key = 10, Next = l2 };
//            IncN(l1, n);
//            return l1;
//        }
//
//        public ListNode IncSymbolicList(ListNode l, int n)
//        {
//            l.Next.Next.Next.Key += 1;
//            IncN(l, n);
//            return l;
//        }
//
//        private int a = 0;
//
//        private bool DoSmth()
//        {
//            a += 1;
//            return a > 3;
//        }

        [TestSvm]
        public bool Construct()
        {
            var a = new List<int>(4) {1, 2, 3, 4};
            var b = new int[4, 1];
            var c = new int[4] {5, 6, 7, 8};
            return a.Count == b.Length && b.Length == c.Length && c.Length == c[3] - 4;
        }

        [TestSvm]
        public int SymbolicInitialize(int a)
        {
            var arr = new int[4] {a, 6, 7, 8};
            return arr[0];
        }

        [TestSvm]
        public int[] Mutate(int i)
        {
            var a = new int[] {1, 2, 3, 4, 5};
            a[i] = 10;
            return a;
        }

        [TestSvm]
        public int LowerBoundTest()
        {
            var c = new int[4, 2] {{1, 1}, {2, 2}, {3, 3}, {4, 4}};
            return c.GetLowerBound(1);
        }

        [TestSvm]
        public int LowerBoundExceptionTest(int[,] array)
        {
            return array.GetLowerBound(2);
        }

        [TestSvm]
        public int LowerBoundSymbolicTest(int[,] array, int dimension)
        {
            return array.GetLowerBound(dimension);
        }

        [TestSvm]
        public int UpperBoundTest()
        {
            var c = new int[4, 2] {{1, 1}, {2, 2}, {3, 3}, {4, 4}};
            return c.GetUpperBound(0);
        }

        [TestSvm(100)]
        public int ArrayLength(int f)
        {
            int[] tmp;
            switch (f)
            {
                case 0:
                    tmp = new[] {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
                    break;
                case 1:
                    tmp = new[] {1, 2, 3};
                    break;
                default:
                    tmp = new int [f + 1];
                    break;
            }

            return tmp.Length;
        }

        [TestSvm]
        public int RankTest()
        {
            var c = new int[4, 2] {{1, 1}, {2, 2}, {3, 3}, {4, 4}};
            return c.Rank;
        }

        [TestSvm(100)]
        public int ReadByFuncResult(string[] str, Func<int> d)
        {
            str[0] = "abc";
            str[1] = "cbd";
            str[2] = "edf";
            if (str[d()] == null)
                return 1;
            return 0;
        }

        [TestSvm]
        public int ReadByFuncResult1(Func<int> d)
        {
            string[] str = new string[3];
            str[0] = "abc";
            str[1] = "cbd";
            str[2] = "edf";
            if (str[d()] == null)
                return 1;
            return 0;
        }

        [TestSvm(100)]
        public static int[] RetOneDArray1(bool flag1, bool flag2)
        {
            int[] arr = new int[5];
            if (flag1)
            {
                arr[1] = 42;
            }
            else if (flag2)
            {
                arr[1] = 89;
            }

            return arr;
        }

        [TestSvm]
        public void ConcreteClearTest()
        {
            var a = new int[4] { 5, 6, 7, 8 };
            Array.Clear(a, 1, 2);
        }

        [TestSvm]
        public void CopyToConcreteToConcreteArray()
        {
            var a = new int[4] { 5, 6, 7, 8 };
            var b = new int[3];
            a.CopyTo(b, 1);
        }

        [TestSvm]
        public static int CopyConcreteToConcreteArray()
        {
            int[] arr = new int[5] {10, 2, 3, 4, 5};
            int[] a = new int[5] {1, 1, 1, 1, 1};
            Array.Copy(arr, 1, a, 1, 3);
            return a[2];
        }

        [TestSvm(100)]
        public static int[] CopyConcreteToSymbolicArray(int[] a)
        {
            int[] arr = new int[5] {1, 2, 3, 4, 5};
            Array.Copy(arr, 2, a, 2, 2);
            return a;
        }

        [TestSvm(100)]
        public static int[] CopyAndThenWrite(int[] a)
        {
            int[] arr = new int[5] {1, 2, 3, 4, 5};
            Array.Copy(arr, 2, a, 2, 2);
            a[2] = 42;
            return a;
        }

        [TestSvm(100)]
        public static int CopyAndBranch(int[] a, int i)
        {
            int[] arr = new int[5] {1, 2, 3, 4, 5};
            Array.Copy(arr, 2, a, 2, 2);
            a[2] = 42;
            if (a[i] == 2)
                return 1;
            return 2;
        }

        [TestSvm(100)]
        public static int[] WriteAndThenCopy(int[] a)
        {
            int[] arr = new int[5] {1, 2, 3, 4, 5};
            a[2] = 42;
            Array.Copy(arr, 2, a, 2, 2);
            return a;
        }

        [TestSvm(95)]
        public static int DoubleWriteAfterCopy(int[] a, int i, int[] b)
        {
            if (a.Length == b.Length)
            {
                a[i] = 1;
                Array.Copy(b, a, a.Length - 1);
                a[i] = 3;
                a[i] = 3;
                if (a[0] != b[0] && i > 0)
                    return -1;
                return 1;
            }

            return 0;
        }

        [TestSvm(95)]
        public static int DoubleWriteAfterCopy1(int[] a, int k, int i, int j, int[] b)
        {
            if (a.Length == b.Length)
            {
                a[k] = 1;
                Array.Copy(b, a, a.Length - 1);
                a[i] = 3;
                a[j] = 3;
                if (a[0] != b[0] && i > 0 && j > 0)
                    return -1;
                return 1;
            }

            return 0;
        }

        [TestSvm(100)]
        public static int[] CopySymbolicIndicesToConcreteArray(int srcI, int dstI, int len)
        {
            int[] arr = new int[5] {10, 2, 3, 4, 5};
            int[] a = new int[5] {1, 1, 1, 1, 1};
            Array.Copy(arr, srcI, a, dstI, len);
            if (a[2] == 3)
                return Array.Empty<int>();
            return a;
        }

        [TestSvm(100)]
        public static int[] CopySymbolicIndicesToConcreteArray1(int srcI1, int dstI1, int len1, int srcI2, int dstI2, int len2)
        {
            int[] arr = new int[5] {10, 2, 3, 4, 5};
            int[] a = new int[5] {1, 1, 1, 1, 1};
            Array.Copy(arr, srcI1, a, dstI1, len1);
            if (a[2] == 3)
                return a;
            int[] b = new int[5] {1, 1, 1, 1, 1};
            Array.Copy(a, srcI2, b, dstI2, len2);
            if (b[2] == 3)
                return Array.Empty<int>();
            return a;
        }

        [TestSvm(100)]
        public static int[] CopySymbolicIndicesToConcreteArray2(int srcI, int dstI, int len)
        {
            int[] arr = new int[5] {10, 2, 3, 4, 5};
            int[] a = new int[5] {1, 1, 1, 1, 1};
            arr[dstI + len] = len;
            Array.Copy(arr, srcI, a, dstI, len);
            int[] b = new int[5] {3, 3, 3, 3, 3};
            Array.Copy(a, dstI, b, dstI, len + 1);
            if (b[dstI + len] == len)
                // Should be unreachable
                return Array.Empty<int>();
            return a;
        }

        [TestSvm(94)]
        public static int TestSolvingCopy(int[] a, int[] b, int i)
        {
            if (a.Length > b.Length && 0 <= i && i < b.Length)
            {
                Array.Fill(a, 1);
                Array.Copy(a, b, b.Length);

                if (b[i] == b[i + 1])
                    return 42;
                return 10;
            }
            return 3;
        }

        [TestSvm(100)]
        public static int TestSolvingCopy1(int[] a, int i, int[] b)
        {
            if (a != null && b != null && a.Length > b.Length)
            {
                a[0] = 42;
                b[i] = 4;
                Array.Copy(a, 0, b, 0, b.Length);
                b[0] = 31;
                var x = b.Length == 3;
                var y = a[0] == 42;
                var z = b[0] == 31;
                var k = a[1] == b[1];
                var j = a[2] == b[2];

                if (x && y && z && k && j)
                    return 42;

                a[0] = 12;
                a[3] = 31;
                Array.Copy(a, 0, b, 0, b.Length);

                if (b.Length == 4 && a[0] == b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3])
                    return 12;

                return 10;
            }
            return 3;
        }

        [TestSvm(100)]
        public static int TestSolvingCopy2(int[] a, int[] b, int[] c)
        {
            if (a != null && b != null && c != null && a.Length > b.Length && b.Length > c.Length)
            {
                a[0] = 42;
                Array.Copy(a, 0, b, 0, b.Length);
                b[0] = 31;
                var x = b.Length == 3;
                var y = a[0] == 42;
                var z = b[0] == 31;
                var k = a[1] == b[1];
                var j = a[2] == b[2];

                if (x && y && z && k && j)
                    return 42;

                b[0] = 12;
                b[3] = 31;
                Array.Copy(b, 0, c, 0, c.Length);

                if (c.Length == 4 && c[0] == b[0] && c[1] == b[1] && c[2] == b[2] && c[3] == b[3])
                    return 12;

                return 10;
            }
            return 3;
        }

        [TestSvm(97)]
        public static int TestSolvingCopy3(int[] a, int[] b, int[] c)
        {
            if (a != null && b != null && c != null && a.Length > b.Length && b.Length > c.Length && c.Length > 3)
            {
                a[0] = 42;
                Array.Copy(a, 0, b, 0, 3);
                Array.Copy(b, 0, c, 0, 4);

                // Should be always true
                if (c[0] == b[0] && c[1] == b[1] && c[2] == b[2] && c[3] == b[3])
                    return 12;

                // Unreachable
                return 10;
            }
            return 3;
        }

        [TestSvm(100)]
        public static int TestSolvingCopy4(int[] a, int[] b)
        {
            if (a != null && b != null && a.Length > b.Length && b.Length > 3)
            {
                a[0] = 42;
                Array.Copy(a, 0, b, 0, 3);

                if (b[1] > 0)
                    return b[0];

                return b[1];
            }
            return 3;
        }

        [TestSvm(100)]
        public static int TestSolvingCopy5(int[] a, int[] b, int i)
        {
            if (a.Length > b.Length && 0 <= i && i < b.Length)
            {
                a[i] = 1; a[0] = 2;
                Array.Copy(a, b, b.Length);

                if (b[i] == b[i + 1])
                    return 42;
                return 10;
            }
            return 3;
        }

        [TestSvm(100)]
        public static int TestSolvingCopy6(int[] a, int[] b, int i)
        {
            if (a.Length > b.Length && 0 <= i && i < b.Length)
            {
                a[i] = 1; a[0] = 2;
                Array.Copy(a, b, b.Length);
                b[i] = 4;
                b[0] = 3;

                if (b[i] == b[i + 1])
                    return 42;
                return 10;
            }
            return 3;
        }

        [TestSvm(98)]
        public static int TestSolvingCopy7(int[] a, int i, int[] b)
        {
            if (a != null && b != null && a.Length > b.Length)
            {
                a[0] = 42;
                b[i] = 4;
                Array.Copy(a, 0, b, 0, b.Length - 1);
                b[0] = 31;

                a[0] = 12;
                a[3] = 31;
                Array.Copy(a, 0, b, 0, b.Length - 1);

                if (i == b.Length - 1 && b[i] != 4 && i > 0)
                    return -1;

                if (b.Length == 4 && a[0] == b[0] && a[1] == b[1] && a[2] == b[2] && a[3] == b[3])
                    return 12;

                return 10;
            }
            return 3;
        }

        [TestSvm(93)]
        public static int TestOverlappingCopy(int[] a)
        {
            if (a != null && a.Length > 5)
            {
                a[0] = 42;
                a[2] = 41;
                Array.Copy(a, 0, a, 2, 3);

                // Should be always false
                if (a[2] != 42)
                    // Unreachable
                    return 42;

                return 10;
            }
            return 3;
        }

        [TestSvm(100)]
        public static int TestOverlappingCopy1(int[] a, int i)
        {
            if (a != null && a.Length > 5)
            {
                a[0] = 42;
                Array.Copy(a, 0, a, 2, 3);

                if (a[i] != 42)
                    return 42;

                a[i] = 41;
                Array.Copy(a, 0, a, i, 3);

                if (a[i] != 42)
                    return 42;

                return 10;
            }
            return 3;
        }

        [TestSvm(95)]
        public static int TestSolvingCopy8(object[] a, object[] b, int i)
        {
            if (a.Length > b.Length && 0 <= i && i < b.Length)
            {
                Array.Fill(a, 1);
                Array.Copy(a, b, b.Length);

                if (b[i] == b[i + 1])
                    return 42;
                return 10;
            }
            return 3;
        }

        [TestSvm(97)]
        public static int TestSolvingCopy9(object[] a, int i, object[] b)
        {
            if (a != null && b != null && a.Length > b.Length)
            {
                var x = (object)4;
                var y = (object)31;
                a[0] = 42;
                b[i] = x;
                Array.Copy(a, 0, b, 0, b.Length - 1);
                b[0] = y;

                a[0] = 12;
                a[3] = y;
                Array.Copy(a, 0, b, 0, b.Length - 1);

                if (i == b.Length - 1 && b[i] != x && i > 0)
                    return -1;

                return 10;
            }
            return 3;
        }

        [TestSvm(100)]
        public static int TestSolvingCopy10(string[] a, int i, string[] b)
        {
            if (a.Length > b.Length && 0 <= i && i < b.Length)
            {
                Array.Copy(a, b, b.Length);

                if (b[i][0] == b[i + 1][0])
                    return 42;
                return 10;
            }
            return 3;
        }

        [TestSvm(90)]
        public static int ArrayAliasWrite(object[] o, string[] s, string str1, string str2)
        {
            if (o[42] == str1)
            {
                if (str1 != String.Empty)
                {
                    s[42] = str2;
                    if (o[42] == str2)
                        return 1;
                    if (o[42] != str1)
                        throw new ArgumentException("unreachable");
                }
            }

            return 0;
        }

        [TestSvm(100)]
        public static int MakeDefaultAndWrite(int k)
        {
            int[] arr = new int[5];
            arr[k] = 42;
            return arr[2];
        }

        [TestSvm(100)]
        public static int SymbolicWriteAfterConcreteWrite(int k)
        {
            int[] arr = new int[5];
            arr[2] = 42;
            arr[k] = 12;
            return arr[2];
        }

        [TestSvm(100)]
        public static int SymbolicWriteAfterConcreteWrite2(int[] a, int k)
        {
            a[2] = 42;
            a[k] = 12;
            return a[2];
        }

        [TestSvm(100)]
        public static int SolverTestArrayKey(int[] a, int x)
        {
            a[1] = 12;
            a[x] = 12;
            if (x != 10)
            {
                a[10] = 42;
            }
            var res = 0;
            if (a[x] == 12)
            {
                res = 1;
            }
            return res;
        }

        [TestSvm]
        public static int SolverTestConcreteArray(int x)
        {
            var res = 0;
            var a = new int[3] {1, 2, 3};
            a[2] = x;
            var len = a.Length;
            var lb = a.GetLowerBound(0);
            if (len == 3 && lb == 0)
                res = 1;
            return res;
        }

        [Ignore("Raise OverflowException on allocation of array with negative size")]
        public static int SolverTestMultiDimensionArray(int[,] a, int x, int y)
        {
            var res = 0;
            var b = new int[x + 1, y + 1];
            var axy = a[x, y];
            var bxy = b[x, y];
            if (axy == bxy)
                res = 1;
            return res;
        }

        [Ignore("needs extern 'Array.InternalCreate'")]
        public static int SpecifyLowerBounds(int x, int y)
        {
            var a = Array.CreateInstance(typeof(int), new[] { 4 }, new[] { 2011 });
            var res = (int) a.GetValue(new[] {1});
            return res;
        }

        [TestSvm(100)]
        public static int[] RetOneDArray2(int n)
        {
            int[] arr = new int[n];
            if (n == 5)
            {
                arr[4] = 99;
                arr[1] = 42;
            }

            if (n == 8)
            {
                arr[1] = 89;
                arr[7] = 66;
            }

            return arr;
        }

        [TestSvm(90)]
        public static int TestConnectionBetweenIndicesAndValues(int[] a, int i, int j)
        {
            int x = a[i];
            int y = a[j];
            int res = 0;
            if (i == j && x != y)
                res = 1;
            return res;
        }

        [TestSvm(92)]
        public static int TestConnectionBetweenMultiIndicesAndValues(int[,] a, int i, int j, int f, int g)
        {
            int x = a[i, j];
            int y = a[f, g];
            int res = 0;
            if (i == f && j == g && x != y)
                res = 1;
            return res;
        }

        public class MyClass
        {
            public int x;
        }

        [TestSvm(90)]
        public static int ArrayElementsAreReferences(MyClass[] a, int i, int j)
        {
            MyClass x = a[i];
            MyClass y = a[j];
            int res = 0;
            if (i == j && x != y)
                res = 1;
            return res;
        }

        [TestSvm(90)]
        public static bool ArraySymbolicUpdate(int i)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            array[i] = 10;
            if (i == 0 && array[0] != 10)
                return false;
            else
                return true;
        }

        [TestSvm(92)]
        public static bool ArraySymbolicUpdate2(int i)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            array[i] = 10;
            array[0] = 12;
            if (i == 0 && array[0] != 12)
                return false;
            else
                return true;
        }

        [TestSvm(97)]
        public static bool ArraySymbolicUpdate3(int i, int j)
        {
            var array = new int[] {1, 2, 3, 4, 5};
            array[i] = 10;
            array[0] = 12;
            array[j] = 42;
            if ((i == 0 && j == 0 && array[0] != 42) || (i == 0 && j == 2 && array[0] != 12) || (i == 2 && j == 2 && array[2] != 42) || (i == 2 && j == 1 && array[i] != 10) || (i == 2 && j == 1 && array[1] != 42))
                return false;
            else
                return true;
        }

        [Ignore("needs big bound")]
        public static int AddManyElementsToList()
        {
            List<int> l = new List<int>();
            for (var i = 0; i < 1000000; i++)
            {
                l.Add(i);
            }
            return l.Last();
        }

        [TestSvm]
        public static bool CheckArrayContains()
        {
            char[] l = {'c', 'h', 'a', 'r', 's'};
            return l.Contains('a');
        }

        [TestSvm]
        public static int TypeSolverCheck(int i, object[] l)
        {
            if (l[i] is int[] a)
                return a[0];

            return -12;
        }

        [Ignore("Unable to calculate GetHashCode of decimal")]
        public static int TypeSolverCheck2(int i, object a, object b)
        {
            var d = new Dictionary<object, int>();
            d[a] = i;
            if (a is int[])
            {
                if (d[b] == i)
                    return 1;
                return 0;
            }

            return -12;
        }

        [Ignore("Unable to calculate GetHashCode of decimal")]
        public static int TypeSolverCheck3(object a, object b)
        {
            var d = new Dictionary<object, int>();
            d[a] = 0;
            if (a is int[])
            {
                if (d[b] != 0)
                    return 1;
                return 0;
            }

            return -12;
        }

        [Ignore("fix concrete memory (check fully concreteness) and concrete invoke (by ref parameters)")]
        public static int ConcreteDictionaryTest(int v)
        {
            var d = new Dictionary<int, int>();
            d.Add(1, v);
            if (d.TryGetValue(1, out var value))
            {
                if (value != v)
                {
                    return -1;
                }

                return value;
            }

            return 0;
        }

        [Ignore("fix composition with concrete memory")]
        public static int ConcreteDictionaryTest1(int a, int b)
        {
            var d = new Dictionary<int, List<int>>();
            d.Add(1, new List<int> {2, 3});
            d.Add(4, new List<int> {5, 6});
            if (d.TryGetValue(a, out var res))
            {
                if (res.Contains(b))
                    return 1;
                return 0;
            }
            return 2;
        }

        [TestSvm(100)]
        public static int ListContains(int a, int b)
        {
            var l = new List<int> {2, 3, b, 5};
            if (l.Contains(a))
                return 1;
            return 0;
        }

        [Ignore("Support rendering recursive arrays")]
        public static object[] ConcreteRecursiveArray()
        {
            object[] a = new object[1];
            a[0] = a;
            return a;
        }

        [TestSvm]
        public static object SymbolicRecursiveArray(object[] a)
        {
            Array.Fill(a, a);
            return a[0];
        }

        public static Array RetSystemArray1(Array arr)
        {
            if (arr is int[])
            {
                var arrOne = arr as int[];
                arrOne[1] = 5;
            }
            else if (arr is int[,])
            {
                var arrOne = arr as int[,];
                arrOne[1, 1] = 7;
            }

            return arr;
        }

        public static Array RetSystemArray2(Array arr)
        {
            if (arr is int[])
            {
                var arrOne = arr as int[];
                arrOne[1] = 5;
            }

            if (arr is int[,])
            {
                var arrOne = arr as int[,];
                arrOne[1, 1] = 7;
            }

            if (arr is int[,,])
            {
                var arrOne = arr as int[,,];
                arrOne[1, 1, 1] = 42;
            }

            return arr;
        }

        public struct EmptyStruct
        {
        }

        [TestSvm]
        public static EmptyStruct LdElemMustThrowExceptionIfIndexIsNegative(int i)
        {
            var array = new EmptyStruct[3];
            return array[i];
        }

        class A
        {
            public int x;
            public int y;
        }
        class B
        {
        }
        public static int AccessLinearArray(object[] b, object x, int i)
        {
            b[i] = x;
            return i;
        }

        [TestSvm]
        public static int ArrayExceptionsOrder(int f, object[] crr, object c, int i)
        {
            var arr = new A[10];
            object[] brr = arr;
            var a = new A();
            var b = new B();
            switch (f)
            {
                case 0:
                    AccessLinearArray(brr, a, 0); // ok
                    break;
                case 1:
                    AccessLinearArray(brr, b, 0); // pure: arr typ mis
                    break;
                case 2:
                    AccessLinearArray(brr, a, -1); // pure: index
                    break;
                case 3:
                    AccessLinearArray(null, a, 0); // pure: npe
                    break;
                case 4:
                    AccessLinearArray(null, a, -1); // npe < index
                    break;
                case 5:
                    AccessLinearArray(null, b, 0); // npe < arr typ mis
                    break;
                case 6:
                    AccessLinearArray(brr, b, -1); // index < arr typ mis
                    break;
                default:
                    AccessLinearArray(crr, c, i);
                    break;
            }
            return f;
        }

        [Ignore("System.Array.Set(...) is not implemented")]
        public static Array RetSystemArray3(Array arr)
        {
            if (arr is object[])
            {
                var arrOne = arr as object[];
                arrOne[1] = new object();
            }

            if (arr is int[])
            {
                var arrOne = arr as int[];
                arrOne[1] = 3;
            }

            if (arr is string[,])
            {
                var arrOne = arr as string[,];
                arrOne[1, 1] = "42";
            }

            if (arr is int[,])
            {
                var arrOne = arr as int[,];
                arrOne[1, 1] = 42;
            }

            return arr;
        }

        public interface IBucket
        {
            object GetValue();
        }

        public struct Bucket : IBucket
        {
            public object Key;
            public object Value;

            public Bucket(object key, object value)
            {
                Key = key;
                Value = value;
            }

            public object GetValue()
            {
                return Value;
            }
        }

        public class BucketClass : IBucket
        {
            public object Key;
            public object Value;

            public BucketClass(object key, object value)
            {
                Key = key;
                Value = value;
            }

            public object GetValue()
            {
                return Value;
            }
        }

        [TestSvm(91)]
        public static object ConcreteArrayChange()
        {
            var a = new Bucket[3];
            var b = new Bucket("major", 0);
            a[0] = b;
            var c = a[0];
            if (!Equals(c, b))
                return -1;
            a[1] = new Bucket("minor", 1);
            a[2] = new Bucket("patch", 2);
            b = new Bucket("major", 3);
            a[0] = b;
            a[1] = new Bucket("minor", 4);
            a[2] = new Bucket("patch", 5);
            if (!Equals(a[0], b))
                return -1;
            return 0;
        }

        [TestSvm(100)]
        public static int ListTest(decimal d1, decimal d2)
        {
            var l = new List<object>();
            l.Add(d1);
            var i = l.LastIndexOf(d2);
            if (i >= 0)
                return i;
            return -1;
        }

        [TestSvm(100)]
        [IgnoreFuzzer("Need recursion constraints in generators")]
        public static int ListTest1(List<object> l, object e)
        {
            var i = l.LastIndexOf(e);
            if (i >= 0)
                return i;
            return -1;
        }

        [Ignore("implement splitting")]
        public static int HashtableTest(int a)
        {
            Hashtable dataHashtable = new Hashtable();
            dataHashtable[0] = 0;
            dataHashtable[1] = 1;
            dataHashtable[2] = a;
            dataHashtable[a] = 0;
            int data = (int) dataHashtable[2];
            int[] array = null;
            if (data > 0)
            {
                array = new int[data];
            }

            array[0] = 5;
            return array[0];
        }

        [TestSvm(93)]
        public static int HashtableTest1(int a)
        {
            Hashtable dataHashtable = new Hashtable();
            dataHashtable[0] = 0;
            dataHashtable[1] = 1;
            dataHashtable[2] = a;
            if ((int)dataHashtable[2] != a)
                return -1;
            return 1;
        }

        [TestSvm(95)]
        public static int HashtableTest2(int i)
        {
            Hashtable dataHashtable = new Hashtable();
            dataHashtable[0] = 0;
            dataHashtable[1] = 1;
            var a = new A();
            a.x = 1;
            a.y = 2;
            dataHashtable[2] = a;
            a.x = i;
            if (((A)dataHashtable[2]).x != i)
                return -1;
            return 1;
        }

        [TestSvm(96)]
        public static int ConcreteHashtableTest()
        {
            var dataHashtable = new Hashtable(3)
            {
                ["major"] = 0,
                ["minor"] = 1,
                ["patch"] = 2
            };

            dataHashtable["major"] = 3;
            dataHashtable["minor"] = 4;
            dataHashtable["patch"] = 5;
            dataHashtable[3] = "string4";
            dataHashtable[4] = "string1";
            dataHashtable[5] = "string2";


            if ((int)dataHashtable["major"] != 3)
                return -1;
            return 0;
        }

        [TestSvm(88)]
        public static int ConcreteMemoryTest(int i)
        {
            var l = new List<int>();
            l.Add(1);
            l.Add(2);
            l.Add(i);
            if (l[2] != i)
                return -1;
            return 1;
        }

        public static int ConcreteMemoryTestHelper(List<Bucket> l)
        {
            var sum = 0;
            foreach (var bucket in l)
            {
                if (bucket.Value is int[] a)
                {
                    foreach (var elem in a)
                    {
                        sum += elem;
                    }
                }
            }

            return sum;
        }

        [TestSvm(96)]
        public static int ConcreteMemoryTest1(int i)
        {
            var arr1 = new int[] { 1, 2, 3 };
            var arr2 = new int[] { 4, 5, 6 };
            var l = new List<Bucket>();
            l.Add(new Bucket(1, arr1));
            l.Add(new Bucket(2, arr2));
            arr1[0] = i;
            arr2[0] = i;
            if (ConcreteMemoryTestHelper(l) != i * 2 + 2 + 3 + 5 + 6)
                return -1;
            return 1;
        }

        public static int ConcreteMemoryTestHelper1(List<object> l)
        {
            var sum = 0;
            foreach (var bucket in l)
            {
                if (((Bucket)bucket).Value is int[] a)
                {
                    foreach (var elem in a)
                    {
                        sum += elem;
                    }
                }
            }

            return sum;
        }

        [TestSvm(96)]
        public static int ConcreteMemoryTest2(int i)
        {
            var arr1 = new int[] { 1, 2, 3 };
            var arr2 = new int[] { 4, 5, 6 };
            var l = new List<object>();
            l.Add(new Bucket(1, arr1));
            l.Add(new Bucket(2, arr2));
            arr1[0] = i;
            arr2[0] = i;
            if (ConcreteMemoryTestHelper1(l) != i * 2 + 2 + 3 + 5 + 6)
                return -1;
            return 1;
        }

        public static int ConcreteMemoryTestHelper2(List<IBucket> l)
        {
            var sum = 0;
            foreach (var bucket in l)
            {
                if (bucket.GetValue() is int[] a)
                {
                    foreach (var elem in a)
                    {
                        sum += elem;
                    }
                }
            }

            return sum;
        }

        [TestSvm(96)]
        public static int ConcreteMemoryTest3(int i)
        {
            var arr1 = new int[] { 1, 2, 3 };
            var arr2 = new int[] { 4, 5, 6 };
            var l = new List<IBucket>();
            l.Add(new Bucket(1, arr1));
            l.Add(new Bucket(2, arr2));
            arr1[0] = i;
            arr2[0] = i;
            if (ConcreteMemoryTestHelper2(l) != i * 2 + 2 + 3 + 5 + 6)
                return -1;
            return 1;
        }

        [TestSvm(93)]
        public static int ConcreteMemoryTest4(int i)
        {
            var arr1 = new int[] { 1, 2, 3 };
            var arr2 = new int[] { 4, 5, 6 };
            var l = new List<IBucket>();
            if (l.Count != 0)
                return -1;
            var b1 = new BucketClass(1, arr1);
            var b2 = new BucketClass(2, arr2);
            arr1[0] = i;
            arr2[0] = i;
            l.Add(b1);
            l.Add(b2);
            if (ConcreteMemoryTestHelper2(l) != i * 2 + 2 + 3 + 5 + 6)
                return -1;
            return 1;
        }

        [TestSvm(86)]
        public static int LazyDict()
        {
            var d = new Lazy<Dictionary<int, int>>();
            d.Value.Add(1, 42);
            if (d.Value[1] != 42)
                return -1;
            return 0;
        }

        [TestSvm(85)]
        public static int ConcurrentDict(int a, int b)
        {
            var d = new System.Collections.Concurrent.ConcurrentDictionary<int, int>();
            d.TryAdd(a, b);
            if (d[a] != b)
                return -1;
            return 0;
        }

        [TestSvm(83)]
        public static int VolatileWrite()
        {
            var x = 1;
            System.Threading.Volatile.Write(ref x, 42);
            if (x != 42)
                return -1;
            return 0;
        }
    }

    [TestSvmFixture]
    public static class SpanTests
    {
        [TestSvm(96)]
        [IgnoreFuzzer("Need AccessViolation handling")]
        public static unsafe byte SpanTest(int[] a, byte b, int i)
        {
            fixed (void* ptr = a)
            {
                var span = new ReadOnlySpan<byte>(ptr, i);
                var tmp = span[i - 1];
                if (i - 1 == 0 && *(byte*)ptr != tmp)
                    throw new ArgumentException();
                if (tmp > b)
                    return span[3];
                return span[1];
            }
        }
    }

    [TestSvmFixture]
    public class ArrayCopying
    {
        private List<int> _elements;

        public int Count => _elements.Count - 1;

        [TestSvm(100)]
        public void CopyTo(int[] array, int arrayIndex)
        {
            _elements.CopyTo(1, array, arrayIndex, Count);
        }
    }

    public static class Container
    {
        public static int X = 0;
    }

    public class Bag
    {
        public int X;

        public Bag(int x)
        {
            X = x;
        }
    }

    public class First
    {
        public Second A = null;
        public int B;

        public int Get()
        {
            return B;
        }

        public void Inc()
        {
            B++;
        }
    }

    public class Second : First
    {
        private First b;

        public int Get()
        {
            if (b != null)
                return b.Get();
            return 0;
        }

        public void Inc()
        {
            b?.Inc();
        }
    }

    [TestSvmFixture]
    [IgnoreFuzzer("Need recursion constraints in generators")]
    public static class RecursiveAccess
    {
        public static First G(First f)
        {
            if (f != null && f.A != null)
            {
                f.B++;
            }

            return f;
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static int F(int x)
        {
            if (x > 10)
            {
                Container.X = x;
                return x;
            }

            var tmp = new Bag(Container.X);
            Container.X++;
            Container.X = F(Container.X);
            return Container.X + tmp.X;
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static int G(int x)
        {
            return F(5) + 10;
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static int NonEmptyPath(First f)
        {
            int res = 0;
            if (f != null && f.A != null)
            {
                f.A.B = 7;
                var p = G(f.A);
                if (p != null)
                {
                    res = p.B;
                }
            }

            return res;
        }

        [TestSvm(100)]
        public static int TestStack(Second b)
        {
            if (b != null)
            {
                b.Inc();
                return b.Get();
            }

            return 0;
        }

        // [TestSvm]
        public static LinkedListNode<int> G(LinkedListNode<int> l, LinkedListNode<int> n)
        {
            LinkedListNode<int> tmp;
            while (l.Value > 7) // кручу-верчу, запутать хочу
            {
                tmp = l;
                l = n;
                n = tmp;
                l = l.Next;
            }

            return l;
        }

        [TestSvm(100)]
        public static int FirstElementOfLinkedList(LinkedList<int> l)
        {
            if (l == null) return 0;
            if (l.First == null) return 1;
            return l.First.Value += 1;
        }

        // Test on tracking current heap address during access to heap for filtering possible locations
        [TestSvm(100)]
        public static int MemoryTest(LinkedList<int> l)
        {
            LinkedListNode<int> n = new LinkedListNode<int>(10);
            LinkedListNode<int> x = G(l.First, n);
            LinkedListNode<int> m = new LinkedListNode<int>(42);
            return x.Value;
        }

        [TestSvm(100)]
        public static int ArithmeticalProgression(LinkedList<int> list)
        {
            int sum = 0;
            int prev = 0;
            if (list == null)
                return 0;
            LinkedListNode<int> node = list.First;
            for (int i = 0; i < 5 && node != null; ++i)
            {
                if (node.Value == prev + 1)
                    sum += node.Value;
                prev = node.Value;
                node = node.Next;
            }

            if (sum > 15)
                return 1;
            return 2;
        }
    }

    [TestSvmFixture]
    public class EnumerableTests<T> : IEnumerable<T>
        where T : IComparable<T>
    {
        private List<T> _items;

        public EnumerableTests(List<T> items)
        {
            _items = items;
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator()
        {
            return _items.GetEnumerator();
        }

        IEnumerator IEnumerable.GetEnumerator()
        {
            return _items.GetEnumerator();
        }

        [TestSvm(100, timeout:5)]
        public T Max()
        {
            var max = this.First();
            foreach (var elem in this)
            {
                if (elem.CompareTo(max) > 0)
                    max = elem;
            }

            return max;
        }
    }

    [TestSvmFixture]
    public static class StackTests
    {
        public sealed class ListNode
        {
            public int Key;
            public ListNode Next;
        }

        public sealed class A
        {
            public int Field;
            public int OtherField;
        }

        public sealed class ListN
        {
            public int Key;
            public ListN Next = null;

            public ListN(int n)
            {
                Key = n;
            }

            public void Add(int x)
            {
                if (x < Key)
                {
                    if (Next == null)
                        Next = new ListN(x);
                    else
                        Next.Add(x);
                }
            }
        }

        public sealed class List
        {
            private ListN _root = null;
            public int Key => _root.Key;

            public void AddToList(int x)
            {
                if (_root != null)
                    _root.Add(x);
            }
        }

        public sealed class BinTreeNode
        {
            public int Key;
            public BinTreeNode Left = null;
            public BinTreeNode Right = null;

            public BinTreeNode(int x)
            {
                Key = x;
            }

            [Ignore("Forward exploration does not handle recursion now")]
            public void Add(int x)
            {
                if (Key == x)
                    return;
                if (x < Key)
                {
                    if (Left == null)
                        Left = new BinTreeNode(x);
                    else
                        Left.Add(x);
                }
                else
                {
                    if (Right == null)
                        Right = new BinTreeNode(x);
                    else
                        Right.Add(x);
                }
            }

            public void Add2(int x)
            {
                if (x < Key)
                {
                    if (Left == null)
                        Left = new BinTreeNode(x);
                    else
                        Left.Add2(x);
                }
            }

            public bool Contains(int x)
            {
                if (Key == x)
                    return true;
                if (x < Key)
                {
                    if (Left == null)
                        return false;
                    else
                        return Left.Contains(x);
                }
                else
                {
                    if (Right == null)
                        return false;
                    else
                        return Right.Contains(x);
                }
            }
        }

        public sealed class BinTree
        {
            private BinTreeNode _root = null;
            public int Key => _root.Key;

            public void Add(int x)
            {
                if (_root == null)
                    _root = new BinTreeNode(x);
                else
                    _root.Add(x);
            }

            public void Add2(int x)
            {
                if (_root != null)
                    _root.Add2(x);
            }

            public bool Contains(int x)
            {
                if (_root == null)
                    return false;
                else
                    return _root.Contains(x);
            }
        }

        internal static class SharedA
        {
            public static int Positivise(A a)
            {
                if (a.Field >= a.OtherField)
                    return a.Field;
                a.Field++;
                return Positivise(a);
            }

            public static void IncField(A a, int n)
            {
                if (a == null || n <= 0)
                    return;
                a.Field++;
                IncField(a, n - 1);
            }

            public static void AtLeastHundred(A a)
            {
                if (a == null)
                    return;
                if (a.Field >= 100)
                    return;
                a.Field++;
                AtLeastHundred(a);
            }

            public static void Fact(A a)
            {
                if (a == null)
                    return;
                if (a.Field < 2)
                {
                    a.OtherField = 1;
                    return;
                }

                var f = a.Field;
                a.Field--;
                Fact(a);
                a.OtherField *= f;
            }

            public static void JustSetField(A a)
            {
                if (a == null || a.Field == a.OtherField)
                    return;
                a.Field = a.OtherField;
                JustSetField(a);
            }

            public static void StrangeSum(A a)
            {
                if (a == null)
                    return;
                if (a.OtherField <= 0)
                    return;
                a.Field += a.OtherField;
                a.OtherField--;
                StrangeSum(a);
            }

            public static void AddOther(A a, int n)
            {
                if (a == null)
                    return;
                if (n <= 0)
                    return;
                a.Field += a.OtherField;
                AddOther(a, n - 1);
            }

            public static void MoveOtherToField(A a)
            {
                if (a == null)
                    return;
                if (a.OtherField <= 0)
                    return;
                a.Field++;
                a.OtherField--;
                MoveOtherToField(a);
            }

            public static bool IsFieldGreater(A a)
            {
                if (a == null || a.OtherField < 0)
                    return false;
                if (a.OtherField == 0)
                    return a.Field > 0;
                a.Field--;
                a.OtherField--;
                return IsFieldGreater(a);
            }

            public static void FibIter(A a, int n)
            {
                if (n <= 0)
                    return;
                var tmp = a.Field;
                a.Field += a.OtherField;
                a.OtherField = tmp;
                FibIter(a, n - 1);
            }

            public static int AddFields(A a)
            {
                if (a == null || a.Field < 0 || a.OtherField < 0)
                    return 0;
                if (a.Field == 0)
                    return a.OtherField;
                a.Field--;
                return 1 + AddFields(a);
            }

            public static bool FieldsAreEqual(A a)
            {
                if (a == null || a.Field < 0 || a.OtherField < 0)
                    return false;
                if (a.Field == 0)
                    return a.OtherField == 0;
                if (a.OtherField == 0)
                    return false;
                a.Field--;
                a.OtherField--;
                return FieldsAreEqual(a);
            }
        }

        internal static class SharedTree
        {
            public static BinTreeNode Add(BinTreeNode tree, int x)
            {
                if (tree == null)
                    return new BinTreeNode(x);
                if (x < tree.Key)
                    tree.Left = Add(tree.Left, x);
                else if (x > tree.Key)
                    tree.Right = Add(tree.Right, x);

                return tree;
            }

            public static bool Contains(BinTreeNode tree, int x)
            {
                if (tree == null)
                    return false;
                if (tree.Key == x)
                    return true;
                if (x < tree.Key)
                    return Contains(tree.Left, x);
                return Contains(tree.Right, x);
            }

            public static BinTreeNode FromList(ListNode list)
            {
                if (list == null)
                    return null;
                var tree = FromList(list.Next);
                if (tree == null)
                    return new BinTreeNode(list.Key);
                Add(tree, list.Key);
                return tree;
            }

            public static int Max(BinTreeNode tree)
            {
                if (tree == null)
                    return -1;
                if (tree.Right == null)
                    return tree.Key;
                return Max(tree.Right);
            }
        }

        internal static class SharedList
        {
            public static ListNode RemoveOne(ListNode l, int x)
            {
                if (l == null)
                    return null;
                if (l.Key == x)
                    return l.Next;
                l.Next = RemoveOne(l.Next, x);
                return l;
            }

            public static ListNode RemoveAll(ListNode l, int x)
            {
                if (l == null)
                    return null;
                var tail = RemoveAll(l.Next, x);
                if (l.Key == x)
                    return tail;
                l.Next = tail;
                return l;
            }

            public static ListNode CreateList(int n)
            {
                if (n <= 0)
                    return null;
                ListNode tail = CreateList(n - 1);
                ListNode head = new ListNode {Key = 0, Next = tail};
                return head;
            }

            public static ListNode CreateDecreasingList(int n)
            {
                if (n <= 0)
                    return null;
                ListNode tail = CreateDecreasingList(n - 1);
                ListNode head = new ListNode {Key = n, Next = tail};
                return head;
            }

            public static int Length(ListNode l)
            {
                if (l == null)
                    return 0;
                return 1 + Length(l.Next);
            }

            public static int Last(ListNode l)
            {
                if (l == null)
                    return -1;
                if (l.Next == null)
                    return l.Key;
                return Last(l.Next);
            }

            public static int Sum(ListNode l)
            {
                if (l == null)
                    return 0;
                return l.Key + Sum(l.Next);
            }

            public static ListNode Reverse(ListNode l)
            {
                if (l == null || l.Next == null)
                    return l;
                var h = Reverse(l.Next);
                l.Next.Next = l; // l.Next is now the last element
                l.Next = null;
                return h;
            }

            public static void Crop(ListNode l, int n)
            {
                if (n <= 0 || l == null)
                    return;
                if (n == 1)
                {
                    l.Next = null;
                    return;
                }

                Crop(l.Next, n - 1);
            }

            public static ListNode LastNode(ListNode l)
            {
                if (l == null)
                    return null;
                if (l.Next == null)
                    return l;
                return LastNode(l.Next);
            }

            public static void Append(ListNode l1, ListNode l2)
            {
                if (l1 == null)
                    throw new ArgumentException();
                var l1Last = LastNode(l1);
                l1Last.Next = l2;
            }

            public static bool Contains(ListNode l, int k)
            {
                if (l == null)
                    return false;
                if (l.Key == k)
                    return true;
                return Contains(l.Next, k);
            }

            public static void IncN(ListNode l)
            {
                if (l == null)
                    return;
                l.Key += 1;
                IncN(l.Next);
            }

            public static void IncNwithN(ListNode l, int n)
            {
                if (l == null || n == 0)
                    return;
                l.Key += 1;
                IncNwithN(l.Next, n - 1);
            }

            public static int Mult(int x, int y)
            {
                if (x <= 0)
                    return 0;
                return y + Mult(x - 1, y);
            }

            public static ListNode CreateOnes(int n)
            {
                if (n <= 0)
                    return null;
                ListNode tail = CreateOnes(n - 1);
                return new ListNode {Key = 1, Next = tail};
            }

            public static bool IsDecreasingFrom(ListNode l, int n)
            {
                if (l == null)
                    return true;
                if (l.Key > n)
                    return false;
                return IsDecreasingFrom(l.Next, l.Key);
            }

            public static int MaxThan(ListNode l, int max)
            {
                if (l == null)
                    return max;
                if (l.Key > max)
                    return MaxThan(l.Next, l.Key);
                return MaxThan(l.Next, max);
            }

            public static int Item(ListNode l, int i)
            {
                if (l == null)
                    return -1;
                if (i == 0)
                    return l.Key;
                return Item(l.Next, i - 1);
            }
        }

        public static class Container
        {
            public static int X = 0;
        }

        public class Bag
        {
            public int X;

            public Bag(int x)
            {
                X = x;
            }
        }

        public class First
        {
            public Second A = null;
            public int B;

            public int Get()
            {
                return B;
            }

            public void Inc()
            {
                B++;
            }
        }

        public class Second : First
        {
            private First b;

            public int Get()
            {
                if (b != null)
                    return b.Get();
                return 0;
            }

            public void Inc()
            {
                b?.Inc();
            }
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static bool TestBinTree(BinTree tree, int x) // always true
        {
            if (tree == null)
                return true;
            tree.Add(x);
            return tree.Contains(x);
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static void TestBinTree2(BinTree tree, int x)
        {
            if (tree == null)
                return;
            tree.Add2(x);
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static void ListTest(List list)
        {
            if (list == null)
                return;
            list.AddToList(0);
        }

        private static int RecF(int x)
        {
            if (x <= 0)
                return 12;
            else
                return RecG(x - 1);
        }

        private static int RecG(int y)
        {
            if (y <= 0)
                return 56;
            else
                return RecF(y - 2);
        }

        [Ignore("Forward exploration does not handle recursion now")]
        public static int TestRecF(int n)
        {
            return RecF(n - 15);
        }
    }


    public class Customerrr
    {
        public int Id { get; set; }
        public string Name { get; set; }
        public string City { get; set; }

        public override bool Equals(Object other)
        {
            if (other is Customerrr otherCustomer)
            {
                return otherCustomer.Id == this.Id;
            }

            return false;
        }

        protected bool Equals(Customerrr other)
        {
            return Id == other.Id;
        }

        public override int GetHashCode()
        {
            return HashCode.Combine(Id, Name, City);
        }
    }

    [TestSvmFixture]
    public class ContainsCustomerClass
    {
        private Customerrr _customer;

        [TestSvm(100)]
        public bool ListContainsOurCustomer(LinkedList<Customerrr> l)
        {
            return l.Contains(_customer);
        }

        // TODO: fails with ShortestDistance searcher due to type solver bug (assert)
        [TestSvm(100, strat: SearchStrategy.BFS)]
        public bool ArrayContainsOurCustomer(Customerrr[] customers)
        {
            foreach (var other in customers)
            {
                if (other.Equals(_customer))
                {
                    return true;
                }
            }

            return false;
        }

        [TestSvm(100)]
        public bool ContainsOurCustomer(Customerrr other)
        {
            if (other.Equals(_customer))
            {
                return true;
            }

            return false;
        }

    }

    public class ArrayException : Exception{}

    [TestSvmFixture]
    public sealed class PrivateContentArray
    {
        private class Content
        {
            public int X { get; set; }
        }

        [TestSvm]
        public static object GetArray()
        {
            var result = new Content[2];
            result[0] = new Content
            {
                X = 1
            };
            return result;
        }
    }

    [TestSvmFixture]
    public sealed class InnerPrivateContentArray
    {
        private class Content
        {
            public int X { get; set; }
        }

        private readonly Content[] _arr = new Content[10];


        [TestSvm(100)]
        [IgnoreFuzzer("(known bug) DecodeString: unexpected representation")]
        public int GetValue(int index)
        {
            if (index < 0 || index > 10)
                throw new ArrayException();
            return _arr[index].X;
        }

        [TestSvm(100)]
        [IgnoreFuzzer("(known bug) DecodeString: unexpected representation")]
        public void SetValue(int index, int value)
        {
            if (index < 0 || index > 10)
                throw new ArrayException();
            _arr[index].X = value;
        }
    }

    [TestSvmFixture]
    public sealed class InnerPrivateContentArrayMultiDimensional
    {
        private class Content
        {
            public int X { get; set; }
        }

        private readonly Content[,] _arr = new Content[1,1];


        [TestSvm(100)]
        public int GetValue(int index1, int index2)
        {
            if (_arr[index1, index2].X == 100 && _arr[index2, index1].X != 100)
                throw new ArrayException();
            return _arr[index1, index2].X;
        }

        [TestSvm]
        public void SetValue(int index1, int index2, int value)
        {
            _arr[index1, index2].X = value;
        }
    }
}
