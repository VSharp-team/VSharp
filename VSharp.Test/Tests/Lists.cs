using System;
using System.Collections.Generic;
using NUnit.Framework;

namespace VSharp.Test.Tests
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
            var a = new List<int>(4) { 1, 2, 3, 4 };
            var b = new int[4, 1];
            var c = new int[4] { 5, 6, 7, 8 };
            return a.Count == b.Length && b.Length == c.Length && c.Length == c[3] - 4;
        }

        [Ignore("Exceptions handling")]
        public int[] Mutate(int i)
        {
            var a = new int[] {1, 2, 3, 4, 5};
            a[i] = 10;
            return a;
        }

        [TestSvm]
        public int LowerBoundTest()
        {
            var c = new int[4, 2] { { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 } };
            return c.GetLowerBound(1);
        }

        [Ignore("Exceptions handling")]
        public int LowerBoundExceptionTest(int[,] array)
        {
            return array.GetLowerBound(2);
        }

        [Ignore("Exceptions handling")]
        public int LowerBoundSymbolicTest(int[,] array, int dimension)
        {
            return array.GetLowerBound(dimension);
        }

        [TestSvm]
        public int UpperBoundTest()
        {
            var c = new int[4, 2] { { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 } };
            return c.GetUpperBound(0);
        }

        [TestSvm]
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

//        public void ClearTest()
//        {
//            var a = new int[4] { 5, 6, 7, 8 };
//            SystemArray.Clear(a, 1, 2);
//        }
//
//        public void Copy()
//        {
//            var a = new int[4] { 5, 6, 7, 8 };
//            var b = new int[3];
//            a.CopyTo(b, 1);
//        }

        [TestSvm]
        public int RankTest()
        {
            var c = new int[4, 2] { { 1, 1 }, { 2, 2 }, { 3, 3 }, { 4, 4 } };
            return c.Rank;
        }

        [TestSvm]
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

        [Ignore("System.Array.Set(...) is not implemented")]
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
                arrOne[1,1] = 7;
            }
            return arr;
        }

        [Ignore("System.Array.Set(...) is not implemented")]
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
                arrOne[1,1] = 7;
            }
            if (arr is int[,,])
            {
                var arrOne = arr as int[,,];
                arrOne[1,1,1] = 42;
            }
            return arr;
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
                arrOne[1,1] = "42";
            }
            if (arr is int[,])
            {
                var arrOne = arr as int[,];
                arrOne[1,1] = 42;
            }
            return arr;
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

        [TestSvm]
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

        [TestSvm]
        public static int G(int x)
        {
            return F(5) + 10;
        }

        [TestSvm]
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

        [TestSvm]
        public static int TestStack(Second b)
        {
            if (b != null)
            {
                b.Inc();
                return b.Get();
            }
            return 0;
        }

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

        // Test on tracking current heap address during access to heap for filtering possible locations
        [Ignore("Current heap address should be inside ref, otherwise node m will not be filtered out")]
        public static int MemoryTest(LinkedList<int> l)
        {
            LinkedListNode<int> n = new LinkedListNode<int>(10);
            LinkedListNode<int> x = G(l.First, n);
            LinkedListNode<int> m = new LinkedListNode<int>(42);
            return x.Value;
        }
    }
}
