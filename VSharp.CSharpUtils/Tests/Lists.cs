using System.Collections.Generic;

namespace VSharp.CSharpUtils.Tests
{
//    public class ListNode
//    {
//        public int Key { get; set; }
//        public ListNode Next { get; set; }
//    }

    public class Lists
    {
//        private void IncN(ListNode l, int n)
//        {
//            if (l == null || n == 0)
//                return;
//            l.Key += 1;
//            IncN(l.Next, n - 1);
//        }
//
//        public ListNode IncConcreteList(int n)
//        {
//            var l3 = new ListNode { Key = 1, Next = null };
//            var l2 = new ListNode { Key = 1, Next = l3 };
//            var l1 = new ListNode { Key = 1, Next = l2 };
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
//
        public bool Construct()
        {
            var a = new List<int>(4) { 1, 2, 3, 4 };
            var b = new int[4, 1];
            var c = new int[4] { 5, 6, 7, 8 };
            return a.Count == b.Length && b.Length == c.Length && c.Length == c[3] - 4;
        }
    }
}
