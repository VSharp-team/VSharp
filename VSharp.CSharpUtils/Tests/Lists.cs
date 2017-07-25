using System;
using System.Collections.Generic;

namespace VSharp.CSharpUtils.Tests
{
    public class Lists
    {
        public bool Construct()
        {
            var a = new List<int>(4) { 1, 2, 3, 4 };
            var b = new int[4, 1];
            var c = new int[4] { 5, 6, 7, 8 };
            return a.Count == b.Length && b.Length == c.Length && c.Length == c[3] - 4;
        }

        public int GetLengthDimSymbolic(int[,] array, int dimension)
        {
            return array.GetLength(dimension);
        }

        public int LowerBound()
        {
            var c = new int[4, 2] { {1, 1}, {2, 2}, {3, 3}, {4, 4} };
            return c.GetLowerBound(1);
        }

        public int LowerBoundException(int[,] array)
        {
            return array.GetLowerBound(2);
        }

        public int LowerBoundSymbolic(int[,] array, int dimension)
        {
            return array.GetLowerBound(dimension);
        }
//
//        public void Clear()
//        {
//            var a = new int[4] { 5, 6, 7, 8 };
//            System.Array.Clear(a, 1, 2);
//        }
    }
}
