using System;

namespace VSharp.CSharpUtils
{
    public static class Array
    {
        [Implements("System.Int32 System.Array.GetUpperBound(this, System.Int32)")]
        public static int GetUpperBound(System.Array array, int dimension)
        {
            return array.GetLowerBound(dimension) + array.GetLength(dimension) - 1;
        }
    }
}
