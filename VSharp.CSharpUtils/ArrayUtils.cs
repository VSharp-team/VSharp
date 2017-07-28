using System;
using SystemArray = System.Array;

namespace VSharp.CSharpUtils
{
    public static class Array
    {
        [Implements("System.Void System.Array.ClearInternal(System.Array, System.Int32, System.Int32)")]
        public static void ClearInternal(object thisNull, SystemArray array, int index, int length)
        {
            object defaultValue = Activator.CreateInstance(array.GetType().GetElementType());
            for (var ind = index; ind < length; ind++)
            {
                array.SetValue(defaultValue, ind);
            }
        }

        [Implements("System.Int32 System.Array.GetUpperBound(this, System.Int32)")]
        public static int GetUpperBound(SystemArray array, int dimension)
        {
            return array.GetLowerBound(dimension) + array.GetLength(dimension) - 1;
        }
    }
}
