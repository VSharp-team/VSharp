using System;

namespace VSharp.CSharpUtils
{
    public static class Array
    {
        [Implements("System.Void System.Array.ClearInternal(System.Array, System.Int32, System.Int32)")]
        public static void ClearInternal(object thisNull, System.Array array, int index, int length)
        {
            index += array.GetLowerBound(0);
            object defaultValue = Activator.CreateInstance(array.GetType().GetElementType());
            for (var ind = index; ind < length; ind++)
            {
                array.SetValue(defaultValue, ind);
            }
        }

        [Implements("System.Void System.Array.Clear(System.Array, System.Int32, System.Int32)")]
        public static void Clear(object thisNull, System.Array array, int index, int length)
        {
            if (array == null)
                throw new ArgumentNullException(nameof(array), "Array cannot be null");
            if (length < 0)
                throw new IndexOutOfRangeException();
            int lowerBound = array.GetLowerBound(0);
            if (index < lowerBound)
                throw new IndexOutOfRangeException();
            index -= lowerBound;
            if (index > array.Length - length)
                throw new IndexOutOfRangeException();
            ClearInternal(thisNull, array, index, length);
        }

        [Implements("System.Int32 System.Array.GetUpperBound(this, System.Int32)")]
        public static int GetUpperBound(System.Array array, int dimension)
        {
            return array.GetLowerBound(dimension) + array.GetLength(dimension) - 1;
        }
    }
}
