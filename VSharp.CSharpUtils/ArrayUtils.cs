using System;
using SystemArray = System.Array;

namespace VSharp.CSharpUtils
{
    public static class Array
    {
        [Implements("System.Void System.Array.ClearInternal(System.Array, System.Int32, System.Int32)")]
        public static void ClearInternal(SystemArray array, int index, int length)
        {
            object defaultValue = Activator.CreateInstance(array.GetType().GetElementType());
            for (var ind = index; ind < length; ind++)
            {
                array.SetValue(defaultValue, ind);
            }
        }
    }
}
