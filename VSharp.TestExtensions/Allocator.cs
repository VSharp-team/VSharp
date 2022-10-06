using System.Diagnostics;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Serialization;

namespace VSharp.TestExtensions;

internal static class CartesianProductExtension
{
    internal static IEnumerable<IEnumerable<T>> CartesianProduct<T>(this IEnumerable<IEnumerable<T>> sequences) 
    { 
        IEnumerable<IEnumerable<T>> emptyProduct = 
            new[] { Enumerable.Empty<T>() }; 
        return sequences.Aggregate( 
            emptyProduct, 
            (accumulator, sequence) => 
                from accseq in accumulator 
                from item in sequence 
                select accseq.Concat(new[] {item})); 
    }
}

public class Allocator<T>
{
    private static readonly Type ObjectType = typeof(T);
    private object _toAllocate;

    public Allocator()
    {
        _toAllocate = FormatterServices.GetUninitializedObject(ObjectType);
    }

    public Allocator(object? defaultValue, params int[] lengths)
    {
        Debug.Assert(ObjectType.IsArray);
        _toAllocate = Array.CreateInstance(ObjectType.GetElementType()!, lengths);
        Fill(_toAllocate as Array, defaultValue);
    }

    public Allocator(object? defaultValue, int[] lengths, int[] lowerBounds)
    {
        Debug.Assert(ObjectType.IsArray);
        _toAllocate = Array.CreateInstance(ObjectType.GetElementType()!, lengths, lowerBounds);
        Fill(_toAllocate as Array, defaultValue);
    }

    public Allocator(object allocated)
    {
        _toAllocate = allocated;
    }

    public Allocator(object allocated, object defaultValue)
    {
        Debug.Assert(ObjectType.IsArray);
        _toAllocate = allocated;
        Fill(_toAllocate as System.Array, defaultValue);
    }

    public object this[string fieldName]
    {
        set
        {
            var allBindingFlags = BindingFlags.IgnoreCase | BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
            var field = ObjectType.GetField(fieldName, allBindingFlags);
            var property = ObjectType.GetField($"<{fieldName}>k__BackingField", allBindingFlags);
            Debug.Assert(field != null || property != null);
            field ??= property;
            if (field != null)
                field.SetValue(_toAllocate, value);
        }
    }

    public object this[params int[] index]
    {
        set
        {
            Debug.Assert(ObjectType.IsArray);
            var array = _toAllocate as Array;
            array?.SetValue(value, index);
        }
    }

    public T Object => (T) _toAllocate;

    private static void FillFast<TElem>(Array? arr, TElem value)
    {
        if (arr != null)
        {
            ref var bytePtr = ref MemoryMarshal.GetArrayDataReference(arr);
            ref var ptr = ref Unsafe.As<byte, TElem>(ref bytePtr);
            var span = MemoryMarshal.CreateSpan(ref ptr, arr.Length);
            span.Fill(value);
        }
    }

    /// Fills zero-initialized array with value
    private static void Fill(Array? arr, object? value)
    {
        if (value == null)
        {
            // Do nothing because arr is already filled with nulls
            return;
        }

        var t = value.GetType();
        if (arr != null && (!t.IsValueType || Nullable.GetUnderlyingType(t) != null ||
                            value != FormatterServices.GetUninitializedObject(t)))
        {
            var elementType = arr.GetType().GetElementType();
            switch (value)
            {
                case int i when elementType == typeof(int):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(byte):
                    FillFast(arr, i);
                    break;
                case char i when elementType == typeof(char):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(uint):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(Int64):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(UInt64):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(double):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(float):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(Int16):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(UInt16):
                    FillFast(arr, i);
                    break;
                case byte i when elementType == typeof(sbyte):
                    FillFast(arr, i);
                    break;
                default:
                    var rank = arr.Rank;
                    var indices =
                        Enumerable.Range(0, rank)
                            .Select(i => Enumerable.Range(arr.GetLowerBound(i), arr.GetLength(i)))
                            .CartesianProduct();
                    foreach (var i in indices)
                    {
                        arr.SetValue(value, i.ToArray());
                    }

                    break;
            }
        }
    }
}
