using System.Diagnostics;
using System.Runtime.Serialization;

namespace VSharp.TestExtensions;

public class Allocator<T>
{
    private static readonly Type ObjectType = typeof(T);
    private object _toAllocate;

    public Allocator()
    {
        _toAllocate = FormatterServices.GetUninitializedObject(ObjectType);
    }

    public Allocator(object defaultValue, params int[] lengths)
    {
        Debug.Assert(ObjectType.IsArray);
        _toAllocate = System.Array.CreateInstance(ObjectType.GetElementType(), lengths);
        Array.fill(_toAllocate as System.Array, defaultValue);
    }

    public Allocator(object defaultValue, int[] lengths, int[] lowerBounds)
    {
        Debug.Assert(ObjectType.IsArray);
        _toAllocate = System.Array.CreateInstance(ObjectType.GetElementType(), lengths, lowerBounds);
        Array.fill(_toAllocate as System.Array, defaultValue);
    }

    public object this[string fieldName]
    {
        set
        {
            var field = ObjectType.GetField(fieldName, Reflection.allBindingFlags);
            var property = ObjectType.GetField($"<{fieldName}>k__BackingField", Reflection.allBindingFlags);
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
            var array = _toAllocate as System.Array;
            array?.SetValue(value, index);
        }
    }

    public T Object => (T) _toAllocate;
}
