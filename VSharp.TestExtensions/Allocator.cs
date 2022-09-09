using System.Runtime.Serialization;

namespace VSharp.TestExtensions;

public class Allocator<T>
{
    private static readonly Type ObjectType = typeof(T);
    private object _toAllocate = FormatterServices.GetUninitializedObject(ObjectType);

    public object this[string fieldName]
    {
        set
        {
            var field = ObjectType.GetField(fieldName, Reflection.allBindingFlags);
            field?.SetValue(_toAllocate, value);
        }
    }

    public T ToObject()
    {
        return (T) _toAllocate;
    }
}
