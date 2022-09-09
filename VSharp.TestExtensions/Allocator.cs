using System.Diagnostics;
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
            var property = ObjectType.GetField($"<{fieldName}>k__BackingField", Reflection.allBindingFlags);
            Debug.Assert(field != null || property != null);
            field ??= property;
            if (field != null)
                field.SetValue(_toAllocate, value);
        }
    }

    public T Object => (T) _toAllocate;
}
