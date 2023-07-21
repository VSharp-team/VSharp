using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Reflection;

namespace VSharp.TestExtensions;

public static class ObjectsComparer
{
    private class Comparer
    {
        // For circular references handling
        private readonly HashSet<(object, object)> _comparedObjects = new();

        private bool StructurallyEqual(object? expected, object? got)
        {
            Debug.Assert(expected != null && got != null && expected.GetType() == got.GetType());
            var flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance;
            var fields = expected.GetType().GetFields(flags);
            foreach (var field in fields)
            {
                if (!typeof(MulticastDelegate).IsAssignableFrom(field.FieldType) &&
                    !field.Name.Contains("threadid", StringComparison.OrdinalIgnoreCase) &&
                    !InnerCompareObjects(field.GetValue(expected), field.GetValue(got)))
                {
                    return false;
                }
            }

            return true;
        }

        private bool ContentwiseEqual(global::System.Array? expected, global::System.Array? got)
        {
            Debug.Assert(expected != null && got != null && expected.GetType() == got.GetType());
            if (expected.Rank != got.Rank)
                return false;
            for (int i = 0; i < expected.Rank; ++i)
                if (expected.GetLength(i) != got.GetLength(i) || expected.GetLowerBound(i) != got.GetLowerBound(i))
                    return false;
            var enum1 = expected.GetEnumerator();
            var enum2 = got.GetEnumerator();
            while (enum1.MoveNext() && enum2.MoveNext())
            {
                if (!InnerCompareObjects(enum1.Current, enum2.Current))
                    return false;
            }
            return true;
        }

        private bool InnerCompareObjects(object? expected, object? got)
        {
            if (expected == null)
                return got == null;
            if (got == null)
                return false;
            var type = expected.GetType();
            if (type != got.GetType())
                return false;

            if (Object.ReferenceEquals(expected, got))
                return true;

            if (type == typeof(Pointer) || type.IsPrimitive || expected is string || type.IsEnum)
            {
                // TODO: compare double with epsilon?
                return got.Equals(expected);
            }

            if (_comparedObjects.Contains((expected, got)))
            {
                return true;
            }

            _comparedObjects.Add((expected, got));

            if (expected is global::System.Array array)
                return ContentwiseEqual(array, got as global::System.Array);

            return StructurallyEqual(expected, got);
        }

        public bool Compare(object? expected, object? got)
        {
            try
            {
                return InnerCompareObjects(expected, got);
            }
            finally
            {
                _comparedObjects.Clear();
            }

        }
    }

    public static bool CompareObjects(object? expected, object? got)
    {
        var comparer = new Comparer();
        return comparer.Compare(expected, got);
    }

    public static unsafe bool CompareObjects(void* expected, void* got)
    {
        return expected == got;
    }
}
