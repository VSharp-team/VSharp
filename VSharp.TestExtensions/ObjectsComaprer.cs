﻿using System.Diagnostics;
using System.Reflection;

namespace VSharp.TestExtensions;

public static class ObjectsComparer
{
    private static bool StructurallyEqual(object? expected, object? got)
    {
        Debug.Assert(expected != null && got != null && expected.GetType() == got.GetType());
        var flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance;
        var fields = expected.GetType().GetFields(flags);
        foreach (var field in fields)
        {
            if (!TypeUtils.isSubtypeOrEqual(field.FieldType, typeof(MulticastDelegate)) &&
                !field.Name.Contains("threadid", StringComparison.OrdinalIgnoreCase) &&
                !CompareObjects(field.GetValue(expected), field.GetValue(got)))
            {
                return false;
            }
        }

        return true;
    }

    private static bool ContentwiseEqual(System.Array? expected, System.Array? got)
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
            if (!CompareObjects(enum1.Current, enum2.Current))
                return false;
        }
        return true;
    }

    public static bool CompareObjects(object? expected, object? got)
    {
        if (expected == null)
            return got == null;
        if (got == null)
            return false;
        var type = expected.GetType();
        if (type != got.GetType())
            return false;

        if (type == typeof(Pointer) || type.IsPrimitive || expected is string || type.IsEnum)
        {
            // TODO: compare double with epsilon?
            return got.Equals(expected);
        }

        if (expected is System.Array array)
            return ContentwiseEqual(array, got as System.Array);
        return StructurallyEqual(expected, got);
    }
}
