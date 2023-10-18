using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
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

public static class Allocator
{
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
    public static void Fill(Array? arr, object? value)
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

    private static object? Call(Type type, object? thisArg, string methodName, params object[]? args)
    {
        var allBindingFlags = BindingFlags.NonPublic | BindingFlags.Public;
        if (thisArg == null)
            allBindingFlags |= BindingFlags.Static;
        else
            allBindingFlags |= BindingFlags.Instance;

        var method = type.GetMethod(methodName, allBindingFlags);
        Debug.Assert(method != null);
        args ??= new object[] { null! };
        try
        {
            return method.Invoke(thisArg, args);
        }
        catch (TargetInvocationException e)
        {
            var inner = e.InnerException;
            if (inner != null)
                throw inner;
            throw;
        }
    }

    // Calls methods of non-public classes
    public static object? Call(this object thisArg, string methodName, params object[]? args)
    {
        Debug.Assert(thisArg != null);
        var type = thisArg.GetType();
        return Call(type, thisArg, methodName, args);
    }

    // Calls static methods of non-public classes
    public static object? Call(string typeName, string methodName, params object[]? args)
    {
        var type = Type.GetType(typeName);
        Debug.Assert(type != null);
        return Call(type, null, methodName:methodName, args);
    }
}

public class Allocator<T>
{
    private readonly Type _objectType = typeof(T);
    private readonly object _toAllocate;

    public Allocator()
    {
        _toAllocate = FormatterServices.GetUninitializedObject(_objectType);
    }

    public Allocator(string typeName)
    {
        Type? notPublicType = Type.GetType(typeName);
        _objectType = notPublicType ?? _objectType;
        _toAllocate = FormatterServices.GetUninitializedObject(_objectType);
    }

    public Allocator(object? defaultValue, params int[] lengths)
    {
        Debug.Assert(_objectType.IsArray);
        _toAllocate = Array.CreateInstance(_objectType.GetElementType()!, lengths);
        Allocator.Fill(_toAllocate as Array, defaultValue);
    }

    public Allocator(object? defaultValue, int[] lengths, int[] lowerBounds)
    {
        Debug.Assert(_objectType.IsArray);
        _toAllocate = Array.CreateInstance(_objectType.GetElementType()!, lengths, lowerBounds);
        Allocator.Fill(_toAllocate as Array, defaultValue);
    }

    public Allocator(string typeName, object? defaultValue, params int[] lengths)
    {
        Type? notPublicType = Type.GetType(typeName);
        _objectType = notPublicType ?? _objectType;
        Debug.Assert(_objectType.IsArray);
        _toAllocate = Array.CreateInstance(_objectType.GetElementType()!, lengths);
        Allocator.Fill(_toAllocate as Array, defaultValue);
    }

    public Allocator(object allocated)
    {
        Debug.Assert(allocated != null);
        _objectType = allocated.GetType();
        _toAllocate = allocated;
    }

    public Allocator(object allocated, object defaultValue)
    {
        Debug.Assert(allocated != null);
        _objectType = allocated.GetType();
        Debug.Assert(_objectType.IsArray);
        _toAllocate = allocated;
        Allocator.Fill(_toAllocate as Array, defaultValue);
    }

    private FieldInfo? GetField(string fieldName)
    {
        const BindingFlags flags = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
        var t = _objectType;
        var index = fieldName.IndexOf('.');
        if (index != -1)
        {
            var typeName = fieldName[.. index];
            fieldName = fieldName[(index + 1) ..];
            while (t != null && t.Name != typeName)
            {
                t = t.BaseType;
            }
        }

        // Basic field case
        var field = t?.GetField(fieldName, flags);
        // Property case
        field ??= t?.GetField($"<{fieldName}>k__BackingField", flags);
        // Mock field case
        field ??= t?.BaseType?.GetField(fieldName, flags);
        // Mock property case
        field ??= t?.BaseType?.GetField($"<{fieldName}>k__BackingField", flags);

        return field;
    }

    public object this[string fieldName]
    {
        set
        {
            var field = GetField(fieldName);
            Debug.Assert(field != null);
            field.SetValue(_toAllocate, value);
        }
    }

    public object this[params int[] index]
    {
        set
        {
            Debug.Assert(_objectType.IsArray);
            var array = _toAllocate as Array;
            array?.SetValue(value, index);
        }
    }

    public T Object => (T)_toAllocate;
}
