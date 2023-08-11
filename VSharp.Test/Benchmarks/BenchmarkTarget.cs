#nullable enable
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using VSharp.CSharpUtils;

namespace VSharp.Test.Benchmarks;

public class BenchmarkTarget
{
    private readonly List<Type> _types = new();
    public MethodBase? Method { get; private set; }
    public IReadOnlyList<Type> Types => _types;
    public Assembly Assembly { get; private set; }

    public BenchmarkTarget(MethodBase method)
    {
        Assembly = method.Module.Assembly;
        Method = method;
    }

    public BenchmarkTarget(Assembly assembly, string className)
    {
        var type = assembly.ResolveType(className);
        if (type is null)
        {
            throw new Exception($"Cannot resolve type {className}");
        }

        Assembly = assembly;
        _types.Add(type);
    }

    public override string ToString()
    {
        if (Method is not null)
        {
            return $"{Assembly.GetName().Name}.{Method.DeclaringType}.{Method.Name}";
        }

        if (_types.Count == 1)
        {
            return $"{Assembly.GetName().Name}.{_types.Single().Name}";
        }

        if (_types.Count > 1)
        {
            return $"{Assembly.GetName().Name}.[{_types.First().Name}-{_types.Last().Name}]";
        }

        throw new UnreachableException("Inconsistent benchmark target");
    }

    public static IEnumerable<BenchmarkTarget> ForAllMethods(Assembly assembly, IEnumerable<string> classNames)
    {
        foreach (var className in classNames)
        {
            var type = assembly.ResolveType(className);
            if (type is null)
            {
                throw new Exception($"Cannot resolve type {className}");
            }

            foreach (var method in type.EnumerateExplorableMethods())
            {
                yield return new BenchmarkTarget(method);
            }
        }
    }
}
