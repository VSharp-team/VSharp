using System.Collections.Generic;

namespace VSharp.Test.Benchmarks;

internal class LifetimesTargets
{
    private const string LifetimesSuiteName = "jb_lifetimes";
    private const string LifetimesDllName = "JetBrains.Lifetimes";

    private static readonly List<string> CollectionsClasses = new()
    {
        "SynchronizedDictionary",
        "SynchronizedList",
        "SynchronizedSet",

        "CollectionEx",
        "CompactList",
        "CopyOnWriteList",
        // "DictionaryEx" Unknown method: System.Void System.RuntimeTypeHandle.GetInstantiation
        "JetKeyValuePair",
        "JetPriorityQueue",
        "ViewableList",
        "ViewableMap",
        "ViewableProperty",
        "ViewableSet"
    };

    private static readonly List<string> UtilClasses = new()
    {
        "BitHacks",
        "BitSlice",
        "CastTo",
        "EmptyAction",
        "LocalStopwatch",
        "Memory",
        "NullableEx",
        "ReflectionUtil",
        "RuntimeInfo",
        "SingleThreadObjectPool",
        "Statics",
        "Types"
    };

    public static IEnumerable<BenchmarkTarget> Collections()
    {
        var assembly = Benchmarks.LoadBenchmarkAssembly(LifetimesSuiteName, LifetimesDllName);
        return BenchmarkTarget.ForAllMethods(assembly, CollectionsClasses);
    }

    public static IEnumerable<BenchmarkTarget> CompactList()
    {
        var assembly = Benchmarks.LoadBenchmarkAssembly(LifetimesSuiteName, LifetimesDllName);
        return new List<BenchmarkTarget> { new BenchmarkTarget(assembly, "CompactList") };
    }

    public static IEnumerable<BenchmarkTarget> Util()
    {
        var assembly = Benchmarks.LoadBenchmarkAssembly(LifetimesSuiteName, LifetimesDllName);
        return BenchmarkTarget.ForAllMethods(assembly, UtilClasses);
    }
}
