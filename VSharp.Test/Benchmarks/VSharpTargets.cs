using System.Collections.Generic;
using System.Reflection;
using VSharp.CSharpUtils;

namespace VSharp.Test.Benchmarks;

internal class VSharpTargets
{
    public static IEnumerable<BenchmarkTarget> Strings()
    {
        var assembly = Assembly.GetExecutingAssembly();
        var type = assembly.ResolveType("Strings");
        return new List<BenchmarkTarget>
        {
            new(type.GetMethod("FormatInt")),
            new(type.GetMethod("FormatUInt32")),
            new(type.GetMethod("StringFormat2"))
        };
    }

    public static IEnumerable<BenchmarkTarget> LoanExam()
    {
        var assembly = Assembly.GetExecutingAssembly();
        var type = assembly.ResolveType("LoanExam");
        return new List<BenchmarkTarget>
        {
            new(type.GetMethod("Build"))
        };
    }
}
