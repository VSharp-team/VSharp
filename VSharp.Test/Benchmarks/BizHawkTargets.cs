using System.Collections.Generic;
using VSharp.CSharpUtils;

namespace VSharp.Test.Benchmarks;

internal class BizHawkTargets
{
    private const string BizHawkSuiteName = "bizhawk";
    private const string CoresDllName = "BizHawk.Emulation.Cores";

    public static IEnumerable<BenchmarkTarget> LR35902()
    {
        var assembly = Benchmarks.LoadBenchmarkAssembly(BizHawkSuiteName, CoresDllName);
        var type = assembly.ResolveType("LR35902");
        var method = type.GetMethod("ExecuteOne");
        return new List<BenchmarkTarget> { new(method) };
    }
}
