#nullable enable
using VSharp.Interpreter.IL;

namespace VSharp.Test.Benchmarks;

public readonly record struct BenchmarkResult(
    bool IsSuccessful,
    SILIStatistics Statistics,
    UnitTests Tests,
    BenchmarkTarget Target,
    int? Coverage = null
);
