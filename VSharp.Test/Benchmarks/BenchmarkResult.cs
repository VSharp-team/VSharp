#nullable enable
using VSharp.SVM;

namespace VSharp.Test.Benchmarks;

public readonly record struct BenchmarkResult(
    bool IsSuccessful,
    SVMStatistics Statistics,
    UnitTests Tests,
    BenchmarkTarget Target,
    int? Coverage = null
);
