#nullable enable
using System;
using System.Reflection;
using VSharp.Explorer;

namespace VSharp.Test;

public record TestStatistics(
    MethodInfo TestMethodInfo,
    bool ReleaseBranchesEnabled,
    int Timeout,
    SearchStrategy SearchStrategy,
    CoverageZone CoverageZone,
    statisticsDump? SvmStatisticsDump = null,
    uint? Coverage = null,
    uint? TestsGenerated = null,
    string TestsOutputDirectory = "",
    Exception? Exception = null
);
