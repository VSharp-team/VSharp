using System;
using System.Reflection;
using VSharp.Interpreter.IL;

namespace VSharp.Test;

public record TestStatistics(
    MethodInfo TestMethodInfo,
    bool ReleaseBranchesEnabled,
    int Timeout,
    SearchStrategy SearchStrategy,
    CoverageZone CoverageZone,
    statisticsDump SiliStatisticsDump = null,
    uint? Coverage = null,
    uint? TestsGenerated = null,
    string TestsOutputDirectory = "",
    Exception Exception = null
);
