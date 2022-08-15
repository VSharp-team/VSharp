using System;
using System.Reflection;
using VSharp.Interpreter.IL;

namespace VSharp.Test;

public record TestStatistics(
    MethodInfo TestMethodInfo,
    bool IsGuidedMode,
    SearchStrategy SearchStrategy,
    CoverageZone CoverageZone,
    statisticsDump SiliStatisticsDump = null,
    int? Coverage = null,
    uint TestsGenerated = 0,
    Exception Exception = null
);
