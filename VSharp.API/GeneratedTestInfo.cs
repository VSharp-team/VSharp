using System;

namespace VSharp;

public readonly record struct GeneratedTestInfo(
    bool IsError,
    TimeSpan ExecutionTime,
    uint StepsCount,
    double Coverage);