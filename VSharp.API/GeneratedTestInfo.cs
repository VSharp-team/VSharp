using System;

namespace VSharp;

/// <summary>
/// Describes symbolic machine state by the specific test generation moment.
/// </summary>
/// <param name="IsError">Determines if the item is found error or generated unit test.</param>
/// <param name="ExecutionTime">Time elapsed by the test generation moment.</param>
/// <param name="StepsCount">Number of symbolic machine steps made by the test generation moment.</param>
/// <param name="Coverage">Approximate target methods coverage after test was generated.</param>
public readonly record struct GeneratedTestInfo(
    bool IsError,
    TimeSpan ExecutionTime,
    uint StepsCount,
    double Coverage);
