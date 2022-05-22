using VSharp.Core;

namespace VSharp;

public readonly record struct Parameters(
    bool IsConstraintIndependenceEnabled,
    bool IsConditionEvalEnabled,
    bool IsIncrementalityEnabled,
    string RunId = ""
)
{
    public featureFlags GetFeatureFlags() =>
        new featureFlags(
            isConstraintIndependenceEnabled: IsConstraintIndependenceEnabled,
            isConditionEvalEnabled: IsConditionEvalEnabled,
            isIncrementalityEnabled: IsIncrementalityEnabled
        );
}