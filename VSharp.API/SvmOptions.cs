using VSharp.Core;

namespace VSharp
{
    /// <summary>
    /// Advanced symbolic virtual machine options.
    /// </summary>
    /// <param name="IsConstraintIndependenceEnabled">If true, independent constraint sets are maintained (constraint independence optimization).</param>
    /// <param name="IsConditionEvaluationEnabled">If true, branch condition is evaluated with current model to avoid extra SMT solver queries.</param>
    /// <param name="IsSolverIncrementalityEnabled">If true, SMT solver works in incremental mode.</param>
    public readonly record struct SvmOptions(
        bool IsConstraintIndependenceEnabled = false,
        bool IsConditionEvaluationEnabled = false,
        bool IsSolverIncrementalityEnabled = false
    )
    {
        internal featureFlags GetFeatureFlags() =>
            new featureFlags(
                isConstraintIndependenceEnabled: IsConstraintIndependenceEnabled,
                isConditionEvaluationEnabled: IsConditionEvaluationEnabled,
                isIncrementalityEnabled: IsSolverIncrementalityEnabled
            );
    }
}
