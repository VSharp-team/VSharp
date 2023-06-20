namespace VSharp;

/// <summary>
/// Strategy which symbolic virtual machine uses for branch selection.
/// </summary>
public enum SearchStrategy
{
    /// <summary>
    /// Depth-first search strategy.
    /// </summary>
    DFS,
    /// <summary>
    /// Breadth-first search strategy.
    /// </summary>
    BFS,
    /// <summary>
    /// Picks the state closest to not covered locations.
    /// </summary>
    ShortestDistance,
    /// <summary>
    /// With a high probability picks the state closest to not covered locations.
    /// </summary>
    RandomShortestDistance,
    /// <summary>
    /// Picks a state which has visited the largest number of not covered locations.
    /// </summary>
    ContributedCoverage,
    /// <summary>
    /// Interleaves <see cref="ShortestDistance"/> and <see cref="ContributedCoverage"/> strategies.
    /// </summary>
    Interleaved
}

/// <summary>
/// Determines which messages are displayed in output.
/// </summary>
public enum Verbosity
{
    /// <summary>
    /// No output messages.
    /// </summary>
    Quiet,
    /// <summary>
    /// Only critical error messages.
    /// </summary>
    Critical,
    /// <summary>
    /// Only error messages.
    /// </summary>
    Error,
    /// <summary>
    /// Error and warning messages.
    /// </summary>
    Warning,
    /// <summary>
    /// Error, warning and info messages.
    /// </summary>
    Info
}

/// <summary>
/// Symbolic virtual machine options.
/// </summary>
/// <param name="Timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
/// <param name="SolverTimeout">Timeout for SMT solver in seconds. Negative value means no timeout.</param>
/// <param name="OutputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
/// <param name="RenderTests">If true, NUnit tests are rendered</param>
/// <param name="SearchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
/// <param name="Verbosity">Determines which messages are displayed in output.</param>
/// <param name="RecursionThreshold">If greater than zero, terminate exploration of states which have visited the same loop entry or method more times than the value.</param>
public readonly record struct VSharpOptions(
    int Timeout = -1,
    int SolverTimeout = -1,
    string OutputDirectory = "",
    bool RenderTests = false,
    SearchStrategy SearchStrategy = SearchStrategy.BFS,
    Verbosity Verbosity = Verbosity.Quiet,
    uint RecursionThreshold = 0u);
