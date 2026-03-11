using System.IO;
using Microsoft.FSharp.Core;
using VSharp.Explorer;

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
    /// Picks the next state by randomly descending from root to leaf in symbolic execution tree.
    /// </summary>
    ExecutionTree,
    /// <summary>
    /// Interleaves <see cref="ExecutionTree"/> and <see cref="ContributedCoverage"/> strategies.
    /// </summary>
    ExecutionTreeContributedCoverage,
    /// <summary>
    /// Interleaves <see cref="ShortestDistance"/> and <see cref="ContributedCoverage"/> strategies.
    /// </summary>
    Interleaved,
    AI
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
    Info,
    /// <summary>
    /// Error, warning, info and debug messages.
    /// </summary>
    Trace
}

/// <summary>
/// Determines which mode is used for exploration.
/// </summary>
public enum ExplorationMode
{
    Fuzzing,
    Sili,
    Interleaving
}

public readonly record struct VSharpOptions
{
    private const int DefaultTimeout = -1;
    private const int DefaultSolverTimeout = -1;
    private const string DefaultOutputDirectory = "";
    private const string DefaultRenderedTestsDirectory = "";
    private const bool DefaultRenderTests = false;
    private const SearchStrategy DefaultSearchStrategy = SearchStrategy.ExecutionTreeContributedCoverage;
    private const Verbosity DefaultVerbosity = Verbosity.Quiet;
    private const uint DefaultRecursionThreshold = 0u;
    private const ExplorationMode DefaultExplorationMode = ExplorationMode.Sili;
    private const bool DefaultReleaseBranches = true;
    private const int DefaultRandomSeed = -1;
    private const uint DefaultStepsLimit = 0;
    private const string DefaultPathToModel = "models/model.onnx";

    public readonly int Timeout = DefaultTimeout;
    public readonly int SolverTimeout = DefaultSolverTimeout;
    public readonly string OutputDirectory = DefaultOutputDirectory;
    public readonly string RenderedTestsDirectory = DefaultRenderedTestsDirectory;
    public readonly bool RenderTests = DefaultRenderTests;
    public readonly SearchStrategy SearchStrategy = DefaultSearchStrategy;
    public readonly Verbosity Verbosity = DefaultVerbosity;
    public readonly uint RecursionThreshold = DefaultRecursionThreshold;
    public readonly ExplorationMode ExplorationMode = DefaultExplorationMode;
    public readonly bool ReleaseBranches = DefaultReleaseBranches;
    public readonly int RandomSeed = DefaultRandomSeed;
    public readonly uint StepsLimit = DefaultStepsLimit;
    public readonly AIOptions? AIOptions = null;
    public readonly string PathToModel = DefaultPathToModel;
    public readonly bool UseGPU = false;
    public readonly bool Optimize = false;

    /// <summary>
    /// Symbolic virtual machine options.
    /// </summary>
    /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
    /// <param name="solverTimeout">Timeout for SMT solver in seconds. Negative value means no timeout.</param>
    /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
    /// <param name="renderedTestsDirectory">Directory to place the project with rendered NUnit tests (if <see cref="renderTests"/> is enabled). If null or empty, parent of process working directory is used.</param>
    /// <param name="renderTests">If true, NUnit tests are rendered.</param>
    /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
    /// <param name="verbosity">Determines which messages are displayed in output.</param>
    /// <param name="recursionThreshold">If greater than zero, terminate exploration of states which have visited the same loop entry or method more times than the value.</param>
    /// <param name="explorationMode">Determines which mode is used for exploration.</param>
    /// <param name="releaseBranches">If true and timeout is specified, a part of allotted time in the end is given to execute remaining states without branching.</param>
    /// <param name="randomSeed">Fixed seed for random operations. Used if greater than or equal to zero.</param>
    /// <param name="stepsLimit">Number of symbolic machine steps to stop execution after. Zero value means no limit.</param>
    /// <param name="aiOptions">Settings for AI searcher training.</param>
    /// <param name="pathToModel">Path to ONNX file with model to use in AI searcher.</param>
    /// <param name="useGPU">Specifies whether the ONNX execution session should use a CUDA-enabled GPU.</param>
    /// <param name="optimize">Enabling options like parallel execution and various graph transformations to enhance performance of ONNX.</param>
    public VSharpOptions(
        int timeout = DefaultTimeout,
        int solverTimeout = DefaultSolverTimeout,
        string outputDirectory = DefaultOutputDirectory,
        string renderedTestsDirectory = DefaultRenderedTestsDirectory,
        bool renderTests = DefaultRenderTests,
        SearchStrategy searchStrategy = DefaultSearchStrategy,
        Verbosity verbosity = DefaultVerbosity,
        uint recursionThreshold = DefaultRecursionThreshold,
        ExplorationMode explorationMode = DefaultExplorationMode,
        bool releaseBranches = DefaultReleaseBranches,
        int randomSeed = DefaultRandomSeed,
        uint stepsLimit = DefaultStepsLimit,
        AIOptions? aiOptions = null,
        string pathToModel = DefaultPathToModel,
        bool useGPU = false,
        bool optimize = false)
    {
        Timeout = timeout;
        SolverTimeout = solverTimeout;
        OutputDirectory = outputDirectory;
        RenderedTestsDirectory = renderedTestsDirectory;
        RenderTests = renderTests;
        SearchStrategy = searchStrategy;
        Verbosity = verbosity;
        RecursionThreshold = recursionThreshold;
        ExplorationMode = explorationMode;
        ReleaseBranches = releaseBranches;
        RandomSeed = randomSeed;
        StepsLimit = stepsLimit;
        AIOptions = aiOptions;
        PathToModel = pathToModel;
        UseGPU = useGPU;
        Optimize = optimize;
    }

    /// <summary>
    /// <seealso cref="RenderedTestsDirectory"/>
    /// </summary>
    public DirectoryInfo RenderedTestsDirectoryInfo =>
        Directory.Exists(RenderedTestsDirectory) ? new DirectoryInfo(RenderedTestsDirectory) : null;

    public string GetDefaultPathToModel()
    {
        return DefaultPathToModel;
    }
}
