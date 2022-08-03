using System.IO;
using VSharp.Interpreter.IL;

namespace VSharp
{
    /// <summary>
    /// Symbolic virtual machine execution mode.
    /// </summary>
    public enum ExecutionMode
    {
        /// <summary>
        /// Pure symbolic execution. Cannot cope with external dependencies in code. 
        /// </summary>
        Symbolic,
        /// <summary>
        /// (Experimental) Concrete and symbolic execution combination. Allows to explore code with external dependencies by
        /// instantiating concrete objects.
        /// </summary>
        Concolic
    }

    /// <summary>
    /// 
    /// </summary>
    public enum SearchStrategy
    {
        DFS,
        BFS,
        ShortestDistance
    }

    /// <summary>
    /// Parameters of the test generation run. 
    /// </summary>
    /// <param name="OutputDirectory">Directory to place generated <c>*.vst</c> tests. If <c>null</c> or empty, process working directory is used.
    /// <c>null</c> by default. </param>
    /// <param name="SearchStrategy"></param>
    /// <param name="ExecutionMode">Symbolic virtual machine execution mode. <see cref="F:ExecutionMode.Symbolic"/> by default.</param>
    /// <param name="GuidedSearch"></param>
    /// <param name="RecThreshold"></param>
    /// <param name="Timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).
    /// -1 by default</param>
    /// <param name="IsConstraintIndependenceEnabled">If true, constraint independence optimization is enabled: independent constraint sets are maintained
    /// during symbolic execution. In general, improves execution time. <c>true</c> by default.</param>
    /// <param name="IsSolverIncrementalityEnabled">If true, SMT solver works in incremental mode during symbolic execution. May improve execution
    /// time in some cases. <c>false</c> by default.</param>
    /// <param name="Visualize">If true, symbolic execution process animation is saved in <c>/visualize</c> subdirectory of the <see cref="OutputDirectory"/>.</param>
    public readonly record struct CoverOptions(
        DirectoryInfo OutputDirectory = null,
        SearchStrategy SearchStrategy = SearchStrategy.DFS,
        ExecutionMode ExecutionMode = ExecutionMode.Symbolic,
        bool GuidedSearch = false,
        uint RecThreshold = 0u,
        int Timeout = -1,
        bool IsConstraintIndependenceEnabled = true,
        bool IsSolverIncrementalityEnabled = false,
        bool Visualize = false
    )
    {
        internal SiliOptions ToSiliOptions(coverageZone coverageZone)
        {
            var searchMode = SearchStrategy switch
            {
                SearchStrategy.DFS => Interpreter.IL.searchMode.DFSMode,
                SearchStrategy.BFS => Interpreter.IL.searchMode.BFSMode,
                SearchStrategy.ShortestDistance => Interpreter.IL.searchMode.ShortestDistanceBasedMode
            };

            if (GuidedSearch)
            {
                searchMode = searchMode.NewGuidedMode(searchMode);
            }

            var executionMode = ExecutionMode switch
            {
                ExecutionMode.Symbolic => Interpreter.IL.executionMode.SymbolicMode,
                ExecutionMode.Concolic => Interpreter.IL.executionMode.ConcolicMode
            };

            return new SiliOptions(
                explorationMode.NewTestCoverageMode(coverageZone, searchMode),
                executionMode,
                OutputDirectory,
                RecThreshold,
                Timeout,
                Visualize
            );
        }
    }
}
