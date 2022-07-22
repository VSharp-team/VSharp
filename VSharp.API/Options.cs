using VSharp.Interpreter.IL;

namespace VSharp
{
    /// <summary>
    /// 
    /// </summary>
    public enum ExecutionMode
    {
        Symbolic,
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
    /// 
    /// </summary>
    public enum CoverageZone
    {
        Method,
        Class,
        Module
    }

    /// <summary>
    /// 
    /// </summary>
    /// <param name="OutputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
    /// <param name="SearchStrategy"></param>
    /// <param name="CoverageZone"></param>
    /// <param name="ExecutionMode"></param>
    /// <param name="GuidedSearch"></param>
    /// <param name="RecThreshold"></param>
    /// <param name="Timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
    /// <param name="IsConstraintIndependenceEnabled"></param>
    /// <param name="IsSolverIncrementalityEnabled"></param>
    public readonly record struct CoverOptions(
        string OutputDirectory = "",
        SearchStrategy SearchStrategy = SearchStrategy.ShortestDistance,
        CoverageZone CoverageZone = CoverageZone.Method,
        ExecutionMode ExecutionMode = ExecutionMode.Symbolic,
        bool GuidedSearch = false,
        uint RecThreshold = 0u,
        int Timeout = -1,
        bool IsConstraintIndependenceEnabled = true,
        bool IsSolverIncrementalityEnabled = false
    )
    {
        internal SiliOptions ToSiliOptions()
        {
            var coverageZone = CoverageZone switch
            {
                CoverageZone.Method => Interpreter.IL.coverageZone.MethodZone,
                CoverageZone.Class => Interpreter.IL.coverageZone.ClassZone,
                CoverageZone.Module => Interpreter.IL.coverageZone.ModuleZone
            };
            
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
                OutputDirectory,
                explorationMode.NewTestCoverageMode(coverageZone, searchMode),
                executionMode,
                RecThreshold,
                Timeout
            );
        }
    }
}
