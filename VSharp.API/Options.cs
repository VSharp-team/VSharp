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
    public enum SearchMode
    {
        DFS,
        BFS,
        Guided
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
    /// <param name="SearchMode"></param>
    /// <param name="CoverageZone"></param>
    /// <param name="ExecutionMode"></param>
    /// <param name="RecThreshold"></param>
    /// <param name="IsConstraintIndependenceEnabled"></param>
    /// <param name="IsSolverIncrementalityEnabled"></param>
    public readonly record struct CoverOptions(
        string OutputDirectory = "",
        SearchMode SearchMode = SearchMode.Guided,
        CoverageZone CoverageZone = CoverageZone.Method,
        ExecutionMode ExecutionMode = ExecutionMode.Symbolic,
        uint RecThreshold = 0u,
        bool IsConstraintIndependenceEnabled = true,
        bool IsSolverIncrementalityEnabled = false
    )
    {
        internal siliOptions toSiliOptions()
        {
            var coverageZone = CoverageZone switch
            {
                CoverageZone.Method => Interpreter.IL.coverageZone.MethodZone,
                CoverageZone.Class => Interpreter.IL.coverageZone.ClassZone,
                CoverageZone.Module => Interpreter.IL.coverageZone.ModuleZone
            };
            
            var searchMode = SearchMode switch
            {
                SearchMode.DFS => Interpreter.IL.searchMode.DFSMode,
                SearchMode.BFS => Interpreter.IL.searchMode.BFSMode,
                SearchMode.Guided => Interpreter.IL.searchMode.GuidedMode
            };

            var executionMode = ExecutionMode switch
            {
                ExecutionMode.Symbolic => Interpreter.IL.executionMode.SymbolicMode,
                ExecutionMode.Concolic => Interpreter.IL.executionMode.ConcolicMode
            };

            return new siliOptions(
                OutputDirectory,
                explorationMode.NewTestCoverageMode(coverageZone, searchMode),
                executionMode,
                RecThreshold
            );
        }
    }
}
