#nullable enable
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using Microsoft.FSharp.Core;
using VSharp.CSharpUtils;
using VSharp.Interpreter.IL;
using VSharp.Solver;

namespace VSharp
{
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
        /// Only error messages.
        /// </summary>
        Error,
        /// <summary>
        /// Error and info messages.
        /// </summary>
        Info,
        /// <summary>
        /// Only error messages with detailed continuous statistics saved to .csv file in the output directory.
        /// </summary>
        StatisticsCollection
    }

    /// <summary>
    /// Summary of V# test generation process.
    /// </summary>
    public sealed class Statistics
    {
        internal Statistics(TimeSpan time, DirectoryInfo outputDir, uint tests, uint errors, IEnumerable<string> iies)
        {
            TestGenerationTime = time;
            OutputDir = outputDir;
            TestsCount = tests;
            ErrorsCount = errors;
            IncompleteBranches = iies;
        }

        /// <summary>
        /// Overall time of test generation.
        /// </summary>
        public TimeSpan TestGenerationTime { get; }

        /// <summary>
        /// Directory where *.vst tests are placed
        /// </summary>
        public DirectoryInfo OutputDir { get; }

        /// <summary>
        /// The amount of generated unit tests.
        /// </summary>
        public uint TestsCount { get; }

        /// <summary>
        /// The amount of errors found.
        /// </summary>
        public uint ErrorsCount { get; }

        /// <summary>
        /// Some program branches might be failed to investigate. This enumerates the reasons of such failures.
        /// </summary>
        public IEnumerable<string> IncompleteBranches { get; }

        /// <summary>
        /// Writes textual summary of test generation process.
        /// </summary>
        /// <param name="writer">Output writer.</param>
        public void GenerateReport(TextWriter writer)
        {
            writer.WriteLine("Total time: {0:00}:{1:00}:{2:00}.{3}.", TestGenerationTime.Hours,
                TestGenerationTime.Minutes, TestGenerationTime.Seconds, TestGenerationTime.Milliseconds);
            var count = IncompleteBranches.Count();
            if (count > 0)
            {
                writer.WriteLine();
                writer.WriteLine("{0} branch(es) with insufficient input information!", count);
                foreach (var message in IncompleteBranches)
                {
                    writer.WriteLine(message);
                }
            }
            writer.WriteLine("Test results written to {0}", OutputDir.FullName);
        }

        /// <summary>
        /// Gets all generated '.vst' files
        /// </summary>
        public IEnumerable<FileInfo> Results()
        {
            return OutputDir.GetFiles("*.vst");
        }
    }

    public static class TestGenerator
    {
        public const SearchStrategy DefaultSearchStrategy = SearchStrategy.BFS;
        public const Verbosity DefaultVerbosity = Verbosity.Quiet;

        private static Statistics StartExploration(IEnumerable<MethodBase> methods, string resultsFolder,
            coverageZone coverageZone, SearchStrategy searchStrategy, Verbosity verbosity, string[] mainArguments = null, int timeout = -1)
        {
            Logger.current_log_level = verbosity.ToLoggerLevel();

            var recThreshold = 0u;
            var unitTests = new UnitTests(resultsFolder);
            var baseSearchMode = searchStrategy.ToSiliMode();
            var collectStatistics = verbosity == Verbosity.StatisticsCollection;
            // TODO: customize search strategies via console options
            var options =
                new SiliOptions(
                    explorationMode: explorationMode.NewTestCoverageMode(
                        coverageZone,
                        timeout > 0 ? searchMode.NewFairMode(baseSearchMode) : baseSearchMode
                    ),
                    executionMode: executionMode.SymbolicMode,
                    outputDirectory: unitTests.TestDirectory,
                    recThreshold: recThreshold,
                    timeout: timeout,
                    visualize: false,
                    releaseBranches: true,
                    maxBufferSize: 128,
                    checkAttributes: true,
                    collectContinuousDump: collectStatistics,
                    stopOnCoverageAchieved: 100
                );

            using var explorer = new SILI(options);
            Core.API.ConfigureSolver(SolverPool.mkSolver());

            void HandleInternalFail(Method method, Exception exception)
            {
                if (verbosity == Verbosity.Quiet)
                {
                    return;
                }

                var messageBuilder = new StringBuilder("EXCEPTION |");
                if (method is not null)
                {
                    messageBuilder.Append($" {method.DeclaringType}.{method.Name} |");
                }

                messageBuilder.Append($" {exception.GetType().Name} {exception.Message}");

                Console.WriteLine(messageBuilder.ToString());
            }

            var isolated = new List<MethodBase>();
            var entryPoints = new List<Tuple<MethodBase, string[]>>();

            foreach (var method in methods)
            {
                if (method == method.Module.Assembly.EntryPoint)
                {
                    entryPoints.Add(new Tuple<MethodBase, string[]>(method, mainArguments));
                }
                else
                {
                    isolated.Add(method);
                }
            }

            explorer.Interpret(isolated, entryPoints, unitTests.GenerateTest, unitTests.GenerateError, _ => { },
                (m, e) => HandleInternalFail(OptionModule.ToObj(m), e));

            if (verbosity == Verbosity.StatisticsCollection)
            {
                StatisticsReporter.SaveReports(unitTests.TestDirectory.FullName, explorer.Statistics, true);
            }

            var statistics = new Statistics(explorer.Statistics.CurrentExplorationTime, unitTests.TestDirectory,
                unitTests.UnitTestsCount, unitTests.ErrorsCount,
                explorer.Statistics.IncompleteStates.Select(e => e.iie.Value.Message).Distinct());
            unitTests.WriteReport(explorer.Statistics.PrintStatistics);

            return statistics;
        }

        private static void Render(Statistics statistics, Type? declaringType = null)
        {
            TestRenderer.Renderer.Render(
                statistics.Results(),
                false,
                declaringType,
                // Getting parent directory of tests: "VSharp.tests.*/.."
                statistics.OutputDir.Parent
            );
        }

        private static searchMode ToSiliMode(this SearchStrategy searchStrategy)
        {
            return searchStrategy switch
            {
                SearchStrategy.BFS => searchMode.BFSMode,
                SearchStrategy.DFS => searchMode.DFSMode,
                SearchStrategy.ShortestDistance => searchMode.ShortestDistanceBasedMode,
                SearchStrategy.RandomShortestDistance => searchMode.RandomShortestDistanceBasedMode,
                SearchStrategy.ContributedCoverage => searchMode.ContributedCoverageMode,
                SearchStrategy.Interleaved => searchMode.NewInterleavedMode(searchMode.ShortestDistanceBasedMode, 1, searchMode.ContributedCoverageMode, 9)
            };
        }

        private static int ToLoggerLevel(this Verbosity verbosity)
        {
            return verbosity switch
            {
                Verbosity.Quiet => Logger.Quiet,
                Verbosity.Error => Logger.Error,
                Verbosity.Info => Logger.Info,
                Verbosity.StatisticsCollection => Logger.Error
            };
        }

        public static bool Reproduce(DirectoryInfo testDir)
        {
            return TestRunner.TestRunner.ReproduceTests(testDir);
        }

        /// <summary>
        /// Generates test coverage for specified method.
        /// </summary>
        /// <param name="method">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>Summary of tests generation process.</returns>
        public static Statistics Cover(
            MethodBase method,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            AssemblyManager.Load(method.Module.Assembly);
            var methods = new List<MethodBase> {method};
            var statistics = StartExploration(methods, outputDirectory, coverageZone.MethodZone, searchStrategy, verbosity, null, timeout);
            if (renderTests)
                Render(statistics, method.DeclaringType);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of specified type.
        /// </summary>
        /// <param name="type">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static Statistics Cover(
            Type type,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            AssemblyManager.Load(type.Module.Assembly);

            var methods = new List<MethodBase>(type.EnumerateExplorableMethods());
            if (methods.Count == 0)
            {
                throw new ArgumentException("I've not found any public method or constructor of class " + type.FullName);
            }

            var statistics = StartExploration(methods, outputDirectory, coverageZone.ClassZone, searchStrategy, verbosity, null, timeout);
            if (renderTests)
                Render(statistics, type);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of specified types.
        /// </summary>
        /// <param name="types">Types to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if specified classes don't contain public methods..</exception>
        public static Statistics Cover(
            IEnumerable<Type> types,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            List<MethodBase> methods = new List<MethodBase>();
            var typesArray = types as Type[] ?? types.ToArray();
            foreach (var type in typesArray)
            {
                AssemblyManager.Load(type.Module.Assembly);
                methods.AddRange(type.EnumerateExplorableMethods());
            }

            if (methods.Count == 0)
            {
                var names = String.Join(", ", typesArray.Select(t => t.FullName));
                throw new ArgumentException("I've not found any public methods or constructors of classes " + names);
            }

            var statistics = StartExploration(methods, outputDirectory, coverageZone.ClassZone, searchStrategy, verbosity, null, timeout);
            if (renderTests)
                Render(statistics);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of all public classes in the specified assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if no public methods found in assembly.
        /// </exception>
        public static Statistics Cover(
            Assembly assembly,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            AssemblyManager.Load(assembly);
            var methods =
                new List<MethodBase>(assembly.EnumerateExplorableTypes().SelectMany(t => t.EnumerateExplorableMethods()));
            if (methods.Count == 0)
            {
                throw new ArgumentException("I've not found any public method in assembly");
            }

            var statistics = StartExploration(methods, outputDirectory, coverageZone.ModuleZone, searchStrategy, verbosity, null, timeout);
            if (renderTests)
                Render(statistics);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for the entry point of the specified assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="args">Command line arguments of entry point</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if assembly does not contain entry point.
        /// </exception>
        public static Statistics Cover(
            Assembly assembly,
            string[] args,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            AssemblyManager.Load(assembly);

            var entryPoint = assembly.EntryPoint;
            if (entryPoint == null)
            {
                throw new ArgumentException("I've not found entry point in assembly");
            }

            var methods = new List<MethodBase> { entryPoint };

            return StartExploration(methods, outputDirectory, coverageZone.MethodZone, searchStrategy, verbosity, args, timeout);
        }

        /// <summary>
        /// Generates test coverage for the specified method and runs all tests.
        /// </summary>
        /// <param name="method">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>True if all generated tests have passed.</returns>
        public static bool CoverAndRun(
            MethodBase method,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            var stats = Cover(method, timeout, outputDirectory, renderTests, searchStrategy, verbosity);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for the specified type and runs all tests.
        /// </summary>
        /// <param name="type">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static bool CoverAndRun(
            Type type,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            var stats = Cover(type, timeout, outputDirectory, renderTests, searchStrategy, verbosity);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for the specified types and runs all tests.
        /// </summary>
        /// <param name="types">Types to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if specified classes don't contain public methods.</exception>
        public static bool CoverAndRun(
            IEnumerable<Type> types,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            var stats = Cover(types, timeout, outputDirectory, renderTests, searchStrategy, verbosity);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for all public methods of all public classes of the specified assembly and runs all tests.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if no public methods found in assembly.
        /// </exception>
        public static bool CoverAndRun(
            Assembly assembly,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            var stats = Cover(assembly, timeout, outputDirectory, renderTests, searchStrategy, verbosity);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for entry point of the specified assembly and runs all tests.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="args">Command line arguments of entry point</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <param name="searchStrategy">Strategy which symbolic virtual machine uses for branch selection.</param>
        /// <param name="verbosity">Determines which messages are displayed in output.</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if assembly does not contain entry point.</exception>
        public static bool CoverAndRun(
            Assembly assembly,
            string[] args,
            int timeout = -1,
            string outputDirectory = "",
            bool renderTests = false,
            SearchStrategy searchStrategy = DefaultSearchStrategy,
            Verbosity verbosity = DefaultVerbosity)
        {
            var stats = Cover(assembly, args, timeout, outputDirectory, renderTests, searchStrategy, verbosity);
            return Reproduce(stats.OutputDir);
        }

    }

}
