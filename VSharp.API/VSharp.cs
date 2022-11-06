#nullable enable
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using VSharp.Interpreter.IL;
using VSharp.Solver;

namespace VSharp
{
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
        internal IEnumerable<FileInfo> Results()
        {
            return OutputDir.GetFiles("*.vst");
        }
    }

    public static class TestGenerator
    {
        private static Statistics StartExploration(List<MethodBase> methods, string resultsFolder, string[]? mainArguments = null, int timeout = -1)
        {
            var recThreshold = 0u;
            UnitTests unitTests = new UnitTests(resultsFolder);
            // TODO: customize search strategies via console options
            var options =
                new SiliOptions(
                    explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchMode.DFSMode),
                    executionMode.SymbolicMode,
                    unitTests.TestDirectory,
                    recThreshold,
                    timeout,
                    false,
                    true,
                    128,
                    true);
            SILI explorer = new SILI(options);
            Core.API.ConfigureSolver(SolverPool.mkSolver());

            void HandleInternalFail(MethodBase method, Exception exception)
            {
                Console.WriteLine($"EXCEPTION | {method.DeclaringType}.{method.Name} | {exception.GetType().Name} {exception.Message}");
            }

            foreach (var method in methods)
            {
                if (method == method.Module.Assembly.EntryPoint)
                {
                    explorer.InterpretEntryPoint(method, mainArguments, unitTests.GenerateTest, unitTests.GenerateError, _ => { },
                        e => HandleInternalFail(method, e));
                }
                else
                {
                    explorer.InterpretIsolated(method, unitTests.GenerateTest, unitTests.GenerateError, _ => { },
                        e => HandleInternalFail(method, e));
                }
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

        public static bool Reproduce(DirectoryInfo testDir)
        {
            return TestRunner.TestRunner.ReproduceTests(testDir);
        }

        private static readonly BindingFlags MethodsFlags =
            BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.DeclaredOnly;
        /// <summary>
        /// Generates test coverage for specified method.
        /// </summary>
        /// <param name="method">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interruption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>Summary of tests generation process.</returns>
        public static Statistics Cover(MethodBase method, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            AssemblyManager.Load(method.Module.Assembly);
            List<MethodBase> methods = new List<MethodBase> {method};
            var statistics = StartExploration(methods, outputDirectory, null, timeout);
            if (renderTests)
                Render(statistics, method.DeclaringType);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of specified type.
        /// </summary>
        /// <param name="type">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static Statistics Cover(Type type, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            AssemblyManager.Load(type.Module.Assembly);
            List<MethodBase> methods = new List<MethodBase>(type.GetConstructors());

            methods.AddRange(type.GetMethods(MethodsFlags));

            if (methods.Count == 0)
            {
                throw new ArgumentException("I've not found any public method or constructor of class " + type.FullName);
            }

            var statistics = StartExploration(methods, outputDirectory, null, timeout);
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
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static Statistics Cover(IEnumerable<Type> types, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            List<MethodBase> methods = new List<MethodBase>();
            var typesArray = types as Type[] ?? types.ToArray();
            foreach (var type in typesArray)
            {
                AssemblyManager.Load(type.Module.Assembly);
                methods.AddRange(type.GetConstructors());
                methods.AddRange(type.GetMethods(MethodsFlags));
            }

            if (methods.Count == 0)
            {
                var names = String.Join(", ", typesArray.Select(t => t.FullName));
                throw new ArgumentException("I've not found any public methods or constructors of classes " + names);
            }

            var statistics = StartExploration(methods, outputDirectory, null, timeout);
            if (renderTests)
                Render(statistics);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for all public methods of all public classes in the specified assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if no public methods found in assembly.
        /// </exception>
        public static Statistics Cover(Assembly assembly, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            AssemblyManager.Load(assembly);
            List<MethodBase> methods;
            methods = new List<MethodBase>();

            foreach (var t in assembly.GetTypes())
            {
                if (t.IsPublic || t.IsNestedPublic)
                {
                    foreach (var m in t.GetMethods(MethodsFlags))
                    {
                        if (m.GetMethodBody() != null && !Reflection.hasByRefLikes(m))
                            methods.Add(m);
                    }
                }
            }

            if (methods.Count == 0)
            {
                throw new ArgumentException("I've not found any public method in assembly");
            }

            var statistics = StartExploration(methods, outputDirectory, null, timeout);
            if (renderTests)
                Render(statistics);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for the entry point of the specified assembly.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="args">Command line arguments of entry point</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>Summary of tests generation process.</returns>
        /// <exception cref="ArgumentException">Thrown if assembly does not contain entry point.
        /// </exception>
        public static Statistics Cover(Assembly assembly, string[]? args, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            AssemblyManager.Load(assembly);
            var entryPoint = assembly.EntryPoint;
            if (entryPoint == null)
            {
                throw new ArgumentException("I've not found entry point in assembly");
            }
            var methods = new List<MethodBase> { entryPoint };

            var statistics = StartExploration(methods, outputDirectory, args, timeout);
            if (renderTests)
                Render(statistics, entryPoint.DeclaringType);
            return statistics;
        }

        /// <summary>
        /// Generates test coverage for the specified method and runs all tests.
        /// </summary>
        /// <param name="method">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>True if all generated tests have passed.</returns>
        public static bool CoverAndRun(MethodBase method, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            var stats = Cover(method, timeout, outputDirectory, renderTests);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for the specified type and runs all tests.
        /// </summary>
        /// <param name="type">Type to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static bool CoverAndRun(Type type, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            var stats = Cover(type, timeout, outputDirectory, renderTests);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for the specified types and runs all tests.
        /// </summary>
        /// <param name="types">Types to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if specified class does not contain public methods.</exception>
        public static bool CoverAndRun(IEnumerable<Type> types, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            var stats = Cover(types, timeout, outputDirectory, renderTests);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for all public methods of all public classes of the specified assembly and runs all tests.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if no public methods found in assembly.
        /// </exception>
        public static bool CoverAndRun(Assembly assembly, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            var stats = Cover(assembly, timeout, outputDirectory, renderTests);
            return Reproduce(stats.OutputDir);
        }

        /// <summary>
        /// Generates test coverage for entry point of the specified assembly and runs all tests.
        /// </summary>
        /// <param name="assembly">Assembly to be covered with tests.</param>
        /// <param name="args">Command line arguments of entry point</param>
        /// <param name="timeout">Timeout for code exploration in seconds. Negative value means infinite timeout (up to exhaustive coverage or user interuption).</param>
        /// <param name="outputDirectory">Directory to place generated *.vst tests. If null or empty, process working directory is used.</param>
        /// <param name="renderTests">Flag, that identifies whether to render NUnit tests or not</param>
        /// <returns>True if all generated tests have passed.</returns>
        /// <exception cref="ArgumentException">Thrown if assembly does not contain entry point.</exception>
        public static bool CoverAndRun(Assembly assembly, string[] args, int timeout = -1, string outputDirectory = "", bool renderTests = false)
        {
            var stats = Cover(assembly, args, timeout, outputDirectory, renderTests);
            return Reproduce(stats.OutputDir);
        }

    }

}
