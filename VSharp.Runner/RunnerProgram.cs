#nullable enable
using System;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;
using System.Linq;
using System.Reflection;
using VSharp.CSharpUtils;

namespace VSharp.Runner
{
    public static class RunnerProgram
    {

        private static Assembly? TryLoadAssembly(FileInfo assemblyPath)
        {
            try
            {
                return AssemblyManager.LoadFromAssemblyPath(assemblyPath.FullName);
            }
            catch (Exception ex)
            {
                Console.Error.WriteLine($"Cannot load assembly {assemblyPath.FullName}; Error: {ex.Message}");
                Console.Error.WriteLine(ex.StackTrace);
                return null;
            }
        }

        private static void PostProcess(Statistics statistics)
        {
            statistics.GenerateReport(Console.Out);
        }

        public static int Main(string[] args)
        {
            var defaultOptions = new VSharpOptions();

            var assemblyPathArgument =
                new Argument<FileInfo>("assembly-path", description: "Path to the target assembly");
            var timeoutOption = new Option<int>(
                aliases: new[] { "--timeout", "-t" },
                () => -1,
                "Time for test generation in seconds. Negative value means no timeout.");
            var solverTimeoutOption = new Option<int>(
                aliases: new[] { "--solver-timeout", "-st" },
                () => -1,
                "Timeout for SMT solver in seconds. Negative value means no timeout.");
            var outputOption = new Option<DirectoryInfo>(
                aliases: new[] { "--output", "-o" },
                () => new DirectoryInfo(Directory.GetCurrentDirectory()),
                "Path where unit tests will be generated");
            var concreteArguments =
                new Argument<string[]>("args", description: "Command line arguments");
            var unknownArgsOption =
                new Option("--unknown-args", description: "Force engine to generate various input console arguments");
            var renderTestsOption =
                new Option("--render-tests", description: "Render generated tests as NUnit project to specified output path");
            var runTestsOption =
                new Option("--run-tests", description: "Reproduce generated tests");
            var singleFileOption =
                new Option("--single-file") { IsHidden = true };
            var searchStrategyOption = new Option<SearchStrategy>(
                aliases: new[] { "--strat" },
                () => defaultOptions.SearchStrategy,
                "Strategy which symbolic virtual machine uses for branch selection.");
            var verbosityOption = new Option<Verbosity>(
                aliases: new[] { "--verbosity", "-v" },
                () => defaultOptions.Verbosity,
                "Determines which messages are displayed in output.");
            var recursionThresholdOption = new Option<uint>(
                aliases: new[] { "--rec-threshold", "-rt" },
                () => defaultOptions.RecursionThreshold,
                "Terminate exploration of states which have visited the same loop entry or method more times than this value");
            var explorationModeOption = new Option<ExplorationMode>(
                aliases: new[] { "--exploration-mode", "-em" },
                () => ExplorationMode.Sili,
                "Determines which mode is used for exploration"
            );

            var rootCommand = new RootCommand();

            var entryPointCommand =
                new Command("--entry-point", "Generate test coverage from the entry point of assembly (assembly must contain Main method)");
            rootCommand.AddCommand(entryPointCommand);
            entryPointCommand.AddArgument(assemblyPathArgument);
            entryPointCommand.AddArgument(concreteArguments);
            entryPointCommand.AddGlobalOption(timeoutOption);
            entryPointCommand.AddGlobalOption(solverTimeoutOption);
            entryPointCommand.AddGlobalOption(outputOption);
            entryPointCommand.AddOption(unknownArgsOption);
            entryPointCommand.AddGlobalOption(renderTestsOption);
            entryPointCommand.AddGlobalOption(runTestsOption);
            entryPointCommand.AddGlobalOption(searchStrategyOption);
            entryPointCommand.AddGlobalOption(verbosityOption);
            entryPointCommand.AddGlobalOption(recursionThresholdOption);
            entryPointCommand.AddGlobalOption(explorationModeOption);
            var allPublicMethodsCommand =
                new Command("--all-public-methods", "Generate unit tests for all public methods of all public classes of assembly");
            rootCommand.AddCommand(allPublicMethodsCommand);
            allPublicMethodsCommand.AddArgument(assemblyPathArgument);
            allPublicMethodsCommand.AddGlobalOption(timeoutOption);
            allPublicMethodsCommand.AddGlobalOption(solverTimeoutOption);
            allPublicMethodsCommand.AddGlobalOption(outputOption);
            allPublicMethodsCommand.AddGlobalOption(renderTestsOption);
            allPublicMethodsCommand.AddGlobalOption(runTestsOption);
            allPublicMethodsCommand.AddOption(singleFileOption);
            allPublicMethodsCommand.AddGlobalOption(searchStrategyOption);
            allPublicMethodsCommand.AddGlobalOption(verbosityOption);
            allPublicMethodsCommand.AddGlobalOption(recursionThresholdOption);
            allPublicMethodsCommand.AddGlobalOption(explorationModeOption);
            var publicMethodsOfClassCommand =
                new Command("--type", "Generate unit tests for all public methods of specified class");
            rootCommand.AddCommand(publicMethodsOfClassCommand);
            var classArgument = new Argument<string>("class-name");
            publicMethodsOfClassCommand.AddArgument(classArgument);
            publicMethodsOfClassCommand.AddArgument(assemblyPathArgument);
            publicMethodsOfClassCommand.AddGlobalOption(timeoutOption);
            publicMethodsOfClassCommand.AddGlobalOption(solverTimeoutOption);
            publicMethodsOfClassCommand.AddGlobalOption(outputOption);
            publicMethodsOfClassCommand.AddGlobalOption(renderTestsOption);
            publicMethodsOfClassCommand.AddGlobalOption(runTestsOption);
            publicMethodsOfClassCommand.AddGlobalOption(searchStrategyOption);
            publicMethodsOfClassCommand.AddGlobalOption(verbosityOption);
            publicMethodsOfClassCommand.AddGlobalOption(recursionThresholdOption);
            publicMethodsOfClassCommand.AddGlobalOption(explorationModeOption);
            var specificMethodCommand =
                new Command("--method", "Try to resolve and generate unit test coverage for the specified method");
            rootCommand.AddCommand(specificMethodCommand);
            var methodArgument = new Argument<string>("method-name");
            specificMethodCommand.AddArgument(methodArgument);
            specificMethodCommand.AddArgument(assemblyPathArgument);
            specificMethodCommand.AddGlobalOption(timeoutOption);
            specificMethodCommand.AddGlobalOption(solverTimeoutOption);
            specificMethodCommand.AddGlobalOption(outputOption);
            specificMethodCommand.AddGlobalOption(renderTestsOption);
            specificMethodCommand.AddGlobalOption(runTestsOption);
            specificMethodCommand.AddGlobalOption(searchStrategyOption);
            specificMethodCommand.AddGlobalOption(verbosityOption);
            specificMethodCommand.AddGlobalOption(recursionThresholdOption);
            specificMethodCommand.AddGlobalOption(explorationModeOption);
            var namespaceCommand =
                new Command("--namespace", "Try to resolve and generate unit test coverage for all public methods of specified namespace");
            rootCommand.AddCommand(namespaceCommand);
            var namespaceArgument = new Argument<string>("namespace-name");
            namespaceCommand.AddArgument(namespaceArgument);
            namespaceCommand.AddArgument(assemblyPathArgument);
            namespaceCommand.AddGlobalOption(timeoutOption);
            namespaceCommand.AddGlobalOption(solverTimeoutOption);
            namespaceCommand.AddGlobalOption(outputOption);
            namespaceCommand.AddGlobalOption(renderTestsOption);
            namespaceCommand.AddGlobalOption(runTestsOption);
            namespaceCommand.AddGlobalOption(searchStrategyOption);
            namespaceCommand.AddGlobalOption(verbosityOption);
            namespaceCommand.AddGlobalOption(recursionThresholdOption);
            namespaceCommand.AddGlobalOption(explorationModeOption);

            rootCommand.Description = "Symbolic execution engine for .NET";

            entryPointCommand.Handler = CommandHandler.Create<FileInfo, string[], int, int, DirectoryInfo, bool, bool, bool, SearchStrategy, Verbosity, uint, ExplorationMode>(
                (assemblyPath, args, timeout, solverTimeout, output, unknownArgs, renderTests, runTests, strat, verbosity, recursionThreshold, explorationMode) =>
                {
                    var assembly = TryLoadAssembly(assemblyPath);
                    var inputArgs = unknownArgs ? null : args;
                    var options = new VSharpOptions
                    {
                        Timeout = timeout,
                        SolverTimeout = solverTimeout,
                        OutputDirectory = output.FullName,
                        RenderTests = renderTests,
                        SearchStrategy = strat,
                        Verbosity = verbosity,
                        RecursionThreshold = recursionThreshold,
                        ExplorationMode = explorationMode
                    };

                    if (assembly == null) return;

                    if (runTests)
                    {
                        TestGenerator.CoverAndRun(assembly, inputArgs, out var statistics, options);
                        PostProcess(statistics);
                    }
                    else PostProcess(TestGenerator.Cover(assembly, inputArgs, options));
                });

            allPublicMethodsCommand.Handler = CommandHandler.Create<FileInfo, int, int, DirectoryInfo, bool, bool, bool, SearchStrategy, Verbosity, uint, ExplorationMode>(
                (assemblyPath, timeout, solverTimeout, output, renderTests, runTests, singleFile, strat, verbosity, recursionThreshold, explorationMode) =>
                {
                    var assembly = TryLoadAssembly(assemblyPath);
                    var options = new VSharpOptions
                    {
                        Timeout = timeout,
                        SolverTimeout = solverTimeout,
                        OutputDirectory = output.FullName,
                        RenderTests = renderTests,
                        SearchStrategy = strat,
                        Verbosity = verbosity,
                        RecursionThreshold = recursionThreshold,
                        ExplorationMode = explorationMode
                    };

                    if (assembly == null) return;

                    if (runTests)
                    {
                        TestGenerator.CoverAndRun(assembly, out var statistics, options);
                        PostProcess(statistics);
                    }
                    else PostProcess(TestGenerator.Cover(assembly, options));
                });

            publicMethodsOfClassCommand.Handler = CommandHandler.Create<string, FileInfo, int, int, DirectoryInfo, bool, bool, SearchStrategy, Verbosity, uint, ExplorationMode>(
                (className, assemblyPath, timeout, solverTimeout, output, renderTests, runTests, strat, verbosity, recursionThreshold, explorationMode) =>
                {
                    var assembly = TryLoadAssembly(assemblyPath);
                    if (assembly == null) return;

                    var type = assembly.ResolveType(className);
                    if (type == null)
                    {
                        Console.Error.WriteLine($"Cannot find type with name {className} in assembly {assembly.Location}");
                        return;
                    }

                    var options = new VSharpOptions
                    {
                        Timeout = timeout,
                        SolverTimeout = solverTimeout,
                        OutputDirectory = output.FullName,
                        RenderTests = renderTests,
                        SearchStrategy = strat,
                        Verbosity = verbosity,
                        RecursionThreshold = recursionThreshold,
                        ExplorationMode = explorationMode
                    };

                    if (runTests)
                    {
                        TestGenerator.CoverAndRun(type, out var statistics, options);
                        PostProcess(statistics);
                    }
                    else PostProcess(TestGenerator.Cover(type, options));
                });

            specificMethodCommand.Handler = CommandHandler.Create<string, FileInfo, int, int, DirectoryInfo, bool, bool, SearchStrategy, Verbosity, uint, ExplorationMode>(
                (methodName, assemblyPath, timeout, solverTimeout, output, renderTests, runTests, strat, verbosity, recursionThreshold, explorationMode) =>
                {
                    var assembly = TryLoadAssembly(assemblyPath);
                    if (assembly == null) return;

                    MethodBase? method;

                    if (int.TryParse(methodName, out var methodToken))
                    {
                        method = assembly.ResolveMethod(methodToken);

                        if (method == null)
                        {
                            Console.Error.WriteLine($"Cannot find method with token {methodToken} in assembly {assembly.Location}");
                            return;
                        }
                    }
                    else
                    {
                        method = assembly.ResolveMethod(methodName);

                        if (method == null)
                        {
                            Console.Error.WriteLine($"Cannot find method with name {methodName} in assembly {assembly.Location}");
                            return;
                        }
                    }

                    var options = new VSharpOptions
                    {
                        Timeout = timeout,
                        SolverTimeout = solverTimeout,
                        OutputDirectory = output.FullName,
                        RenderTests = renderTests,
                        SearchStrategy = strat,
                        Verbosity = verbosity,
                        RecursionThreshold = recursionThreshold,
                        ExplorationMode = explorationMode
                    };

                    if (runTests)
                    {
                        TestGenerator.CoverAndRun(method, out var statistics, options);
                        PostProcess(statistics);
                    }
                    else PostProcess(TestGenerator.Cover(method, options));
                });

            namespaceCommand.Handler = CommandHandler.Create<string, FileInfo, int, int, DirectoryInfo, bool, bool, SearchStrategy, Verbosity, uint, ExplorationMode>(
                (namespaceName, assemblyPath, timeout, solverTimeout, output, renderTests, runTests, strat, verbosity, recursionThreshold, explorationMode) =>
                {
                    var assembly = TryLoadAssembly(assemblyPath);
                    if (assembly == null) return;

                    var namespaceTypes = assembly.ResolveNamespace(namespaceName).ToArray();
                    if (namespaceTypes.Length == 0)
                    {
                        Console.Error.WriteLine($"Cannot find any types in namespace {namespaceName}");
                        return;
                    }

                    var options = new VSharpOptions
                    {
                        Timeout = timeout,
                        SolverTimeout = solverTimeout,
                        OutputDirectory = output.FullName,
                        RenderTests = renderTests,
                        SearchStrategy = strat,
                        Verbosity = verbosity,
                        RecursionThreshold = recursionThreshold,
                        ExplorationMode = explorationMode
                    };
                    if (runTests)
                    {
                        TestGenerator.CoverAndRun(namespaceTypes, out var statistics, options);
                        PostProcess(statistics);
                    }
                    else PostProcess(TestGenerator.Cover(namespaceTypes, options));
                });

            return rootCommand.Invoke(args);
        }
    }
}
