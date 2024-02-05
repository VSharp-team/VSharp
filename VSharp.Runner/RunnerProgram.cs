#nullable enable
using System;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using VSharp.CSharpUtils;

namespace VSharp.Runner
{
    public static class RunnerProgram
    {

        public static Assembly? TryLoadAssembly(FileInfo assemblyPath)
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

        public static Type? ResolveType(Assembly assembly, string? classArgumentValue)
        {
            if (classArgumentValue == null)
            {
                Console.Error.WriteLine("Specified class can not be null");
                return null;
            }

            var specificClass =
                assembly.GetType(classArgumentValue) ??
                assembly.GetTypes()
                    .Where(t => (t.FullName ?? t.Name).Contains(classArgumentValue))
                    .MinBy(t => t.FullName?.Length ?? t.Name.Length);
            if (specificClass == null)
            {
                Console.Error.WriteLine("I did not found type you specified {0} in assembly {1}", classArgumentValue,
                    assembly.Location);
                return null;
            }

            return specificClass;
        }

        public static MethodBase? ResolveMethod(Assembly assembly, string? methodArgumentValue)
        {
            if (methodArgumentValue == null)
            {
                Console.Error.WriteLine("Specified method can not be null");
                return null;
            }

            MethodBase? method = null;
            if (Int32.TryParse(methodArgumentValue, out var metadataToken))
            {
                foreach (var module in assembly.GetModules())
                {
                    try
                    {
                        method = module.ResolveMethod(metadataToken);
                        if (method != null) break;
                    }
                    catch
                    {
                        // ignored
                    }
                }

                if (method == null)
                {
                    Console.Error.WriteLine("I did not found method you specified by token {0} in assembly {1}",
                        metadataToken, assembly.Location);
                    return null;
                }
            }
            else
            {
                foreach (var type in assembly.GetTypes())
                {
                    try
                    {
                        var t = methodArgumentValue.Split('.');
                        var className = t.Length == 1 ? "" : t[t.Length - 2];
                        var methodName = t.Last(); 
                        //method = type.GetMethod(t.Last(), Reflection.allBindingFlags);
                        //method = type.GetMethod(methodArgumentValue, Reflection.allBindingFlags);
                        var x = type.GetMethods(Reflection.allBindingFlags);
                        //foreach (var m in x)
                        //{
                        //    Console.WriteLine($"{type.FullName}.{m.Name}");
                        //}
                        method ??= x
                            .Where(m => type.FullName.Split('.').Last().Contains(className) && m.Name.Contains(methodName))
                            .MinBy(m => m.Name.Length);
                        if (method != null) 
                            break;
                    }
                    catch (Exception)
                    {
                        // ignored
                    }
                }

                if (method == null)
                {
                    Console.Error.WriteLine("I did not found method you specified by name {0} in assembly {1}",
                        methodArgumentValue, assembly.Location);
                    return null;
                }
            }

            return method;
        }

        private static void PostProcess(Statistics statistics)
        {
            statistics.GenerateReport(Console.Out);
        }

        private static void EntryPointHandler(
            FileInfo assemblyPath,
            string[] args,
            int timeout,
            int solverTimeout,
            DirectoryInfo output,
            bool unknownArgs,
            bool renderTests,
            bool runTests,
            SearchStrategy strat,
            Verbosity verbosity,
            uint recursionThreshold,
            ExplorationMode explorationMode,
            string pathToModel)
        {
            var assembly = TryLoadAssembly(assemblyPath);
            var inputArgs = unknownArgs ? null : args;
            var options =
                new VSharpOptions(
                    timeout: timeout,
                    solverTimeout: solverTimeout,
                    outputDirectory: output.FullName,
                    renderTests: renderTests,
                    searchStrategy: strat,
                    verbosity: verbosity,
                    recursionThreshold: recursionThreshold,
                    explorationMode: explorationMode,
                    pathToModel: pathToModel);

            if (assembly == null) return;

            Statistics statistics;
            if (runTests)
                TestGenerator.CoverAndRun(assembly, inputArgs, out statistics, options);
            else
                statistics = TestGenerator.Cover(assembly, inputArgs, options);
            PostProcess(statistics);
        }

        private static void AllPublicMethodsHandler(
            FileInfo assemblyPath,
            int timeout,
            int solverTimeout,
            DirectoryInfo output,
            bool renderTests,
            bool runTests,
            SearchStrategy strat,
            Verbosity verbosity,
            uint recursionThreshold,
            ExplorationMode explorationMode,
            string pathToModel)
        {
            var assembly = TryLoadAssembly(assemblyPath);
            var options =
                new VSharpOptions(
                    timeout: timeout,
                    solverTimeout: solverTimeout,
                    outputDirectory: output.FullName,
                    renderTests: renderTests,
                    searchStrategy: strat,
                    verbosity: verbosity,
                    recursionThreshold: recursionThreshold,
                    explorationMode: explorationMode,
                    pathToModel: pathToModel);

            if (assembly == null) return;

            Statistics statistics;
            if (runTests)
                TestGenerator.CoverAndRun(assembly, out statistics, options);
            else
                statistics = TestGenerator.Cover(assembly, options);
            PostProcess(statistics);
        }

        private static void PublicMethodsOfTypeHandler(
            FileInfo assemblyPath,
            string typeName,
            int timeout,
            int solverTimeout,
            DirectoryInfo output,
            bool renderTests,
            bool runTests,
            SearchStrategy strat,
            Verbosity verbosity,
            uint recursionThreshold,
            ExplorationMode explorationMode,
            string pathToModel)
        {
            var assembly = TryLoadAssembly(assemblyPath);
            if (assembly == null) return;

            var type = assembly.ResolveType(typeName);
            if (type == null)
            {
                Console.Error.WriteLine($"Cannot find type with name {typeName} in assembly {assembly.Location}");
                return;
            }

            var options =
                new VSharpOptions(
                    timeout: timeout,
                    solverTimeout: solverTimeout,
                    outputDirectory: output.FullName,
                    renderTests: renderTests,
                    searchStrategy: strat,
                    verbosity: verbosity,
                    recursionThreshold: recursionThreshold,
                    explorationMode: explorationMode,
                    pathToModel: pathToModel);

            Statistics statistics;
            if (runTests)
                TestGenerator.CoverAndRun(type, out statistics, options);
            else
                statistics = TestGenerator.Cover(type, options);
            PostProcess(statistics);
        }

        private static void SpecificMethodHandler(
            FileInfo assemblyPath,
            string methodName,
            int timeout,
            int solverTimeout,
            DirectoryInfo output,
            bool renderTests,
            bool runTests,
            bool checkCoverage,
            SearchStrategy strat,
            Verbosity verbosity,
            uint recursionThreshold,
            ExplorationMode explorationMode,
            string pathToModel)
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

            var options =
                new VSharpOptions(
                    timeout: timeout,
                    solverTimeout: solverTimeout,
                    outputDirectory: output.FullName,
                    renderTests: renderTests,
                    searchStrategy: strat,
                    verbosity: verbosity,
                    recursionThreshold: recursionThreshold,
                    explorationMode: explorationMode,
                    pathToModel: pathToModel);

            Statistics statistics;
            if (runTests || checkCoverage)
                TestGenerator.CoverAndRun(method, out statistics, options, checkCoverage);
            else
                statistics = TestGenerator.Cover(method, options);
            PostProcess(statistics);
        }

        private static void NamespaceHandler(
            FileInfo assemblyPath,
            string namespaceName,
            int timeout,
            int solverTimeout,
            DirectoryInfo output,
            bool renderTests,
            bool runTests,
            SearchStrategy strat,
            Verbosity verbosity,
            uint recursionThreshold,
            ExplorationMode explorationMode,
            string pathToModel)
        {
            var assembly = TryLoadAssembly(assemblyPath);
            if (assembly == null) return;

            var namespaceTypes = assembly.ResolveNamespace(namespaceName).ToArray();
            if (namespaceTypes.Length == 0)
            {
                Console.Error.WriteLine($"Cannot find any types in namespace {namespaceName}");
                return;
            }

            var options =
                new VSharpOptions(
                    timeout: timeout,
                    solverTimeout: solverTimeout,
                    outputDirectory: output.FullName,
                    renderTests: renderTests,
                    searchStrategy: strat,
                    verbosity: verbosity,
                    recursionThreshold: recursionThreshold,
                    explorationMode: explorationMode,
                    pathToModel: pathToModel);

            Statistics statistics;
            if (runTests)
                TestGenerator.CoverAndRun(namespaceTypes, out statistics, options);
            else
                statistics = TestGenerator.Cover(namespaceTypes, options);
            PostProcess(statistics);
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
            var pathToModelOption = new Option<string>(
                aliases: new[] { "--model", "-m" },
                () => defaultOptions.PathToModel,
                "Path to ONNX file with model for AI searcher.");
            var solverTimeoutOption = new Option<int>(
                aliases: new[] { "--solver-timeout", "-st" },
                () => -1,
                "Timeout for SMT solver in seconds. Negative value means no timeout.");
            var outputOption = new Option<DirectoryInfo>(
                aliases: new[] { "--output", "-o" },
                () => new DirectoryInfo(Directory.GetCurrentDirectory()),
                "Path where unit tests will be generated");
            var renderTestsOption = new Option<bool>(
                "--render-tests",
                description: "Render generated tests as NUnit project to specified output path");
            var runTestsOption =
                new Option<bool>("--run-tests", description: "Reproduce generated tests");
            var checkCoverageOption = new Option<bool>(
                "--check-coverage",
                description: "After test generation starts coverage checking tool and shows coverage");
            var singleFileOption = new Option<bool>("--single-file") { IsHidden = true };
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

            var rootCommand = new RootCommand("Symbolic execution engine for .NET");
            rootCommand.AddGlobalOption(timeoutOption);
            rootCommand.AddGlobalOption(solverTimeoutOption);
            rootCommand.AddGlobalOption(outputOption);
            rootCommand.AddGlobalOption(renderTestsOption);
            rootCommand.AddGlobalOption(runTestsOption);
            rootCommand.AddGlobalOption(searchStrategyOption);
            rootCommand.AddGlobalOption(verbosityOption);
            rootCommand.AddGlobalOption(recursionThresholdOption);
            rootCommand.AddGlobalOption(explorationModeOption);
            rootCommand.AddGlobalOption(pathToModelOption);

            var entryPointCommand =
                new Command("--entry-point", "Generate test coverage from the entry point of assembly (assembly must contain Main method)");
            var argsArgument =
                new Argument<string[]>("args", description: "Command line arguments");
            var unknownArgsOption = new Option<bool>(
                "--unknown-args",
                description: "Force engine to generate various input console arguments");
            rootCommand.AddCommand(entryPointCommand);
            entryPointCommand.AddArgument(assemblyPathArgument);
            entryPointCommand.AddArgument(argsArgument);
            entryPointCommand.AddOption(unknownArgsOption);
            entryPointCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                var output = parseResult.GetValueForOption(outputOption);
                Debug.Assert(output is not null);
                EntryPointHandler(
                    parseResult.GetValueForArgument(assemblyPathArgument),
                    parseResult.GetValueForArgument(argsArgument),
                    parseResult.GetValueForOption(timeoutOption),
                    parseResult.GetValueForOption(solverTimeoutOption),
                    output,
                    parseResult.GetValueForOption(unknownArgsOption),
                    parseResult.GetValueForOption(renderTestsOption),
                    parseResult.GetValueForOption(runTestsOption),
                    parseResult.GetValueForOption(searchStrategyOption),
                    parseResult.GetValueForOption(verbosityOption),
                    parseResult.GetValueForOption(recursionThresholdOption),
                    parseResult.GetValueForOption(explorationModeOption),
                parseResult.GetValueForOption(pathToModelOption)
                );
            });

            var allPublicMethodsCommand =
                new Command("--all-public-methods", "Generate unit tests for all public methods of all public classes of assembly");
            rootCommand.AddCommand(allPublicMethodsCommand);
            allPublicMethodsCommand.AddArgument(assemblyPathArgument);
            allPublicMethodsCommand.AddOption(singleFileOption);

            allPublicMethodsCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                var output = parseResult.GetValueForOption(outputOption);
                Debug.Assert(output is not null);
                AllPublicMethodsHandler(
                    parseResult.GetValueForArgument(assemblyPathArgument),
                    parseResult.GetValueForOption(timeoutOption),
                    parseResult.GetValueForOption(solverTimeoutOption),
                    output,
                    parseResult.GetValueForOption(renderTestsOption),
                    parseResult.GetValueForOption(runTestsOption),
                    parseResult.GetValueForOption(searchStrategyOption),
                    parseResult.GetValueForOption(verbosityOption),
                    parseResult.GetValueForOption(recursionThresholdOption),
                    parseResult.GetValueForOption(explorationModeOption),
                    parseResult.GetValueForOption(pathToModelOption)
                );
            });

            var publicMethodsOfClassCommand =
                new Command("--type", "Generate unit tests for all public methods of specified class");
            rootCommand.AddCommand(publicMethodsOfClassCommand);
            var classArgument = new Argument<string>("class-name");
            publicMethodsOfClassCommand.AddArgument(classArgument);
            publicMethodsOfClassCommand.AddArgument(assemblyPathArgument);

            publicMethodsOfClassCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                var output = parseResult.GetValueForOption(outputOption);
                Debug.Assert(output is not null);
                PublicMethodsOfTypeHandler(
                    parseResult.GetValueForArgument(assemblyPathArgument),
                    parseResult.GetValueForArgument(classArgument),
                    parseResult.GetValueForOption(timeoutOption),
                    parseResult.GetValueForOption(solverTimeoutOption),
                    output,
                    parseResult.GetValueForOption(renderTestsOption),
                    parseResult.GetValueForOption(runTestsOption),
                    parseResult.GetValueForOption(searchStrategyOption),
                    parseResult.GetValueForOption(verbosityOption),
                    parseResult.GetValueForOption(recursionThresholdOption),
                    parseResult.GetValueForOption(explorationModeOption),
                    parseResult.GetValueForOption(pathToModelOption)
                );
            });

            var specificMethodCommand =
                new Command("--method", "Try to resolve and generate unit test coverage for the specified method");
            rootCommand.AddCommand(specificMethodCommand);
            var methodArgument = new Argument<string>("method-name");
            specificMethodCommand.AddArgument(methodArgument);
            specificMethodCommand.AddArgument(assemblyPathArgument);
            specificMethodCommand.AddOption(checkCoverageOption);

            specificMethodCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                var pathToModel = parseResult.GetValueForOption(pathToModelOption);
                var output = parseResult.GetValueForOption(outputOption);
                Debug.Assert(output is not null);
                SpecificMethodHandler(
                    parseResult.GetValueForArgument(assemblyPathArgument),
                    parseResult.GetValueForArgument(methodArgument),
                    parseResult.GetValueForOption(timeoutOption),
                    parseResult.GetValueForOption(solverTimeoutOption),
                    output,
                    parseResult.GetValueForOption(renderTestsOption),
                    parseResult.GetValueForOption(runTestsOption),
                    parseResult.GetValueForOption(checkCoverageOption),
                    parseResult.GetValueForOption(searchStrategyOption),
                    parseResult.GetValueForOption(verbosityOption),
                    parseResult.GetValueForOption(recursionThresholdOption),
                    parseResult.GetValueForOption(explorationModeOption),
                    pathToModel
                );
            });

            var namespaceCommand =
                new Command("--namespace", "Try to resolve and generate unit test coverage for all public methods of specified namespace");
            rootCommand.AddCommand(namespaceCommand);
            var namespaceArgument = new Argument<string>("namespace-name");
            namespaceCommand.AddArgument(namespaceArgument);
            namespaceCommand.AddArgument(assemblyPathArgument);

            namespaceCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                var output = parseResult.GetValueForOption(outputOption);
                var pathToModel = parseResult.GetValueForOption(pathToModelOption);
                Debug.Assert(output is not null);
                NamespaceHandler(
                    parseResult.GetValueForArgument(assemblyPathArgument),
                    parseResult.GetValueForArgument(namespaceArgument),
                    parseResult.GetValueForOption(timeoutOption),
                    parseResult.GetValueForOption(solverTimeoutOption),
                    output,
                    parseResult.GetValueForOption(renderTestsOption),
                    parseResult.GetValueForOption(runTestsOption),
                    parseResult.GetValueForOption(searchStrategyOption),
                    parseResult.GetValueForOption(verbosityOption),
                    parseResult.GetValueForOption(recursionThresholdOption),
                    parseResult.GetValueForOption(explorationModeOption),
                    pathToModel
                );
            });

            return rootCommand.Invoke(args);
        }
    }
}
