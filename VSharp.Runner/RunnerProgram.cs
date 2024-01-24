#nullable enable
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using VSharp.CSharpUtils;
using VSharp.Explorer;

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

        private static void EntryPointHandler(
            FileInfo assemblyPath,
            string[]? args,
            int timeout,
            int solverTimeout,
            DirectoryInfo output,
            bool renderTests,
            bool runTests,
            SearchStrategy strat,
            Verbosity verbosity,
            uint recursionThreshold,
            ExplorationMode explorationMode)
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
                    explorationMode: explorationMode);

            if (assembly == null) return;

            var configuration = new EntryPointConfiguration(args);
            Statistics statistics;
            if (runTests)
                TestGenerator.CoverAndRun(assembly, configuration, out statistics, options);
            else
                statistics = TestGenerator.Cover(assembly, configuration, options);
            PostProcess(statistics);
        }

        private static DirectoryInfo FindRootPath(FileInfo assemblyPath, string? assemblyName)
        {
            DirectoryInfo? rootPath = null;
            var currentPath = assemblyPath.Directory;
            while (rootPath == null && currentPath != null)
            {
                IEnumerable<FileSystemInfo> files;
                if (assemblyName == null)
                    files = currentPath.EnumerateDirectories("Controllers");
                else
                    files = currentPath.EnumerateFiles($"{assemblyName}.csproj");
                if (files.Any())
                    rootPath = currentPath;
                currentPath = currentPath.Parent;
            }

            if (rootPath == null)
                throw new ArgumentException("Cannot find content root path, please provide it via option '--root-path'");

            return rootPath;
        }

        private static void WebAppHandler(
            FileInfo assemblyPath,
            string[]? args,
            string? environmentName,
            DirectoryInfo? contentRootPath,
            string? applicationName,
            int timeout,
            int solverTimeout,
            DirectoryInfo output,
            bool renderTests,
            bool runTests,
            SearchStrategy strat,
            Verbosity verbosity,
            uint recursionThreshold,
            ExplorationMode explorationMode)
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
                    explorationMode: explorationMode);

            if (assembly == null) return;

            environmentName ??= "Production";
            var assemblyName = assembly.GetName().Name;
            applicationName ??= assemblyName;
            if (applicationName == null)
                throw new ArgumentException("Cannot find application name, please provide it via option '--app-name'");
            contentRootPath ??= FindRootPath(assemblyPath, assemblyName);

            Debug.Assert(environmentName != null && applicationName != null && contentRootPath != null);
            var webConfiguration = new WebConfiguration(args, environmentName, contentRootPath, applicationName);

            Statistics statistics;
            if (runTests)
                TestGenerator.CoverAndRun(assembly, webConfiguration, out statistics, options);
            else
                statistics = TestGenerator.Cover(assembly, webConfiguration, options);
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
            ExplorationMode explorationMode)
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
                    explorationMode: explorationMode);

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
            ExplorationMode explorationMode)
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
                    explorationMode: explorationMode);

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
            ExplorationMode explorationMode)
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
                    explorationMode: explorationMode);

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
            ExplorationMode explorationMode)
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
                    explorationMode: explorationMode);

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

            var entryPointCommand =
                new Command("--entry-point", "Generate test coverage from the entry point of assembly (assembly must contain Main method)");
            var argsOption = new Option<string[]?>(
                "--args",
                description: "Command line arguments");
            rootCommand.AddCommand(entryPointCommand);
            entryPointCommand.AddArgument(assemblyPathArgument);
            entryPointCommand.AddOption(argsOption);
            entryPointCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                var output = parseResult.GetValueForOption(outputOption);
                Debug.Assert(output is not null);
                EntryPointHandler(
                    parseResult.GetValueForArgument(assemblyPathArgument),
                    parseResult.GetValueForOption(argsOption),
                    parseResult.GetValueForOption(timeoutOption),
                    parseResult.GetValueForOption(solverTimeoutOption),
                    output,
                    parseResult.GetValueForOption(renderTestsOption),
                    parseResult.GetValueForOption(runTestsOption),
                    parseResult.GetValueForOption(searchStrategyOption),
                    parseResult.GetValueForOption(verbosityOption),
                    parseResult.GetValueForOption(recursionThresholdOption),
                    parseResult.GetValueForOption(explorationModeOption)
                );
            });

            var webCommand =
                new Command("--web", "Generate integration tests for web application");
            rootCommand.AddCommand(webCommand);
            webCommand.AddArgument(assemblyPathArgument);
            webCommand.AddOption(argsOption);
            var environmentNameOption = new Option<string>(
                "--e-name",
                description: "Name of environment in which web application will be explored");
            var rootPathOption = new Option<DirectoryInfo?>(
                "--root-path",
                description: "Root path of web application");
            var applicationNameOption = new Option<string>(
                "--app-name",
                description: "Web application name");
            webCommand.SetHandler(context =>
            {
                var parseResult = context.ParseResult;
                var output = parseResult.GetValueForOption(outputOption);
                Debug.Assert(output is not null);
                WebAppHandler(
                    parseResult.GetValueForArgument(assemblyPathArgument),
                    parseResult.GetValueForOption(argsOption),
                    parseResult.GetValueForOption(environmentNameOption),
                    parseResult.GetValueForOption(rootPathOption),
                    parseResult.GetValueForOption(applicationNameOption),
                    parseResult.GetValueForOption(timeoutOption),
                    parseResult.GetValueForOption(solverTimeoutOption),
                    output,
                    parseResult.GetValueForOption(renderTestsOption),
                    parseResult.GetValueForOption(runTestsOption),
                    parseResult.GetValueForOption(searchStrategyOption),
                    parseResult.GetValueForOption(verbosityOption),
                    parseResult.GetValueForOption(recursionThresholdOption),
                    parseResult.GetValueForOption(explorationModeOption)
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
                    parseResult.GetValueForOption(explorationModeOption)
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
                    parseResult.GetValueForOption(explorationModeOption)
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
                    parseResult.GetValueForOption(explorationModeOption)
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
                    parseResult.GetValueForOption(explorationModeOption)
                );
            });

            return rootCommand.Invoke(args);
        }
    }
}
