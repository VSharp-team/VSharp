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
            var assemblyPathArgument =
                new Argument<FileInfo>("assembly-path", description: "Path to the target assembly");
            var timeoutOption =
                new Option<int>(aliases: new[] { "--timeout", "-t" },
                    () => -1,
                    "Time for test generation in seconds. Negative value means no timeout.");
            var solverTimeoutOption =
                new Option<int>(aliases: new[] { "--solver-timeout", "-st" },
                    () => -1,
                    "Timeout for SMT solver in seconds. Negative value means no timeout.");
            var outputOption =
                new Option<DirectoryInfo>(aliases: new[] { "--output", "-o" },
                () => new DirectoryInfo(Directory.GetCurrentDirectory()),
                "Path where unit tests will be generated");
            var concreteArguments =
                new Argument<string[]>("args", description: "Command line arguments");
            var unknownArgsOption =
                new Option("--unknown-args", description: "Force engine to generate various input console arguments");
            var renderTestsOption =
                new Option("--render-tests", description: "Render generated tests as NUnit project to specified output path");
            var singleFileOption =
                new Option("--single-file") { IsHidden = true };
            var searchStrategyOption =
                new Option<SearchStrategy>(aliases: new[] { "--strat" },
                    () => TestGenerator.DefaultSearchStrategy,
                    "Strategy which symbolic virtual machine uses for branch selection.");
            var verbosityOption =
                new Option<Verbosity>(aliases: new[] { "--verbosity", "-v" },
                    () => TestGenerator.DefaultVerbosity,
                    "Determines which messages are displayed in output.");

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
            entryPointCommand.AddGlobalOption(searchStrategyOption);
            entryPointCommand.AddGlobalOption(verbosityOption);
            var allPublicMethodsCommand =
                new Command("--all-public-methods", "Generate unit tests for all public methods of all public classes of assembly");
            rootCommand.AddCommand(allPublicMethodsCommand);
            allPublicMethodsCommand.AddArgument(assemblyPathArgument);
            allPublicMethodsCommand.AddGlobalOption(timeoutOption);
            allPublicMethodsCommand.AddGlobalOption(solverTimeoutOption);
            allPublicMethodsCommand.AddGlobalOption(outputOption);
            allPublicMethodsCommand.AddGlobalOption(renderTestsOption);
            allPublicMethodsCommand.AddOption(singleFileOption);
            allPublicMethodsCommand.AddGlobalOption(searchStrategyOption);
            allPublicMethodsCommand.AddGlobalOption(verbosityOption);
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
            publicMethodsOfClassCommand.AddGlobalOption(searchStrategyOption);
            publicMethodsOfClassCommand.AddGlobalOption(verbosityOption);
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
            specificMethodCommand.AddGlobalOption(searchStrategyOption);
            specificMethodCommand.AddGlobalOption(verbosityOption);
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
            namespaceCommand.AddGlobalOption(searchStrategyOption);
            namespaceCommand.AddGlobalOption(verbosityOption);

            rootCommand.Description = "Symbolic execution engine for .NET";

            entryPointCommand.Handler = CommandHandler.Create<FileInfo, string[], int, int, DirectoryInfo, bool, bool, SearchStrategy, Verbosity>((assemblyPath, args, timeout, solverTimeout, output, unknownArgs, renderTests, strat, verbosity) =>
            {
                var assembly = TryLoadAssembly(assemblyPath);
                var inputArgs = unknownArgs ? null : args;
                if (assembly != null)
                    PostProcess(TestGenerator.Cover(assembly, inputArgs, timeout, solverTimeout, output.FullName, renderTests, strat, verbosity));
            });

            allPublicMethodsCommand.Handler = CommandHandler.Create<FileInfo, int, int, DirectoryInfo, bool, bool, SearchStrategy, Verbosity>((assemblyPath, timeout, solverTimeout, output, renderTests, singleFile, strat, verbosity) =>
            {
                var assembly = TryLoadAssembly(assemblyPath);
                if (assembly != null)
                    PostProcess(TestGenerator.Cover(assembly, timeout, solverTimeout, output.FullName, renderTests, singleFile, strat, verbosity));
            });

            publicMethodsOfClassCommand.Handler = CommandHandler.Create<string, FileInfo, int, int, DirectoryInfo, bool, SearchStrategy, Verbosity>((className, assemblyPath, timeout, solverTimeout, output, renderTests, strat, verbosity) =>
            {
                var assembly = TryLoadAssembly(assemblyPath);
                if (assembly == null) return;

                var type = assembly.ResolveType(className);
                if (type == null)
                {
                    Console.Error.WriteLine($"Cannot find type with name {className} in assembly {assembly.Location}");
                    return;
                }

                PostProcess(TestGenerator.Cover(type, timeout, solverTimeout, output.FullName, renderTests, strat, verbosity));
            });

            specificMethodCommand.Handler = CommandHandler.Create<string, FileInfo, int, int, DirectoryInfo, bool, SearchStrategy, Verbosity>((methodName, assemblyPath, timeout, solverTimeout, output, renderTests, strat, verbosity) =>
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

                PostProcess(TestGenerator.Cover(method, timeout, solverTimeout, output.FullName, renderTests, strat, verbosity));
            });

            namespaceCommand.Handler = CommandHandler.Create<string, FileInfo, int, int, DirectoryInfo, bool, SearchStrategy, Verbosity>((namespaceName, assemblyPath, timeout, solverTimeout, output, renderTests, strat, verbosity) =>
            {
                var assembly = TryLoadAssembly(assemblyPath);
                if (assembly == null) return;

                var namespaceTypes = assembly.ResolveNamespace(namespaceName).ToArray();
                if (namespaceTypes.Length == 0)
                {
                    Console.Error.WriteLine($"Cannot find any types in namespace {namespaceName}");
                    return;
                }

                PostProcess(TestGenerator.Cover(namespaceTypes, timeout, solverTimeout, output.FullName, renderTests, strat, verbosity));
            });

            return rootCommand.Invoke(args);
        }
    }
}
