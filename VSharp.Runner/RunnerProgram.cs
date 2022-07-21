﻿using System;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;
using System.Reflection;

namespace VSharp.Runner
{
    public static class RunnerProgram
    {

        private static Assembly ResolveAssembly(FileInfo assemblyPath)
        {
            try
            {
                return Assembly.LoadFrom(assemblyPath.FullName);
            }
            catch (Exception)
            {
                Console.Error.WriteLine("I did not found assembly {0}", assemblyPath.FullName);
                return null;
            }
        }

        private static Type ResolveType(Assembly assembly, string classArgumentValue)
        {
            if (classArgumentValue == null)
            {
                Console.Error.WriteLine("Specified class can not be null");
                return null;
            }

            var specificClass = assembly.GetType(classArgumentValue);
            if (specificClass == null)
            {
                Console.Error.WriteLine("I did not found type you specified {0} in assembly {1}", classArgumentValue,
                    assembly.Location);
                return null;
            }

            return specificClass;
        }

        private static MethodBase ResolveMethod(Assembly assembly, string methodArgumentValue)
        {
            if (methodArgumentValue == null)
            {
                Console.Error.WriteLine("Specified method can not be null");
                return null;
            }

            MethodBase method = null;
            int metadataToken;
            if (Int32.TryParse(methodArgumentValue, out metadataToken))
            {
                foreach (var module in assembly.GetModules())
                {
                    try
                    {
                        var nullOrMethod = module.ResolveMethod(metadataToken);
                        if (nullOrMethod == null) continue;
                        method = nullOrMethod;
                        break;
                    }
                    catch (Exception)
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
                        var nullOrMethod = type.GetMethod(methodArgumentValue, Reflection.allBindingFlags);
                        if (nullOrMethod == null) continue;
                        method = nullOrMethod;
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

        public static int Main(string[] args)
        {
            var assemblyPathArgument =
                new Argument<FileInfo>("assembly-path", description: "Path to the target assembly");
            var timeoutOption =
                new Option<int>(aliases: new[] { "--timeout", "-t" },
                    () => -1,
                    "Time for test generation. Negative values mean no timeout.");
            var outputOption =
                new Option<DirectoryInfo>(aliases: new[] { "--output", "-o" },
                () => new DirectoryInfo(Directory.GetCurrentDirectory()),
                "Path where unit tests will be generated");
            var concreteArguments =
                new Argument<string[]>("args", description: "Command line arguments");
            var unknownArgsOption =
                new Option("--unknown-args", description: "Force engine to generate various input console arguments");

            var constraintIndependenceOption = new Option<bool>(
                "--c-independence",
                description: "Maintain independent constraint sets (constraint independence optimization). True by default",
                getDefaultValue: () => true
            );
            var incrementalityOption = new Option<bool>(
                "--incrementality",
                description: "Enable SMT solver incremental mode. False by default",
                getDefaultValue: () => false
            );

            var rootCommand = new RootCommand();

            var entryPointCommand =
                new Command("--entry-point", "Generate test coverage from the entry point of assembly (assembly must contain Main method)");
            rootCommand.AddCommand(entryPointCommand);
            entryPointCommand.AddArgument(assemblyPathArgument);
            entryPointCommand.AddArgument(concreteArguments);
            entryPointCommand.AddGlobalOption(timeoutOption);
            entryPointCommand.AddGlobalOption(outputOption);
            entryPointCommand.AddOption(unknownArgsOption);
            entryPointCommand.AddOption(constraintIndependenceOption);
            entryPointCommand.AddOption(incrementalityOption);
            
            var allPublicMethodsCommand =
                new Command("--all-public-methods", "Generate unit tests for all public methods of all public classes of assembly");
            rootCommand.AddCommand(allPublicMethodsCommand);
            allPublicMethodsCommand.AddArgument(assemblyPathArgument);
            allPublicMethodsCommand.AddGlobalOption(timeoutOption);
            allPublicMethodsCommand.AddGlobalOption(outputOption);
            allPublicMethodsCommand.AddOption(constraintIndependenceOption);
            allPublicMethodsCommand.AddOption(incrementalityOption);
            
            var publicMethodsOfClassCommand =
                new Command("--public-methods-of-class", "Generate unit tests for all public methods of specified class");
            rootCommand.AddCommand(publicMethodsOfClassCommand);
            var classArgument = new Argument<string>("class-name");
            publicMethodsOfClassCommand.AddArgument(classArgument);
            publicMethodsOfClassCommand.AddArgument(assemblyPathArgument);
            publicMethodsOfClassCommand.AddGlobalOption(timeoutOption);
            publicMethodsOfClassCommand.AddGlobalOption(outputOption);
            publicMethodsOfClassCommand.AddOption(constraintIndependenceOption);
            publicMethodsOfClassCommand.AddOption(incrementalityOption);

            var specificMethodCommand =
                new Command("--method", "Try to resolve and generate unit test coverage for the specified method");
            rootCommand.AddCommand(specificMethodCommand);
            var methodArgument = new Argument<string>("method-name");
            specificMethodCommand.AddArgument(methodArgument);
            specificMethodCommand.AddArgument(assemblyPathArgument);
            specificMethodCommand.AddGlobalOption(timeoutOption);
            specificMethodCommand.AddGlobalOption(outputOption);
            specificMethodCommand.AddOption(constraintIndependenceOption);
            specificMethodCommand.AddOption(incrementalityOption);

            rootCommand.Description = "Symbolic execution engine for .NET";

            entryPointCommand.Handler = CommandHandler.Create<FileInfo, string[], DirectoryInfo, bool, bool, bool>
            (
                (assemblyPath, args, output, unknownArgs, cIndependence, incrementality) =>
                {
                    var assembly = ResolveAssembly(assemblyPath);
                    if (unknownArgs)
                        args = null;
                    if (assembly != null)
                    {
                        var options = new CoverOptions(
                            OutputDirectory: output.FullName,
                            IsConstraintIndependenceEnabled: cIndependence,
                            IsSolverIncrementalityEnabled: incrementality
                        );
                        
                        PostProcess(TestGenerator.Cover(assembly, args, options));
                    }
                }
            );
            allPublicMethodsCommand.Handler = CommandHandler.Create<FileInfo, DirectoryInfo, bool, bool>
            (
                (assemblyPath, output, cIndependence, incrementality) =>
                {
                    var assembly = ResolveAssembly(assemblyPath);
                    if (assembly != null)
                    {
                        var options = new CoverOptions(
                            OutputDirectory: output.FullName,
                            IsConstraintIndependenceEnabled: cIndependence,
                            IsSolverIncrementalityEnabled: incrementality
                        );
                        
                        PostProcess(TestGenerator.Cover(assembly, options));
                    }
                }
            );
            publicMethodsOfClassCommand.Handler = CommandHandler.Create<string, FileInfo, DirectoryInfo, bool, bool>
            (
                (className, assemblyPath, output, cIndependence, incrementality) =>
                {
                    var assembly = ResolveAssembly(assemblyPath);
                    if (assembly != null)
                    {
                        var type = ResolveType(assembly, className);
                        if (type != null)
                        {
                            var options = new CoverOptions(
                                OutputDirectory: output.FullName,
                                IsConstraintIndependenceEnabled: cIndependence,
                                IsSolverIncrementalityEnabled: incrementality
                            );
                            
                            PostProcess(TestGenerator.Cover(type, options));
                        }
                    }
                }
            );
            specificMethodCommand.Handler = CommandHandler.Create<string, FileInfo, DirectoryInfo, bool, bool>
            (
                (methodName, assemblyPath, output, cIndependence, incrementality) =>
                {
                    var assembly = ResolveAssembly(assemblyPath);
                    if (assembly != null)
                    {
                        var method = ResolveMethod(assembly, methodName);
                        if (method != null)
                        {
                            var options = new CoverOptions(
                                OutputDirectory: output.FullName,
                                IsConstraintIndependenceEnabled: cIndependence,
                                IsSolverIncrementalityEnabled: incrementality
                            );
                            
                            PostProcess(TestGenerator.Cover(method, options));
                        }
                    }
                }
            );

            return rootCommand.InvokeAsync(args).Result;
        }
    }
}
