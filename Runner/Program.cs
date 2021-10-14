using System;
using System.Collections.Generic;
using System.CommandLine;
using System.CommandLine.Invocation;
using System.IO;
using System.Reflection;
using VSharp;
using VSharp.Interpreter.IL;

namespace Runner
{
    class Program
    {

        private enum CoverageZone
        {
            EntryPoint,
            PublicMethodsOfClass,
            AllPublicMethods,
            SpecificMethod
        }

        private static void StartExploration(List<MethodBase> methods, string resultsFolder)
        {
            var maxBound = 15u;
            var options =
                new SiliOptions(explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchMode.DFSMode),
                    executionMode.SymbolicMode, maxBound);
            SILI explorer = new SILI(options);
            UnitTests unitTests = new UnitTests(resultsFolder);
            foreach (var method in methods)
            {
                explorer.InterpretIsolated(method, unitTests.GenerateTest, unitTests.GenerateError, _ => { }, e => throw e);
            }
            explorer.GenerateReport(Console.Out);
            Console.WriteLine("Test results written to {0}", resultsFolder);
            unitTests.WriteReport(explorer.GenerateReport);
        }

        private static void Run(FileInfo assemblyPath, DirectoryInfo resultsPath, CoverageZone zone, string classArgumentValue, string methodArgumentValue)
        {
            // TODO: unify with IntegrationTests #do
            // TODO: need statistics?
            Assembly assembly;
            try
            {
                assembly = Assembly.LoadFrom(assemblyPath.FullName);
            }
            catch (Exception)
            {
                Console.Error.WriteLine("I did not found assembly {0}", assemblyPath.FullName);
                return;
            }

            string resultsFolder = resultsPath.CreateSubdirectory("TestResults").FullName;
            switch (zone)
            {
                case CoverageZone.EntryPoint:
                {
                    var entryPoint = assembly.EntryPoint;
                    if (entryPoint == null)
                    {
                        Console.Error.WriteLine("I did not found entry point in assembly {0}", assembly.Location);
                        return;
                    }

                    List<MethodBase> methods = new List<MethodBase> { assembly.EntryPoint };
                    StartExploration(methods, resultsFolder);
                    break;
                }
                case CoverageZone.AllPublicMethods:
                {
                    BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.DeclaredOnly;
                    List<MethodBase> methods = new List<MethodBase>();
                    foreach (var t in assembly.GetTypes())
                    {
                        if (t.IsPublic)
                        {
                            foreach (var m in t.GetMethods(bindingFlags))
                            {
                                methods.Add(m);
                            }
                        }
                    }

                    if (methods.Count == 0)
                    {
                        Console.Error.WriteLine("I did not found any public method in assembly {0}", assembly.Location);
                        return;
                    }

                    StartExploration(methods, resultsFolder);
                    break;
                }
                case CoverageZone.PublicMethodsOfClass:
                {
                    if (classArgumentValue == null)
                    {
                        Console.Error.WriteLine("Specified class can not be null");
                        return;
                    }

                    var specificClass = assembly.GetType(classArgumentValue);
                    if (specificClass == null)
                    {
                        Console.Error.WriteLine("I did not found type you specified {0} in assembly {1}", classArgumentValue, assembly.Location);
                        return;
                    }

                    BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public |
                                                BindingFlags.DeclaredOnly;
                    List<MethodBase> methods = new List<MethodBase>();
                    foreach (var m in specificClass.GetMethods(bindingFlags))
                    {
                        methods.Add(m);
                    }

                    if (methods.Count == 0)
                    {
                        Console.Error.WriteLine("I did not found any public method of class {0} in assembly {1}",
                            specificClass.FullName, assembly.Location);
                        return;
                    }

                    StartExploration(methods, resultsFolder);
                    break;
                }
                case CoverageZone.SpecificMethod:
                {
                    if (methodArgumentValue == null)
                    {
                        Console.Error.WriteLine("Specified method can not be null");
                        return;
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
                            Console.Error.WriteLine("I did not found method you specified by token {0} in assembly {1}", metadataToken, assembly.Location);
                            return;
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
                            Console.Error.WriteLine("I did not found method you specified by name {0} in assembly {1}", methodArgumentValue, assembly.Location);
                            return;
                        }
                    }

                    List<MethodBase> methods = new List<MethodBase> { method };
                    StartExploration(methods, resultsFolder);
                    break;
                }
            }
        }
        static int Main(string[] args)
        {
            var assemblyPathArgument =
                new Argument<FileInfo>("assembly-path", description: "Path to the target assembly");
            var outputOption =
                new Option<DirectoryInfo>(aliases: new[] { "--output", "-o" },
                () => new DirectoryInfo(Directory.GetCurrentDirectory()),
                "Path where unit tests will be generated");

            var rootCommand = new RootCommand();

            var entryPointCommand =
                new Command("--entry-point", "Generate test coverage from the entry point of assembly (assembly must contain Main method)");
            rootCommand.AddCommand(entryPointCommand);
            entryPointCommand.AddArgument(assemblyPathArgument);
            entryPointCommand.AddGlobalOption(outputOption);
            var allPublicMethodsCommand =
                new Command("--all-public-methods", "Generate unit tests for all public methods of all public classes of assembly");
            rootCommand.AddCommand(allPublicMethodsCommand);
            allPublicMethodsCommand.AddArgument(assemblyPathArgument);
            allPublicMethodsCommand.AddGlobalOption(outputOption);
            var publicMethodsOfClassCommand =
                new Command("--public-methods-of-class", "Generate unit tests for all public methods of specified class");
            rootCommand.AddCommand(publicMethodsOfClassCommand);
            var classArgument = new Argument<string>("class-name");
            publicMethodsOfClassCommand.AddArgument(classArgument);
            publicMethodsOfClassCommand.AddArgument(assemblyPathArgument);
            publicMethodsOfClassCommand.AddGlobalOption(outputOption);
            var specificMethodCommand =
                new Command("--method", "Try to resolve and generate unit test coverage for the specified method");
            rootCommand.AddCommand(specificMethodCommand);
            var methodArgument = new Argument<string>("method-name");
            specificMethodCommand.AddArgument(methodArgument);
            specificMethodCommand.AddArgument(assemblyPathArgument);
            specificMethodCommand.AddGlobalOption(outputOption);

            rootCommand.Description = "Symbolic execution engine for .NET";

            // NOTE: default behaviour
            CoverageZone zone = CoverageZone.AllPublicMethods;

            entryPointCommand.Handler = CommandHandler.Create<FileInfo, DirectoryInfo>((assemblyPath, output) =>
            {
                Run(assemblyPath, output, CoverageZone.EntryPoint, null, null);
            });
            allPublicMethodsCommand.Handler = CommandHandler.Create<FileInfo, DirectoryInfo>((assemblyPath, output) =>
            {
                Run(assemblyPath, output, CoverageZone.AllPublicMethods, null, null);
            });
            publicMethodsOfClassCommand.Handler = CommandHandler.Create<string, FileInfo, DirectoryInfo>((className, assemblyPath, output) =>
            {
                Run(assemblyPath, output, CoverageZone.PublicMethodsOfClass, className, null);
            });
            specificMethodCommand.Handler = CommandHandler.Create<string, FileInfo, DirectoryInfo>((methodName, assemblyPath, output) =>
            {
                Run(assemblyPath, output, CoverageZone.SpecificMethod, null, methodName);
            });

            rootCommand.Handler = CommandHandler.Create<FileInfo, DirectoryInfo>((assemblyPath, output) =>
            {
                Run(assemblyPath, output, zone, null, null);
            });

            return rootCommand.InvokeAsync(args).Result;
        }
    }
}
