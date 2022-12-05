using System.Diagnostics;
using System.Reflection;
using System.Runtime.Loader;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

// TODO: unify with TestRunner's assembly load method
internal class AssemblyResolver
{
    private static IEnumerable<string>? _extraAssemblyLoadDirs;

    public static void Configure(IEnumerable<string> extraAssemblyLoadDirs)
    {
        _extraAssemblyLoadDirs = extraAssemblyLoadDirs;
    }

    public static void AddResolve(AssemblyLoadContext? assemblyLoadContext = null)
    {
        if (assemblyLoadContext != null)
            assemblyLoadContext.Resolving += ResolveAssembly;
        else
            AppDomain.CurrentDomain.AssemblyResolve += ResolveAssembly;
    }

    public static void RemoveResolve(AssemblyLoadContext? assemblyLoadContext = null)
    {
        if (assemblyLoadContext != null)
            assemblyLoadContext.Resolving -= ResolveAssembly;
        else
            AppDomain.CurrentDomain.AssemblyResolve -= ResolveAssembly;
    }

    private static Assembly? ResolveAssembly(AssemblyLoadContext assemblyLoadContext, AssemblyName args)
    {
        return AssemblyLoadContextOnResolving(assemblyLoadContext.Assemblies, args);
    }

    private static Assembly? ResolveAssembly(object? _, ResolveEventArgs args)
    {
        var loadedAssemblies = AppDomain.CurrentDomain.GetAssemblies();
        var assemblyName = new AssemblyName(args.Name);
        return AssemblyLoadContextOnResolving(loadedAssemblies, assemblyName);
    }

    private static Assembly? AssemblyLoadContextOnResolving(IEnumerable<Assembly> loadedAssemblies, AssemblyName args)
    {
        var existingInstance =
            loadedAssemblies.FirstOrDefault(assembly => assembly.FullName == args.Name);
        if (existingInstance != null)
        {
            return existingInstance;
        }

        if (_extraAssemblyLoadDirs == null) return null;

        var argsAssemblyName = args.Name + ".dll";
        Debug.Assert(argsAssemblyName != null, "args.Name != null");
        foreach (var path in _extraAssemblyLoadDirs)
        {
            var assemblyPath = Path.Combine(path, argsAssemblyName);
            if (!File.Exists(assemblyPath))
                return null;
            // Old version
            // Assembly assembly = Assembly.LoadFrom(assemblyPath);
            // return assembly;

            // Max Arshinov's version
            using var stream = File.OpenRead(assemblyPath);
            var assembly = AssemblyLoadContext.Default.LoadFromStream(stream);
            return assembly;
        }

        return null;
    }
}

public static class Renderer
{
    private static void RunDotnet(ProcessStartInfo startInfo)
    {
        startInfo.FileName = "dotnet";
        RunProcess(startInfo);
    }

    private static void RunProcess(ProcessStartInfo startInfo)
    {
        startInfo.FileName = "dotnet";
        startInfo.RedirectStandardError = true;

        var pi = Process.Start(startInfo);
        pi?.WaitForExit();
    }

    private static void AddUnderTestProjectReference(FileInfo testProject, FileInfo testingProject)
    {
        if (testingProject.Extension == ".csproj")
        {
            RunDotnet(new ProcessStartInfo
            {
                WorkingDirectory = testProject.DirectoryName,
                Arguments = $"add reference {testingProject.FullName}",
            });
        }
        else
        {
            Debug.Assert(testingProject.Extension == ".dll");
            var assembly = Assembly.LoadFrom(testingProject.FullName);
            var text = File.ReadAllText(testProject.FullName);
            var location = $"<HintPath>{assembly.Location}</HintPath>";
            if (!text.Contains(location))
            {
                var reference = $"<Reference Include=\"{assembly.FullName}\">\n{location}\n</Reference>";
                text = text.Replace("</ItemGroup>", $"{reference}\n</ItemGroup>");
                File.WriteAllText(testProject.FullName, text);
            }
        }
    }

    private static void AllowUnsafeBlocks(FileInfo testProject)
    {
        // TODO: add, only if generated tests have unsafe modifier
        var text = File.ReadAllText(testProject.FullName);
        var allow = "<AllowUnsafeBlocks>true</AllowUnsafeBlocks>";
        if (!text.Contains(allow))
        {
            text = text.Replace("</PropertyGroup>", $"{allow}\n</PropertyGroup>");
            File.WriteAllText(testProject.FullName, text);
        }
    }

    private static void AddProjectToSolution(DirectoryInfo? solutionPath, FileInfo testProject)
    {
        if (solutionPath != null && solutionPath.Exists)
        {
            RunDotnet(new ProcessStartInfo
            {
                WorkingDirectory = solutionPath.FullName,
                Arguments = $"sln add {testProject.FullName}"
            });
        }
    }

    private static void AddHelpers(DirectoryInfo testProject)
    {
        var extensionsFolder = testProject.CreateSubdirectory("Extensions");

        File.WriteAllText(
            Path.Combine(extensionsFolder.FullName, "Allocator.cs"),
            ReadFromResource("VSharp.TestExtensions.Allocator.cs"));

        File.WriteAllText(
            Path.Combine(extensionsFolder.FullName, "ObjectsComparer.cs"),
            ReadFromResource("VSharp.TestExtensions.ObjectsComparer.cs"));
    }

    private static string ReadFromResource(string resourceName)
    {
        using var stream = typeof(TestExtensions.Allocator<>).Assembly.GetManifestResourceStream(resourceName);
        using var reader = new StreamReader(stream!);
        return reader.ReadToEnd();
    }

    private static void AddMoqReference(DirectoryInfo testProjectPath)
    {
        RunDotnet(new ProcessStartInfo
        {
            WorkingDirectory = testProjectPath.FullName,
            Arguments = "add package Moq -v 4.8.0",
        });
    }

    private static DirectoryInfo CreateTestProject(DirectoryInfo outputDir, FileInfo testingProject)
    {
        DirectoryInfo testProjectPath;
        // Choosing directory, where tests will be generated
        if (testingProject.Extension == ".csproj")
        {
            // Rider extension case
            var projectName = testingProject.Directory?.Name;
            Debug.Assert(projectName != null);
            testProjectPath = outputDir.CreateSubdirectory($"{projectName}.Tests");
            var parentDir = testingProject.Directory?.Parent;
            Debug.Assert(parentDir != null);
            outputDir = parentDir;
        }
        else
        {
            // VSharp case
            Debug.Assert(testingProject.Extension == ".dll");
            testProjectPath = outputDir.CreateSubdirectory("VSharp.RenderedTests");
        }

        // Creating nunit project
        RunDotnet(new ProcessStartInfo
        {
            WorkingDirectory = outputDir.FullName,
            // TODO: get framework version from the project
            Arguments = $"new nunit --force --name {testProjectPath.Name} --framework net6.0",
        });
        return testProjectPath;
    }

    private static DirectoryInfo GenerateTestProject(DirectoryInfo outputDir, FileInfo testingProject, FileInfo? solution)
    {
        // Creating nunit project
        var testProjectPath = CreateTestProject(outputDir, testingProject);
        var testProject = testProjectPath.EnumerateFiles("*.csproj").First();

        // Adding testing project reference to it
        AddUnderTestProjectReference(testProject, testingProject);
        // Allowing unsafe code inside project
        AllowUnsafeBlocks(testProject);
        // Adding it to solution
        AddProjectToSolution(solution?.Directory, testProject);
        // Copying test extensions (mock extensions, allocator, object comparer) to nunit project
        AddHelpers(testProjectPath);

        return testProjectPath;
    }

    private static DirectoryInfo FindVSharpDir()
    {
        var dir = new FileInfo(typeof(Renderer).Assembly.Location).Directory;
        Debug.Assert(dir != null);
        while (dir.FullName.Contains("VSharp.") && !dir.Name.Contains("VSharp."))
        {
            dir = dir.Parent;
            Debug.Assert(dir != null);
        }

        Debug.Assert(dir.Parent != null);
        return dir.Parent;
    }

    private static void WriteCompInFile(string testFilePath, SyntaxNode compilation)
    {
        using var streamWriter = new StreamWriter(File.Create(testFilePath));
        compilation.WriteTo(streamWriter);
    }

    private static string NameOfMember(MemberDeclarationSyntax member)
    {
        switch (member)
        {
            case MethodDeclarationSyntax method:
            {
                return
                    $"{method.ExplicitInterfaceSpecifier}{method.ReturnType}" +
                    $"{method.TypeParameterList}{method.Identifier}{method.ParameterList}";
            }
            case ConstructorDeclarationSyntax ctor:
            {
                return $"{ctor.Identifier}{ctor.ParameterList}";
            }
            case FieldDeclarationSyntax field:
            {
                var variables = field.Declaration.Variables;
                Debug.Assert(variables.Count == 1);
                return variables[0].Identifier.ToString();
            }
            case TypeDeclarationSyntax typeDecl:
            {
                return typeDecl.Identifier.ToString();
            }
            case DelegateDeclarationSyntax delegateDecl:
            {
                return delegateDecl.Identifier.ToString();
            }
            case PropertyDeclarationSyntax propertyDecl:
            {
                return propertyDecl.Identifier.ToString();
            }
            default:
                throw new NotImplementedException($"NameOfMember: unexpected case {member}");
        }
    }

    private static TypeDeclarationSyntax? MergeType(
        TypeDeclarationSyntax oldType,
        TypeDeclarationSyntax newType)
    {
        var oldMembers = oldType.Members;
        if (oldMembers.Count == 0)
            return null;

        var dictWithMembers = new Dictionary<string, MemberDeclarationSyntax>();
        foreach (var oldMember in oldMembers)
        {
            var key = NameOfMember(oldMember);
            dictWithMembers.Add(key, oldMember);
        }
        foreach (var newMember in newType.Members)
        {
            var key = NameOfMember(newMember);
            dictWithMembers[key] = newMember;
        }

        var values = dictWithMembers.Values;
        var offset = TestsRenderer.LastOffset(values.First());

        var members =
            values
                .Select(m =>
                    m is FieldDeclarationSyntax
                        ? m.WithLeadingTrivia(offset).WithTrailingTrivia(LineFeed)
                        : m.WithLeadingTrivia(offset).WithTrailingTrivia(LineFeed, LineFeed)
                ).ToList();
        var count = members.Count;
        members[count - 1] = members[count - 1].WithTrailingTrivia(LineFeed);
        return oldType.WithMembers(List(members));
    }

    private static IEnumerable<MemberDeclarationSyntax>? MergeTypes(
        SyntaxList<MemberDeclarationSyntax> oldTypes,
        SyntaxList<MemberDeclarationSyntax> newTypes)
    {
        if (oldTypes.Count == 0)
            return null;

        var dictWithMembers = new Dictionary<string, MemberDeclarationSyntax>();
        foreach (var oldMember in oldTypes)
        {
            var key = NameOfMember(oldMember);
            dictWithMembers.Add(key, oldMember);
        }
        foreach (var newMember in newTypes)
        {
            var key = NameOfMember(newMember);
            switch (newMember)
            {
                case TypeDeclarationSyntax type
                    when dictWithMembers.TryGetValue(key, out var oldValue):
                {
                    var oldType = oldValue as TypeDeclarationSyntax;
                    Debug.Assert(oldType != null);
                    var mergedType = MergeType(oldType, type);
                    if (mergedType == null)
                        return null;
                    dictWithMembers[key] = mergedType;
                    break;
                }
                case TypeDeclarationSyntax type:
                    dictWithMembers.Add(key, type);
                    break;
                default:
                    dictWithMembers[key] = newMember;
                    break;
            }
        }

        var types =
            dictWithMembers.Values
                .Select(t => t.WithLeadingTrivia(LineFeed).WithTrailingTrivia(LineFeed));
        return types;
    }

    private static IEnumerable<UsingDirectiveSyntax> MergeUsings(
        IEnumerable<UsingDirectiveSyntax> oldUsigns,
        IEnumerable<UsingDirectiveSyntax> newUsings)
    {
        var mergedUsigns = oldUsigns.ToList();
        var count = mergedUsigns.Count;
        var oldUsingsStr = mergedUsigns.Select(u => u.Name.ToString()).ToHashSet();

        foreach (var newUsing in newUsings)
        {
            if (!oldUsingsStr.Contains(newUsing.Name.ToString()))
                mergedUsigns.Add(newUsing);
        }

        if (mergedUsigns.Count > count)
            mergedUsigns[^1] = mergedUsigns[^1].WithTrailingTrivia(LineFeed);

        return mergedUsigns;
    }

    private static void AddRenderedInFile(string testFilePath, SyntaxNode compilation)
    {
        if (File.Exists(testFilePath))
        {
            // Rendered class already exists case, so rendering only new methods into it
            var programText = File.ReadAllText(testFilePath);
            var oldComp = CSharpSyntaxTree.ParseText(programText).GetCompilationUnitRoot();
            var newComp = compilation as CompilationUnitSyntax;
            Debug.Assert(newComp != null);

            var mergedUsings = MergeUsings(oldComp.Usings, newComp.Usings);

            var oldMembers = oldComp.Members;
            var newMembers = newComp.Members;
            if (!oldMembers.Any())
            {
                WriteCompInFile(testFilePath, compilation);
                return;
            }
            Debug.Assert(oldMembers.Count == 1 && newMembers.Count == 1);
            var oldNamespace = oldMembers[0] as BaseNamespaceDeclarationSyntax;
            var newNamespace = newMembers[0] as BaseNamespaceDeclarationSyntax;
            Debug.Assert(oldNamespace != null && newNamespace != null &&
                newNamespace.Name.ToString() == oldNamespace.Name.ToString());
            var types =
                MergeTypes(oldNamespace.Members, newNamespace.Members);
            if (types == null)
            {
                WriteCompInFile(testFilePath, compilation);
                return;
            }

            oldNamespace = oldNamespace.WithMembers(List(types));
            oldComp = oldComp.WithUsings(List(mergedUsings));
            // TODO: use Format from TestsRenderer? (safer for indents, but slower)
            compilation = oldComp.WithMembers(SingletonList<MemberDeclarationSyntax>(oldNamespace));
        }
        WriteCompInFile(testFilePath, compilation);
    }

    private static UnitTest DeserializeTest(FileInfo test, AssemblyLoadContext? assemblyLoadContext)
    {
        testInfo ti;
        using (var stream = new FileStream(test.FullName, FileMode.Open, FileAccess.Read))
        {
            ti = UnitTest.DeserializeTestInfo(stream);
        }

        AssemblyResolver.Configure(ti.extraAssemblyLoadDirs);

        return UnitTest.DeserializeFromTestInfo(ti, true);
    }

    private static List<UnitTest> DeserializeTests(
        IEnumerable<FileInfo> tests,
        AssemblyLoadContext? assemblyLoadContext)
    {
        var deserializedTests = new List<UnitTest>();
        foreach (var test in tests)
        {
            try
            {
                deserializedTests.Add(DeserializeTest(test, assemblyLoadContext));
            }
            catch (Exception e)
            {
                Logger.writeLineString(Logger.Error, $"Tests renderer: deserialization of test failed: {e}");
            }
        }

        return deserializedTests;
    }

    private static (SyntaxNode, SyntaxNode?, string, Assembly) RunTestsRenderer(
        IEnumerable<FileInfo> tests,
        Type? declaringType,
        bool wrapErrors = false,
        AssemblyLoadContext? assemblyLoadContext = null)
    {
        AssemblyResolver.AddResolve(assemblyLoadContext);

        var unitTests = DeserializeTests(tests, assemblyLoadContext);
        if (unitTests.Count == 0)
            throw new Exception("No *.vst files were generated, nothing to render");
        Assembly testAssembly = unitTests.First().Method.Module.Assembly;

        var (testsProgram, mocksProgram,  typeName) =
            TestsRenderer.RenderTests(unitTests, wrapErrors, declaringType);

        AssemblyResolver.RemoveResolve(assemblyLoadContext);

        return (testsProgram, mocksProgram, typeName, testAssembly);
    }

    // Writing generated tests and mocks to files
    private static List<string> WriteResults(
        DirectoryInfo outputDir,
        string typeName,
        SyntaxNode testsProgram,
        SyntaxNode? mocksProgram)
    {
        var files = new List<string>();
        var testFilePath = Path.Combine(outputDir.FullName, $"{typeName}Tests.cs");
        AddRenderedInFile(testFilePath, testsProgram);
        files.Add(testFilePath);
        if (mocksProgram != null)
        {
            var mocksFilePath = Path.Combine(outputDir.FullName, "Mocks.cs");
            AddRenderedInFile(mocksFilePath, mocksProgram);
            files.Add(mocksFilePath);
        }

        return files;
    }

    // API method for Rider extension
    public static (DirectoryInfo, List<string>) Render(
        IEnumerable<FileInfo> tests,
        FileInfo testingProject,
        Type declaringType,
        AssemblyLoadContext assemblyLoadContext,
        FileInfo? solutionForTests = null)
    {
        var (testsProgram, mocksProgram, typeName, _) =
            RunTestsRenderer(tests, declaringType, false, assemblyLoadContext);

        var outputDir = testingProject.Directory?.Parent;
        Debug.Assert(outputDir != null && outputDir.Exists);
        var testProjectPath = GenerateTestProject(outputDir, testingProject, solutionForTests);
        var renderedFiles = WriteResults(testProjectPath, typeName, testsProgram, mocksProgram);

        return (testProjectPath, renderedFiles);
    }

    // API method for VSharp
    public static void Render(
        IEnumerable<FileInfo> tests,
        bool wrapErrors = false,
        Type? declaringType = null,
        DirectoryInfo? outputDir = null)
    {
        var (testsProgram, mocksProgram, typeName, assembly) =
            RunTestsRenderer(tests, declaringType, wrapErrors);

        if (outputDir == null)
        {
            // Internal integration tests case
            var vSharpDir = FindVSharpDir();
            var testDir = vSharpDir.EnumerateDirectories("VSharp.Test").First();
            outputDir = testDir.CreateSubdirectory("GeneratedTests");
        }
        else
        {
            // API or console runner case
            if (!outputDir.Exists) outputDir.Create();
            outputDir = GenerateTestProject(outputDir, new FileInfo(assembly.Location), null);
        }

        WriteResults(outputDir, typeName, testsProgram, mocksProgram);
    }
}
