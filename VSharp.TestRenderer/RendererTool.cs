using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

public static class Renderer
{
    private static readonly string NewLine = Environment.NewLine;

    private static void RunDotnet(ProcessStartInfo startInfo)
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
            // TODO: try to add reference via 'dotnet add reference' (like with '.csproj' reference)
            Debug.Assert(testingProject.Extension == ".dll");
            var assembly = AssemblyManager.LoadFromAssemblyPath(testingProject.FullName);
            var text = File.ReadAllText(testProject.FullName);
            var location = $"<HintPath>{assembly.Location}</HintPath>";
            if (!text.Contains(location))
            {
                var reference = $"<Reference Include=\"{assembly.FullName}\">{NewLine}{location}{NewLine}</Reference>";
                text = text.Replace("</ItemGroup>", $"{reference}{NewLine}</ItemGroup>");
                File.WriteAllText(testProject.FullName, text);
            }
        }
    }

    private static void AddTestProjectProperties(FileInfo testProject)
    {
        var text = File.ReadAllText(testProject.FullName);
        // TODO: add, only if generated tests have unsafe modifier
        var allow = "    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>";
        // Without this property duplicate AssemblyInfo.cs may appear and the solution will fail to build
        var disableAssemblyInfoGeneration = "  <GenerateAssemblyInfo>false</GenerateAssemblyInfo>";
        if (!text.Contains(allow))
        {
            text = text.Replace("</PropertyGroup>", $"{disableAssemblyInfoGeneration}{NewLine}{allow}{NewLine}  </PropertyGroup>");
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

    private static void CopyHelpers(DirectoryInfo testProject)
    {
        var extensionsFolder = testProject.CreateSubdirectory("Extensions");

        File.WriteAllText(
            Path.Combine(extensionsFolder.FullName, "Allocator.cs"),
            ReadFromResource("VSharp.TestExtensions.Allocator.cs"));

        File.WriteAllText(
            Path.Combine(extensionsFolder.FullName, "ObjectsComparer.cs"),
            ReadFromResource("VSharp.TestExtensions.ObjectsComparer.cs"));
    }

    private static CompilationUnitSyntax AddHelpersToTests(CompilationUnitSyntax testsComp)
    {
        // TODO: add helper, only if it was used
        var allocatorProgram = ReadFromResource("VSharp.TestExtensions.Allocator.cs");
        var allocatorComp = CSharpSyntaxTree.ParseText(allocatorProgram).GetCompilationUnitRoot();
        var comparerProgram = ReadFromResource("VSharp.TestExtensions.ObjectsComparer.cs");
        var comparerComp = CSharpSyntaxTree.ParseText(comparerProgram).GetCompilationUnitRoot();

        return
            MergeCompilations(
                testsComp,
                MergeCompilations(allocatorComp, comparerComp)
            );
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

    private static DirectoryInfo CreateTestProject(
        DirectoryInfo outputDir,
        FileInfo testingProject,
        string? targetFramework = null)
    {
        DirectoryInfo testProjectPath;
        // Choosing directory, where tests will be generated
        if (testingProject.Extension == ".csproj")
        {
            // Rider extension case
            var projectName = testingProject.Directory?.Name;
            Debug.Assert(projectName != null);
            testProjectPath = outputDir.CreateSubdirectory($"{projectName}.Tests");
        }
        else
        {
            // VSharp case
            Debug.Assert(testingProject.Extension == ".dll");
            // TODO: unify getting name with 'RunTestsRenderer'
            var testProjectName = Path.GetFileNameWithoutExtension(testingProject.Name) + ".Tests";
            testProjectPath = outputDir.CreateSubdirectory(testProjectName);
        }

        // TODO: parse testing project '.csproj' and get target version from there
        targetFramework ??= "net6.0";

        // Creating nunit project
        RunDotnet(new ProcessStartInfo
        {
            WorkingDirectory = outputDir.FullName,
            Arguments = $"new nunit --force --name {testProjectPath.Name} --framework {targetFramework}",
        });

        // Deleting redundant files generated by nunit
        foreach (var file in testProjectPath.EnumerateFiles("UnitTest1.cs"))
            file.Delete();
        foreach (var file in testProjectPath.EnumerateFiles("Usings.cs"))
            file.Delete();

        return testProjectPath;
    }

    private static DirectoryInfo GenerateTestProject(
        DirectoryInfo outputDir,
        FileInfo testingProject,
        FileInfo? solution,
        string? targetFramework = null,
        bool singleFile = false)
    {
        // Creating nunit project
        var testProjectPath = CreateTestProject(outputDir, testingProject, targetFramework);
        var testProject = testProjectPath.EnumerateFiles("*.csproj").First();

        // Adding testing project reference to it
        AddUnderTestProjectReference(testProject, testingProject);
        // Allowing unsafe code inside project
        AddTestProjectProperties(testProject);
        // Adding it to solution
        AddProjectToSolution(solution?.Directory, testProject);
        // Need to copy extension, only if rendering standard project,
        // in case of single file project helpers will be added to tests program
        if (!singleFile)
            // Copying test extensions (mock extensions, allocator, object comparer) to nunit project
            CopyHelpers(testProjectPath);

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
            case IndexerDeclarationSyntax indexer:
            {
                return
                    $"{indexer.Modifiers}{indexer.Type}{indexer.ExplicitInterfaceSpecifier}" +
                    $"{indexer.AccessorList}{indexer.ParameterList}";
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
                return $"{typeDecl.Keyword}{typeDecl.Identifier}{typeDecl.TypeParameterList}";
            }
            case DelegateDeclarationSyntax delegateDecl:
            {
                return delegateDecl.Identifier.ToString();
            }
            case PropertyDeclarationSyntax propertyDecl:
            {
                return $"{propertyDecl.ExplicitInterfaceSpecifier}{propertyDecl.Type}{propertyDecl.Identifier}";
            }
            case BaseNamespaceDeclarationSyntax namespaceDecl:
            {
                return namespaceDecl.Name.ToString();
            }
            default:
            {
                Logger.printLogString(Logger.Error, $"NameOfMember: unexpected case {member}");
                return member.ToString();
            }
        }
    }

    private static TypeDeclarationSyntax? MergeType(
        TypeDeclarationSyntax oldType,
        TypeDeclarationSyntax newType)
    {
        var oldMembers = oldType.Members;
        if (oldMembers.Count == 0)
            return null;

        var dictWithMembers = new SortedDictionary<string, MemberDeclarationSyntax>();
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

    private static IEnumerable<MemberDeclarationSyntax>? MergeNamespaces(
        IEnumerable<MemberDeclarationSyntax> oldNamespaces,
        IEnumerable<MemberDeclarationSyntax> newNamespaces)
    {
        var dictWithMembers = new Dictionary<string, MemberDeclarationSyntax>();
        foreach (var oldMember in oldNamespaces)
        {
            var key = NameOfMember(oldMember);
            dictWithMembers.Add(key, oldMember);
        }
        foreach (var newMember in newNamespaces)
        {
            var key = NameOfMember(newMember);
            switch (newMember)
            {
                case BaseNamespaceDeclarationSyntax namespaceDecl
                    when dictWithMembers.TryGetValue(key, out var oldValue):
                {
                    var oldNamespace = oldValue as BaseNamespaceDeclarationSyntax;
                    Debug.Assert(oldNamespace != null);
                    var types =
                        MergeTypes(oldNamespace.Members, namespaceDecl.Members);
                    if (types == null)
                        return null;
                    dictWithMembers[key] = oldNamespace.WithMembers(List(types));
                    break;
                }
                case BaseNamespaceDeclarationSyntax namespaceDecl:
                    dictWithMembers.Add(key, namespaceDecl);
                    break;
                default:
                    dictWithMembers[key] = newMember;
                    break;
            }
        }

        var values = dictWithMembers.Values;
        var members = new List<MemberDeclarationSyntax>();

        var count = values.Count;
        if (count == 0) return null;
        if (count == 1) return values;

        foreach (var member in values)
        {
            switch (member)
            {
                case FileScopedNamespaceDeclarationSyntax namespaceDecl:
                {
                    var decl =
                        NamespaceDeclaration(namespaceDecl.Name)
                            // TODO: make it faster (create trivia manually)
                            .NormalizeWhitespace()
                            .WithMembers(namespaceDecl.Members)
                            .WithLeadingTrivia(LineFeed)
                            .WithTrailingTrivia(LineFeed);
                    members.Add(decl);
                    break;
                }
                default:
                {
                    var value =
                        member.WithLeadingTrivia(LineFeed)
                            .WithTrailingTrivia(LineFeed);
                    members.Add(value);
                    break;
                }
            }
        }

        return members;
    }

    private static CompilationUnitSyntax MergeCompilations(
        CompilationUnitSyntax oldComp,
        CompilationUnitSyntax newComp)
    {
        var mergedUsings = MergeUsings(oldComp.Usings, newComp.Usings);

        var oldMembers = oldComp.Members;
        if (!oldMembers.Any())
            return newComp;

        var namespaces =
            MergeNamespaces(oldMembers, newComp.Members);
        if (namespaces == null)
            return newComp;

        oldComp = oldComp.WithUsings(List(mergedUsings));
        // TODO: use Format from TestsRenderer? (safer for indents, but slower)
        return oldComp.WithMembers(List(namespaces));
    }

    private static void AddRenderedInFile(string testFilePath, CompilationUnitSyntax newComp)
    {
        if (File.Exists(testFilePath))
        {
            // Rendered class already exists case, so rendering only new methods into it
            var programText = File.ReadAllText(testFilePath);
            var oldComp = CSharpSyntaxTree.ParseText(programText).GetCompilationUnitRoot();

            newComp = MergeCompilations(oldComp, newComp);
        }
        WriteCompInFile(testFilePath, newComp);
    }

    private static UnitTest DeserializeTest(FileInfo test)
    {
        testInfo ti;
        using (var stream = new FileStream(test.FullName, FileMode.Open, FileAccess.Read))
        {
            ti = UnitTest.DeserializeTestInfo(stream);
        }

        return UnitTest.DeserializeFromTestInfo(ti, true);
    }

    private static List<UnitTest> DeserializeTests(IEnumerable<FileInfo> tests)
    {
        var deserializedTests = new List<UnitTest>();
        foreach (var test in tests)
        {
            try
            {
                deserializedTests.Add(DeserializeTest(test));
            }
            catch (Exception e)
            {
                Logger.printLogString(Logger.Error, $"Tests renderer: deserialization of test failed: {e}");
            }
        }

        return deserializedTests;
    }

    private static (List<(CompilationUnitSyntax, string)>, Assembly) RunTestsRenderer(
        IEnumerable<FileInfo> tests,
        Type? declaringType,
        string? testProjectName,
        bool wrapErrors = false,
        bool singleFile = false)
    {
        var unitTests = DeserializeTests(tests);
        if (unitTests.Count == 0)
            throw new Exception("No *.vst files were generated, nothing to render");
        var originAssembly = unitTests.First().Method.Module.Assembly;
        var exploredAssembly = AssemblyManager.LoadCopy(originAssembly);

        testProjectName ??= exploredAssembly.GetName().Name + ".Tests";
        Debug.Assert(testProjectName != null);

        var testsPrograms =
            TestsRenderer.RenderTests(unitTests, testProjectName, wrapErrors, singleFile, declaringType);

        return (testsPrograms, exploredAssembly);
    }

    // Writing generated tests and mocks to files
    private static List<string> WriteResults(
        DirectoryInfo outputDir,
        List<(CompilationUnitSyntax, string)> testsPrograms)
    {
        var files = new List<string>();
        foreach (var (testsProgram, programName) in testsPrograms)
        {
            var testFilePath = Path.Combine(outputDir.FullName, $"{programName}.cs");
            AddRenderedInFile(testFilePath, testsProgram);
            files.Add(testFilePath);
        }

        return files;
    }

    // API method for Rider extension
    public static (DirectoryInfo, List<string>) Render(
        IEnumerable<FileInfo> tests,
        FileInfo testingProject,
        Type? declaringType = null,
        FileInfo? solutionForTests = null,
        string? targetFramework = null)
    {
        var outputDir = solutionForTests?.Directory;
        Debug.Assert(outputDir is { Exists: true });
        var testProjectPath = GenerateTestProject(outputDir, testingProject, solutionForTests, targetFramework);

        var (testsPrograms, _) =
            RunTestsRenderer(tests, declaringType, testProjectPath.Name, false, false);

        var renderedFiles = WriteResults(testProjectPath, testsPrograms);

        return (testProjectPath, renderedFiles);
    }

    // API method for VSharp
    public static void Render(
        IEnumerable<FileInfo> tests,
        bool wrapErrors = false,
        bool singleFile = false,
        Type? declaringType = null,
        DirectoryInfo? outputDir = null)
    {
        var testProjectName = outputDir == null ? "VSharp.Test.GeneratedTests" : null;
        var (testsPrograms, assembly) =
            RunTestsRenderer(tests, declaringType, testProjectName, wrapErrors, singleFile);

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
            var testingProject = new FileInfo(assembly.Location);
            outputDir = GenerateTestProject(outputDir, testingProject, null, singleFile:singleFile);
            if (singleFile)
            {
                var (testsProgram, programName) = testsPrograms.Single();
                testsProgram = AddHelpersToTests(testsProgram);
                testsPrograms.Clear();
                testsPrograms.Add((testsProgram, programName));
            }
        }

        WriteResults(outputDir, testsPrograms);
    }
}
