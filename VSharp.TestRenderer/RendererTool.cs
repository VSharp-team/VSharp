using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

public static class Renderer
{
    private static IEnumerable<string>? _extraAssemblyLoadDirs;

    // TODO: create class 'Expression' with operators?

    private static void RenderTest(
        MethodRenderer test,
        MethodBase method,
        IEnumerable<object> args,
        object? thisArg,
        Type? ex,
        object expected)
    {
        var mainBlock = test.Body;

        // Declaring arguments and 'this' of testing method
        var renderedArgs = args.Select(mainBlock.RenderObject);
        IdentifierNameSyntax thisArgId = null!;
        if (thisArg != null)
        {
            Debug.Assert(Reflection.hasThis(method));
            thisArgId = mainBlock.AddDecl("thisArg", ObjectType, mainBlock.RenderObject(thisArg));
        }

        // Calling testing method
        var callMethod = RenderCall(thisArgId, method, renderedArgs.ToArray());

        if (ex == null)
        {
            var resultId = mainBlock.AddDecl("result", ObjectType, callMethod);
            var condition =
                RenderCall(
                    CompareObjects,
                    resultId,
                    mainBlock.RenderObject(expected)
                );
            mainBlock.AddAssert(condition);
        }
        else
        {
            // Handling exceptions
            var delegateExpr = ParenthesizedLambdaExpression(callMethod);
            var assertThrows =
                // TODO: use parsing of AST to get names? (instead of strings)
                RenderCall(
                    "Assert", "Throws",
                    new []{ RenderType(ex) },
                    delegateExpr
                );
            mainBlock.AddCall(assertThrows);
        }
    }

    private static Assembly TryLoadAssemblyFrom(object sender, ResolveEventArgs args)
    {
        var existingInstance = AppDomain.CurrentDomain.GetAssemblies().FirstOrDefault(assembly => assembly.FullName == args.Name);
        if (existingInstance != null)
        {
            return existingInstance;
        }
        foreach (string path in _extraAssemblyLoadDirs)
        {
            string assemblyPath = Path.Combine(path, new AssemblyName(args.Name).Name + ".dll");
            if (!File.Exists(assemblyPath))
                return null;
            Assembly assembly = Assembly.LoadFrom(assemblyPath);
            return assembly;
        }

        return null;
    }

    public static void RenderTests(IEnumerable<FileInfo> tests)
    {
        AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;

        // Creating main class for generating tests
        var generatedClass =
            new ClassRenderer(
                "GeneratedClass",
                RenderAttributeList("TestFixture"),
                null
            );

        foreach (FileInfo fi in tests)
        {
            testInfo ti;
            using (FileStream stream = new FileStream(fi.FullName, FileMode.Open, FileAccess.Read))
            {
                ti = UnitTest.DeserializeTestInfo(stream);
            }

            _extraAssemblyLoadDirs = ti.extraAssemblyLoadDirs;
            UnitTest test = UnitTest.DeserializeFromTestInfo(ti);
            // _extraAssemblyLoadDirs = test.ExtraAssemblyLoadDirs;

            var method = test.Method;
            object[] parameters = test.Args ?? method.GetParameters()
                .Select(t => Reflection.defaultOf(t.ParameterType)).ToArray();
            object thisArg = test.ThisArg;
            if (thisArg == null && !method.IsStatic)
                thisArg = Reflection.createObject(method.DeclaringType);
            var testRenderer = generatedClass.AddMethod(
                $"{method.Name}Test",
                RenderAttributeList("Test"),
                new[] { Public, Static },
                VoidType,
                System.Array.Empty<(TypeSyntax, string)>()
            );
            RenderTest(
                testRenderer,
                method,
                parameters,
                thisArg,
                test.Exception,
                test.Expected
            );
        }

        var comp =
            RenderProgram(
                "GeneratedNamespace",
                generatedClass.Render()
            );

        comp.NormalizeWhitespace().WriteTo(Console.Out);
        var dir = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}generated.cs";
        using var streamWriter = new StreamWriter(File.Create(dir));
        // comp.NormalizeWhitespace().WriteTo(streamWriter);
    }
}
