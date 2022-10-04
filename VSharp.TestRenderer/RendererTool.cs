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
    private static readonly List<string> CallingTests = new ();

    // TODO: create class 'Expression' with operators?

    private static void RenderTest(
        MethodRenderer test,
        MethodBase method,
        IEnumerable<object> args,
        object? thisArg,
        bool isError,
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
            var renderedThis = mainBlock.RenderObject(thisArg);
            if (renderedThis is IdentifierNameSyntax id)
            {
                thisArgId = id;
            }
            else
            {
                var thisType = RenderType(method.DeclaringType ?? typeof(object));
                thisArgId = mainBlock.AddDecl("thisArg", thisType, mainBlock.RenderObject(thisArg));
            }
        }

        // Calling testing method
        var callMethod = RenderCall(thisArgId, method, renderedArgs.ToArray());

        if ((Reflection.hasNonVoidResult(method) || method.IsConstructor) && ex == null && !isError)
        {
            var resultId = mainBlock.AddDecl("result", ObjectType, callMethod);
            CallingTests.Add(callMethod.NormalizeWhitespace().ToString());

            // Adding namespace of objects comparer to usings
            AddObjectsComparer();
            ExpressionSyntax expectedExpr;
            expectedExpr = method.IsConstructor ? thisArgId : mainBlock.RenderObject(expected);
            var condition = RenderCall(CompareObjects, resultId, expectedExpr);
            mainBlock.AddAssert(condition);
        }
        else if (ex == null || isError)
        {
            mainBlock.AddCall(callMethod);
            var call = callMethod as InvocationExpressionSyntax;
            Debug.Assert(call != null);
            CallingTests.Add(call.Expression.NormalizeWhitespace().ToString());
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
            CallingTests.Add(assertThrows.Expression.NormalizeWhitespace().ToString());
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

    private class IndentsRewriter : CSharpSyntaxRewriter
    {
        private const int TabSize = 4;
        private int _currentOffset = 0;

        private static SyntaxTrivia WhitespaceTrivia(int offset)
        {
            return Whitespace(new string(' ', offset));
        }

        public override SyntaxNode? Visit(SyntaxNode? node)
        {
            if (node == null)
                return null;

            if (node.HasLeadingTrivia)
            {
                var triviaList = node.GetLeadingTrivia();
                var whitespaces =
                    triviaList
                        .Where(trivia => trivia.IsKind(SyntaxKind.WhitespaceTrivia))
                        .ToArray();
                Debug.Assert(whitespaces.Length == 1);
                _currentOffset = whitespaces[0].ToFullString().Length;
            }

            switch (node)
            {
                // Deleting whitespace between 'null' and '!'
                case PostfixUnaryExpressionSyntax unary
                    when unary.IsKind(SyntaxKind.SuppressNullableWarningExpression):
                {
                    var operand = unary.Operand.WithTrailingTrivia();
                    var unaryOperator = unary.OperatorToken.WithLeadingTrivia();
                    node = unary.WithOperatorToken(unaryOperator).WithOperand(operand);
                    return base.Visit(node);
                }
                // TODO: add blank line, only if there is statement before/after
                // Adding blank lines between args declaration, target method invocation and result checking
                case LocalDeclarationStatementSyntax varDecl:
                {
                    var vars = varDecl.Declaration.Variables;
                    if (vars.Count > 0)
                    {
                        var init = vars[0].Initializer;
                        if (init != null && CallingTests.Contains(init.Value.ToString()))
                        {
                            node = node.WithLeadingTrivia(LineFeed, WhitespaceTrivia(_currentOffset));
                            node = node.WithTrailingTrivia(LineFeed, LineFeed);
                            return base.Visit(node);
                        }
                    }
                    break;
                }
                // Adding blank line before 'Assert.Throws'
                case ExpressionStatementSyntax { Expression: InvocationExpressionSyntax call }
                    when CallingTests.Contains(call.Expression.ToString()):
                {
                    node = node.WithLeadingTrivia(LineFeed, WhitespaceTrivia(_currentOffset));
                    return base.Visit(node);
                }
                // Formatting initializer with line breaks
                case ObjectCreationExpressionSyntax objCreation:
                {
                    var init = objCreation.Initializer;
                    if (init != null)
                    {
                        var expressions = init.Expressions;
                        if (expressions.Count > 0)
                        {
                            var formattedExpressions = new List<ExpressionSyntax>();
                            foreach (var assign in expressions)
                            {
                                var formatted =
                                    assign.WithLeadingTrivia(LineFeed, WhitespaceTrivia(_currentOffset + TabSize));
                                formattedExpressions.Add(formatted);
                            }

                            init = init.WithExpressions(SeparatedList(formattedExpressions));
                            var formattedCloseBrace =
                                init.CloseBraceToken
                                    .WithLeadingTrivia(LineFeed, WhitespaceTrivia(_currentOffset));
                            init = init.WithCloseBraceToken(formattedCloseBrace);
                        }

                        node = objCreation.WithInitializer(init);
                    }

                    return base.Visit(node);
                }
            }

            return base.Visit(node);
        }
    }

    public static SyntaxNode Format(SyntaxNode node)
    {
        var normalized = node.NormalizeWhitespace();
        var myRewriter = new IndentsRewriter();
        var formatted = myRewriter.Visit(normalized);
        Debug.Assert(formatted != null);
        return formatted;
    }

    public static void RenderTests(IEnumerable<FileInfo> tests)
    {
        AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;

        PrepareCache();
        CallingTests.Clear();
        // Adding NUnit namespace to usings
        AddNUnit();

        // Creating main class for generating tests
        var generatedClass =
            new ClassRenderer(
                "GeneratedClass",
                RenderAttributeList("TestFixture"),
                null
            );

        foreach (var fi in tests)
        {
            testInfo ti;
            using (var stream = new FileStream(fi.FullName, FileMode.Open, FileAccess.Read))
            {
                ti = UnitTest.DeserializeTestInfo(stream);
            }

            _extraAssemblyLoadDirs = ti.extraAssemblyLoadDirs;
            var test = UnitTest.DeserializeFromTestInfo(ti, true);
            // _extraAssemblyLoadDirs = test.ExtraAssemblyLoadDirs;

            var method = test.Method;
            var parameters = test.Args ?? method.GetParameters()
                .Select(t => Reflection.defaultOf(t.ParameterType)).ToArray();
            var thisArg = test.ThisArg;
            if (thisArg == null && !method.IsStatic)
                thisArg = Reflection.createObject(method.DeclaringType);
            string suitTypeName;
            if (fi.Name.Contains("error"))
                suitTypeName = "Error";
            else
            {
                Debug.Assert(fi.Name.Contains("test"));
                suitTypeName = "Test";
            }

            string testName;
            if (method.IsConstructor && method.DeclaringType != null)
                testName = $"{RenderType(method.DeclaringType)}Constructor";
            else testName = method.Name;

            var testRenderer = generatedClass.AddMethod(
                testName + suitTypeName,
                RenderAttributeList("Test"),
                new[] { Public },
                VoidType,
                System.Array.Empty<(TypeSyntax, string)>()
            );
            RenderTest(testRenderer, method, parameters, thisArg, test.IsError, test.Exception, test.Expected);
        }

        SyntaxNode compilation =
            RenderProgram(
                "GeneratedNamespace",
                generatedClass.Render()
            );

        compilation = Format(compilation);
        // compilation.WriteTo(Console.Out);
        var dir = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}generated.cs";
        using var streamWriter = new StreamWriter(File.Create(dir));
        compilation.WriteTo(streamWriter);
    }
}
