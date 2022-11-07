using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

public static class TestsRenderer
{
    // Struct, which helps to format generated methods
    private struct MethodFormat
    {
        public bool HasArgs = false;
        public string? CallingTest = null;

        public MethodFormat()
        {
        }
    }

    // TODO: move all format features to non-static Formatter class
    private static readonly Dictionary<string, MethodFormat> MethodsFormat = new ();

    private static IEnumerable<string>? _extraAssemblyLoadDirs;

    // Used only for rendering names, does not support adding usings and references
    private static readonly CodeRenderer InternalCodeRenderer = new (new EmptyReferenceManager());

    // TODO: create class 'Expression' with operators?

    private static Assembly? TryLoadAssemblyFrom(object? sender, ResolveEventArgs args)
    {
        var existingInstance = AppDomain.CurrentDomain.GetAssemblies().FirstOrDefault(assembly => assembly.FullName == args.Name);
        if (existingInstance != null)
        {
            return existingInstance;
        }

        if (_extraAssemblyLoadDirs == null) return null;

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

    internal static SyntaxTrivia LastOffset(SyntaxNode node)
    {
        var whitespaces =
            node.GetLeadingTrivia()
                .Where(trivia => trivia.IsKind(SyntaxKind.WhitespaceTrivia))
                .ToArray();
        return whitespaces[^1];
    }

    private class IndentsRewriter : CSharpSyntaxRewriter
    {
        private const int TabSize = 4;
        private int _currentOffset = 0;
        private MethodFormat _format = new();
        private string? _firstExpr = null;
        private static readonly SyntaxTrivia ArrangeComment = Comment("// arrange");
        private static readonly SyntaxTrivia ActComment = Comment("// act");
        private static readonly SyntaxTrivia AssertComment = Comment("// assert");
        private static readonly SyntaxTrivia IgnoredComment = Comment("// ignored");

        private static SyntaxTrivia WhitespaceTrivia(int offset)
        {
            return Whitespace(new string(' ', offset));
        }

        private SyntaxTrivia CurrentOffset()
        {
            return WhitespaceTrivia(_currentOffset);
        }

        private SyntaxTrivia ShiftedOffset()
        {
            return WhitespaceTrivia(_currentOffset + TabSize);
        }

        private SyntaxNode AddActComment(SyntaxNode node)
        {
            var whitespaces = CurrentOffset();
            // If method has lines with generated args and it's not try block, adding empty line before calling test
            // In case of try block, empty line was already added before it
            var beforeTrivia =
                _format.HasArgs && node.Parent is not BlockSyntax { Parent: TryStatementSyntax }
                    ? new[] { LineFeed, whitespaces, ActComment, LineFeed, whitespaces }
                    : new[] { whitespaces, ActComment, LineFeed, whitespaces };
            return node.WithLeadingTrivia(beforeTrivia);
        }

        public override SyntaxNode? Visit(SyntaxNode? node)
        {
            if (node == null)
                return null;

            if (node.HasLeadingTrivia)
            {
                _currentOffset = LastOffset(node).ToFullString().Length;
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
                                    assign.WithLeadingTrivia(LineFeed, ShiftedOffset());
                                formattedExpressions.Add(formatted);
                            }

                            init = init.WithExpressions(SeparatedList(formattedExpressions));
                            var formattedCloseBrace =
                                init.CloseBraceToken
                                    .WithLeadingTrivia(LineFeed, CurrentOffset());
                            init = init.WithCloseBraceToken(formattedCloseBrace);
                        }

                        node = objCreation.WithInitializer(init);
                    }

                    return base.Visit(node);
                }
                // Adding '// arrange' comment before generated args
                case StatementSyntax statement when statement.ToString() == _firstExpr:
                {
                    var whitespaces = CurrentOffset();
                    node = statement.WithLeadingTrivia(whitespaces, ArrangeComment, LineFeed, whitespaces);
                    return base.Visit(node);
                }
                // Adding '// act' comment before calling test in case of variable declaration
                case LocalDeclarationStatementSyntax varDecl:
                {
                    var vars = varDecl.Declaration.Variables;
                    if (vars.Count > 0)
                    {
                        var init = vars[0].Initializer;
                        var callingTest = _format.CallingTest;
                        if (callingTest != null && init != null && callingTest == init.Value.ToString())
                            node = AddActComment(node);

                        return base.Visit(node);
                    }

                    break;
                }
                // Adding '// act' comment before calling test in case of call statement
                case ExpressionStatementSyntax expr
                    when _format.CallingTest != null && _format.CallingTest == expr.Expression.ToString():
                {
                    return base.Visit(AddActComment(node));
                }
                // Adding blank line before try block with test calling
                case TryStatementSyntax:
                {
                    if (_format.HasArgs)
                        node = node.WithLeadingTrivia(LineFeed, CurrentOffset());

                    return base.Visit(node);
                }
                // Remembering current method
                case MethodDeclarationSyntax expr:
                {
                    _firstExpr = null;
                    MethodsFormat.TryGetValue(expr.Identifier.ToString(), out _format);
                    if (_format.HasArgs)
                        _firstExpr = expr.Body?.Statements[0].ToString();

                    return base.Visit(node);
                }
                // Adding blank line and '// assert' comment before assert
                case ExpressionStatementSyntax expr when expr.ToString().Contains("Assert"):
                {
                    var whitespaces = CurrentOffset();
                    node = node.WithLeadingTrivia(LineFeed, whitespaces, AssertComment, LineFeed, whitespaces);
                    return base.Visit(node);
                }
                case FileScopedNamespaceDeclarationSyntax namespaceDecl:
                {
                    var semicolon = namespaceDecl.SemicolonToken.WithTrailingTrivia(LineFeed, LineFeed);
                    node = namespaceDecl.WithSemicolonToken(semicolon);
                    return base.Visit(node);
                }
                case CatchClauseSyntax catchClause when !catchClause.Block.Statements.Any():
                {
                    var block = catchClause.Block;
                    var openBrace = block.OpenBraceToken;
                    openBrace = openBrace.WithTrailingTrivia(LineFeed, ShiftedOffset(), IgnoredComment, LineFeed);
                    block = catchClause.Block.WithOpenBraceToken(openBrace);
                    node = catchClause.WithBlock(block);
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

    private class EmptyReferenceManager : IReferenceManager
    {

        public void AddUsing(string _) { }

        public void AddStaticUsing(string _) { }

        public void AddAssembly(Assembly _) { }

        public void AddTestExtensions() { }

        public void AddObjectsComparer() { }
    }

    private static string NameForType(Type? t)
    {
        if (t == null)
            return "GeneratedClass";
        var name = InternalCodeRenderer.RenderType(t).ToString();
        return
            // Filtering all non letter or digit symbols from rendered name
            new string(
                (from c in name
                    where char.IsLetterOrDigit(c)
                    select c
                ).ToArray()
            );
    }

    private static string NameForMethod(MethodBase method)
    {
        if (method.IsConstructor)
            return $"{NameForType(method.DeclaringType)}Constructor";

        if (IsGetPropertyMethod(method, out var testName))
            return $"Get{testName}";

        if (IsSetPropertyMethod(method, out testName))
            return $"Set{testName}";

        return method.Name;

    }

    private static void AddCall(IBlock block, bool shouldUseDecl, ExpressionSyntax callMethod)
    {
        if (shouldUseDecl)
            block.AddDecl("unused", null, callMethod);
        else
            block.AddExpression(callMethod);
    }

    private static ExpressionSyntax RenderArgument(IBlock block, object? obj, ParameterInfo parameter)
    {
        var paramType = parameter.ParameterType;
        // For this types there is no data type suffix, so if parameter type is upcast, explicit cast is needed
        var needExplicitType =
            obj is byte or sbyte or char or short or ushort
            && (paramType == typeof(object) || paramType == typeof(ValueType));
        return block.RenderObject(obj, parameter.Name, needExplicitType);
    }

    private static void RenderTest(
        MethodRenderer test,
        MethodBase method,
        IEnumerable<object> args,
        object? thisArg,
        bool isError,
        bool wrapErrors,
        Type? ex,
        object expected)
    {
        var mainBlock = test.Body;
        MethodFormat f = new MethodFormat();

        // Declaring arguments and 'this' of testing method
        IdentifierNameSyntax? thisArgId = null;
        string thisArgName = "thisArg";
        if (thisArg != null)
        {
            Debug.Assert(Reflection.hasThis(method));
            var renderedThis = mainBlock.RenderObject(thisArg, thisArgName);
            if (renderedThis is IdentifierNameSyntax id)
            {
                thisArgId = id;
            }
            else
            {
                var thisType = test.RenderType(method.DeclaringType ?? typeof(object));
                thisArgId = mainBlock.AddDecl(thisArgName, thisType, renderedThis);
            }
        }
        var parameters = method.GetParameters();
        var renderedArgs =
            args.Select((obj, index) => RenderArgument(mainBlock, obj, parameters[index])).ToArray();

        Debug.Assert(parameters.Length == renderedArgs.Length);
        for (int i = 0; i < parameters.Length; i++)
        {
            var parameterInfo = parameters[i];
            var type = parameterInfo.ParameterType;
            var value = renderedArgs[i];
            var valueIsVar = value is IdentifierNameSyntax;
            f.HasArgs |= valueIsVar;
            if (type.IsByRef && !valueIsVar)
            {
                Debug.Assert(type.GetElementType() != null);
                var typeExpr = test.RenderType(type.GetElementType()!);
                var id = mainBlock.AddDecl(parameterInfo.Name ?? "value", typeExpr, value);
                renderedArgs[i] = id;
            }
        }

        f.HasArgs |= thisArgId != null;

        // Calling testing method
        var callMethod = test.RenderCall(thisArgId, method, renderedArgs);
        f.CallingTest = callMethod.NormalizeWhitespace().ToString();

        var hasResult = Reflection.hasNonVoidResult(method) || method.IsConstructor;
        var shouldUseDecl = method.IsConstructor || IsGetPropertyMethod(method, out _);
        var shouldCompareResult = hasResult && ex == null && !isError;

        if (shouldCompareResult)
        {
            var retType = Reflection.getMethodReturnType(method);
            var isPrimitive = retType.IsPrimitive || retType == typeof(string);

            var expectedExpr =
                method.IsConstructor ? thisArgId : mainBlock.RenderObject(expected, "expected", true);
            Debug.Assert(expectedExpr != null);
            f.HasArgs |= expectedExpr is IdentifierNameSyntax;
            var resultId = mainBlock.AddDecl("result", null, callMethod);
            if (isPrimitive)
                mainBlock.AddAssertEqual(expectedExpr, resultId);
            else
                mainBlock.AddAssert(
                    RenderCall(test.CompareObjects(), expectedExpr, resultId)
                );
        }
        else if (isError && wrapErrors)
        {
            var tryBlock = mainBlock.NewBlock();
            AddCall(tryBlock, shouldUseDecl, callMethod);
            var emptyBlock = mainBlock.NewBlock().Render();
            mainBlock.AddTryCatch(tryBlock.Render(), emptyBlock);
        }
        else if (ex == null || isError && !wrapErrors)
        {
            AddCall(mainBlock, shouldUseDecl, callMethod);
        }
        else
        {
            // Handling exceptions
            Debug.Assert(ex != null);
            LambdaExpressionSyntax delegateExpr;
            if (shouldUseDecl)
            {
                var block = mainBlock.NewBlock();
                block.AddDecl("unused", null, callMethod);
                delegateExpr = ParenthesizedLambdaExpression(block.Render());
            }
            else
                delegateExpr = ParenthesizedLambdaExpression(callMethod);

            var assertThrows =
                RenderCall(
                    "Assert", "Throws",
                    new []{ test.RenderType(ex) },
                    delegateExpr
                );
            mainBlock.AddExpression(assertThrows);
        }
        MethodsFormat[test.MethodId.ToString()] = f;
    }

    private static (ArrayTypeSyntax, SimpleNameSyntax) RenderMockedMethod(
        TypeRenderer mock,
        Mocking.Method method)
    {
        var m = method.BaseMethod;
        var returnType = (ArrayTypeSyntax) mock.RenderType(m.ReturnType.MakeArrayType());
        var valuesFieldName = $"_clauses{m.Name}";
        var emptyArray =
            RenderCall(mock.SystemArray, "Empty", new [] { mock.RenderType(m.ReturnType) });
        var valuesField =
            mock.AddField(returnType, valuesFieldName, new[] { Private }, emptyArray);
        var currentName = $"_currentClause{m.Name}";
        var zero = RenderLiteral(0);
        var currentValueField =
            mock.AddField(mock.RenderType(typeof(int)), currentName, new[] { Private }, zero);

        var setupMethod =
            mock.AddMethod(
                $"Setup{m.Name}Clauses",
                null,
                new [] { Public },
                mock.VoidType,
                (returnType, "clauses")
            );
        var setupBody = setupMethod.Body;
        var valuesArg = setupMethod.GetOneArg();
        setupBody.AddExpression(RenderAssignment(valuesField, valuesArg));

        var mockedMethod = mock.AddMethod(m);
        var body = mockedMethod.Body;
        var length = RenderMemberAccess(valuesField, "Length");
        var cond =
            RenderOr(
                BinaryExpression(SyntaxKind.LessThanExpression, currentValueField, zero),
                BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression, currentValueField, length)
            );
        var throwCase =
            ThrowStatement(
                RenderObjectCreation(
                    mock.RenderType(typeof(InvalidOperationException)),
                    new ExpressionSyntax[]{RenderLiteral("Invalid mock")},
                    System.Array.Empty<ExpressionSyntax>()
                )
            );
        body.AddIf(cond, throwCase);
        var fieldWithIncrement =
            PostfixUnaryExpression(SyntaxKind.PostIncrementExpression, currentValueField);
        var validCase =
            RenderArrayAccess(valuesField, new ExpressionSyntax[] { fieldWithIncrement });
        body.AddReturn(validCase);

        return (returnType, setupMethod.MethodId);
    }

    private static MemberDeclarationSyntax? RenderMockedType(ProgramRenderer mocksProgram, Mocking.Type typeMock)
    {
        if (HasMockInfo(typeMock.Id))
            return null;

        List<Type> superTypes = new List<Type>();
        var baseType = typeMock.BaseClass;
        var isStruct = baseType == typeof(ValueType) || baseType is { IsValueType: true };
        if (baseType != null && baseType != typeof(object) && baseType != typeof(ValueType))
            superTypes.Add(baseType);
        superTypes.AddRange(typeMock.Interfaces);
        var structPrefix = isStruct ? "Struct" : "";
        var name = structPrefix + string.Join("", superTypes.Select(NameForType));
        var mock =
            mocksProgram.AddType(
                $"{name}Mock",
                isStruct,
                superTypes,
                null,
                new []{Internal}
            );
        if (isStruct)
        {
            mock.AddConstructor(null, new[] { Public });
        }
        else if (baseType != null && baseType.GetConstructor(Type.EmptyTypes) == null)
        {
            const BindingFlags instanceFlags = BindingFlags.IgnoreCase | BindingFlags.Instance | BindingFlags.Public;
            var constructors =
                baseType.GetConstructors(instanceFlags)
                    .OrderBy(ctor => ctor.GetParameters().Length)
                    .Take(1)
                    .ToList();
            if (constructors.Count == 0)
                throw new InvalidOperationException($"Can not find public constructors to create object of {baseType}");
            var ctor = constructors[0];
            var ctorRenderer = mock.AddMethod(ctor);
            var args = ctorRenderer.GetArgs();
            ctorRenderer.CallBaseConstructor(args);
        }
        var methodsInfo = new List<(ArrayTypeSyntax, SimpleNameSyntax)>();
        foreach (var method in typeMock.Methods)
        {
            var (valuesType, setupMethod) = RenderMockedMethod(mock, method);
            methodsInfo.Add((valuesType, setupMethod));
        }

        var info = new MockInfo { MockName = mock.TypeId, MethodsInfo = methodsInfo };
        AddMockInfo(typeMock.Id, info);

        return mock.Render();
    }

    public static (SyntaxNode, SyntaxNode?, string) RenderTests(
        List<UnitTest> tests,
        bool wrapErrors = false,
        Type? declaringType = null)
    {
        AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;

        // Creating main class for generating tests
        var typeName = NameForType(declaringType);
        var namespaceName =
            declaringType == null
                ? "GeneratedNamespace"
                : $"{declaringType.Namespace}.Tests";
        var testsProgram = new ProgramRenderer(namespaceName);
        // Adding NUnit namespace to usings
        testsProgram.AddNUnitToUsigns();
        var generatedClass =
            testsProgram.AddType(
                $"{typeName}Tests",
                false,
                null,
                RenderAttributeList("TestFixture"),
                null
            );

        var mocksProgram = new ProgramRenderer(namespaceName);
        var mockedTypes = new List<MemberDeclarationSyntax>();
        foreach (var test in tests)
        {
            // Rendering mocked types
            foreach (var mock in test.TypeMocks)
            {
                var renderedMock = RenderMockedType(mocksProgram, mock);
                if (renderedMock != null)
                    mockedTypes.Add(renderedMock);
            }
            // Rendering test
            var method = test.Method;
            var parameters = test.Args ?? method.GetParameters()
                .Select(t => Reflection.defaultOf(t.ParameterType)).ToArray();
            var thisArg = test.ThisArg;
            if (thisArg == null && !method.IsStatic)
                thisArg = Reflection.createObject(method.DeclaringType);
            string suitTypeName = test.IsError ? "Error" : "Test";

            var testName = NameForMethod(method);
            bool isUnsafe =
                Reflection.getMethodReturnType(method).IsPointer || method.GetParameters()
                    .Select(arg => arg.ParameterType)
                    .Any(type => type.IsPointer);
            var modifiers = isUnsafe ? new[] { Public, Unsafe } : new[] { Public };
            var attributes =
                RenderAttributeList(
                    RenderAttribute("Test"),
                    RenderAttribute("Category", "Generated")
                );

            var testRenderer = generatedClass.AddMethod(
                testName + suitTypeName,
                attributes,
                modifiers,
                generatedClass.VoidType,
                System.Array.Empty<(TypeSyntax, string)>()
            );
            RenderTest(testRenderer, method, parameters, thisArg, test.IsError,
                wrapErrors, test.Exception, test.Expected);
        }

        var mockedTypesArray = mockedTypes.ToArray();
        SyntaxNode? renderedMocksProgram = null;
        if (mockedTypesArray.Length > 0)
            renderedMocksProgram = Format(mocksProgram.Render());

        SyntaxNode renderedTestsProgram = Format(testsProgram.Render());

        return (renderedTestsProgram, renderedMocksProgram, typeName);
    }
}
