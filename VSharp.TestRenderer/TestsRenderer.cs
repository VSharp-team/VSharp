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

    // TODO: create class 'Expression' with operators?

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
        private string? _firstArrangeExpr = null;
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
                case StatementSyntax statement when statement.ToString() == _firstArrangeExpr:
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
                    _firstArrangeExpr = null;
                    MethodsFormat.TryGetValue(expr.Identifier.ToString(), out _format);
                    if (_format.HasArgs)
                        _firstArrangeExpr = expr.Body?.Statements[0].ToString();

                    return base.Visit(node);
                }
                // Adding blank line and '// assert' comment before assert
                case ExpressionStatementSyntax expr when expr.ToString().Contains("Assert"):
                {
                    var whitespaces = CurrentOffset();
                    if (node.Parent?.Parent is MethodDeclarationSyntax { Body.Statements.Count: 1 })
                        node = node.WithLeadingTrivia(whitespaces, AssertComment, LineFeed, whitespaces);
                    else
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

    public static SyntaxNode? Format(SyntaxNode? node)
    {
        if (node == null) return null;

        var normalized = node.NormalizeWhitespace();
        var myRewriter = new IndentsRewriter();
        var formatted = myRewriter.Visit(normalized);
        Debug.Assert(formatted != null);
        return formatted;
    }

    public static CompilationUnitSyntax? Format(CompilationUnitSyntax? node)
    {
        return Format(node as SyntaxNode) as CompilationUnitSyntax;
    }

    private static string NameForNamespace(string str)
    {
        return
            new string(
                str.Select(c =>
                    c is '-' or '<' or '>' or '`' or ','
                        ? '_'
                        : c
                ).ToArray()
            );
    }

    private static string NameForType(Type? t)
    {
        if (t == null)
            return "GeneratedClass";

        var name = t.Name;
        var typeArgsStart = name.IndexOf('`');
        if (typeArgsStart >= 0)
            name = name.Remove(typeArgsStart);

        if (t.IsGenericType)
        {
            var typeArgs = String.Join("_", t.GetGenericArguments().Select(NameForType));
            return $"{name}_{typeArgs}";
        }

        return name;
    }

    private static string NameForMethod(MethodBase method)
    {
        if (method.IsConstructor)
            return $"{NameForType(method.DeclaringType)}Constructor";

        if (IsPropertyMethod(method, out var testName, out var accessorType))
        {
            return accessorType switch
            {
                AccessorType.Get => $"Get{testName}",
                AccessorType.Set => $"Set{testName}",
                _ => throw new ArgumentOutOfRangeException()
            };
        }

        return method.Name.Split('.').Last();

    }

    private static void AddCall(IBlock block, bool shouldUseDecl, ExpressionSyntax callMethod)
    {
        if (shouldUseDecl)
            block.AddDecl("unused", null, callMethod);
        else
            block.AddExpression(callMethod);
    }

    private static ExpressionSyntax RenderArgument(IBlock block, object? obj, ParameterInfo parameter, bool hasOverloads)
    {
        var needExplicitType = NeedExplicitType(obj, parameter.ParameterType) || hasOverloads;
        var correctName =
            parameter.Name is null
                ? null
                : CorrectNameGenerator.GetVariableName(parameter.Name);
        var explicitType = needExplicitType ? parameter.ParameterType : null;
        return block.RenderObject(obj, correctName, explicitType);
    }

    private static void RenderTest(
        MethodRenderer test,
        MethodBase method,
        IEnumerable<object> args,
        object? thisArg,
        bool isError,
        bool wrapErrors,
        Type? ex,
        object? expected)
    {
        var mainBlock = test.Body;
        MethodFormat f = new MethodFormat();

        // Declaring arguments and 'this' of testing method
        IdentifierNameSyntax? thisArgId = null;
        string thisArgName = "thisArg";
        Type? thisArgType = null;
        if (thisArg != null)
        {
            Debug.Assert(Reflection.hasThis(method));
            thisArgType = thisArg.GetType();
            var needExplicitType = thisArg is Delegate ? thisArgType : null;
            var renderedThis = mainBlock.RenderObject(thisArg, thisArgName, needExplicitType);
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
        var hasOverloads =
            method
                .DeclaringType?
                .GetMethods(Reflection.allBindingFlags)
                .Count(m => m.Name == method.Name) > 1;
        var renderedArgs =
            args.Select((obj, index) =>
                RenderArgument(mainBlock, obj, parameters[index], hasOverloads)).ToArray();

        Debug.Assert(parameters.Length == renderedArgs.Length);
        for (int i = 0; i < parameters.Length; i++)
        {
            var parameterInfo = parameters[i];
            var type = parameterInfo.ParameterType;
            var value = renderedArgs[i];
            if (type.IsByRef && value is not IdentifierNameSyntax)
            {
                Debug.Assert(type.GetElementType() != null);
                var typeExpr = test.RenderType(type.GetElementType()!);
                var id = mainBlock.AddDecl(parameterInfo.Name ?? "value", typeExpr, value);
                renderedArgs[i] = id;
            }
        }
        // If where were statements before calling test, need 'arrange' comment
        f.HasArgs = mainBlock.StatementsCount() > 0;

        // Calling testing method
        var callMethod = test.RenderCall(thisArgId, thisArgType, method, renderedArgs);
        f.CallingTest = callMethod.NormalizeWhitespace().ToString();

        var hasResult = Reflection.hasNonVoidResult(method) || method.IsConstructor;
        var shouldUseDecl = method.IsConstructor || IsPropertyMethod(method, out _, out var accessorType) && accessorType == AccessorType.Get ;
        var shouldCompareResult = hasResult && ex == null && !isError;

        if (shouldCompareResult)
        {
            var retType = Reflection.getMethodReturnType(method);
            var isPrimitive = retType.IsPrimitive || retType == typeof(string);

            var expectedExpr =
                method.IsConstructor
                    ? thisArgId
                    : mainBlock.RenderObject(expected, "expected", retType);
            Debug.Assert(expectedExpr != null);
            // If rendering expected result added statements to method block, need 'arrange' comment
            f.HasArgs |= mainBlock.StatementsCount() > 0;
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
        test.Render();
        MethodsFormat[test.MethodId.ToString()] = f;
    }

    private static (Type, SimpleNameSyntax, SimpleNameSyntax) RenderNonVoidMockedMethod(
        TypeRenderer mock,
        MethodInfo m)
    {
        // Rendering mocked method clauses
        var returnType = m.ReturnType.MakeArrayType();
        var renderedReturnType = (ArrayTypeSyntax) mock.RenderType(returnType);
        var methodName = CorrectNameGenerator.GetVariableName(m);
        var valuesFieldName = $"_clauses{methodName}";
        var emptyArray =
            m.ReturnType.IsPointer
                ? RenderEmptyArrayInitializer()
                : RenderCall(mock.SystemArray, "Empty", new [] { mock.RenderType(m.ReturnType) });
        var valuesField =
            mock.AddField(renderedReturnType, valuesFieldName, new[] { Private }, emptyArray);
        var currentName = $"_currentClause{methodName}";
        var zero = RenderLiteral(0);
        var currentValueField =
            mock.AddField(mock.RenderType(typeof(int)), currentName, new[] { Private }, zero);

        var setupMethod =
            mock.AddMethod(
                $"Setup{methodName}Clauses",
                null,
                new [] { Public },
                mock.VoidType,
                new ParameterRenderInfo("clauses", renderedReturnType)
            );
        var setupBody = setupMethod.Body;
        var valuesArg = setupMethod.GetOneArg();
        setupBody.AddExpression(RenderAssignment(valuesField, valuesArg));
        setupMethod.Render();

        // Rendering mocked method
        var mockedMethod = mock.AddMockMethod(m);
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
        mockedMethod.Render();
        return (returnType, setupMethod.MethodId, mockedMethod.MethodId);
    }

    private static SimpleNameSyntax RenderVoidMockedMethod(
        TypeRenderer mock,
        MethodInfo m)
    {
        // Rendering mocked method
        var mockedMethod = mock.AddMockMethod(m);
        mockedMethod.Render();
        return mockedMethod.MethodId;
    }

    private static void RenderMockConstructor(TypeRenderer mock, Type baseType)
    {
        const BindingFlags instanceFlags = BindingFlags.IgnoreCase | BindingFlags.Instance | BindingFlags.Public;
        var constructor =
            baseType.GetConstructors(instanceFlags)
                .MinBy(ctor => ctor.GetParameters().Length);
        if (constructor == null)
            return;
        var ctorRenderer = mock.AddMockMethod(constructor);
        var args = ctorRenderer.GetArgs();
        ctorRenderer.CallBaseConstructor(args);
        ctorRenderer.Render();
    }

    private static void RenderMockMethods(
        TypeRenderer mock,
        Mocking.Type typeMock,
        bool isDelegate)
    {
        var methodsInfo = new List<(MethodInfo, Type, SimpleNameSyntax)>();

        SimpleNameSyntax? methodId = null;

        foreach (var method in typeMock.MethodMocks)
        {
            var m = method.BaseMethod;
            if (Reflection.hasNonVoidResult(m))
            {
                var (returnArrayType, setupClausesId, mockMethodId) =
                    RenderNonVoidMockedMethod(mock, m);
                methodsInfo.Add((m, returnArrayType, setupClausesId));
                methodId = mockMethodId;
            }
            else
            {
                methodId = RenderVoidMockedMethod(mock, m);
            }
        }

        MockInfo info;
        if (isDelegate)
        {
            var baseClass = typeMock.BaseClass;
            Debug.Assert(typeMock.MethodMocks.Count() == 1 && methodId != null && baseClass != null);
            info = new DelegateMockInfo(mock.TypeId, typeMock, methodsInfo, methodId, baseClass);
        }
        else
        {
            info = new MockInfo(mock.TypeId, typeMock, methodsInfo);
        }
        AddMockInfo(typeMock.Id, info);
    }

    private static MemberDeclarationSyntax? RenderMockedType(ProgramRenderer mocksProgram, Mocking.Type typeMock)
    {
        if (HasMockInfo(typeMock.Id))
            return null;

        List<Type> superTypes = new List<Type>();
        var baseType = typeMock.BaseClass;
        var isDelegate = TypeUtils.isDelegate(baseType);
        var isStruct = baseType == typeof(ValueType) || baseType is { IsValueType: true };
        // It's useless to derive from object
        // If base type is ValueType, this means, that mock is struct
        // Class can not be derived from delegate
        if (baseType != null && baseType != typeof(object) && baseType != typeof(ValueType) && !isDelegate)
            superTypes.Add(baseType);
        else baseType = null;
        superTypes.AddRange(typeMock.Interfaces);
        var structPrefix = isStruct ? "Struct" : "";
        var name =
            isDelegate
                ? NameForType(typeMock.BaseClass)
                : structPrefix + string.Join("", superTypes.Select(NameForType));
        var isUnsafe = typeMock.MethodMocks.Any(m => m.BaseMethod.ReturnType.IsPointer);
        var modifiers = isUnsafe ? new[] { Internal, Unsafe } : new[] { Internal };
        mocksProgram.AddExtensionsToUsings();
        var attributes = RenderAttributeList("Generated");
        var mock =
            mocksProgram.AddType(
                $"{name}Mock",
                isStruct,
                superTypes.Count > 0 ? superTypes : null,
                attributes,
                modifiers
            );
        if (isStruct)
        {
            var ctor = mock.AddConstructor(null, new[] { Public });
            ctor.Render();
        }
        else if (baseType != null && baseType.GetConstructor(Type.EmptyTypes) == null)
        {
            RenderMockConstructor(mock, baseType);
        }

        RenderMockMethods(mock, typeMock, isDelegate);

        return mock.Render();
    }

    private static string RenderTestsForType(
        IEnumerable<UnitTest> tests,
        ProgramRenderer testsProgram,
        ProgramRenderer mocksProgram,
        Type declaringType,
        bool wrapErrors)
    {
        // Creating class for generating tests
        var typeName = $"{NameForType(declaringType)}Tests";
        var generatedClass =
            testsProgram.AddType(
                typeName,
                false,
                null,
                RenderAttributeList("TestFixture"),
                null
            );

        foreach (var test in tests)
        {
            try
            {
                var method = test.Method;
                var methodType = method.DeclaringType;
                CompactRepresentations = test.CompactRepresentations;
                BoxedLocations = test.BoxedLocations;
                Debug.Assert(methodType != null);
                Debug.Assert(
                    methodType == declaringType
                    || methodType.IsGenericType && declaringType.IsGenericType
                    && methodType.GetGenericTypeDefinition() == declaringType.GetGenericTypeDefinition());

                if (method.IsConstructor)
                    throw new NotImplementedException("rendering constructors not supported yet");

                // Rendering mocked types
                foreach (var mock in test.TypeMocks)
                    // TODO: cache mocks among all generated tests
                    RenderMockedType(mocksProgram, mock);

                // Rendering test
                var parameters = test.Args ?? method.GetParameters()
                    .Select(t => Reflection.defaultOf(t.ParameterType)).ToArray();
                var thisArg = test.ThisArg;
                if (thisArg == null && !method.IsStatic)
                    thisArg = Reflection.createObject(methodType);
                string suiteTypeName = test.IsError ? "Error" : "Test";

                var testName = NameForMethod(method);
                bool isUnsafe =
                    Reflection.getMethodReturnType(method).IsPointer || method.GetParameters()
                        .Select(arg => arg.ParameterType)
                        .Any(type => type.IsPointer);
                var modifiers = isUnsafe ? new[] { Public, Unsafe } : new[] { Public };

                var attributeList = new List<AttributeSyntax>
                {
                    RenderAttribute("Test"),
                    RenderAttribute("Category", "Generated")
                };
                if (test.IsFatalError)
                    attributeList.Add(RenderAttribute("Category", "FatalError"));
                var attributes = RenderAttributeList(attributeList);

                var testRenderer = generatedClass.AddMethod(
                    testName + suiteTypeName,
                    attributes,
                    modifiers,
                    generatedClass.VoidType,
                    System.Array.Empty<ParameterRenderInfo>()
                );
                RenderTest(testRenderer, method, parameters, thisArg, test.IsError,
                    wrapErrors, test.Exception, test.Expected);
            }
            catch (Exception e)
            {
                Logger.printLogString(Logger.Error, $"Tests renderer: rendering test failed: {e}");
            }
        }
        generatedClass.Render();
        return typeName;
    }

    internal class ProgramsBuilder
    {
        private readonly List<(CompilationUnitSyntax, string)> _programs = new();
        private readonly HashSet<Assembly> _references = new();

        internal void AddProgram(ProgramRenderer program, string fileName)
        {
            var rendered = Format(program.Render());

            if (rendered == null) return;

            _programs.Add((rendered, fileName));
            _references.UnionWith(program.UsedAssemblies());
        }

        internal bool IsEmpty => _programs.Count == 0;

        internal void Union(ProgramsBuilder other)
        {
            _programs.AddRange(other._programs);
            _references.UnionWith(other._references);
        }

        internal List<(CompilationUnitSyntax, string)> Programs => _programs;

        internal IEnumerable<Assembly> References => _references;
    }

    private static ProgramsBuilder RenderProgramForType(
        IEnumerable<UnitTest> tests,
        string namespaceName,
        ProgramRenderer mocksProgram,
        Type declaringType,
        bool wrapErrors)
    {
        // Creating main program for generating tests
        var testsProgram = new ProgramRenderer(namespaceName);

        // Adding NUnit namespace to usings
        testsProgram.AddNUnitToUsings();

        var typeName = RenderTestsForType(tests, testsProgram, mocksProgram, declaringType, wrapErrors);

        var results = new ProgramsBuilder();

        results.AddProgram(testsProgram, typeName);

        return results;
    }

    private static ProgramsBuilder RenderSingleFile(
        List<UnitTest> tests,
        string namespaceName,
        string testProjectName,
        bool wrapErrors = false,
        Type? declaringType = null)
    {
        var program = new ProgramRenderer(namespaceName);
        program.AddNUnitToUsings();

        if (declaringType != null)
        {
            RenderTestsForType(tests, program, program, declaringType, wrapErrors);
        }
        else
        {
            foreach (var unitTests in tests.GroupBy(test => test.Method.DeclaringType))
            {
                declaringType = unitTests.Key;
                Debug.Assert(declaringType != null);

                RenderTestsForType(unitTests, program, program, declaringType, wrapErrors);
            }
        }

        var renderedPrograms = new ProgramsBuilder();

        renderedPrograms.AddProgram(program, testProjectName);

        if (renderedPrograms.IsEmpty)
            throw new Exception("Tests renderer: no tests were generated!");

        return renderedPrograms;
    }

    private static ProgramsBuilder RenderMultipleFiles(
        List<UnitTest> tests,
        string namespaceName,
        bool wrapErrors = false,
        Type? declaringType = null)
    {
        var mocks = new ProgramRenderer(namespaceName);
        var renderedPrograms = new ProgramsBuilder();

        if (declaringType != null)
        {
            renderedPrograms = RenderProgramForType(tests, namespaceName, mocks, declaringType, wrapErrors);
        }
        else
        {
            foreach (var unitTests in tests.GroupBy(test => test.Method.DeclaringType))
            {
                declaringType = unitTests.Key;
                Debug.Assert(declaringType != null);

                var renderedTestsProgram =
                    RenderProgramForType(unitTests, namespaceName, mocks, declaringType, wrapErrors);
                if (!renderedTestsProgram.IsEmpty)
                    renderedPrograms.Union(renderedTestsProgram);
            }
        }

        renderedPrograms.AddProgram(mocks, "Mocks");

        if (renderedPrograms.IsEmpty)
            throw new Exception("Tests renderer: no tests were generated!");

        return renderedPrograms;
    }

    internal static ProgramsBuilder RenderTests(
        List<UnitTest> tests,
        string testProjectName,
        bool wrapErrors = false,
        bool singleFile = false,
        Type? declaringType = null)
    {
        PrepareCache();
        MethodsFormat.Clear();

        testProjectName = NameForNamespace(testProjectName);
        var namespaceName =
            declaringType == null || string.IsNullOrEmpty(declaringType.Namespace)
                ? testProjectName
                : $"{declaringType.Namespace}.Tests";

        if (singleFile)
            return RenderSingleFile(tests, namespaceName, testProjectName, wrapErrors, declaringType);

        return RenderMultipleFiles(tests, namespaceName, wrapErrors, declaringType);
    }
}
