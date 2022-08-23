using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

public static class Renderer
{
    // TODO: create non-static class for one test rendering?
    private static IEnumerable<string>? _extraAssemblyLoadDirs;

    // Program parts
    private static readonly Dictionary<string, IdentifierNameSyntax> VarsInitializers = new ();
    private static readonly Dictionary<string, SyntaxToken> Identifiers = new ();
    private static IdentifierNameSyntax _assemblyId = null!;

    // Prerendered program types
    private static readonly TypeSyntax ObjectType = ParseTypeName("object");
    private static readonly TypeSyntax VoidType = ParseTypeName("void");
    private static readonly ArrayTypeSyntax VectorOfObjects = RenderVectorType("object");
    private static readonly TypeSyntax MethodBaseType = ParseTypeName("MethodBase");
    private static readonly TypeSyntax ModuleType = ParseTypeName("Module");
    private static readonly TypeSyntax AssemblyType = ParseTypeName("Assembly");
    private static readonly TypeSyntax SystemType = ParseTypeName("Type");
    private static readonly TypeSyntax TargetInvocationExceptionType = ParseTypeName("TargetInvocationException");

    // Prerendered 'BindingFlags' expression
    private static readonly ExpressionSyntax BindingFlags =
        BinaryExpression(
            SyntaxKind.BitwiseOrExpression,
            RenderMemberAccess("BindingFlags", "Static"),
            BinaryExpression(
                SyntaxKind.BitwiseOrExpression,
                RenderMemberAccess("BindingFlags", "NonPublic"),
                BinaryExpression(
                    SyntaxKind.BitwiseOrExpression,
                    RenderMemberAccess("BindingFlags", "Public"),
                    RenderMemberAccess("BindingFlags", "Instance")
                )
            )
        );

    static Renderer()
    {
        IdGenerator.reset();
    }

    private static void ResetCache()
    {
        IdGenerator.restore();
        IdGenerator.reset();
        VarsInitializers.Clear();
        Identifiers.Clear();
        _assemblyId = null!;
    }

    private static (SyntaxToken, IdentifierNameSyntax) GenerateIdentifier(string identifierName)
    {
        identifierName = IdGenerator.startingWith(identifierName);
        SyntaxToken identifier = Identifier(identifierName);
        if (!Identifiers.TryAdd(identifierName, identifier))
            throw new ArgumentException("ID generator failed!");

        return (identifier, IdentifierName(identifierName));
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

    private static ExpressionSyntax RenderArray(List<StatementSyntax> block, Array obj)
    {
        var rank = obj.Rank;
        var elemTypeName = obj.GetType().GetElementType()?.ToString();
        if (elemTypeName == null)
            throw new ArgumentException();
        var type = RenderArrayType(elemTypeName, obj.Rank);
        var initializer = new List<ExpressionSyntax>();
        if (rank > 1)
        {
            throw new NotImplementedException("implement rendering for non-vector arrays");
            // for (int i = 0; i < obj.Rank; i++)
            // {
            //     var innerInitializer = new List<ExpressionSyntax>();
            //     for (int j = obj.GetLowerBound(i); j <= obj.GetUpperBound(i); j++)
            //     {
            //         
            //     }
            // }
        }
        else
        {
            for (int i = obj.GetLowerBound(0); i <= obj.GetUpperBound(0); i++)
            {
                // TODO: if lower bound != 0, use Array.CreateInstance
                initializer.Add(RenderObject(block, obj.GetValue(i)));
            }
        }

        return RenderArrayCreation(type, initializer);
    }

    private static ExpressionSyntax RenderFields(List<StatementSyntax> block, object obj)
    {
        var type = obj.GetType();
        var typeId = AddTypeDecl(block, type);
        // TODO: minimize
        var getUninitializedObjectId = RenderMemberAccess("FormatterServices", "GetUninitializedObject");
        var getUninitializedObject = RenderCall(getUninitializedObjectId, typeId);
        var objId = AddDecl(block, "obj", ObjectType, getUninitializedObject);
        var fields = Reflection.fieldsOf(false, type);
        foreach (var (_, fieldInfo) in fields)
        {
            var getFieldId = RenderMemberAccess(typeId, "GetField");
            var getField = RenderCall(getFieldId, RenderLiteral(fieldInfo.Name), BindingFlags);
            var setValueId = RenderMemberAccess(getField, "SetValue");
            var fieldValue = fieldInfo.GetValue(obj);
            var setValue = RenderCall(setValueId, objId, RenderObject(block, fieldValue));
            block.Add(ExpressionStatement(setValue));
        }
        return objId;
    }

    private static ExpressionSyntax RenderObject(List<StatementSyntax> block, object? obj) => obj switch
    {
        null        => LiteralExpression(SyntaxKind.NullLiteralExpression),
        true        => LiteralExpression(SyntaxKind.TrueLiteralExpression),
        false       => LiteralExpression(SyntaxKind.FalseLiteralExpression),
        byte n      => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        sbyte n     => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        char n      => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        short n     => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        ushort n    => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        int n       => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        uint n      => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        long n      => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        ulong n     => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        float n     => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        double n    => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        decimal n   => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        nuint n     => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        nint n      => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
        string s    => LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(s)),
        Array a     => RenderArray(block, a),
        ValueType   => RenderFields(block, obj),
        _ when obj.GetType().IsPointer => throw new NotImplementedException("implement rendering of pointers"),
        _           => RenderFields(block, obj)
    };

    private static InvocationExpressionSyntax RenderCall(ExpressionSyntax function)
    {
        return
            InvocationExpression(
                function,
                ArgumentList()
            );
    }

    private static InvocationExpressionSyntax RenderCall(ExpressionSyntax function, params ExpressionSyntax[] args)
    {
        return
            InvocationExpression(
                function,
                ArgumentList(
                    SeparatedList(args.Select(Argument))
                )
            );
    }

    private static InvocationExpressionSyntax RenderCall(ExpressionSyntax function, params string[] args)
    {
        return
            InvocationExpression(
                function,
                ArgumentList(
                    SeparatedList(
                        args.Select(RenderLiteral).Select(Argument)
                    )
                )
            );
    }

    private static VariableDeclarationSyntax RenderVarDecl(TypeSyntax type, SyntaxToken var, ExpressionSyntax init)
    {
        return
            VariableDeclaration(
                type,
                SingletonSeparatedList(
                    VariableDeclarator(var).WithInitializer(EqualsValueClause(init))
                )
            );
    }

    private static ArrayCreationExpressionSyntax RenderArrayCreation(ArrayTypeSyntax type, IEnumerable<ExpressionSyntax> init)
    {
        return
            ArrayCreationExpression(
                Token(SyntaxKind.NewKeyword),
                type,
                InitializerExpression(
                    SyntaxKind.ArrayInitializerExpression,
                    SeparatedList(init)
                )
            );
    }

    private static ObjectCreationExpressionSyntax RenderObjectCreation(TypeSyntax type, params ExpressionSyntax[] init)
    {
        return
            ObjectCreationExpression(
                Token(SyntaxKind.NewKeyword),
                type,
                null,
                InitializerExpression(
                    SyntaxKind.ObjectInitializerExpression,
                    SeparatedList(init)
                )
            );
    }

    private static ExpressionSyntax RenderMemberAccess(ExpressionSyntax memberOf, params string[] memberNames)
    {
        Debug.Assert(memberNames.Length > 0);
        ExpressionSyntax result = memberOf;
        for (int i = 0; i < memberNames.Length; i++)
        {
            result =
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    result,
                    IdentifierName(memberNames[i])
                );
        }

        return result;
    }

    private static ExpressionSyntax RenderMemberAccess(params string[] memberNames)
    {
        Debug.Assert(memberNames.Length > 1);
        ExpressionSyntax result = IdentifierName(memberNames[0]);
        for (int i = 1; i < memberNames.Length; i++)
        {
            result =
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    result,
                    IdentifierName(memberNames[i])
                );
        }

        return result;
    }

    private static ArrayTypeSyntax RenderArrayType(string elemTypeName, int rank)
    {
        var dims = Enumerable.Repeat(OmittedArraySizeExpression(), rank);
        var arrayRankSpecifier = ArrayRankSpecifier(SeparatedList<ExpressionSyntax>(dims));
        return
            ArrayType(
                ParseTypeName(elemTypeName),
                SingletonList(arrayRankSpecifier)
            );
    }

    private static ArrayTypeSyntax RenderVectorType(string elemTypeName)
    {
        return RenderArrayType(elemTypeName, 1);
    }

    private static IdentifierNameSyntax AddDecl(List<StatementSyntax> block, string varName, TypeSyntax type, ExpressionSyntax init, bool reuse = false)
    {
        // TODO: to check for equality of syntax nodes use 'AreEquivalent'
        string initializerString = init.ToString();
        if (reuse && VarsInitializers.TryGetValue(initializerString, out var result))
            return result;
        var (var, varId) = GenerateIdentifier(varName);
        var varDecl = RenderVarDecl(type, var, init);
        block.Add(LocalDeclarationStatement(varDecl));
        VarsInitializers[initializerString] = varId;
        return varId;
    }

    private static LiteralExpressionSyntax RenderLiteral(string? literal)
    {
        return LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(literal ?? String.Empty));
    }

    private static LiteralExpressionSyntax RenderLiteral(int literal)
    {
        return LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(literal));
    }

    private static IdentifierNameSyntax AddModuleDecl(List<StatementSyntax> block, Module module)
    {
        // TODO: care about dynamic modules (mocks and others)
        var getModule =
            RenderCall(
                RenderMemberAccess(_assemblyId, "GetModule"), 
                RenderLiteral(module.Name)
            );
        return AddDecl(block, "module", ModuleType, getModule, true);
    }

    private static IdentifierNameSyntax AddTypeDecl(List<StatementSyntax> block, Type type)
    {
        var moduleId = AddModuleDecl(block, type.Module);
        var getType =
            RenderCall(
                RenderMemberAccess(moduleId, "GetType"),
                RenderLiteral(type.FullName)
            );
        return AddDecl(block, "type", SystemType, getType, true);
    }

    private static IdentifierNameSyntax AddAssemblyDecl(List<StatementSyntax> block, Assembly assembly)
    {
        var loadAssembly =
            RenderCall(
                RenderMemberAccess(IdentifierName("Assembly"), "LoadFrom"),
                RenderLiteral(assembly.Location)
            );
        return AddDecl(block, "assembly", AssemblyType, loadAssembly, true);
    }

    private static AttributeListSyntax RenderAttributeList(params string[] attributeNames)
    {
        var attributes = attributeNames.Select(s => Attribute(IdentifierName(s)));
        return AttributeList(SeparatedList(attributes));
    }

    private static StatementSyntax RenderAssert(ExpressionSyntax condition)
    {
        var assertId = RenderMemberAccess("Assert", "IsTrue");
        return ExpressionStatement(RenderCall(assertId, condition));
    }

    private static StatementSyntax RenderAssertEqual(ExpressionSyntax x, ExpressionSyntax y)
    {
        var assertId = RenderMemberAccess("Assert", "AreEqual");
        return ExpressionStatement(RenderCall(assertId, x, y));
    }

    private static MethodDeclarationSyntax RenderTest(
        MethodBase method,
        int i,
        IEnumerable<object> args,
        object? thisArg,
        Type? ex,
        object expected)
    {
        ResetCache();
        var globalStatements = new List<StatementSyntax>();

        // NOTE: declaring assembly and module of testing method 
        var methodModule = method.Module;
        _assemblyId = AddAssemblyDecl(globalStatements, methodModule.Assembly);
        var moduleId = AddModuleDecl(globalStatements, methodModule);
        var resolveMethod =
            RenderCall(
                RenderMemberAccess(moduleId, "ResolveMethod"),
                RenderLiteral(method.MetadataToken)
            );
        var methodId = AddDecl(globalStatements, "method", MethodBaseType, resolveMethod);

        // NOTE: declaring arguments and 'this' of testing method
        var createArray = RenderArrayCreation(VectorOfObjects, args.Select(o => RenderObject(globalStatements, o)));
        var argsId = AddDecl(globalStatements, "args", VectorOfObjects, createArray);
        var thisArgId = AddDecl(globalStatements, "thisArg", ObjectType, RenderObject(globalStatements, thisArg));

        // NOTE: calling testing method
        var invokeMethod =
            RenderCall(
                RenderMemberAccess(methodId, "Invoke"),
                thisArgId, argsId
            );

        // NOTE: handling exceptions
        // TODO: use Assert.Throws instead of catch clause
        BlockSyntax body;
        if (ex == null)
        {
            var resultId = AddDecl(globalStatements, "result", ObjectType, invokeMethod);
            // TODO: add object comparison function 
            var resultAssert = RenderAssertEqual(resultId, RenderObject(globalStatements, expected));
            body = Block(globalStatements).AddStatements(resultAssert);
        }
        else
        {
            var tryStatements = new List<StatementSyntax>();
            AddDecl(tryStatements, "result", ObjectType, invokeMethod);
            var (exception, exceptionId) = GenerateIdentifier("ex");
            var innerException = RenderMemberAccess(exceptionId, "InnerException");
            var notNull =
                BinaryExpression(SyntaxKind.NotEqualsExpression, innerException,
                    LiteralExpression(SyntaxKind.NullLiteralExpression));
            var exGetTypeId = RenderMemberAccess(innerException, "GetType");
            var exGetType = RenderCall(exGetTypeId);
            var catchStatements = new List<StatementSyntax>();
            var expectedExType = AddTypeDecl(catchStatements, ex);
            var eq =
                BinaryExpression(SyntaxKind.EqualsExpression, exGetType, expectedExType);
            var condition =
                BinaryExpression(SyntaxKind.LogicalAndExpression, notNull, eq);
            catchStatements.Add(RenderAssert(condition));
            var declaration = CatchDeclaration(TargetInvocationExceptionType, exception);
            var catchClause = CatchClause(declaration, null, Block(catchStatements));
            var clauses = SingletonList(catchClause);
            var tryCatchBlock = TryStatement(Block(tryStatements), clauses, null);
            body = Block(globalStatements).AddStatements(tryCatchBlock);
        }

        return
            MethodDeclaration(VoidType, $"{method.Name}Test{i}")
                .AddAttributeLists(RenderAttributeList("Test"))
                .AddModifiers(Token(SyntaxKind.PublicKeyword), Token(SyntaxKind.StaticKeyword))
                .WithBody(body);
    }

    public static void RenderTests(IEnumerable<FileInfo> tests)
    {
        AppDomain.CurrentDomain.AssemblyResolve += TryLoadAssemblyFrom;
        MemberDeclarationSyntax[] generatedTests = new MemberDeclarationSyntax[tests.Count()];
        int i = 0;
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

            // Console.Out.WriteLine("Starting rendering test for method {0}", method);
            object[] parameters = test.Args ?? method.GetParameters()
                .Select(t => Reflection.defaultOf(t.ParameterType)).ToArray();
            object thisArg = test.ThisArg;
            if (thisArg == null && !method.IsStatic)
                thisArg = Reflection.createObject(method.DeclaringType);
            generatedTests[i] = RenderTest(method, i, parameters, thisArg, test.Exception, test.Expected);
            i++;
        }

        var comp =
            CompilationUnit()
                .AddUsings(
                    UsingDirective(ParseName("System")),
                    UsingDirective(ParseName("System.Reflection")), 
                    UsingDirective(ParseName("System.Runtime.Serialization")),
                    UsingDirective(ParseName("System.Diagnostics")),
                    UsingDirective(ParseName("NUnit.Framework"))
                )
                .AddMembers(
                    NamespaceDeclaration(IdentifierName("GeneratedNamespace"))
                        .AddMembers(
                            ClassDeclaration("GeneratedClass")
                                .AddAttributeLists(RenderAttributeList("TestFixture"))
                                .AddMembers(generatedTests)
                        )
                );

        // comp.NormalizeWhitespace().WriteTo(Console.Out);
        var dir = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}generated.cs";
        using var streamWriter = new StreamWriter(File.Create(dir));
        comp.NormalizeWhitespace().WriteTo(streamWriter);
    }
}
