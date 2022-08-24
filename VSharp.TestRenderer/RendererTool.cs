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

    private static LiteralExpressionSyntax RenderLiteral(string? literal)
    {
        return LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(literal ?? String.Empty));
    }

    private static LiteralExpressionSyntax RenderLiteral(int literal)
    {
        return LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(literal));
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

    private static (IdentifierNameSyntax, CatchDeclarationSyntax) RenderCatchDeclaration(TypeSyntax catchType)
    {
        var (exception, exceptionId) = GenerateIdentifier("ex");
        var declaration = CatchDeclaration(catchType, exception);
        return (exceptionId, declaration);
    }

    private static ExpressionSyntax RenderExceptionCondition(IdentifierNameSyntax caughtExObj, IdentifierNameSyntax expectedExType)
    {
        var innerException = RenderMemberAccess(caughtExObj, "InnerException");
        var notNull =
            BinaryExpression(SyntaxKind.NotEqualsExpression, innerException,
                LiteralExpression(SyntaxKind.NullLiteralExpression));
        var exGetTypeId = RenderMemberAccess(innerException, "GetType");
        var exGetType = RenderCall(exGetTypeId);
        var eq = BinaryExpression(SyntaxKind.EqualsExpression, exGetType, expectedExType);
        return BinaryExpression(SyntaxKind.LogicalAndExpression, notNull, eq);
        ;
    }

    private static CompilationUnitSyntax RenderProgram(
        string[] usings,
        string namespaceName,
        string className,
        AttributeListSyntax classAttributes,
        MemberDeclarationSyntax[] methods)
    {
        var usingDetectives = new UsingDirectiveSyntax[usings.Length];
        for (var i = 0; i < usings.Length; i++)
        {
            usingDetectives[i] = UsingDirective(ParseName(usings[i]));
        }

        var renderedClass =
            ClassDeclaration(className)
                .AddAttributeLists(classAttributes)
                .AddMembers(methods);
        var renderedNamespace =
            NamespaceDeclaration(IdentifierName(namespaceName))
                .AddMembers(renderedClass);
        return
            CompilationUnit()
                .AddUsings(usingDetectives)
                .AddMembers(renderedNamespace);
    }

    internal class Block
    {
        private readonly List<StatementSyntax> _statements = new ();

        internal IdentifierNameSyntax AddDecl(string varName, TypeSyntax type, ExpressionSyntax init, bool reuse = false)
        {
            // TODO: to check for equality of syntax nodes use 'AreEquivalent'
            string initializerString = init.ToString();
            if (reuse && VarsInitializers.TryGetValue(initializerString, out var result))
                return result;
            var (var, varId) = GenerateIdentifier(varName);
            var varDecl = RenderVarDecl(type, var, init);
            _statements.Add(LocalDeclarationStatement(varDecl));
            VarsInitializers[initializerString] = varId;
            return varId;
        }

        internal IdentifierNameSyntax AddModuleDecl(Module module)
        {
            // TODO: care about dynamic modules (mocks and others)
            var getModule =
                RenderCall(
                    RenderMemberAccess(_assemblyId, "GetModule"), 
                    RenderLiteral(module.Name)
                );
            return AddDecl("module", ModuleType, getModule, true);
        }

        internal IdentifierNameSyntax AddTypeDecl(Type type)
        {
            var moduleId = AddModuleDecl(type.Module);
            var getType =
                RenderCall(
                    RenderMemberAccess(moduleId, "GetType"),
                    RenderLiteral(type.FullName)
                );
            return AddDecl("type", SystemType, getType, true);
        }

        internal IdentifierNameSyntax AddMethodDecl(MethodBase method, IdentifierNameSyntax? moduleId = null!)
        {
            moduleId ??= AddModuleDecl(method.Module);
            var resolveMethod =
                RenderCall(
                    RenderMemberAccess(moduleId, "ResolveMethod"),
                    RenderLiteral(method.MetadataToken)
                );

            return AddDecl("method", MethodBaseType, resolveMethod);
        }

        internal IdentifierNameSyntax AddAssemblyDecl(Assembly assembly)
        {
            var loadAssembly =
                RenderCall(
                    RenderMemberAccess(IdentifierName("Assembly"), "LoadFrom"),
                    RenderLiteral(assembly.Location)
                );
            return AddDecl("assembly", AssemblyType, loadAssembly, true);
        }

        internal void AddAssertEqual(ExpressionSyntax x, ExpressionSyntax y)
        {
            _statements.Add(RenderAssertEqual(x, y));
        }

        internal void AddAssert(ExpressionSyntax condition)
        {
            _statements.Add(RenderAssert(condition));
        }

        internal void AddTryCatch(BlockSyntax tryBlock, CatchDeclarationSyntax catchDecl, BlockSyntax catchBlock)
        {
            var catchClause = CatchClause(catchDecl, null, catchBlock);
            var clauses = SingletonList(catchClause);
            var tryCatchBlock = TryStatement(tryBlock, clauses, null);
            _statements.Add(tryCatchBlock);
        }

        private ExpressionSyntax RenderArray(Array obj)
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
                    initializer.Add(RenderObject(obj.GetValue(i)));
                }
            }

            return RenderArrayCreation(type, initializer);
        }

        private ExpressionSyntax RenderFields(object obj)
        {
            var type = obj.GetType();
            var typeId = AddTypeDecl(type);
            // TODO: minimize
            var getUninitializedObjectId = RenderMemberAccess("FormatterServices", "GetUninitializedObject");
            var getUninitializedObject = RenderCall(getUninitializedObjectId, typeId);
            var objId = AddDecl("obj", ObjectType, getUninitializedObject);
            var fields = Reflection.fieldsOf(false, type);
            foreach (var (_, fieldInfo) in fields)
            {
                var getFieldId = RenderMemberAccess(typeId, "GetField");
                var getField = RenderCall(getFieldId, RenderLiteral(fieldInfo.Name), BindingFlags);
                var setValueId = RenderMemberAccess(getField, "SetValue");
                var fieldValue = fieldInfo.GetValue(obj);
                var setValue = RenderCall(setValueId, objId, RenderObject(fieldValue));
                _statements.Add(ExpressionStatement(setValue));
            }
            return objId;
        }

        internal ExpressionSyntax RenderObject(object? obj) => obj switch
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
            Array a     => RenderArray(a),
            ValueType   => RenderFields(obj),
            _ when obj.GetType().IsPointer => throw new NotImplementedException("implement rendering of pointers"),
            _           => RenderFields(obj)
        };

        public BlockSyntax GetBlock()
        {
            return Block(_statements);
        }
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
        var mainBlock = new Block();

        // NOTE: declaring assembly and module of testing method 
        var methodModule = method.Module;
        _assemblyId = mainBlock.AddAssemblyDecl(methodModule.Assembly);
        var moduleId = mainBlock.AddModuleDecl(methodModule);
        var methodId = mainBlock.AddMethodDecl(method, moduleId);

        // NOTE: declaring arguments and 'this' of testing method
        var createArray = RenderArrayCreation(VectorOfObjects, args.Select(mainBlock.RenderObject));
        var argsId = mainBlock.AddDecl("args", VectorOfObjects, createArray);
        var thisArgId = mainBlock.AddDecl("thisArg", ObjectType, mainBlock.RenderObject(thisArg));

        // NOTE: calling testing method
        var invokeMethod =
            RenderCall(
                RenderMemberAccess(methodId, "Invoke"),
                thisArgId, argsId
            );

        BlockSyntax body;
        if (ex == null)
        {
            var resultId = mainBlock.AddDecl("result", ObjectType, invokeMethod);
            mainBlock.AddAssertEqual(resultId, mainBlock.RenderObject(expected));
            body = mainBlock.GetBlock();
        }
        else
        {
            // NOTE: handling exceptions
            // TODO: use Assert.Throws instead of catch clause
            var tryBlock = new Block();
            tryBlock.AddDecl("result", ObjectType, invokeMethod);
            var catchBlock = new Block();
            var (exceptionId, declaration) = RenderCatchDeclaration(TargetInvocationExceptionType);
            var expectedExType = catchBlock.AddTypeDecl(ex);
            catchBlock.AddAssert(RenderExceptionCondition(exceptionId, expectedExType));
            mainBlock.AddTryCatch(tryBlock.GetBlock(), declaration, catchBlock.GetBlock());
            body = mainBlock.GetBlock();
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

        string[] usings =
        {
            "System",
            "System.Reflection",
            "System.Runtime.Serialization",
            "System.Diagnostics",
            "NUnit.Framework"
        };
        var comp =
            RenderProgram(
                usings,
                "GeneratedNamespace",
                "GeneratedClass",
                RenderAttributeList("TestFixture"),
                generatedTests
            );

        // comp.NormalizeWhitespace().WriteTo(Console.Out);
        var dir = $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}generated.cs";
        using var streamWriter = new StreamWriter(File.Create(dir));
        comp.NormalizeWhitespace().WriteTo(streamWriter);
    }
}
