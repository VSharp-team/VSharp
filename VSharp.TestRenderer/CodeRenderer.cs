using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using VSharp.TestExtensions;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

internal class CodeRenderer
{
    private readonly IReferenceManager _referenceManager;

    public CodeRenderer(IReferenceManager referenceManager)
    {
        _referenceManager = referenceManager;
    }

    internal class MockInfo
    {
        public readonly SimpleNameSyntax MockName;
        public readonly List<(MethodInfo, ArrayTypeSyntax, SimpleNameSyntax)> SetupClauses;

        public MockInfo(SimpleNameSyntax mockName, List<(MethodInfo, ArrayTypeSyntax, SimpleNameSyntax)> setupClauses)
        {
            MockName = mockName;
            SetupClauses = setupClauses;
        }

        public bool Equals(MockInfo mockInfo)
        {
            // TODO: need to compare all parts?
            return MockName.Identifier.ToString() == mockInfo.MockName.Identifier.ToString();
        }

        public override bool Equals(object? obj)
        {
            if (obj is MockInfo other)
            {
                return Equals(other);
            }
            return false;
        }

        public override int GetHashCode()
        {
            return MockName.Identifier.ToString().GetHashCode();
        }
    }

    internal class DelegateMockInfo : MockInfo
    {
        public readonly SimpleNameSyntax DelegateMethod;
        public readonly Type DelegateType;

        public DelegateMockInfo(
            SimpleNameSyntax mockName,
            List<(MethodInfo, ArrayTypeSyntax, SimpleNameSyntax)> setupClauses,
            SimpleNameSyntax delegateMethod,
            Type delegateType) : base(mockName, setupClauses)
        {
            DelegateMethod = delegateMethod;
            DelegateType = delegateType;
        }
    }

    private static readonly Dictionary<string, MockInfo> MocksInfo = new ();

    public static void PrepareCache()
    {
        MocksInfo.Clear();
    }

    public static void AddMockInfo(string id, MockInfo info)
    {
        Debug.Assert(!MocksInfo.TryGetValue(id, out var old) || Equals(old, info));
        MocksInfo[id] = info;
    }

    public static bool HasMockInfo(string? id)
    {
        return id != null && MocksInfo.ContainsKey(id);
    }

    public static MockInfo GetMockInfo(string id)
    {
        return MocksInfo[id];
    }

    public static bool IsGetPropertyMethod(MethodBase method, out string propertyName)
    {
        var name = method.Name;
        if (method.IsSpecialName && method.DeclaringType != null && name.Contains("get_"))
        {
            propertyName = name.Substring(name.IndexOf('_') + 1);
            return method.DeclaringType.GetProperty(propertyName, Reflection.allBindingFlags) != null;
        }

        propertyName = String.Empty;
        return false;
    }

    public static bool IsSetPropertyMethod(MethodBase method, out string propertyName)
    {
        var name = method.Name;
        if (method.IsSpecialName && method.DeclaringType != null && name.Contains("set_"))
        {
            propertyName = name.Substring(name.IndexOf('_') + 1);
            return method.DeclaringType.GetProperty(propertyName, Reflection.allBindingFlags) != null;
        }

        propertyName = String.Empty;
        return false;
    }

    public static bool NeedExplicitType(object? obj, Type? containerType)
    {
        var needExplicitNumericType =
            // For this types there is no data type suffix, so if parameter type is upcast, explicit cast is needed
            obj is byte or sbyte or short or ushort
            && (containerType == typeof(object) || containerType == typeof(ValueType));
        var needExplicitDelegateType =
            // Member group can not be upcasted to object, so explicit delegate type is needed
            obj is Delegate && containerType == typeof(object);
        return needExplicitNumericType || needExplicitDelegateType;
    }

    internal static readonly Dictionary<Type, string> PrimitiveTypes = new()
        {
            [typeof(void)] = "void",
            [typeof(byte)] = "byte",
            [typeof(sbyte)] = "sbyte",
            [typeof(short)] = "short",
            [typeof(ushort)] = "ushort",
            [typeof(int)] = "int",
            [typeof(uint)] = "uint",
            [typeof(long)] = "long",
            [typeof(ulong)] = "ulong",
            [typeof(char)] = "char",
            [typeof(float)] = "float",
            [typeof(double)] = "double",
            [typeof(decimal)] = "decimal",
            [typeof(bool)] = "bool",
            [typeof(object)] = "object",
            [typeof(string)] = "string"
        };

    internal static readonly HashSet<string> CSharpKeywords = new()
        {
            "bool", "byte", "sbyte", "short", "ushort", "int", "uint", "long", "ulong",
            "double", "float", "decimal", "string", "char", "void", "object", "typeof",
            "sizeof", "null", "true", "false", "if", "else", "while", "for", "foreach",
            "do", "switch", "case", "default", "lock", "try", "throw", "catch", "finally",
            "goto", "break", "continue", "return", "public", "private", "internal", "protected",
            "static", "readonly", "sealed", "const", "fixed", "stackalloc", "volatile", "new",
            "override", "abstract", "virtual", "event", "extern", "ref", "out", "in", "is", "as",
            "params", "__arglist", "__makeref", "__reftype", "__refvalue", "this", "base",
            "namespace", "using", "class", "struct", "interface", "enum", "delegate", "checked",
            "unchecked", "unsafe", "operator", "implicit", "explicit"
        };

    private static ArrayTypeSyntax RenderArrayType(TypeSyntax elemType, int rank)
    {
        var dims = Enumerable.Repeat(OmittedArraySizeExpression(), rank);
        var arrayRankSpecifier = ArrayRankSpecifier(SeparatedList<ExpressionSyntax>(dims));
        return
            ArrayType(
                elemType,
                SingletonList(arrayRankSpecifier)
            );
    }

    public static GenericNameSyntax RenderGenericName(string name, params TypeSyntax[] genericArgs)
    {
        var typeArgsStart = name.IndexOf('`');
        if (typeArgsStart >= 0)
            name = name.Remove(typeArgsStart);
        var genericName = GenericName(name);
        if (genericArgs.Length > 0)
            genericName =
                genericName.WithTypeArgumentList(
                    TypeArgumentList(SeparatedList(genericArgs))
                );
        return genericName;
    }

    public TypeSyntax RenderType(Type type)
    {
        Debug.Assert(type != null);

        _referenceManager.AddAssembly(type.Assembly);

        if (PrimitiveTypes.TryGetValue(type, out var name))
            return ParseTypeName(name);

        if (type.IsGenericParameter)
            return ParseTypeName(type.ToString());

        var typeNamespace = type.Namespace;
        if (typeNamespace != null)
            _referenceManager.AddUsing(typeNamespace);

        if (type.IsArray)
        {
            var elemType = type.GetElementType();
            Debug.Assert(elemType != null);
            return RenderArrayType(RenderType(elemType), type.GetArrayRank());
        }

        if (HasMockInfo(type.Name))
            return GetMockInfo(type.Name).MockName;

        string typeName = type.Name;
        if (CSharpKeywords.Contains(typeName))
            typeName = $"@{typeName}";

        if (type.IsNested && type.DeclaringType != null)
        {
            // TODO: use QualifiedName with list of types?
            typeName = $"{RenderType(type.DeclaringType).ToString()}.{typeName}";
        }

        if (type.IsGenericType)
        {
            var typeArgs = type.GetGenericArguments().Select(RenderType).ToArray();
            return RenderGenericName(typeName, typeArgs);
        }

        return ParseTypeName(typeName);
    }

    // TODO: unify with RenderType
    public SimpleNameSyntax RenderTypeName(Type type)
    {
        var typeNamespace = type.Namespace;
        if (typeNamespace != null)
            _referenceManager.AddUsing(typeNamespace);

        string typeName = type.Name;
        if (type.IsNested && type.DeclaringType != null)
        {
            typeName = $"{RenderType(type.DeclaringType)}.{typeName}";
        }

        if (type.IsGenericType)
        {
            var typeArgs = type.GetGenericArguments().Select(RenderType).ToArray();
            return RenderGenericName(typeName, typeArgs);
        }

        return IdentifierName(typeName);
    }

    public SimpleNameSyntax RenderMethodName(MethodBase method)
    {
        var type = method.DeclaringType;
        return method switch
        {
            { IsGenericMethod : true } => GenericName(method.Name),
            { IsConstructor : true } when type != null => RenderTypeName(type),
            _ when IsGetPropertyMethod(method, out var propertyName) => IdentifierName(propertyName),
            _ when IsSetPropertyMethod(method, out var propertyName) => IdentifierName(propertyName),
            _ => IdentifierName(method.Name)
        };
    }

    public ExpressionSyntax RenderMethod(MethodBase method)
    {
        _referenceManager.AddAssembly(method.Module.Assembly);
        var type = method.DeclaringType;
        SimpleNameSyntax methodName = RenderMethodName(method);

        if (methodName is GenericNameSyntax name && method.IsGenericMethod)
        {
            var typeArgs = method.GetGenericArguments().Select(RenderType).ToArray();
            var typeArgsList = TypeArgumentList(SeparatedList(typeArgs));
            methodName = name.WithTypeArgumentList(typeArgsList);
        }

        if (type == null || method.IsConstructor)
            return methodName;

        if (method.IsStatic)
            return RenderMemberAccess(RenderType(type), methodName);

        var typeNamespace = type.Namespace;
        if (typeNamespace != null)
            _referenceManager.AddUsing(typeNamespace);

        // TODO: instead of usings use full name of method?
        return methodName;
    }

    // Prerendered extern functions
    public ExpressionSyntax CompareObjects()
    {
        _referenceManager.AddObjectsComparer();
        return IdentifierName(nameof(ObjectsComparer.CompareObjects));
    }

    public IdentifierNameSyntax AllocatorObject => IdentifierName(nameof(Allocator<int>.Object));

    // Prerendered program types
    public TypeSyntax ObjectType => RenderType(typeof(object));
    public TypeSyntax StringType => RenderType(typeof(string));
    public TypeSyntax BoolType => RenderType(typeof(bool));
    public TypeSyntax VoidType => RenderType(typeof(void));
    public ArrayTypeSyntax VectorOfObjects => (ArrayTypeSyntax) RenderType(typeof(object[]));
    public TypeSyntax MethodBaseType => RenderType(typeof(MethodBase));
    public TypeSyntax ModuleType => RenderType(typeof(Module));
    public TypeSyntax AssemblyType => RenderType(typeof(Assembly));
    public TypeSyntax SystemType => RenderType(typeof(Type));
    public TypeSyntax SystemArray => RenderType(typeof(System.Array));

    public TypeSyntax AllocatorType(TypeSyntax typeArg)
    {
        _referenceManager.AddTestExtensions();
        var type = typeof(Allocator<int>).GetGenericTypeDefinition();
        return RenderGenericName(type.Name, typeArg);
    }

    public TypeSyntax AllocatorType()
    {
        _referenceManager.AddTestExtensions();
        return RenderType(typeof(Allocator));
    }

    // Prerendered tokens
    public static readonly SyntaxToken Public = Token(SyntaxKind.PublicKeyword);
    public static readonly SyntaxToken Internal = Token(SyntaxKind.InternalKeyword);
    public static readonly SyntaxToken Private = Token(SyntaxKind.PrivateKeyword);
    public static readonly SyntaxToken Static = Token(SyntaxKind.StaticKeyword);
    public static readonly SyntaxToken Unsafe = Token(SyntaxKind.UnsafeKeyword);
    public static readonly SyntaxToken Override = Token(SyntaxKind.OverrideKeyword);

    // Prerendered expressions
    public static readonly ExpressionSyntax True = LiteralExpression(SyntaxKind.TrueLiteralExpression);
    public static readonly ExpressionSyntax False = LiteralExpression(SyntaxKind.FalseLiteralExpression);
    public static readonly ExpressionSyntax Zero = LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(0));

    public static readonly IdentifierNameSyntax VarKeyword =
        IdentifierName(
            Identifier(
                TriviaList(),
                SyntaxKind.VarKeyword,
                "var",
                "var",
                TriviaList())
        );

    public static ExpressionSyntax RenderNot(ExpressionSyntax expression)
    {
        return PrefixUnaryExpression(SyntaxKind.LogicalNotExpression, expression);
    }

    public static ExpressionSyntax RenderAnd(params ExpressionSyntax[] expressions)
    {
        var result = expressions.Aggregate((x, y) =>
            BinaryExpression(SyntaxKind.LogicalAndExpression, x, y));
        return result;
    }

    public static ExpressionSyntax RenderOr(params ExpressionSyntax[] expressions)
    {
        var result = expressions.Aggregate((x, y) =>
            BinaryExpression(SyntaxKind.LogicalOrExpression, x, y));
        return result;
    }

    public static ExpressionSyntax RenderEq(ExpressionSyntax x, ExpressionSyntax y)
    {
        return BinaryExpression(SyntaxKind.EqualsExpression, x, y);
    }

    // TODO: use operators?
    public static ExpressionSyntax RenderNotEq(ExpressionSyntax x, ExpressionSyntax y)
    {
        return BinaryExpression(SyntaxKind.NotEqualsExpression, x, y);
    }

    public static ExpressionSyntax RenderNull()
    {
        return
            PostfixUnaryExpression(
                SyntaxKind.SuppressNullableWarningExpression,
                LiteralExpression(SyntaxKind.NullLiteralExpression)
            );
    }

    public ExpressionSyntax RenderByte(byte n, bool explicitType = false)
    {
        var literal = LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n));
        return explicitType ? CastExpression(RenderType(typeof(byte)), literal) : literal;
    }

    public ExpressionSyntax RenderSByte(sbyte n, bool explicitType = false)
    {
        var literal = LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n));
        return explicitType ? CastExpression(RenderType(typeof(sbyte)), literal) : literal;
    }

    public ExpressionSyntax RenderChar(char c, bool explicitType = false)
    {
        var literal = LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(c));
        return explicitType ? CastExpression(RenderType(typeof(char)), literal) : literal;
    }

    public ExpressionSyntax RenderShort(short n, bool explicitType = false)
    {
        var literal = LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n));
        return explicitType ? CastExpression(RenderType(typeof(short)), literal) : literal;
    }

    public ExpressionSyntax RenderUShort(ushort n, bool explicitType = false)
    {
        var literal = LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n));
        return explicitType ? CastExpression(RenderType(typeof(ushort)), literal) : literal;
    }

    // Type is Double or Single
    public static ExpressionSyntax RenderNaN(TypeSyntax type)
    {
        return MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, type, IdentifierName("NaN"));
    }

    // Type is Double or Single
    public static ExpressionSyntax RenderEpsilon(TypeSyntax type)
    {
        return MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, type, IdentifierName("Epsilon"));
    }

    // Type is Double or Single
    public static ExpressionSyntax RenderPosInfinity(TypeSyntax type)
    {
        return MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, type, IdentifierName("PositiveInfinity"));
    }

    // Type is Double or Single
    public static ExpressionSyntax RenderNegInfinity(TypeSyntax type)
    {
        return MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, type, IdentifierName("NegativeInfinity"));
    }

    public ExpressionSyntax RenderEnum(Enum e)
    {
        var type = e.GetType();
        var typeExpr = RenderType(type);
        var value = Enum.GetName(type, e);
        Debug.Assert(value != null);

        return RenderMemberAccess(typeExpr, IdentifierName(value));
    }

    public static AssignmentExpressionSyntax RenderAssignment(ExpressionSyntax left, ExpressionSyntax right)
    {
        return AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, left, right);
    }

    public static VariableDeclarationSyntax RenderVarDecl(TypeSyntax? type, SyntaxToken var, ExpressionSyntax? init = null)
    {
        type ??= VarKeyword;

        var varDeclarator = VariableDeclarator(var);
        if (init != null)
            varDeclarator = varDeclarator.WithInitializer(EqualsValueClause(init));
        return
            VariableDeclaration(
                type,
                SingletonSeparatedList(varDeclarator)
            );
    }

    public static ArrayCreationExpressionSyntax RenderArrayCreation(ArrayTypeSyntax type, params int[] lengths)
    {
        var arrayRankSpecifier =
            ArrayRankSpecifier(
                SeparatedList<ExpressionSyntax>(
                    lengths.Select(RenderLiteral)
                )
            );
        type = type.WithRankSpecifiers(SingletonList(arrayRankSpecifier));
        return
            ArrayCreationExpression(
                Token(SyntaxKind.NewKeyword),
                type,
                null
            );
    }

    public static ExpressionSyntax RenderArrayCreation(
        ArrayTypeSyntax type,
        IEnumerable<ExpressionSyntax>? init,
        bool allowImplicit)
    {
        InitializerExpressionSyntax? initializer = null;
        if (init != null)
            initializer =
                InitializerExpression(
                    SyntaxKind.ArrayInitializerExpression,
                    SeparatedList(init)
                );

        ExpressionSyntax array;
        array = ArrayCreationExpression(Token(SyntaxKind.NewKeyword), type, initializer);
        return array;
    }

    public static ElementAccessExpressionSyntax RenderArrayAccess(
        ExpressionSyntax array,
        ExpressionSyntax[] index)
    {
        Debug.Assert(index.Length > 0);
        var indexArgument = BracketedArgumentList(SeparatedList(index.Select(Argument)));
        return ElementAccessExpression(array).WithArgumentList(indexArgument);
    }

    public static AssignmentExpressionSyntax RenderArrayAssignment(
        ExpressionSyntax array,
        ExpressionSyntax value,
        params int[] index)
    {
        Debug.Assert(index.Length > 0);
        var indices = index.Select(i => (ExpressionSyntax)RenderLiteral(i)).ToArray();
        return
            AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                RenderArrayAccess(array, indices),
                value
            );
    }

    public static ObjectCreationExpressionSyntax RenderObjectCreation(
        TypeSyntax type,
        ArgumentSyntax[]? args,
        ExpressionSyntax[]? init)
    {
        InitializerExpressionSyntax? initializer = null;
        ArgumentListSyntax? argumentList = null;

        if (init != null && init.Length != 0)
        {
            initializer = InitializerExpression(SyntaxKind.ObjectInitializerExpression, SeparatedList(init));
        }

        if (init == null || init.Length == 0 || args != null && args.Length != 0)
        {
            argumentList = ArgumentList(SeparatedList(args));
        }

        return
            ObjectCreationExpression(
                Token(SyntaxKind.NewKeyword),
                type,
                argumentList,
                initializer
            );
    }

    public static ObjectCreationExpressionSyntax RenderObjectCreation(
        TypeSyntax type,
        ExpressionSyntax[]? args,
        ExpressionSyntax[]? init)
    {
        return RenderObjectCreation(type, args?.Select(Argument).ToArray(), init);
    }

    public static ObjectCreationExpressionSyntax RenderObjectCreation(
        TypeSyntax type,
        ExpressionSyntax[]? args,
        (ExpressionSyntax, ExpressionSyntax)[] init)
    {
        ExpressionSyntax[] keysWithValues = new ExpressionSyntax[init.Length];
        var i = 0;
        foreach (var (key, value) in init)
        {
            var keyAccess =
                ImplicitElementAccess(
                    BracketedArgumentList(
                        SingletonSeparatedList(Argument(key))
                    )
                );
            keysWithValues[i] = RenderAssignment(keyAccess, value);
            i++;
        }

        return RenderObjectCreation(type, args, keysWithValues);
    }

    // 'memberOf' is 'this' or some type
    public static ExpressionSyntax RenderMemberAccess(ExpressionSyntax memberOf, SimpleNameSyntax member)
    {
        return
            MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                memberOf,
                member
            );
    }

    public static ExpressionSyntax RenderMemberAccess(ExpressionSyntax memberOf, params string[] memberNames)
    {
        Debug.Assert(memberNames.Length > 0);
        ExpressionSyntax result = memberOf;
        foreach (var memberName in memberNames)
        {
            result =
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    result,
                    IdentifierName(memberName)
                );
        }

        return result;
    }

    public static ExpressionSyntax RenderMemberAccess(params string[] memberNames)
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

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax function, params ArgumentSyntax[] args)
    {
        return
            InvocationExpression(
                function,
                ArgumentList(
                    SeparatedList(args)
                )
            );
    }

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax function, params ExpressionSyntax[] args)
    {
        return
            InvocationExpression(
                function,
                ArgumentList(
                    SeparatedList(args.Select(Argument))
                )
            );
    }

    public static InvocationExpressionSyntax RenderCall(GenericNameSyntax genericFunction, TypeSyntax[] genericArgs, params ExpressionSyntax[] args)
    {
        Debug.Assert(genericFunction.IsUnboundGenericName);
        genericFunction =
            genericFunction.WithTypeArgumentList(
                TypeArgumentList(SeparatedList(genericArgs))
            );
        return RenderCall(genericFunction, args);
    }

    public static InvocationExpressionSyntax RenderCall(string functionName, params ExpressionSyntax[] args)
    {
        return RenderCall(IdentifierName(functionName), args);
    }

    public ExpressionSyntax RenderCall(ExpressionSyntax? thisArg, MethodBase method, params ExpressionSyntax[] args)
    {
        var functionArgs = args.Select(Argument).ToArray();
        var parameters = method.GetParameters();
        Debug.Assert(parameters.Length == functionArgs.Length);
        for (int i = 0; i < parameters.Length; i++)
        {
            var parameterInfo = parameters[i];
            var arg = functionArgs[i];
            if (parameterInfo.ParameterType.IsByRef)
            {
                if (parameterInfo.IsOut)
                    functionArgs[i] = arg.WithRefOrOutKeyword(Token(SyntaxKind.OutKeyword));
                else if (parameterInfo.IsIn)
                    functionArgs[i] = arg.WithRefOrOutKeyword(Token(SyntaxKind.InKeyword));
                else
                    functionArgs[i] = arg.WithRefOrOutKeyword(Token(SyntaxKind.RefKeyword));
            }
        }

        if (method.IsConstructor)
        {
            Debug.Assert(method.DeclaringType != null);
            var init = System.Array.Empty<ExpressionSyntax>();
            return RenderObjectCreation(RenderType(method.DeclaringType), functionArgs, init);
        }

        if (method.IsSpecialName && method.Name == "get_Item")
        {
            // Indexer may be only in non-static context
            Debug.Assert(thisArg != null);
            var indexArgument =
                BracketedArgumentList(SeparatedList(args.Select(Argument)));
            return ElementAccessExpression(thisArg).WithArgumentList(indexArgument);
        }

        if (method.IsSpecialName && method.Name == "set_Item")
        {
            // Indexer may be only in non-static context
            Debug.Assert(thisArg != null);
            // Last arg is value to write, all args before are indices
            var value = args.Last();
            var indices = args.SkipLast(1).Select(Argument);
            var indexArgument =
                BracketedArgumentList(SeparatedList(indices));
            var access = ElementAccessExpression(thisArg).WithArgumentList(indexArgument);
            return RenderAssignment(access, value);
        }

        ExpressionSyntax function;
        if (thisArg == null)
        {
            Debug.Assert(!Reflection.hasThis(method));
            function = RenderMethod(method);
        }
        else
        {
            var rendered = RenderMethod(method);
            Debug.Assert(Reflection.hasThis(method) && rendered is SimpleNameSyntax);
            function = RenderMemberAccess(thisArg, (SimpleNameSyntax) rendered);
        }

        if (IsGetPropertyMethod(method, out _))
        {
            Debug.Assert(args.Length == 0);
            return function;
        }

        if (IsSetPropertyMethod(method, out _))
        {
            Debug.Assert(args.Length == 1);
            return RenderAssignment(function, args[0]);
        }

        return RenderCall(function, functionArgs);
    }

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax memberOf, string memberName, params ExpressionSyntax[] args)
    {
        var function = RenderMemberAccess(memberOf, memberName);
        return RenderCall(function, args);
    }

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax memberOf, SimpleNameSyntax member, params ExpressionSyntax[] args)
    {
        var function = RenderMemberAccess(memberOf, member);
        return RenderCall(function, args);
    }

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax memberOf, string memberName, TypeSyntax[] genericArgs, params ExpressionSyntax[] args)
    {
        var member = RenderGenericName(memberName, genericArgs);
        var function = RenderMemberAccess(memberOf, member);
        return RenderCall(function, args);
    }

    public static InvocationExpressionSyntax RenderCall(string memberOf, string memberName, TypeSyntax[] genericArgs, params ExpressionSyntax[] args)
    {
        return RenderCall(IdentifierName(memberOf), memberName, genericArgs, args);
    }

    public static PropertyDeclarationSyntax RenderPropertyDeclaration(
        TypeSyntax propertyType,
        SyntaxToken propertyId,
        SyntaxToken[] modifiers,
        SimpleNameSyntax? interfaceName)
    {
        var propertyDecl =
            PropertyDeclaration(propertyType, propertyId)
                .AddModifiers(modifiers);
        if (interfaceName != null)
            propertyDecl = propertyDecl.WithExplicitInterfaceSpecifier(
                ExplicitInterfaceSpecifier(interfaceName)
            );
        return propertyDecl;
    }

    public static LiteralExpressionSyntax RenderLiteral(string? literal)
    {
        return LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(literal ?? String.Empty));
    }

    public static LiteralExpressionSyntax RenderLiteral(int literal)
    {
        return LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(literal));
    }

    public static AttributeSyntax RenderAttribute(string name, params string[] args)
    {
        var attribute = Attribute(IdentifierName(name));
        if (args.Length > 0)
        {
            var renderedArgs =
                args.Select(arg => AttributeArgument(RenderLiteral(arg)));
            attribute = attribute.WithArgumentList(AttributeArgumentList(SeparatedList(renderedArgs)));
        }

        return attribute;
    }

    public static AttributeListSyntax RenderAttributeList(params AttributeSyntax[] attributes)
    {
        return AttributeList(SeparatedList(attributes));
    }

    public static AttributeListSyntax RenderAttributeList(params string[] attributeNames)
    {
        var attributes = attributeNames.Select(s => Attribute(IdentifierName(s)));
        return AttributeList(SeparatedList(attributes));
    }

    public static ExpressionSyntax RenderNotNull(ExpressionSyntax expr)
    {
        return RenderNotEq(expr, LiteralExpression(SyntaxKind.NullLiteralExpression));
    }

    public static ExpressionSyntax RenderIsNull(ExpressionSyntax expr)
    {
        return RenderEq(expr, LiteralExpression(SyntaxKind.NullLiteralExpression));
    }

    public static ExpressionSyntax RenderGetType(ExpressionSyntax expr)
    {
        return RenderCall(expr, "GetType");
    }

    public static ExpressionSyntax RenderSubType(ExpressionSyntax type1, ExpressionSyntax type2)
    {
        return RenderCall(type2, "IsAssignableFrom", type1);
    }

    public static ExpressionSyntax RenderIsType(ExpressionSyntax expr, TypeSyntax type)
    {
        return BinaryExpression(SyntaxKind.IsExpression, expr, type);
    }

    public static ExpressionSyntax RenderIsType(ExpressionSyntax expr, TypeSyntax type, SyntaxToken var)
    {
        return IsPatternExpression(expr, DeclarationPattern(type, SingleVariableDesignation(var)));
    }

    public static ExpressionSyntax RenderTypeOf(TypeSyntax type)
    {
        return TypeOfExpression(type);
    }

    public static ExpressionSyntax RenderAsType(ExpressionSyntax expr, TypeSyntax type)
    {
        return BinaryExpression(SyntaxKind.AsExpression, expr, type);
    }

    public static ExpressionSyntax RenderAssert(ExpressionSyntax condition)
    {
        return RenderCall(IdentifierName("Assert"), "IsTrue", condition);
    }

    public static ExpressionSyntax RenderAssertEqual(ExpressionSyntax x, ExpressionSyntax y)
    {
        return RenderCall(IdentifierName("Assert"), "AreEqual", x, y);
    }
}
