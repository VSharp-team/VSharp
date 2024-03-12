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

    internal enum AccessorType
    {
        Get,
        Set
    }

    internal enum EventMethodType
    {
        Add,
        Remove
    }

    internal enum OperatorType
    {
        Equality,
        Inequality,
        Greater,
        GreaterOrEq,
        Less,
        LessOrEq,
        ImplicitConv,
        ExplicitConv
    }

    internal class MockInfo
    {
        public readonly SimpleNameSyntax MockName;
        public readonly Mocking.Type TypeMock;
        // TODO: need to save type of clauses from mock definition?
        public readonly List<(MethodInfo, Type, SimpleNameSyntax)> SetupClauses;

        public MockInfo(
            SimpleNameSyntax mockName,
            Mocking.Type typeMock,
            List<(MethodInfo, Type, SimpleNameSyntax)> setupClauses)
        {
            MockName = mockName;
            TypeMock = typeMock;
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
            Mocking.Type typeMock,
            List<(MethodInfo, Type, SimpleNameSyntax)> setupClauses,
            SimpleNameSyntax delegateMethod,
            Type delegateType) : base(mockName, typeMock, setupClauses)
        {
            DelegateMethod = delegateMethod;
            DelegateType = delegateType;
        }
    }

    public static readonly bool RenderDefaultValues = true;

    private static readonly Dictionary<string, MockInfo> MocksInfo = new ();

    // TODO: make non-static
    public static Dictionary<object, CompactArrayRepr> CompactRepresentations = new ();

    // TODO: make non-static
    public static HashSet<physicalAddress> BoxedLocations = new ();

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

    private static bool IsPropertyMethod(MethodBase method, out string propertyName, string prefix)
    {
        Debug.Assert(prefix is "get_" or "set_");
        var name = method.Name;
        if (method is MethodInfo { IsSpecialName: true, DeclaringType: not null } mi && name.Contains(prefix))
        {
            propertyName = name.Substring(name.IndexOf('_') + 1);
            Type[] argumentTypes;
            Type returnType;
            if (prefix is "get_")
            {
                argumentTypes = mi.GetParameters().Select(p => p.ParameterType).ToArray();
                returnType = mi.ReturnType;
            }
            else
            {
                Debug.Assert(prefix is "set_");
                var types = mi.GetParameters().Select(p => p.ParameterType);
                argumentTypes = types.SkipLast(1).ToArray();
                returnType = types.Last();
            }
            var declaringType = mi.DeclaringType;
            return declaringType.GetProperty(propertyName, Reflection.allBindingFlags, null, returnType, argumentTypes, null) != null;
        }

        propertyName = string.Empty;
        return false;
    }

    public static bool IsPropertyMethod(MethodBase method, out string propertyName, out AccessorType accessorType)
    {
        if (IsPropertyMethod(method, out var propertyNameValue, "get_"))
        {
            accessorType = AccessorType.Get;
            propertyName = propertyNameValue;
            return true;
        }
        if (IsPropertyMethod(method, out propertyNameValue, "set_"))
        {
            accessorType = AccessorType.Set;
            propertyName = propertyNameValue;
            return true;
        }
        propertyName = String.Empty;
        accessorType = default;
        return false;
    }

    public static bool IsGetItem(MethodBase method)
    {
        return method.IsSpecialName && method.Name == "get_Item";
    }

    public static bool IsSetItem(MethodBase method)
    {
        return method.IsSpecialName && method.Name == "set_Item";
    }

    public static bool IsIndexer(MethodBase method)
    {
        return IsGetItem(method) || IsSetItem(method);
    }

    public static bool IsEventMethod(MethodBase method, out string eventName, out EventMethodType eventMethodType)
    {
        var name = method.Name;
        if (method.IsSpecialName && name.StartsWith("add_"))
        {
            eventName = name[(name.IndexOf('_') + 1)..];
            eventMethodType = EventMethodType.Add;
            return true;
        }

        if (method.IsSpecialName && name.StartsWith("remove_"))
        {
            eventName = name[(name.IndexOf('_') + 1)..];
            eventMethodType = EventMethodType.Remove;
            return true;
        }

        eventName = string.Empty;
        eventMethodType = default;
        return false;
    }

    public static bool IsOperator(MethodBase method, out OperatorType operatorType)
    {
        var name = method.Name;
        if (method.IsSpecialName && name.StartsWith("op_"))
        {
            operatorType = name switch
            {
                "op_Equality" => OperatorType.Equality,
                "op_Inequality" => OperatorType.Inequality,
                "op_LessThan" => OperatorType.Less,
                "op_GreaterThan" => OperatorType.Greater,
                "op_LessThanOrEqual" => OperatorType.LessOrEq,
                "op_GreaterThanOrEqual" => OperatorType.GreaterOrEq,
                "op_Implicit" => OperatorType.ImplicitConv,
                "op_Explicit" => OperatorType.ExplicitConv,
                _ => throw new ArgumentOutOfRangeException()
            };
            return true;
        }

        operatorType = default;
        return false;
    }

    public static bool NumericWithoutSuffix(Type? type)
    {
        return
            type != null &&
            (type == typeof(byte)
                || type == typeof(sbyte)
                || type == typeof(short)
                || type == typeof(ushort));
    }

    public static bool NeedExplicitType(object? obj, Type? containerType)
    {
        var isBoxed =
            obj is ValueType && containerType is { IsValueType: false };
        var needExplicitNumericType =
            // For this types there is no data type suffix, so if parameter type is upcast, explicit cast is needed
            obj is byte or sbyte or short or ushort
            && (containerType == typeof(object) || containerType == typeof(ValueType));
        var needExplicitDelegateType =
            // Member group can not be upcasted to object, so explicit delegate type is needed
            obj is Delegate && containerType == typeof(object);
        return isBoxed || needExplicitNumericType || needExplicitDelegateType;
    }

    internal static readonly Dictionary<Type, string> PredefinedTypes = new()
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

    private static ArrayTypeSyntax RenderArrayType(TypeSyntax elemType, IEnumerable<int> ranks)
    {
        var arrayRankSpecifiers = ranks.Select(rank =>
        {
            var dims = Enumerable.Repeat(OmittedArraySizeExpression(), rank);
            return ArrayRankSpecifier(SeparatedList<ExpressionSyntax>(dims));
        });
        return
            ArrayType(
                elemType,
                new SyntaxList<ArrayRankSpecifierSyntax>(arrayRankSpecifiers)
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

    private ArrayTypeSyntax RenderArrayType(Type arrayType)
    {
        Debug.Assert(arrayType.IsArray);
        var currentElementType = arrayType.GetElementType();
        Debug.Assert(currentElementType != null);
        var ranks = new List<int>();
        ranks.Add(arrayType.GetArrayRank());

        while (currentElementType.IsArray)
        {
            ranks.Add(currentElementType.GetArrayRank());
            currentElementType = currentElementType.GetElementType();
            Debug.Assert(currentElementType != null);
        }

        return RenderArrayType(RenderType(currentElementType), ranks);
    }

    public TypeSyntax RenderType(Type type)
    {
        Debug.Assert(type != null);

        if (type.IsArray)
        {
            return RenderArrayType(type);
        }

        if (type.IsPointer)
        {
            var elemType = type.GetElementType();
            Debug.Assert(elemType != null);
            return PointerType(RenderType(elemType));
        }

        if (type.IsByRef)
        {
            var elemType = type.GetElementType();
            Debug.Assert(elemType != null);
            return RenderType(elemType);
        }

        if (PredefinedTypes.TryGetValue(type, out var name))
            return ParseTypeName(name);

        return RenderTypeName(type);
    }

    public SimpleNameSyntax RenderSimpleTypeName(Type type)
    {
        Debug.Assert(type != null);

        if (type.IsByRef)
        {
            var elemType = type.GetElementType();
            Debug.Assert(elemType != null);
            return RenderSimpleTypeName(elemType);
        }

        if (type.IsGenericParameter)
            return IdentifierName(type.ToString());

        ReferenceType(type);

        if (HasMockInfo(type.Name))
            return GetMockInfo(type.Name).MockName;

        string typeName = CorrectNameGenerator.GetTypeName(type);

        if (type.IsGenericType)
        {
            var typeArgs = type.GetGenericArguments().Select(RenderType).ToArray();
            return RenderGenericName(typeName, typeArgs);
        }

        return IdentifierName(typeName);
    }

    public NameSyntax RenderTypeName(Type type)
    {
        return RenderTypeNameRec(type).Item1;
    }

    private void ReferenceAssembly(Assembly assembly)
    {
        _referenceManager.AddAssembly(assembly);
    }

    private void ReferenceType(Type type)
    {
        ReferenceAssembly(type.Assembly);

        var typeNamespace = type.Namespace;
        if (typeNamespace != null)
            _referenceManager.AddUsing(typeNamespace);
    }

    private (NameSyntax, int) RenderTypeNameRec(Type type, TypeSyntax[]? typeArgs = null)
    {
        Debug.Assert(type != null);

        if (type.IsByRef)
        {
            var elemType = type.GetElementType();
            Debug.Assert(elemType != null);
            return RenderTypeNameRec(elemType, typeArgs);
        }

        string typeName = CorrectNameGenerator.GetTypeName(type);

        var isNested = type.IsNested;
        if (type.IsGenericParameter || !isNested && (typeArgs == null || !type.IsGenericType))
            return (RenderSimpleTypeName(type), 0);

        ReferenceType(type);

        if (type.IsGenericType)
        {
            var genericArgs = type.GetGenericArguments();
            typeArgs ??= genericArgs.Select(RenderType).ToArray();
            NameSyntax? declaringType = null;
            var usedGenerics = 0;
            if (isNested)
            {
                Debug.Assert(type.DeclaringType != null);
                (declaringType, usedGenerics) = RenderTypeNameRec(type.DeclaringType, typeArgs);
                if (usedGenerics == typeArgs.Length)
                    return (QualifiedName(declaringType, IdentifierName(typeName)), usedGenerics);
            }
            var allGenericsLength = genericArgs.Length;
            var neededGenericsLength = allGenericsLength - usedGenerics;
            var neededGenerics =
                typeArgs
                    .Skip(usedGenerics)
                    .Take(neededGenericsLength)
                    .ToArray();
            var current = RenderGenericName(typeName, neededGenerics);
            NameSyntax result = declaringType == null ? current : QualifiedName(declaringType, current);
            return (result, allGenericsLength);
        }

        Debug.Assert(type.IsNested && type.DeclaringType != null);
        return (QualifiedName(RenderTypeName(type.DeclaringType), IdentifierName(typeName)), 0);
    }

    public SimpleNameSyntax RenderMethodName(MethodBase method)
    {
        var type = method.DeclaringType;
        return method switch
        {
            { IsGenericMethod : true } => GenericName(method.Name),
            { IsConstructor : true } when type != null => RenderSimpleTypeName(type),
            _ when IsPropertyMethod(method, out var propertyName, out _) => IdentifierName(propertyName),
            _ when IsEventMethod(method, out var eventName, out _) => IdentifierName(eventName),
            _ => IdentifierName(method.Name)
        };
    }

    public ExpressionSyntax RenderMethod(MethodBase method)
    {
        ReferenceAssembly(method.Module.Assembly);
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
    public IdentifierNameSyntax AllocatorCall => IdentifierName(nameof(Allocator.Call));
    public IdentifierNameSyntax AllocatorFill => IdentifierName(nameof(Allocator.Fill));

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

    public static ExpressionSyntax RenderNotEq(ExpressionSyntax x, ExpressionSyntax y)
    {
        return BinaryExpression(SyntaxKind.NotEqualsExpression, x, y);
    }

    public static ExpressionSyntax RenderLess(ExpressionSyntax x, ExpressionSyntax y)
    {
        return BinaryExpression(SyntaxKind.LessThanExpression, x, y);
    }

    public static ExpressionSyntax RenderLessOrEq(ExpressionSyntax x, ExpressionSyntax y)
    {
        return BinaryExpression(SyntaxKind.LessThanOrEqualExpression, x, y);
    }

    public static ExpressionSyntax RenderGreater(ExpressionSyntax x, ExpressionSyntax y)
    {
        return BinaryExpression(SyntaxKind.GreaterThanExpression, x, y);
    }

    public static ExpressionSyntax RenderGreaterOrEq(ExpressionSyntax x, ExpressionSyntax y)
    {
        return BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression, x, y);
    }

    public static ExpressionSyntax RenderCastExpression(ExpressionSyntax x, TypeSyntax type)
    {
        return CastExpression(type, x);
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
        // TODO: handle masks, for example 'BindingFlags.Public | BindingFlags.NonPublic' (value will be 'null')
        var value = Enum.GetName(type, e);
        if (value != null)
        {
            return RenderMemberAccess(typeExpr, IdentifierName(value));
        }

        var number = Convert.ChangeType(e, Enum.GetUnderlyingType(type));
        var renderedNumber = number switch
        {
            byte n => RenderByte(n),
            sbyte n => RenderSByte(n),
            short n => RenderShort(n),
            ushort n => RenderUShort(n),
            int n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            uint n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            long n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            ulong n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            _ => throw new ArgumentException($"RenderEnum: unexpected enum {e}")
        };
        return RenderCastExpression(renderedNumber, typeExpr);
    }

    public static AssignmentExpressionSyntax RenderAssignment(ExpressionSyntax left, ExpressionSyntax right)
    {
        return AssignmentExpression(SyntaxKind.SimpleAssignmentExpression, left, right);
    }

    public static AssignmentExpressionSyntax RenderAddAssignment(ExpressionSyntax left, ExpressionSyntax right)
    {
        return AssignmentExpression(SyntaxKind.AddAssignmentExpression, left, right);
    }

    public static AssignmentExpressionSyntax RenderSubAssignment(ExpressionSyntax left, ExpressionSyntax right)
    {
        return AssignmentExpression(SyntaxKind.SubtractAssignmentExpression, left, right);
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
        var rankSpecifiers = type.RankSpecifiers.ToList();

        // need to add length only for top-level array
        var rankSpecifierWithLengths = rankSpecifiers[0].WithSizes(
                SeparatedList<ExpressionSyntax>(
                    lengths.Select(RenderLiteral)
                    )
            );
        rankSpecifiers[0] = rankSpecifierWithLengths;

        type = type.WithRankSpecifiers(List(rankSpecifiers));
        return
            ArrayCreationExpression(
                Token(SyntaxKind.NewKeyword),
                type,
                null
            );
    }

    public static ExpressionSyntax RenderEmptyArrayInitializer()
    {
        return InitializerExpression(SyntaxKind.ArrayInitializerExpression);
    }

    public static ExpressionSyntax RenderArrayCreation(
        ArrayTypeSyntax type,
        List<ExpressionSyntax>? init,
        bool allowImplicit)
    {
        InitializerExpressionSyntax? initializer = null;
        if (init != null)
        {
            initializer =
                InitializerExpression(
                    SyntaxKind.ArrayInitializerExpression,
                    SeparatedList(init)
                );
            // If init is empty, we should use explicit type
            if (init.Count == 0) allowImplicit = false;
        }

        ExpressionSyntax array;
        if (allowImplicit && initializer != null)
            // TODO: update for multidimensional arrays (use .WithCommas)
            array = ImplicitArrayCreationExpression(initializer);
        else
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
        return RenderObjectCreation(type, args, init.Select(x => (new [] {x.Item1}, x.Item2)).ToArray());
    }

    public static ObjectCreationExpressionSyntax RenderObjectCreation(
        TypeSyntax type,
        ExpressionSyntax[]? args,
        (ExpressionSyntax[], ExpressionSyntax)[] init)
    {
        ExpressionSyntax[] keysWithValues = new ExpressionSyntax[init.Length];
        var i = 0;
        foreach (var (key, value) in init)
        {
            var keyAccess =
                ImplicitElementAccess(
                    BracketedArgumentList(
                        SeparatedList(key.Select(Argument))
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

    private ExpressionSyntax RenderPrivateCall(
        ExpressionSyntax? thisArg,
        MethodBase method,
        ArgumentSyntax[] args)
    {
        var length = args.Length;
        ArgumentSyntax[] newArgs;
        ExpressionSyntax function;
        if (thisArg == null)
        {
            Debug.Assert(!Reflection.hasThis(method));
            newArgs = new ArgumentSyntax[length + 2];
            System.Array.Copy(args, 0, newArgs, 2, length);
            var type = method.DeclaringType;
            Debug.Assert(type != null);
            newArgs[0] = Argument(RenderLiteral(type.AssemblyQualifiedName));
            newArgs[1] = Argument(RenderLiteral(method.Name));
            function = RenderMemberAccess(AllocatorType(), AllocatorCall);
            return RenderCall(function, newArgs);
        }

        Debug.Assert(Reflection.hasThis(method));
        newArgs = new ArgumentSyntax[length + 1];
        System.Array.Copy(args, 0, newArgs, 1, length);
        newArgs[0] = Argument(RenderLiteral(method.Name));
        function = RenderMemberAccess(thisArg, AllocatorCall);
        return RenderCall(function, newArgs);
    }

    public ExpressionSyntax RenderCall(
        ExpressionSyntax? thisArg,
        Type? thisType,
        MethodBase method,
        params ExpressionSyntax[] args)
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

        // Adding reference for method's return type assembly
        ReferenceAssembly(((MethodInfo)method).ReturnType.Assembly);

        if (!method.IsPublic || thisType != null && !TypeUtils.isPublic(thisType) ||
            thisArg == null && method.DeclaringType != null && !TypeUtils.isPublic(method.DeclaringType))
            return RenderPrivateCall(thisArg, method, functionArgs);

        if (args.Length > 0 && IsGetItem(method))
        {
            // Indexer may be only in non-static context
            Debug.Assert(thisArg != null);
            var indexArgument =
                BracketedArgumentList(SeparatedList(args.Select(Argument)));
            return ElementAccessExpression(thisArg).WithArgumentList(indexArgument);
        }

        if (args.Length > 1 && IsSetItem(method))
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

        if (IsOperator(method, out var operatorType))
        {
            switch (operatorType)
            {
                case OperatorType.Equality:
                    Debug.Assert(args.Length == 2);
                    return RenderEq(args[0], args[1]);
                case OperatorType.Inequality:
                    Debug.Assert(args.Length == 2);
                    return RenderNotEq(args[0], args[1]);
                case OperatorType.Greater:
                    Debug.Assert(args.Length == 2);
                    return RenderGreater(args[0], args[1]);
                case OperatorType.GreaterOrEq:
                    Debug.Assert(args.Length == 2);
                    return RenderGreaterOrEq(args[0], args[1]);
                case OperatorType.Less:
                    Debug.Assert(args.Length == 2);
                    return RenderLess(args[0], args[1]);
                case OperatorType.LessOrEq:
                    Debug.Assert(args.Length == 2);
                    return RenderLessOrEq(args[0], args[1]);
                case OperatorType.ImplicitConv:
                case OperatorType.ExplicitConv:
                {
                    Debug.Assert(args.Length == 1 && method.DeclaringType != null);
                    var type = RenderType(method.DeclaringType);
                    return RenderCastExpression(args[0], type);
                }
                default:
                    throw new ArgumentOutOfRangeException();
            }
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

        if (IsPropertyMethod(method, out _, out var accessorType))
        {
            switch (accessorType)
            {
                case AccessorType.Get:
                    Debug.Assert(args.Length == 0);
                    return function;
                case AccessorType.Set:
                    Debug.Assert(args.Length == 1);
                    return RenderAssignment(function, args[0]);
                default:
                    throw new ArgumentOutOfRangeException();
            }
        }

        if (IsEventMethod(method, out _, out EventMethodType methodType))
        {
            Debug.Assert(args.Length == 1);
            var value = args.First();
            switch (methodType)
            {
                case EventMethodType.Add:
                    return RenderAddAssignment(function, value);
                case EventMethodType.Remove:
                    return RenderSubAssignment(function, value);
                default:
                    throw new ArgumentOutOfRangeException();
            }
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
        NameSyntax? interfaceName)
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

    public static IndexerDeclarationSyntax RenderIndexerDeclaration(
        TypeSyntax indexerType,
        SyntaxToken[] modifiers,
        IEnumerable<ParameterSyntax> args)
    {
        var indexerDecl =
            IndexerDeclaration(indexerType)
                .AddModifiers(modifiers)
                .WithParameterList(BracketedParameterList(SeparatedList(args)));
        return indexerDecl;
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

    public static AttributeListSyntax RenderAttributeList(IEnumerable<AttributeSyntax> attributes)
    {
        return AttributeList(SeparatedList(attributes));
    }

    public static AttributeListSyntax RenderAttributeList(params AttributeSyntax[] attributes)
    {
        return RenderAttributeList((IEnumerable<AttributeSyntax>) attributes);
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

internal static class CorrectNameGenerator
{
    private class EmptyReferenceManager : IReferenceManager
    {
        public void AddUsing(string name) { }

        public void AddStaticUsing(string name) { }

        public void AddAssembly(Assembly assembly) { }

        public void AddTestExtensions() { }

        public void AddObjectsComparer() { }
    }

    public static string GetTypeName(Type type)
    {
        var typeName = type.Name;

        // Name can't be a csharp keyword
        if (CodeRenderer.CSharpKeywords.Contains(typeName))
            typeName = $"@{typeName}";

        return typeName.TrimEnd('&'); // ByRef types contains & in name
    }

    public static string GetVariableName(string name)
    {
        var correctName = new System.Text.StringBuilder(name);

        // Name must start from letter, '@' or '_'
        var firstCorrectCharIndex = -1;
        for (var i = 0; i < correctName.Length; i++)
        {
            if (System.Text.RegularExpressions.Regex.IsMatch($"{correctName[i]}", "[a-zA-Z_@]"))
            {
                firstCorrectCharIndex = i;
                break;
            }
        }
        if (firstCorrectCharIndex == -1)
        {
            throw new Exception($"Unable to correct name: {name}");
        }
        correctName.Remove(0, firstCorrectCharIndex);

        // Whitespaces are not allowed at name
        correctName.Replace(" ", "");

        // @ allowed only in start of name
        correctName.Replace("@", "", 1, correctName.Length - 1);

        // Name can't be a csharp keyword
        if (CodeRenderer.CSharpKeywords.Contains(name))
        {
            correctName.Insert(0, '@');
        }

        Debug.Assert(correctName.Length > 0);
        return correctName.ToString();
    }

    public static string GetVariableName(MethodBase method)
    {
        var codeRenderer = new CodeRenderer(new EmptyReferenceManager());
        string varName;
        var renderedName = codeRenderer.RenderMethodName(method);
        switch (renderedName)
        {
            case GenericNameSyntax g:
            {
                var sb = new System.Text.StringBuilder();
                sb.Append(g.Identifier.ToString());
                foreach (var t in method.GetGenericArguments())
                {
                    sb.Append($"_{t}");
                }

                varName = sb.ToString();
                break;
            }
            default:
                varName = renderedName.ToString();
                break;
        }

        return GetVariableName(varName);
    }
}
