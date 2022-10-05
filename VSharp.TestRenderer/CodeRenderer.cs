using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using VSharp.TestExtensions;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

internal static class CodeRenderer
{
    // Needed usings
    private static readonly HashSet<string> usings = new ();

    // Needed static usings
    private static readonly HashSet<string> staticUsings = new ();

    // Used assemblies
    private static readonly HashSet<Assembly> assemblies = new ();

    private static UsingDirectiveSyntax[] ProgramUsings()
    {
        var nonStaticElems = usings.Select(x =>
            UsingDirective(ParseName(x)));
        var staticElems = staticUsings.Select(x =>
            UsingDirective(Static, null, ParseName(x)));
        return nonStaticElems.Concat(staticElems).ToArray();
    }

    public static void AddNUnit()
    {
        usings.Add("NUnit.Framework");
    }

    public static void AddTestExtensions()
    {
        usings.Add("VSharp.TestExtensions");
    }

    private static bool _objectsComparerAdded = false;

    public static void AddObjectsComparer()
    {
        if (_objectsComparerAdded) return;
        var name = typeof(ObjectsComparer).FullName;
        Debug.Assert(name != null);
        staticUsings.Add(name);
        assemblies.Add(typeof(ObjectsComparer).Assembly);
        _objectsComparerAdded = true;
    }

    public static void PrepareCache()
    {
        _objectsComparerAdded = false;
        usings.Clear();
        staticUsings.Clear();
        assemblies.Clear();
    }

    public static Assembly[] UsedAssemblies()
    {
        return assemblies.ToArray();
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

    private static readonly Dictionary<Type, string> PrimitiveTypes = new()
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

    private static GenericNameSyntax RenderGenericName(string name, params TypeSyntax[] genericArgs)
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

    public static TypeSyntax RenderType(Type type)
    {
        assemblies.Add(type.Assembly);

        if (PrimitiveTypes.TryGetValue(type, out var name))
            return ParseTypeName(name);

        var typeNamespace = type.Namespace;
        if (typeNamespace != null)
            usings.Add(typeNamespace);

        if (type.IsArray)
        {
            var elemType = type.GetElementType();
            Debug.Assert(elemType != null);
            return RenderArrayType(RenderType(elemType), type.GetArrayRank());
        }

        // TODO: use QualifiedName with list of types?
        string typeName = type.Name;
        if (type.IsNested && type.DeclaringType != null)
        {
            typeName = $"{RenderType(type.DeclaringType).ToString()}.{typeName}";
        }

        if (type.IsGenericType)
        {
            Debug.Assert(type.IsConstructedGenericType);
            var typeArgs = type.GetGenericArguments().Select(RenderType).ToArray();
            return RenderGenericName(typeName, typeArgs);
        }

        return ParseTypeName(typeName);
    }

    public static ExpressionSyntax RenderMethod(MethodBase method)
    {
        assemblies.Add(method.Module.Assembly);
        var type = method.DeclaringType;
        var methodName = IdentifierName(method.Name);

        if (type == null)
            return methodName;

        if (method.IsConstructor)
            return IdentifierName(RenderType(type).ToString());

        string propertyName;
        if (IsGetPropertyMethod(method, out propertyName))
            methodName = IdentifierName(propertyName);
        if (IsSetPropertyMethod(method, out propertyName))
            methodName = IdentifierName(propertyName);

        if (method.IsStatic)
            return RenderMemberAccess(RenderType(type), methodName);

        var typeNamespace = type.Namespace;
        if (typeNamespace != null)
            usings.Add(typeNamespace);

        // TODO: instead of usings use full name of method?
        return methodName;
    }

    // Prerendered extern function
    public static readonly ExpressionSyntax CompareObjects = IdentifierName(nameof(ObjectsComparer.CompareObjects));

    public static readonly IdentifierNameSyntax AllocatorObject = IdentifierName(nameof(Allocator<int>.Object));

    // Prerendered program types
    public static readonly TypeSyntax ObjectType = RenderType(typeof(object));
    public static readonly TypeSyntax StringType = RenderType(typeof(string));
    public static readonly TypeSyntax BoolType = RenderType(typeof(bool));
    public static readonly TypeSyntax VoidType = RenderType(typeof(void));
    public static readonly ArrayTypeSyntax VectorOfObjects = (ArrayTypeSyntax) RenderType(typeof(object[]));
    public static readonly TypeSyntax MethodBaseType = RenderType(typeof(MethodBase));
    public static readonly TypeSyntax ModuleType = RenderType(typeof(Module));
    public static readonly TypeSyntax AssemblyType = RenderType(typeof(Assembly));
    public static readonly TypeSyntax SystemType = RenderType(typeof(Type));
    public static readonly TypeSyntax SystemArray = RenderType(typeof(Array));

    public static TypeSyntax AllocatorType(TypeSyntax typeArg)
    {
        var type = typeof(Allocator<int>).GetGenericTypeDefinition();
        return RenderGenericName(type.Name, typeArg);
    }

    // Prerendered tokens
    public static readonly SyntaxToken Public = Token(SyntaxKind.PublicKeyword);
    public static readonly SyntaxToken Private = Token(SyntaxKind.PrivateKeyword);
    public static readonly SyntaxToken Static = Token(SyntaxKind.StaticKeyword);

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

    public static ExpressionSyntax RenderEnum(Enum e)
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

    public static ArrayCreationExpressionSyntax RenderArrayCreation(ArrayTypeSyntax type, IEnumerable<ExpressionSyntax>? init)
    {
        InitializerExpressionSyntax? initializer = null;
        if (init != null)
            initializer =
                InitializerExpression(
                    SyntaxKind.ArrayInitializerExpression,
                    SeparatedList(init)
                );
        return
            ArrayCreationExpression(
                Token(SyntaxKind.NewKeyword),
                type,
                initializer
            );
    }

    public static AssignmentExpressionSyntax RenderArrayAssignment(
        ExpressionSyntax array,
        ExpressionSyntax value,
        params int[] index)
    {
        Debug.Assert(index.Length > 0);
        var indexArgument =
            BracketedArgumentList(
                SeparatedList(
                    index.Select(elem => Argument(RenderLiteral(elem)))
                )
            );
        return
            AssignmentExpression(
                SyntaxKind.SimpleAssignmentExpression,
                ElementAccessExpression(array).WithArgumentList(indexArgument),
                value
            );
    }

    public static ObjectCreationExpressionSyntax RenderObjectCreation(
        TypeSyntax type,
        ArgumentSyntax[] args,
        ExpressionSyntax[] init)
    {
        InitializerExpressionSyntax? initializer = null;
        ArgumentListSyntax? argumentList = null;
        if (init.Length != 0)
        {
            initializer = InitializerExpression(SyntaxKind.ObjectInitializerExpression, SeparatedList(init));
        }
        if (args.Length != 0 || init.Length == 0)
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
        ExpressionSyntax[] args,
        ExpressionSyntax[] init)
    {
        return RenderObjectCreation(type, args.Select(Argument).ToArray(), init);
    }

    public static ObjectCreationExpressionSyntax RenderObjectCreation(TypeSyntax type,
        ExpressionSyntax[] args, (ExpressionSyntax, ExpressionSyntax)[] init)
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

    public static ExpressionSyntax RenderCall(ExpressionSyntax? thisArg, MethodBase method, params ExpressionSyntax[] args)
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
            return function;

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

    public static InvocationExpressionSyntax RenderCall(string memberOf, string memberName, TypeSyntax[] genericArgs, params ExpressionSyntax[] args)
    {
        var member = RenderGenericName(memberName, genericArgs);
        var function = RenderMemberAccess(IdentifierName(memberOf), member);
        return RenderCall(function, args);
    }

    public static LiteralExpressionSyntax RenderLiteral(string? literal)
    {
        return LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(literal ?? String.Empty));
    }

    public static LiteralExpressionSyntax RenderLiteral(int literal)
    {
        return LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(literal));
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

    public static CompilationUnitSyntax RenderProgram(
        string namespaceName,
        params MemberDeclarationSyntax[] renderedClasses)
    {
        var renderedNamespace =
            NamespaceDeclaration(IdentifierName(namespaceName))
                .AddMembers(renderedClasses);
        return
            CompilationUnit()
                .AddUsings(ProgramUsings())
                .AddMembers(renderedNamespace);
    }
}
