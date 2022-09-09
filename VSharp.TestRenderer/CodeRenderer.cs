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
    // Used assemblies
    private static readonly HashSet<Assembly> assemblies = new ();

    private static readonly string[] PrerenderedUsings =
    {
        nameof(System),
        nameof(System.Reflection),
        "NUnit.Framework",
        "VSharp.TestExtensions"
    };

    private static readonly Assembly[] PrerenderedAssemblies =
    {
        Assembly.GetAssembly(typeof(ObjectsComparer))
    };

    public static void PrepareCache()
    {
        usings.Clear();
        usings.UnionWith(PrerenderedUsings);
        assemblies.Clear();
        assemblies.UnionWith(PrerenderedAssemblies);
    }

    public static Assembly[] UsedAssemblies()
    {
        return assemblies.ToArray();
    }

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
        var typeNamespace = type.Namespace;
        if (typeNamespace != null)
            usings.Add(typeNamespace);

        if (type.IsArray)
        {
            var elemType = type.GetElementType();
            Debug.Assert(elemType != null);
            return RenderArrayType(RenderType(elemType), type.GetArrayRank());
        }

        if (type.IsGenericType)
        {
            Debug.Assert(type.IsConstructedGenericType);
            var typeArgs = type.GetGenericArguments().Select(RenderType).ToArray();
            return RenderGenericName(type.Name, typeArgs);
        }

        return ParseTypeName(type.Name);
    }

    public static IdentifierNameSyntax RenderMethod(MethodBase method)
    {
        assemblies.Add(method.Module.Assembly);
        var methodNamespace = method.DeclaringType?.Namespace;
        if (methodNamespace != null) usings.Add(methodNamespace);

        // TODO: instead of usings use full name of method?
        return IdentifierName(method.Name);
    }

    // Prerendered extern function
    public static readonly IdentifierNameSyntax CompareObjects = IdentifierName(nameof(ObjectsComparer.CompareObjects));
    public static readonly IdentifierNameSyntax AllocatorToObject = IdentifierName(nameof(Allocator<int>.ToObject));

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
    public static readonly TypeSyntax TargetInvocationExceptionType = RenderType(typeof(TargetInvocationException));
    public static readonly TypeSyntax ExceptionType = RenderType(typeof(Exception));
    public static readonly TypeSyntax MulticastDelegate = RenderType(typeof(MulticastDelegate));
    public static readonly TypeSyntax BindingFlagsType = RenderType(typeof(BindingFlags));

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

    public static readonly ExpressionSyntax BindingFlags =
        BinaryExpression(
            SyntaxKind.BitwiseOrExpression,
            RenderEnum(System.Reflection.BindingFlags.Static),
            BinaryExpression(
                SyntaxKind.BitwiseOrExpression,
                RenderEnum(System.Reflection.BindingFlags.NonPublic),
                BinaryExpression(
                    SyntaxKind.BitwiseOrExpression,
                    RenderEnum(System.Reflection.BindingFlags.Public),
                    RenderEnum(System.Reflection.BindingFlags.Instance)
                )
            )
        );

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

    public static ExpressionSyntax RenderEnum(Enum e)
    {
        var type = RenderType(e.GetType());
        var value = Enum.GetName(e.GetType(), e);
        Debug.Assert(value != null);

        return RenderMemberAccess(type, IdentifierName(value));
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

    public static ArrayCreationExpressionSyntax RenderArrayCreation(ArrayTypeSyntax type, IEnumerable<ExpressionSyntax> init)
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

    public static ObjectCreationExpressionSyntax RenderObjectCreation(TypeSyntax type, params ExpressionSyntax[] init)
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

    public static ObjectCreationExpressionSyntax RenderObjectCreation(TypeSyntax type, params (ExpressionSyntax, ExpressionSyntax)[] init)
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

        return RenderObjectCreation(type, keysWithValues);
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

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax? thisArg, MethodBase method, params ExpressionSyntax[] args)
    {
        ExpressionSyntax function;
        if (thisArg == null)
        {
            Debug.Assert(!Reflection.hasThis(method));
            function = RenderMethod(method);
        }
        else
        {
            Debug.Assert(Reflection.hasThis(method));
            function = RenderMemberAccess(thisArg, RenderMethod(method));
        }

        return RenderCall(function, args);
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
        UsingDirectiveSyntax[] usingDirectives =
            usings.Select(usingName => UsingDirective(ParseName(usingName))).ToArray();

        var renderedNamespace =
            NamespaceDeclaration(IdentifierName(namespaceName))
                .AddMembers(renderedClasses);
        return
            CompilationUnit()
                .AddUsings(usingDirectives)
                .AddMembers(renderedNamespace);
    }

}
