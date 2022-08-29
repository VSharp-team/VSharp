using System.Diagnostics;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

internal static class CodeRenderer
{
    // Prerendered program types
    public static readonly TypeSyntax ObjectType = ParseTypeName("object");
    public static readonly TypeSyntax StringType = ParseTypeName("string");
    public static readonly TypeSyntax BoolType = ParseTypeName("bool");
    public static readonly TypeSyntax VoidType = ParseTypeName("void");
    public static readonly ArrayTypeSyntax VectorOfObjects = RenderVectorType("object");
    public static readonly TypeSyntax MethodBaseType = ParseTypeName("MethodBase");
    public static readonly TypeSyntax ModuleType = ParseTypeName("Module");
    public static readonly TypeSyntax AssemblyType = ParseTypeName("Assembly");
    public static readonly TypeSyntax SystemType = ParseTypeName("Type");
    public static readonly TypeSyntax SystemArray = ParseTypeName("Array");
    public static readonly TypeSyntax TargetInvocationExceptionType = ParseTypeName("TargetInvocationException");
    public static readonly TypeSyntax ExceptionType = ParseTypeName("Exception");
    public static readonly TypeSyntax MulticastDelegate = ParseTypeName("MulticastDelegate");
    public static readonly TypeSyntax BindingFlagsType = ParseTypeName("BindingFlags");

    // Prerendered tokends
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

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax memberOf, string[] memberNames, params ExpressionSyntax[] args)
    {
        var function = RenderMemberAccess(memberOf, memberNames);
        return RenderCall(function, args);
    }

    public static InvocationExpressionSyntax RenderCall(ExpressionSyntax memberOf, string memberName, params ExpressionSyntax[] args)
    {
        var function = RenderMemberAccess(memberOf, memberName);
        return RenderCall(function, args);
    }

    // public static InvocationExpressionSyntax RenderCall(string memberOf, string memberName, params ExpressionSyntax[] args)
    // {
    //     var function = RenderMemberAccess(memberOf, memberName);
    //     return RenderCall(function, args);
    // }

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

    public static ExpressionSyntax RenderMemberAccess(ExpressionSyntax memberOf, params string[] memberNames)
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

    public static ArrayTypeSyntax RenderArrayType(string elemTypeName, int rank)
    {
        var dims = Enumerable.Repeat(OmittedArraySizeExpression(), rank);
        var arrayRankSpecifier = ArrayRankSpecifier(SeparatedList<ExpressionSyntax>(dims));
        return
            ArrayType(
                ParseTypeName(elemTypeName),
                SingletonList(arrayRankSpecifier)
            );
    }

    public static ArrayTypeSyntax RenderVectorType(string elemTypeName)
    {
        return RenderArrayType(elemTypeName, 1);
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
        string[] usings,
        string namespaceName,
        ClassDeclarationSyntax renderedClass)

    {
        var usingDetectives = new UsingDirectiveSyntax[usings.Length];
        for (var i = 0; i < usings.Length; i++)
        {
            usingDetectives[i] = UsingDirective(ParseName(usings[i]));
        }

        var renderedNamespace =
            NamespaceDeclaration(IdentifierName(namespaceName))
                .AddMembers(renderedClass);
        return
            CompilationUnit()
                .AddUsings(usingDetectives)
                .AddMembers(renderedNamespace);
    }

}
