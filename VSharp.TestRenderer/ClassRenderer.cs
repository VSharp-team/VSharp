using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

internal class ClassRenderer
{
    private readonly IdentifiersCache cache;
    private readonly ClassDeclarationSyntax declaration;
    private readonly List<FieldDeclarationSyntax> fields = new ();
    private readonly List<MethodRenderer> renderingMethods = new ();

    public ClassRenderer(
        string className,
        AttributeListSyntax? attributes,
        SyntaxToken[]? modifiers)
    {
        // Creating identifiers cache
        IdGenerator.reset();
        cache = new IdentifiersCache();

        declaration = ClassDeclaration(className);
        if (attributes != null)
            declaration = declaration.AddAttributeLists(attributes);
        if (modifiers != null)
            declaration = declaration.AddModifiers(modifiers);
    }

    public IdentifierNameSyntax AddField(
        TypeSyntax fieldType,
        string fieldName,
        SyntaxToken[]? modifiers,
        ExpressionSyntax? fieldInit)
    {
        var (fieldToken, fieldId) = cache.GenerateIdentifier(fieldName);
        var field = FieldDeclaration(RenderVarDecl(fieldType, fieldToken, fieldInit));
        if (modifiers != null)
        {
            var modifiersList = new SyntaxTokenList().AddRange(modifiers);
            field = field.WithModifiers(modifiersList);
        }
        fields.Add(field);
        return fieldId;
    }

    // TODO: add 'IdentifiersCache' arg to MethodRenderer (to pass it as an argument from here)
    public MethodRenderer AddMethod(
        string methodName,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        TypeSyntax resultType,
        params (TypeSyntax, string)[] args)
    {
        var (methodToken, methodId) = cache.GenerateIdentifier(methodName);
        var method =
            new MethodRenderer(
                methodToken,
                methodId,
                attributes,
                modifiers,
                resultType,
                args
            );
        renderingMethods.Add(method);
        return method;
    }

    public ClassDeclarationSyntax Render()
    {
        var members = new List<MemberDeclarationSyntax>();
        members.AddRange(fields);
        members.AddRange(renderingMethods.Select(method => method.Render()));
        return declaration.WithMembers(List(members));
    }

    ~ClassRenderer()
    {
        IdGenerator.restore();
    }
}
