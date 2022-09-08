using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

// internal interface IMethodContext
// {
//     IdentifierNameSyntax? ReferenceField()
// }

internal class ClassRenderer
{
    private readonly IdentifiersCache _cache;
    private readonly ClassDeclarationSyntax _declaration;
    private readonly List<FieldDeclarationSyntax> _fields = new ();
    private readonly List<MethodRenderer> _renderingMethods = new ();

    public ClassRenderer(
        string className,
        AttributeListSyntax? attributes,
        SyntaxToken[]? modifiers)
    {
        // Creating identifiers cache
        _cache = new IdentifiersCache();

        _declaration = ClassDeclaration(className);
        if (attributes != null)
            _declaration = _declaration.AddAttributeLists(attributes);
        if (modifiers != null)
            _declaration = _declaration.AddModifiers(modifiers);
    }

    // TODO: move this to method
    public IdentifierNameSyntax NewGenericParameter(string name)
    {
        return _cache.GenerateIdentifier(name);
    }

    public IdentifierNameSyntax AddField(
        TypeSyntax fieldType,
        string fieldName,
        SyntaxToken[]? modifiers,
        ExpressionSyntax? fieldInit)
    {
        var fieldId = _cache.GenerateIdentifier(fieldName);
        var field = FieldDeclaration(RenderVarDecl(fieldType, fieldId.Identifier, fieldInit));
        if (modifiers != null)
        {
            var modifiersList = new SyntaxTokenList().AddRange(modifiers);
            field = field.WithModifiers(modifiersList);
        }
        _fields.Add(field);
        return fieldId;
    }

    public MethodRenderer AddMethod(
        string methodName,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        TypeSyntax resultType,
        IdentifierNameSyntax[]? genericNames,
        params (TypeSyntax, string)[] args)
    {
        // TODO: use another function for generic methods
        SimpleNameSyntax methodId = _cache.GenerateIdentifier(methodName);
        if (genericNames != null)
            methodId = GenericName(methodId.ToString());
        var method =
            new MethodRenderer(
                _cache,
                methodId,
                attributes,
                modifiers,
                resultType,
                genericNames,
                args
            );
        _renderingMethods.Add(method);
        return method;
    }

    public MethodRenderer AddMethod(
        string methodName,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        TypeSyntax resultType,
        params (TypeSyntax, string)[] args)
    {
        return AddMethod(methodName, attributes, modifiers, resultType, null, args);
    }

    public ClassDeclarationSyntax Render()
    {
        var members = new List<MemberDeclarationSyntax>();
        members.AddRange(_fields);
        members.AddRange(_renderingMethods.Select(method => method.Render()));
        return _declaration.WithMembers(List(members));
    }
}
