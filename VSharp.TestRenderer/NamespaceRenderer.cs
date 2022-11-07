using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

internal class NamespaceRenderer
{
    private readonly IdentifiersCache _cache;
    private readonly BaseNamespaceDeclarationSyntax _declaration;
    private readonly List<TypeRenderer> _renderingTypes = new ();

    public NamespaceRenderer(string name)
    {
        // Creating identifiers cache
        _cache = new IdentifiersCache();
        var namespaceId = _cache.GenerateIdentifier(name);
        _declaration = FileScopedNamespaceDeclaration(namespaceId);
    }

    public TypeRenderer AddType(
        string name,
        bool isStruct,
        IEnumerable<Type>? baseTypes,
        AttributeListSyntax? attributes,
        SyntaxToken[]? modifiers)
    {
        SimpleNameSyntax typeId = _cache.GenerateIdentifier(name);
        var type =
            new TypeRenderer(
                _cache,
                typeId,
                isStruct,
                baseTypes,
                attributes,
                modifiers
            );
        _renderingTypes.Add(type);
        return type;
    }

    public BaseNamespaceDeclarationSyntax Render()
    {
        var members = new List<MemberDeclarationSyntax>();
        members.AddRange(_renderingTypes.Select(type => type.Render()));
        return _declaration.WithMembers(List(members));
    }
}
