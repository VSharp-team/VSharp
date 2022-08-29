using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

public class IdentifiersCache
{
    private readonly Dictionary<string, IdentifierNameSyntax> idInitializers;
    private readonly Dictionary<string, SyntaxToken> identifiers;

    public IdentifiersCache()
    {
        idInitializers = new Dictionary<string, IdentifierNameSyntax>();
        identifiers = new Dictionary<string, SyntaxToken>();
    }

    public IdentifiersCache(IdentifiersCache cache)
    {
        idInitializers = new Dictionary<string, IdentifierNameSyntax>(cache.idInitializers);
        identifiers = new Dictionary<string, SyntaxToken>(cache.identifiers);
    }

    public (SyntaxToken, IdentifierNameSyntax) GenerateIdentifier(string identifierName)
    {
        identifierName = IdGenerator.startingWith(identifierName);
        SyntaxToken identifier = Identifier(identifierName);
        if (!identifiers.TryAdd(identifierName, identifier))
            throw new ArgumentException("ID generator failed!");

        return (identifier, IdentifierName(identifierName));
    }

    public bool TryGetIdByInit(string initializerString, out IdentifierNameSyntax? result)
    {
        return idInitializers.TryGetValue(initializerString, out result);
    }

    public void SetIdInit(IdentifierNameSyntax varId, string initializerString)
    {
        idInitializers[initializerString] = varId;
    }

}
