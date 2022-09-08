using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

public class IdentifiersCache
{
    private readonly Dictionary<string, IdentifierNameSyntax> _idInitializers;
    private readonly Dictionary<string, IdentifierNameSyntax> _identifiers;
    private readonly Dictionary<string, int> _idNames;

    public IdentifiersCache()
    {
        _idInitializers = new Dictionary<string, IdentifierNameSyntax>();
        _identifiers = new Dictionary<string, IdentifierNameSyntax>();
        _idNames = new Dictionary<string, int>();
    }

    public IdentifiersCache(IdentifiersCache cache)
    {
        _idInitializers = new Dictionary<string, IdentifierNameSyntax>(cache._idInitializers);
        _identifiers = new Dictionary<string, IdentifierNameSyntax>(cache._identifiers);
        _idNames = new Dictionary<string, int>(cache._idNames);
    }

    public IdentifierNameSyntax GenerateIdentifier(string identifierName)
    {
        _idNames.TryGetValue(identifierName, out var i);
        _idNames[identifierName] = i + 1;
        if (i > 0)
            identifierName += i + 1;
        var identifier = IdentifierName(identifierName);
        if (!_identifiers.TryAdd(identifierName, identifier))
            throw new ArgumentException("ID generator failed!");

        return identifier;
    }

    public bool TryGetIdByInit(string initializerString, out IdentifierNameSyntax? result)
    {
        return _idInitializers.TryGetValue(initializerString, out result);
    }

    public void SetIdInit(IdentifierNameSyntax varId, string initializerString)
    {
        _idInitializers[initializerString] = varId;
    }

}
