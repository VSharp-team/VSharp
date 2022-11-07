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
        int i = 0;
        _idNames.TryGetValue(identifierName, out i);

        var uniqueName = identifierName;
        IdentifierNameSyntax identifier;
        do
        {
            if (i > 0) uniqueName = identifierName + i;
            identifier = IdentifierName(uniqueName);
            i++;
        } while (!_identifiers.TryAdd(uniqueName, identifier));

        _idNames[identifierName] = i;

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
