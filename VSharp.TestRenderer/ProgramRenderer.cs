using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using VSharp.TestExtensions;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;
using static VSharp.TestRenderer.CodeRenderer;

namespace VSharp.TestRenderer;

internal interface IReferenceManager
{
    void AddUsing(string name);
    void AddStaticUsing(string name);
    void AddAssembly(Assembly assembly);
    void AddTestExtensions();
    void AddObjectsComparer();
}

internal class ProgramRenderer
{
    private readonly IdentifiersCache _cache;
    private readonly ReferenceManager _referenceManager;
    private readonly BaseNamespaceDeclarationSyntax _namespace;
    private readonly List<TypeRenderer> _renderingTypes = new ();

    public ProgramRenderer(string namespaceName)
    {
        // Creating identifiers cache
        _cache = new IdentifiersCache(PredefinedTypes.Values); // TODO: add other types

        // Creating reference manager
        var namespaces = new List<string>();
        var currentNamespace = "";
        foreach (var name in namespaceName.Split('.'))
        {
            currentNamespace += name;
            namespaces.Add(currentNamespace);
            currentNamespace += '.';
        }
        _referenceManager = new ReferenceManager(namespaces);

        var namespaceId = _cache.GenerateIdentifier(namespaceName);
        _namespace = FileScopedNamespaceDeclaration(namespaceId);
    }

    public void AddNUnitToUsings()
    {
        _referenceManager.AddNUnit();
    }

    public void AddExtensionsToUsings()
    {
        _referenceManager.AddTestExtensions();
    }

    public Assembly[] UsedAssemblies()
    {
        return _referenceManager.UsedAssemblies();
    }

    public TypeRenderer AddType(
        string name,
        bool isStruct,
        IEnumerable<Type>? baseTypes,
        AttributeListSyntax? attributes,
        SyntaxToken[]? modifiers)
    {
        IdentifierNameSyntax typeId = _cache.GenerateIdentifier(name);
        var type =
            new TypeRenderer(
                _cache,
                _referenceManager,
                typeId,
                isStruct,
                baseTypes,
                attributes,
                modifiers
            );
        _renderingTypes.Add(type);
        return type;
    }

    public CompilationUnitSyntax? Render()
    {
        var members = new List<MemberDeclarationSyntax>();
        foreach (var renderingType in _renderingTypes)
        {
            var result = renderingType.RenderedType;
            if (result != null)
                members.Add(result);
        }

        if (members.Count == 0)
            return null;

        var renderedNamespace = _namespace.WithMembers(List(members));
        return
            CompilationUnit()
                .AddUsings(_referenceManager.RenderUsings())
                .AddMembers(renderedNamespace);
    }

    // TODO: if type or method was not rendered, exclude it's usings
    private class ReferenceManager : IReferenceManager
    {
        // Needed usings
        private readonly HashSet<string> _usings = new ();

        // Needed static usings
        private readonly HashSet<string> _staticUsings = new ();

        // Namespaces, that should not be included to usings
        private readonly HashSet<string> _nonIncludingNamespaces;

        // Used assemblies
        private readonly HashSet<Assembly> _assemblies = new ();

        private bool _objectsComparerAdded;

        public ReferenceManager(IEnumerable<string> nonIncludingNamespaces)
        {
            _objectsComparerAdded = false;
            _nonIncludingNamespaces = new HashSet<string>(nonIncludingNamespaces);
        }

        public void AddUsing(string name)
        {
            if (!_nonIncludingNamespaces.Contains(name))
                _usings.Add(name);
        }

        public void AddStaticUsing(string name)
        {
            if (!_nonIncludingNamespaces.Contains(name))
                _staticUsings.Add(name);
        }

        public void AddAssembly(Assembly assembly)
        {
            if (assembly != Reflection.mscorlibAssembly)
                _assemblies.Add(assembly);
        }

        public void AddNUnit()
        {
            _usings.Add("NUnit.Framework");
        }

        public void AddTestExtensions()
        {
            _usings.Add("VSharp.TestExtensions");
        }

        public void AddObjectsComparer()
        {
            if (_objectsComparerAdded) return;
            var name = typeof(ObjectsComparer).FullName;
            Debug.Assert(name != null);
            _staticUsings.Add(name);
            _objectsComparerAdded = true;
        }

        public UsingDirectiveSyntax[] RenderUsings()
        {
            var nonStaticElems = _usings.Select(x =>
                UsingDirective(ParseName(x)));
            var staticElems = _staticUsings.Select(x =>
                UsingDirective(Static, null, ParseName(x)));
            return nonStaticElems.Concat(staticElems).ToArray();
        }

        public Assembly[] UsedAssemblies()
        {
            return _assemblies.ToArray();
        }
    }
}
