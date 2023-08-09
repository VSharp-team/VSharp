using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

internal class TypeRenderer : CodeRenderer
{
    private readonly IdentifiersCache _cache;
    private readonly IReferenceManager _referenceManager;
    private TypeDeclarationSyntax _declaration;
    private bool _finished;
    private readonly List<FieldDeclarationSyntax> _fields = new ();
    private readonly List<MethodRenderer> _renderingMethods = new ();

    private struct PropertyAccessors
    {
        public MethodRenderer? Get;
        public MethodRenderer? Set;

        public PropertyAccessors()
        {
            Get = null;
            Set = null;
        }
    }

    private readonly struct PropertyWrapper
    {
        private readonly PropertyInfo _property;
        public readonly BasePropertyDeclarationSyntax Declaration;

        public PropertyWrapper(PropertyInfo property, BasePropertyDeclarationSyntax declaration)
        {
            _property = property;
            Declaration = declaration;
        }

        public override bool Equals(object? obj)
        {
            if (obj is PropertyWrapper other)
            {
                return _property == other._property;
            }
            return false;
        }

        public override int GetHashCode()
        {
            return _property.GetHashCode();
        }
    }

    private readonly Dictionary<PropertyWrapper, PropertyAccessors> _renderingProperties = new ();

    public SimpleNameSyntax TypeId { get; }

    public TypeRenderer(
        IdentifiersCache cache,
        IReferenceManager referenceManager,
        SimpleNameSyntax typeId,
        bool isStruct,
        IEnumerable<Type>? baseTypes,
        AttributeListSyntax? attributes,
        SyntaxToken[]? modifiers) : base(referenceManager)
    {
        // Creating identifiers cache
        _cache = new IdentifiersCache(cache);
        // Remembering referenceManager
        _referenceManager = referenceManager;
        // Marking, that type was not already rendered
        _finished = false;
        // Creating type declaration
        TypeId = typeId;
        if (isStruct)
            _declaration = StructDeclaration(TypeId.Identifier);
        else
            _declaration = ClassDeclaration(TypeId.Identifier);

        var baseTypesArray = baseTypes as List<Type> ?? baseTypes?.ToList();
        if (baseTypesArray != null && baseTypesArray.Count > 0)
        {
            var rendered = baseTypesArray.Select(t => SimpleBaseType(RenderType(t)));
            _declaration = _declaration.WithBaseList(BaseList(SeparatedList<BaseTypeSyntax>(rendered)));
        }
        if (attributes != null)
            _declaration = _declaration.AddAttributeLists(attributes);
        if (modifiers != null)
            _declaration = _declaration.AddModifiers(modifiers);
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

    private MethodRenderer AddMethod(
        SimpleNameSyntax methodId,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        TypeSyntax resultType,
        IdentifierNameSyntax[]? genericNames,
        NameSyntax? interfaceName,
        params ParameterRenderInfo[] args)
    {
        var method =
            new MethodRenderer(
                _cache,
                _referenceManager,
                methodId,
                attributes,
                modifiers,
                false,
                resultType,
                genericNames,
                interfaceName,
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
        params ParameterRenderInfo[] args)
    {
        SimpleNameSyntax methodId = _cache.GenerateIdentifier(methodName);
        return AddMethod(methodId, attributes, modifiers, resultType, null, null, args);
    }

    // Adds to rendering type property or indexer
    private MethodRenderer AddPropertyMethod(
        string propertyName,
        AccessorType accessorType,
        MethodBase method,
        SimpleNameSyntax propertyId,
        SyntaxToken[] modifiers,
        NameSyntax? interfaceName,
        ParameterRenderInfo[] args)
    {
        var declaringType = method.DeclaringType;
        var bindingFlags = BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.Public;
        var property = declaringType?.GetProperty(propertyName, bindingFlags);
        Debug.Assert(property != null);
        var propertyType = RenderType(property.PropertyType);
        BasePropertyDeclarationSyntax propertyDecl;
        if (IsIndexer(method))
        {
            var indexerArgs =
                accessorType == AccessorType.Set
                    // If it's set method, skipping 'value' argument
                    ? args.Take(args.Length - 1)
                    : args;
            var renderedArgs =
                indexerArgs.Select(a =>
                    a.BuildParameter(_cache.GenerateIdentifier(a.ParameterName).Identifier)
                );
            propertyDecl = RenderIndexerDeclaration(propertyType, modifiers, renderedArgs);
        }
        else
        {
            propertyDecl = RenderPropertyDeclaration(propertyType, propertyId.Identifier, modifiers, interfaceName);
        }

        var renderer =
            new MethodRenderer(
                _cache,
                _referenceManager,
                propertyId,
                null,
                System.Array.Empty<SyntaxToken>(),
                false,
                accessorType == AccessorType.Get ? propertyType : VoidType,
                null,
                null
            );
        var propertyWrapper = new PropertyWrapper(property, propertyDecl);
        if (!_renderingProperties.TryGetValue(propertyWrapper, out var accessors))
            accessors = new PropertyAccessors();
        switch (accessorType)
        {
            case AccessorType.Get:
                accessors.Get = renderer;
                break;
            case AccessorType.Set:
                accessors.Set = renderer;
                break;
        }
        _renderingProperties[propertyWrapper] = accessors;

        return renderer;
    }

    public MethodRenderer AddMockMethod(MethodBase method)
    {
        // TODO: render all info (is virtual, and others)
        var declaringType = method.DeclaringType;
        Debug.Assert(declaringType != null);

        var modifiers = new List<SyntaxToken>();
        NameSyntax? interfaceName = null;
        // Indexer has no explicit interface specifier
        if (declaringType.IsInterface && !IsIndexer(method))
        {
            interfaceName = RenderTypeName(declaringType);
        }
        else
        {
            modifiers.Add(Public);
            if (method.IsStatic) modifiers.Add(Static);
            var methodOverrides = method.IsVirtual && !TypeUtils.isDelegate(method.DeclaringType);
            if (methodOverrides) modifiers.Add(Override);
        }
        var resultType = RenderType(Reflection.getMethodReturnType(method));
        IdentifierNameSyntax[]? generics = null;
        if (method.IsGenericMethod)
            generics = method.GetGenericArguments().Select(t => IdentifierName(t.ToString())).ToArray();
        var args =
            method.GetParameters()
                .Select(p => new ParameterRenderInfo(p.Name ?? "arg", RenderType(p.ParameterType), p))
                .ToArray();

        if (method.IsConstructor)
            return AddConstructor(null, modifiers.ToArray(), args);

        var methodId = RenderMethodName(method);
        if (IsPropertyMethod(method, out var propertyName, out var accessorType))
        {
            modifiers.Remove(Override);
            return AddPropertyMethod(propertyName, accessorType, method, methodId, modifiers.ToArray(), interfaceName, args);
        }
        return AddMethod(methodId, null, modifiers.ToArray(), resultType, generics, interfaceName, args);
    }

    public MethodRenderer AddConstructor(
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        params ParameterRenderInfo[] args)
    {
        var method =
            new MethodRenderer(
                _cache,
                _referenceManager,
                TypeId,
                attributes,
                modifiers,
                true,
                VoidType,
                null,
                null,
                args
            );
        _renderingMethods.Add(method);
        return method;
    }

    public TypeDeclarationSyntax? RenderedType => _finished ? _declaration : null;

    public TypeDeclarationSyntax Render()
    {
        if (_finished)
            throw new InvalidOperationException(
                "TypeRenderer: could not render type twice");

        var members = new List<MemberDeclarationSyntax>();
        members.AddRange(_fields);

        foreach (var renderingMethod in _renderingMethods)
        {
            var result = renderingMethod.RenderedMethod;
            if (result != null)
                members.Add(result);
        }

        foreach (var (property, accessors) in _renderingProperties)
        {
            var renderedAccessors = new List<AccessorDeclarationSyntax>();
            var get = accessors.Get;
            if (get != null)
            {
                var method = get.RenderedMethod;
                if (method == null)
                    continue;
                var body = method.Body;
                renderedAccessors.Add(AccessorDeclaration(SyntaxKind.GetAccessorDeclaration, body));
            }
            var set = accessors.Set;
            if (set != null)
            {
                var method = set.RenderedMethod;
                if (method == null)
                    continue;
                var body = method.Body;
                renderedAccessors.Add(AccessorDeclaration(SyntaxKind.SetAccessorDeclaration, body));
            }
            Debug.Assert(get != null || set != null);
            var declaration = property.Declaration.WithAccessorList(
                AccessorList(List(renderedAccessors))
            );
            members.Add(declaration);
        }

        _declaration = _declaration.WithMembers(List(members));
        _finished = true;

        return _declaration;
    }
}
