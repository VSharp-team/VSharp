using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

internal class TypeRenderer
{
    private readonly IdentifiersCache _cache;
    private readonly TypeDeclarationSyntax _declaration;
    private readonly List<FieldDeclarationSyntax> _fields = new ();
    private readonly List<MethodRenderer> _renderingMethods = new ();

    public SimpleNameSyntax TypeId { get; }

    public TypeRenderer(
        IdentifiersCache cache,
        SimpleNameSyntax typeId,
        bool isStruct,
        IEnumerable<Type>? baseTypes,
        AttributeListSyntax? attributes,
        SyntaxToken[]? modifiers)
    {
        // Creating identifiers cache
        _cache = new IdentifiersCache(cache);
        // Creating type declaration
        TypeId = typeId;
        if (isStruct)
            _declaration = StructDeclaration(TypeId.Identifier);
        else
            _declaration = ClassDeclaration(TypeId.Identifier);

        if (baseTypes != null)
        {
            var rendered = baseTypes.Select(t => SimpleBaseType(RenderType(t)));
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
        string methodName,
        bool exactName,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        TypeSyntax resultType,
        IdentifierNameSyntax[]? genericNames,
        NameSyntax? interfaceName,
        params (TypeSyntax, string)[] args)
    {
        SimpleNameSyntax methodId =
            exactName
                ? IdentifierName(methodName)
                : _cache.GenerateIdentifier(methodName);
        if (genericNames != null)
            methodId = GenericName(methodId.ToString());
        var method =
            new MethodRenderer(
                _cache,
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
        params (TypeSyntax, string)[] args)
    {
        return AddMethod(methodName, false, attributes, modifiers, resultType, null, null, args);
    }

    public MethodRenderer AddMethod(MethodBase method)
    {
        // TODO: render all info (is virtual, and others)
        var declaringType = method.DeclaringType;
        Debug.Assert(declaringType != null);

        var modifiers = new List<SyntaxToken>();
        NameSyntax? interfaceName = null;
        if (declaringType.IsInterface)
        {
            interfaceName = RenderTypeName(declaringType);
        }
        else
        {
            modifiers.Add(Public);
            if (method.IsStatic) modifiers.Add(Static);
            if (method.IsVirtual && !method.IsAbstract) modifiers.Add(Override);
        }
        var resultType = RenderType(Reflection.getMethodReturnType(method));
        IdentifierNameSyntax[]? generics = null;
        if (method.IsGenericMethod)
            generics = method.GetGenericArguments().Select(t => IdentifierName(t.ToString())).ToArray();
        var args =
            method.GetParameters()
                .Select(p => (RenderType(p.ParameterType), p.Name ?? "arg"))
                .ToArray();
        var modifiersArray = modifiers.ToArray();
        if (method.IsConstructor)
            return AddConstructor(null, modifiersArray, args);

        return AddMethod(method.Name, true, null, modifiersArray, resultType, generics, interfaceName, args);
    }

    public MethodRenderer AddConstructor(
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        params (TypeSyntax, string)[] args)
    {
        var method =
            new MethodRenderer(
                _cache,
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

    public TypeDeclarationSyntax Render()
    {
        var members = new List<MemberDeclarationSyntax>();
        members.AddRange(_fields);
        members.AddRange(_renderingMethods.Select(method => method.Render()));
        return _declaration.WithMembers(List(members));
    }
}
