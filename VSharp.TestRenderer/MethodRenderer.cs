using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

internal interface IBlock
{
    IdentifierNameSyntax NewIdentifier(string idName);
    IBlock NewBlock();
    IdentifierNameSyntax AddDecl(string varName, TypeSyntax? type, ExpressionSyntax init, bool reuse = false);
    void AddExpression(ExpressionSyntax expression);
    void AddAssertEqual(ExpressionSyntax x, ExpressionSyntax y);
    void AddAssert(ExpressionSyntax condition);
    void AddTryCatch(BlockSyntax tryBlock, TypeSyntax catchType, IdentifierNameSyntax exVar, BlockSyntax catchBlock);
    void AddTryCatch(BlockSyntax tryBlock, BlockSyntax catchBlock);
    void AddIf(ExpressionSyntax condition, StatementSyntax thenBranch, StatementSyntax? elseBranch = null);
    void AddFor(TypeSyntax? type, IdentifierNameSyntax iterator, ExpressionSyntax condition, ExpressionSyntax increment, BlockSyntax forBody);
    void AddFor(TypeSyntax? type, IdentifierNameSyntax iterator, ExpressionSyntax length, BlockSyntax forBody);
    void AddWhile(ExpressionSyntax condition, BlockSyntax whileBody);
    void AddForEach(TypeSyntax? type, IdentifierNameSyntax iterator, IdentifierNameSyntax where, BlockSyntax foreachBody);
    void AddForEach(TypeSyntax? type, IdentifierNameSyntax[] iterators, IdentifierNameSyntax where, BlockSyntax foreachBody);
    void AddReturn(ExpressionSyntax? whatToReturn);
    ExpressionSyntax RenderObject(object? obj, string? preferredName, bool explicitType = false);
    BlockSyntax Render();
}

// After creating method, 'Render()' should be called
// Otherwise it will not be added to declaring type
internal class MethodRenderer : CodeRenderer
{
    private BaseMethodDeclarationSyntax _declaration;
    private bool _finished;
    private readonly IBlock _body;

    public IdentifierNameSyntax[] ParametersIds { get; }
    public SimpleNameSyntax MethodId { get; }

    // TODO: allow method mutation after render?
    public IBlock Body =>
        !_finished
            ? _body
            : throw new InvalidOperationException(
                "MethodRenderer: could not change method body after it was rendered");

    public MethodRenderer(
        IdentifiersCache cache,
        IReferenceManager referenceManager,
        SimpleNameSyntax methodId,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        bool isConstructor,
        TypeSyntax resultType,
        IdentifierNameSyntax[]? generics,
        NameSyntax? interfaceName,
        params (TypeSyntax, string)[] args) : base(referenceManager)
    {
        // Creating identifiers cache
        var methodIdCache = new IdentifiersCache(cache);
        // Creating rendered objects cache
        var methodObjectsCache = new Dictionary<physicalAddress, ExpressionSyntax>();
        // Marking, that method was not already rendered
        _finished = false;
        // Creating method declaration
        MethodId = methodId;
        if (isConstructor)
        {
            _declaration = ConstructorDeclaration(methodId.Identifier);
        }
        else
        {
            var methodDecl = MethodDeclaration(resultType, methodId.Identifier);
            if (generics != null)
            {
                var typeVars =
                    TypeParameterList(
                        SeparatedList(
                            generics.Select(generic =>
                                TypeParameter(generic.Identifier)
                            )
                        )
                    );
                methodDecl = methodDecl.WithTypeParameterList(typeVars);
            }

            if (interfaceName != null)
            {
                Debug.Assert(modifiers.Length == 0);
                methodDecl = methodDecl.WithExplicitInterfaceSpecifier(ExplicitInterfaceSpecifier(interfaceName));
            }
            _declaration = methodDecl;
        }

        if (attributes != null)
            _declaration = _declaration.AddAttributeLists(attributes);
        var parameters = new ParameterSyntax[args.Length];
        ParametersIds = new IdentifierNameSyntax[args.Length];
        for (var i = 0; i < args.Length; i++)
        {
            var (type, varName) = args[i];
            var arg = methodIdCache.GenerateIdentifier(varName);
            ParametersIds[i] = arg;
            parameters[i] = Parameter(arg.Identifier).WithType(type);
        }
        var parameterList =
            ParameterList(
                SeparatedList<ParameterSyntax>()
                    .AddRange(parameters)
            );
        _declaration =
            _declaration
                .AddModifiers(modifiers)
                .WithParameterList(parameterList);
        _body = new BlockBuilder(methodIdCache, referenceManager, methodObjectsCache);
    }

    public void CallBaseConstructor(IEnumerable<ExpressionSyntax> args)
    {
        var declaration = _declaration as ConstructorDeclarationSyntax;
        Debug.Assert(declaration != null);
        var argumentList = ArgumentList(SeparatedList(args.Select(Argument)));
        var initializer = ConstructorInitializer(SyntaxKind.BaseConstructorInitializer, argumentList);
        _declaration = declaration.WithInitializer(initializer);
    }

    public IdentifierNameSyntax GetOneArg()
    {
        Debug.Assert(ParametersIds.Length == 1);
        return ParametersIds[0];
    }

    public (IdentifierNameSyntax, IdentifierNameSyntax) GetTwoArgs()
    {
        Debug.Assert(ParametersIds.Length == 2);
        return (ParametersIds[0], ParametersIds[1]);
    }

    public IdentifierNameSyntax[] GetArgs()
    {
        return ParametersIds;
    }

    // This method is called only after rendering
    internal BaseMethodDeclarationSyntax? RenderedMethod =>
        _finished ? _declaration : null;

    // This method should be invoked just once
    public BaseMethodDeclarationSyntax Render()
    {
        // If method was already rendered, throwing exception
        if (_finished)
            throw new InvalidOperationException(
                "MethodRenderer: could not render method twice");

        // Otherwise remembering result and returning it
        _declaration = _declaration.WithBody(Body.Render());
        _finished = true;

        return _declaration;
    }

    private class BlockBuilder : CodeRenderer, IBlock
    {
        // Variables cache
        private readonly IdentifiersCache _idCache;
        // Variables cache
        private readonly IReferenceManager _referenceManager;
        // Rendering objects cache
        private readonly Dictionary<physicalAddress, ExpressionSyntax> _renderedObjects;
        private readonly HashSet<physicalAddress> _startToRender;

        private readonly List<StatementSyntax> _statements = new();

        public BlockBuilder(
            IdentifiersCache idCache,
            IReferenceManager referenceManager,
            Dictionary<physicalAddress, ExpressionSyntax> renderedObjects) : base(referenceManager)
        {
            _idCache = idCache;
            _referenceManager = referenceManager;
            _renderedObjects = renderedObjects;
            _startToRender = new HashSet<physicalAddress>();
        }

        public IdentifierNameSyntax NewIdentifier(string idName)
        {
            return _idCache.GenerateIdentifier(idName);
        }

        public IBlock NewBlock()
        {
            return new BlockBuilder(new IdentifiersCache(_idCache), _referenceManager, _renderedObjects);
        }

        public IdentifierNameSyntax AddDecl(
            string varName,
            TypeSyntax? type,
            ExpressionSyntax init,
            bool reuse = false)
        {
            // TODO: to check for equality of syntax nodes use 'AreEquivalent'
            string initializerString = init.ToString();
            if (reuse && _idCache.TryGetIdByInit(initializerString, out var result))
            {
                Debug.Assert(result != null);
                return result;
            }

            var var = _idCache.GenerateIdentifier(varName);
            var varDecl = RenderVarDecl(type, var.Identifier, init);
            _statements.Add(LocalDeclarationStatement(varDecl));
            _idCache.SetIdInit(var, initializerString);
            return var;
        }

        public void AddExpression(ExpressionSyntax expression)
        {
            _statements.Add(ExpressionStatement(expression));
        }

        public void AddAssignment(AssignmentExpressionSyntax assignment)
        {
            _statements.Add(ExpressionStatement(assignment));
        }

        public void AddAssertEqual(ExpressionSyntax x, ExpressionSyntax y)
        {
            _statements.Add(ExpressionStatement(RenderAssertEqual(x, y)));
        }

        public void AddAssert(ExpressionSyntax condition)
        {
            _statements.Add(ExpressionStatement(RenderAssert(condition)));
        }

        private void AddTryCatch(BlockSyntax tryBlock, CatchDeclarationSyntax? declaration, BlockSyntax catchBlock)
        {
            var catchClause = CatchClause(declaration, null, catchBlock);
            var clauses = SingletonList(catchClause);
            var tryCatchBlock = TryStatement(tryBlock, clauses, null);
            _statements.Add(tryCatchBlock);
        }

        public void AddTryCatch(BlockSyntax tryBlock, BlockSyntax catchBlock)
        {
            AddTryCatch(tryBlock, null, catchBlock);
        }

        public void AddTryCatch(BlockSyntax tryBlock, TypeSyntax catchType, IdentifierNameSyntax exVar, BlockSyntax catchBlock)
        {
            var declaration = CatchDeclaration(catchType, exVar.Identifier);
            AddTryCatch(tryBlock, declaration, catchBlock);
        }

        public void AddIf(ExpressionSyntax condition, StatementSyntax thenBranch, StatementSyntax? elseBranch = null)
        {
            ElseClauseSyntax elseClause = null!;
            if (elseBranch != null) elseClause = ElseClause(elseBranch);
            var ifExpr = IfStatement(condition, thenBranch, elseClause);
            _statements.Add(ifExpr);
        }

        public void AddFor(
            TypeSyntax? type,
            IdentifierNameSyntax iterator,
            ExpressionSyntax condition,
            ExpressionSyntax increment,
            BlockSyntax forBody)
        {
            type ??= VarKeyword;
            var forStatement =
                ForStatement(
                    RenderVarDecl(type, iterator.Identifier, Zero),
                    SeparatedList<ExpressionSyntax>(),
                    condition,
                    SingletonSeparatedList(increment),
                    forBody
                );
            _statements.Add(forStatement);
        }

        public void AddFor(
            TypeSyntax? type,
            IdentifierNameSyntax iterator,
            ExpressionSyntax length,
            BlockSyntax forBody)
        {
            var increment = PrefixUnaryExpression(SyntaxKind.PreIncrementExpression, iterator);
            var condition = BinaryExpression(SyntaxKind.LessThanExpression, iterator, length);

            AddFor(type, iterator, condition, increment, forBody);
        }

        public void AddWhile(ExpressionSyntax condition, BlockSyntax whileBody)
        {
            var whileStatement = WhileStatement(condition, whileBody);
            _statements.Add(whileStatement);
        }

        public void AddForEach(TypeSyntax? type, IdentifierNameSyntax iterator, IdentifierNameSyntax where, BlockSyntax foreachBody)
        {
            type ??= VarKeyword;
            var forEach = ForEachStatement(type, iterator.Identifier, where, foreachBody);
            _statements.Add(forEach);
        }

        public void AddForEach(TypeSyntax? type, IdentifierNameSyntax[] iterators, IdentifierNameSyntax where, BlockSyntax foreachBody)
        {
            type ??= VarKeyword;
            var designation =
                ParenthesizedVariableDesignation(
                    SeparatedList(
                        iterators.Select(iterator =>
                            (VariableDesignationSyntax) SingleVariableDesignation(iterator.Identifier)
                        )
                    )
                );
            var varDecl = DeclarationExpression(type, designation);
            var forEach = ForEachVariableStatement(varDecl, where, foreachBody);
            _statements.Add(forEach);
        }

        public void AddReturn(ExpressionSyntax? whatToReturn)
        {
            _statements.Add(ReturnStatement(whatToReturn));
        }

        private ExpressionSyntax RenderArray(ArrayTypeSyntax type, System.Array obj, string? preferredName)
        {
            // TODO: use compact array representation, if array is big enough?
            var rank = obj.Rank;
            Debug.Assert(type != null);
            var initializer = new List<ExpressionSyntax>();
            if (rank > 1)
            {
                throw new NotImplementedException("implement rendering for non-vector arrays");
            }
            else
            {
                for (int i = obj.GetLowerBound(0); i <= obj.GetUpperBound(0); i++)
                {
                    var elementPreferredName = (preferredName ?? "array") + "_Elem" + i;
                    // TODO: if lower bound != 0, use Array.CreateInstance
                    initializer.Add(RenderObject(obj.GetValue(i), elementPreferredName));
                }
            }

            var elemType = obj.GetType().GetElementType();
            var allowImplicit = elemType is { IsValueType: true } && rank == 1;
            return RenderArrayCreation(type, initializer, allowImplicit);
        }

        private ExpressionSyntax RenderArray(System.Array obj, string? preferredName)
        {
            var type = (ArrayTypeSyntax) RenderType(obj.GetType());
            return RenderArray(type, obj, preferredName);
        }

        private ExpressionSyntax RenderCompactZeroLb(
            ArrayTypeSyntax type,
            int[] lengths,
            int[][] indices,
            object[] values,
            string? preferredName,
            object? defaultValue = null)
        {
            var arrayPreferredName = preferredName ?? "array";
            var createArray = RenderArrayCreation(type, lengths);
            var arrayId = AddDecl(arrayPreferredName, type, createArray);
            if (defaultValue != null)
            {
                var defaultId = RenderObject(defaultValue, preferredName);
                var call =
                    RenderCall(AllocatorType(), "Fill", arrayId, defaultId);
                AddExpression(call);
            }
            for (int i = 0; i < indices.Length; i++)
            {
                var elementPreferredName = arrayPreferredName + "_Elem" + i;
                var value = RenderObject(values[i], elementPreferredName);
                var assignment = RenderArrayAssignment(arrayId, value, indices[i]);
                AddAssignment(assignment);
            }

            return arrayId;
        }

        private ExpressionSyntax RenderCompactNonVector(
            ArrayTypeSyntax type,
            System.Array array,
            int[][] indices,
            object[] values,
            string? preferredName,
            object? defaultValue = null)
        {
            var rank = array.Rank;
            var lbs = new int[rank];
            var lengths = new int[rank];
            for (var i = 0; i < rank; i++)
            {
                lbs[i] = array.GetLowerBound(i);
                lengths[i] = array.GetLength(i);
            }
            var isZeroLbs = lbs.All(lb => lb == 0);
            if (isZeroLbs)
            {
                return RenderCompactZeroLb(type, lengths, indices, values, preferredName, defaultValue);
            }

            throw new NotImplementedException("rendering of arrays with non-zero lower bounds is not implemented");
        }

        private ExpressionSyntax RenderCompactArray(CompactArrayRepr obj, string? preferredName)
        {
            var array = obj.array;
            var indices = obj.indices;
            var values = obj.values;
            var t = array.GetType();
            var type = (ArrayTypeSyntax) RenderType(t);
            Debug.Assert(type != null);
            var defaultOf = Reflection.defaultOf(t.GetElementType());
            var defaultValue = obj.defaultValue;
            defaultValue = defaultValue == null || defaultValue.Equals(defaultOf) ? null : defaultValue;
            if (t.IsSZArray)
            {
                var lengths = new []{ array.Length };
                return RenderCompactZeroLb(type, lengths, indices, values, preferredName, defaultValue);
            }

            return RenderCompactNonVector(type, array, indices, values, preferredName, defaultValue);
        }

        private (ExpressionSyntax, ExpressionSyntax)[] RenderFieldValues(Type type, object obj)
        {
            var fields = Reflection.fieldsOf(false, type);
            var fieldsWithValues = new (ExpressionSyntax, ExpressionSyntax)[fields.Length];
            var i = 0;
            foreach (var (_, fieldInfo) in fields)
            {
                var name = fieldInfo.Name;
                var index = name.IndexOf(">k__BackingField", StringComparison.Ordinal);
                if (index > 0)
                    name = name[1 .. index];
                var fieldName = RenderObject(name);
                var fieldValue = RenderObject(fieldInfo.GetValue(obj), name);
                fieldsWithValues[i] = (fieldName, fieldValue);
                i++;
            }

            return fieldsWithValues;
        }

        private ExpressionSyntax RenderFields(object obj, string? preferredName)
        {
            var physAddress = new physicalAddress(obj);

            var type = obj.GetType();
            var isPublicType = type.IsPublic || type.IsNestedPublic;
            var typeExpr = RenderType(isPublicType ? type : typeof(object));

            // Rendering field values of object
            (ExpressionSyntax, ExpressionSyntax)[] fieldsWithValues;
            if (_startToRender.Contains(physAddress))
            {
                fieldsWithValues = System.Array.Empty<(ExpressionSyntax, ExpressionSyntax)>();
            }
            else
            {
                _startToRender.Add(physAddress);
                fieldsWithValues = RenderFieldValues(type, obj);
            }

            // Rendering allocator arguments
            ExpressionSyntax[] args;
            var wasRendered = _renderedObjects.TryGetValue(physAddress, out var rendered);
            if (wasRendered)
            {
                Debug.Assert(rendered != null);
                args = new[] {rendered};
            }
            else if (isPublicType)
            {
                args = System.Array.Empty<ExpressionSyntax>();
            }
            else
            {
                // TODO: maybe use fresh variable for name instead of inlining?
                Debug.Assert(type.AssemblyQualifiedName != null);
                args = new ExpressionSyntax[] {RenderLiteral(type.AssemblyQualifiedName)};
            }

            var allocator =
                RenderObjectCreation(AllocatorType(typeExpr), args, fieldsWithValues);
            var resultObject = RenderMemberAccess(allocator, AllocatorObject);
            // If object was not rendered already, declaring new variable for it
            var objId =
                wasRendered
                    ? resultObject
                    : AddDecl(preferredName ?? "obj", typeExpr, resultObject);
            return objId;
        }

        private ExpressionSyntax RenderMock(object obj, string? preferredName)
        {
            var typeOfMock = obj.GetType();
            var storageField =
                typeOfMock.GetFields(BindingFlags.Static | BindingFlags.NonPublic)
                    .First(f => f.Name.Contains("Storage"));
            var storage = storageField.GetValue(null) as global::System.Array;
            Debug.Assert(storage != null);
            var mockInfo = GetMockInfo(obj.GetType().Name);
            var mockType = mockInfo.MockName;
            var empty = System.Array.Empty<ExpressionSyntax>();
            var allocator = RenderObjectCreation(AllocatorType(mockType), empty, empty);
            var resultObject = RenderMemberAccess(allocator, AllocatorObject);
            var mockId = AddDecl(preferredName ?? "mock", mockType, resultObject);
            foreach (var (valuesType, setupMethod) in mockInfo.MethodsInfo)
            {
                var values = RenderArray(valuesType, storage, "values");
                var renderedValues =
                    storage.Length <= 5 ? values : AddDecl("values", null, values);
                AddExpression(RenderCall(mockId, setupMethod, renderedValues));
            }

            return mockId;
        }

        private ExpressionSyntax RenderComplexObject(object obj, string? preferredName = null)
        {
            var physAddress = new physicalAddress(obj);
            if (_renderedObjects.TryGetValue(physAddress, out var renderedResult))
                return renderedResult;

            ExpressionSyntax result = obj switch
            {
                System.Array a => RenderArray(a, preferredName),
                CompactArrayRepr a => RenderCompactArray(a, preferredName),
                Enum e => RenderEnum(e),
                Pointer => throw new NotImplementedException("RenderObject: implement rendering of pointers"),
                ValueType => RenderFields(obj, preferredName),
                _ when HasMockInfo(obj.GetType().Name) => RenderMock(obj, preferredName),
                _ when obj.GetType().IsClass => RenderFields(obj, preferredName),
                _ => throw new NotImplementedException($"RenderObject: unexpected object {obj}")
            };

            // For recursive objects: if (while rendering object) it was already added, assigning new value to it
            if (_renderedObjects.TryGetValue(physAddress, out var rendered))
            {
                AddAssignment(RenderAssignment(rendered, result));
                return rendered;
            }

            // Otherwise adding it to rendered objects
            _renderedObjects[physAddress] = result;

            return result;
        }

        public ExpressionSyntax RenderObject(
            object? obj,
            string? preferredName = null,
            bool explicitType = false) =>
            obj switch
        {
            null => RenderNull(),
            true => True,
            false => False,
            // byte, sbyte, char, short, ushort don't have data type suffix, so rendering cast (if needed)
            byte n => RenderByte(n, explicitType),
            sbyte n => RenderSByte(n, explicitType),
            char n => RenderChar(n, explicitType),
            short n => RenderShort(n, explicitType),
            ushort n => RenderUShort(n, explicitType),
            int n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            uint n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            long n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            ulong n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            float.NaN => RenderNaN(RenderType(typeof(float))),
            double.NaN => RenderNaN(RenderType(typeof(double))),
            float.Epsilon => RenderEpsilon(RenderType(typeof(float))),
            double.Epsilon => RenderEpsilon(RenderType(typeof(double))),
            float.PositiveInfinity => RenderPosInfinity(RenderType(typeof(float))),
            double.PositiveInfinity => RenderPosInfinity(RenderType(typeof(double))),
            float.NegativeInfinity => RenderNegInfinity(RenderType(typeof(float))),
            double.NegativeInfinity => RenderNegInfinity(RenderType(typeof(double))),
            // Using 'Literal($"{n}D", n)' to get data type suffix for double (for default it has not)
            double n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal($"{n}D", n)),
            decimal n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            nuint n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            nint n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            string s => LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(s)),
            _ => RenderComplexObject(obj, preferredName)
        };

        public BlockSyntax Render()
        {
            return Block(_statements);
        }
    }
}
