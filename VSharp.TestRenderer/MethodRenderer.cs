using System.Collections;
using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

internal interface IBlock
{
    IdentifierNameSyntax NewIdentifier(string idName);
    IBlock NewBlock();
    IdentifierNameSyntax AddDecl(string varName, TypeSyntax? type, ExpressionSyntax init, bool reuse = false);
    void AddExpression(ExpressionSyntax expression);
    void AddAssertEqual(ExpressionSyntax x, ExpressionSyntax y);
    void AddAssert(ExpressionSyntax condition);
    void AddTryCatch(BlockSyntax tryBlock, TypeSyntax catchType, IdentifierNameSyntax exVar, BlockSyntax catchBlock);
    void AddIf(ExpressionSyntax condition, StatementSyntax thenBranch, StatementSyntax? elseBranch = null);
    void AddFor(TypeSyntax? type, IdentifierNameSyntax iterator, ExpressionSyntax condition, ExpressionSyntax increment, BlockSyntax forBody);
    void AddFor(TypeSyntax? type, IdentifierNameSyntax iterator, ExpressionSyntax length, BlockSyntax forBody);
    void AddWhile(ExpressionSyntax condition, BlockSyntax whileBody);
    void AddForEach(TypeSyntax? type, IdentifierNameSyntax iterator, IdentifierNameSyntax where, BlockSyntax foreachBody);
    void AddForEach(TypeSyntax? type, IdentifierNameSyntax[] iterators, IdentifierNameSyntax where, BlockSyntax foreachBody);
    void AddReturn(ExpressionSyntax? whatToReturn);
    ExpressionSyntax RenderObject(object? obj);
    BlockSyntax Render();
}

internal class MethodRenderer
{
    private readonly MethodDeclarationSyntax _declaration;

    public IdentifierNameSyntax[] ParametersIds { get; }
    public SimpleNameSyntax MethodId { get; }
    public IBlock Body { get; }

    public MethodRenderer(
        IdentifiersCache cache,
        SimpleNameSyntax methodId,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        TypeSyntax resultType,
        IdentifierNameSyntax[]? generics,
        params (TypeSyntax, string)[] args)
    {
        var methodCache = new IdentifiersCache(cache);
        // Creating method declaration
        MethodId = methodId;
        _declaration = MethodDeclaration(resultType, methodId.Identifier);
        if (attributes != null)
            _declaration = _declaration.AddAttributeLists(attributes);
        var parameters = new ParameterSyntax[args.Length];
        ParametersIds = new IdentifierNameSyntax[args.Length];
        for (var i = 0; i < args.Length; i++)
        {
            var (type, varName) = args[i];
            var arg = methodCache.GenerateIdentifier(varName);
            ParametersIds[i] = arg;
            parameters[i] = Parameter(arg.Identifier).WithType(type);
        }
        var parameterList =
            ParameterList(
                SeparatedList<ParameterSyntax>()
                    .AddRange(parameters)
            );
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
            _declaration = _declaration.WithTypeParameterList(typeVars);
        }

        _declaration =
            _declaration
                .AddModifiers(modifiers)
                .WithParameterList(parameterList);
        Body = new BlockBuilder(methodCache);
    }

    private class BlockBuilder : IBlock
    {
        // Variables cache
        private readonly IdentifiersCache _cache;
        // Rendering objects cache
        private readonly Dictionary<physicalAddress, ExpressionSyntax> _renderedObjects;
        private readonly HashSet<physicalAddress> _startToRender;

        private readonly List<StatementSyntax> _statements = new();

        public BlockBuilder(IdentifiersCache cache)
        {
            _cache = cache;
            _renderedObjects = new Dictionary<physicalAddress, ExpressionSyntax>();
            _startToRender = new HashSet<physicalAddress>();
        }

        public IdentifierNameSyntax NewIdentifier(string idName)
        {
            return _cache.GenerateIdentifier(idName);
        }

        public IBlock NewBlock()
        {
            return new BlockBuilder(new IdentifiersCache(_cache));
        }

        public IdentifierNameSyntax AddDecl(
            string varName,
            TypeSyntax? type,
            ExpressionSyntax init,
            bool reuse = false)
        {
            // TODO: to check for equality of syntax nodes use 'AreEquivalent'
            string initializerString = init.ToString();
            if (reuse && _cache.TryGetIdByInit(initializerString, out var result))
            {
                Debug.Assert(result != null);
                return result;
            }

            var var = _cache.GenerateIdentifier(varName);
            var varDecl = RenderVarDecl(type, var.Identifier, init);
            _statements.Add(LocalDeclarationStatement(varDecl));
            _cache.SetIdInit(var, initializerString);
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

        public void AddTryCatch(BlockSyntax tryBlock, TypeSyntax catchType, IdentifierNameSyntax exVar, BlockSyntax catchBlock)
        {
            var declaration = CatchDeclaration(catchType, exVar.Identifier);
            var catchClause = CatchClause(declaration, null, catchBlock);
            var clauses = SingletonList(catchClause);
            var tryCatchBlock = TryStatement(tryBlock, clauses, null);
            _statements.Add(tryCatchBlock);
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

        private ExpressionSyntax RenderArray(System.Array obj)
        {
            // TODO: use compact array representation, if array is big enough?
            var rank = obj.Rank;
            var type = (ArrayTypeSyntax) RenderType(obj.GetType());
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
                    // TODO: if lower bound != 0, use Array.CreateInstance
                    initializer.Add(RenderObject(obj.GetValue(i)));
                }
            }

            return RenderArrayCreation(type, initializer);
        }

        private ExpressionSyntax RenderCompactVector(
            ArrayTypeSyntax type,
            System.Array array,
            int[][] indices,
            object[] values,
            object? defaultValue = null)
        {
            var createArray = RenderArrayCreation(type, array.Length);
            var arrayId = AddDecl("array", type, createArray);
            if (defaultValue != null)
            {
                var defaultId = RenderObject(defaultValue);
                var call =
                    RenderCall(AllocatorType(), "Fill", arrayId, defaultId);
                AddExpression(call);
            }
            for (int i = 0; i < indices.Length; i++)
            {
                var value = RenderObject(values[i]);
                var assignment = RenderArrayAssignment(arrayId, value, indices[i]);
                AddAssignment(assignment);
            }

            return arrayId;
        }

        private ExpressionSyntax RenderCompactArray(CompactArrayRepr obj)
        {
            var array = obj.array;
            var indices = obj.indices;
            var values = obj.values;
            var t = array.GetType();
            var type = (ArrayTypeSyntax) RenderType(t);
            Debug.Assert(type != null);
            var defaultOf = Reflection.defaultOf(t.GetElementType());
            var defaultValue = obj.defaultValue;
            if (defaultValue == null || defaultValue.Equals(defaultOf))
            {
                if (t.IsSZArray)
                    return RenderCompactVector(type, array, indices, values);
                throw new NotImplementedException();
            }
            if (t.IsSZArray)
                return RenderCompactVector(type, array, indices, values, defaultValue);
            throw new NotImplementedException();
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
                var fieldValue = RenderObject(fieldInfo.GetValue(obj));
                fieldsWithValues[i] = (fieldName, fieldValue);
                i++;
            }

            return fieldsWithValues;
        }

        private ExpressionSyntax RenderFields(object obj)
        {
            var physAddress = new physicalAddress(obj);
            if (_renderedObjects.TryGetValue(physAddress, out var renderedResult))
                return renderedResult;

            // Adding namespace of allocator to usings
            AddTestExtensions();

            var type = obj.GetType();
            var isPublicType = type.IsPublic || type.IsNested && type.IsNestedPublic;
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
            if (_renderedObjects.TryGetValue(physAddress, out var rendered))
            {
                args = new[] {rendered};
            }
            else if (isPublicType)
            {
                args = System.Array.Empty<ExpressionSyntax>();
            }
            else
            {
                Debug.Assert(type.FullName != null);
                var name =
                    LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(type.FullName));
                args = new ExpressionSyntax[] {name};
            }

            var allocator =
                RenderObjectCreation(AllocatorType(typeExpr), args, fieldsWithValues);
            var resultObject = RenderMemberAccess(allocator, AllocatorObject);
            ExpressionSyntax objId;
            // If object was not rendered already, declaring new variable for it
            if (rendered == null)
            {
                objId = AddDecl("obj", typeExpr, resultObject);
                _renderedObjects[physAddress] = objId;
            }
            else
            {
                AddAssignment(RenderAssignment(rendered, resultObject));
                objId = rendered;
            }
            return objId;
        }

        public ExpressionSyntax RenderObject(object? obj) => obj switch
        {
            null => RenderNull(),
            true => True,
            false => False,
            byte n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            sbyte n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            char n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            short n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            ushort n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            int n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            uint n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            long n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            ulong n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            float n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            double n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            decimal n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            nuint n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            nint n => LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(n)),
            string s => LiteralExpression(SyntaxKind.StringLiteralExpression, Literal(s)),
            System.Array a => RenderArray(a),
            CompactArrayRepr a => RenderCompactArray(a),
            Enum e => RenderEnum(e),
            Pointer => throw new NotImplementedException("RenderObject: implement rendering of pointers"),
            ValueType => RenderFields(obj),
            _ when obj.GetType().IsClass => RenderFields(obj),
            _ => throw new NotImplementedException($"RenderObject: unexpected object {obj}")
        };

        public BlockSyntax Render()
        {
            return Block(_statements);
        }
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

    public MethodDeclarationSyntax Render()
    {
        return _declaration.WithBody(Body.Render());
    }
}
