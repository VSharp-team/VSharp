using System.Diagnostics;
using System.Reflection;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

namespace VSharp.TestRenderer;

using static CodeRenderer;

using BlockCreator = Func<IBlock, BlockSyntax>;
using BlockCreatorWithVar = Func<IBlock, IdentifierNameSyntax, BlockSyntax>;
using ExprCreator = Func<IdentifierNameSyntax, ExpressionSyntax>;

internal interface IBlock
{
    IdentifierNameSyntax AddDecl(string varName, TypeSyntax? type, ExpressionSyntax init, bool reuse = false);
    IdentifierNameSyntax AddModuleDecl(IdentifierNameSyntax asemblyId, Module module);
    IdentifierNameSyntax AddModuleDecl(Module module);
    IdentifierNameSyntax AddTypeDecl(IdentifierNameSyntax moduleId, Type type);
    IdentifierNameSyntax AddTypeDecl(Type type);
    IdentifierNameSyntax AddMethodDecl(MethodBase method, IdentifierNameSyntax? moduleId = null!);
    IdentifierNameSyntax AddAssemblyViaPathDecl(Assembly assembly);
    IdentifierNameSyntax AddAssemblyViaNameDecl(Assembly assembly);
    void AddAssertEqual(ExpressionSyntax x, ExpressionSyntax y);
    void AddAssert(ExpressionSyntax condition);
    void AddTryCatch(BlockCreator tryBlockCreator, TypeSyntax catchType, string exVarName, BlockCreatorWithVar catchBlockCreator);
    void AddIf(ExpressionSyntax condition, StatementSyntax thenBranch, StatementSyntax? elseBranch = null);
    void AddIfIsPattern(ExpressionSyntax whatToCast, TypeSyntax type, string varName, BlockCreatorWithVar thenBranchCreator, StatementSyntax? elseBranch = null);
    void AddFor(TypeSyntax? type, string iteratorName, ExprCreator condition, ExprCreator increment, BlockCreatorWithVar creator);
    void AddWhile(ExpressionSyntax condition, BlockCreator blockCreator);
    void AddForEach(TypeSyntax? type, string varName, IdentifierNameSyntax where, BlockCreatorWithVar creator);
    void AddReturn(ExpressionSyntax? whatToReturn);
    ExpressionSyntax RenderObject(object? obj);
    BlockSyntax GetBlock();
}

internal class MethodRenderer
{
    private readonly IdentifiersCache cache;
    private readonly MethodDeclarationSyntax declaration;

    public IdentifierNameSyntax[] ParametersIds { get; }
    public IdentifierNameSyntax MethodId { get; }
    public IBlock Body { get; }

    // TODO: configure useful methods? (like 'LoadAssembly')
    // 'AssemblyLoader' is rendered method, which takes assembly name and it's path,
    // loads it and returns assembly id
    public static IdentifierNameSyntax? AssemblyLoader = null!;
    public static IdentifierNameSyntax? BindingFlags = null!;

    public MethodRenderer(
        SyntaxToken methodToken,
        IdentifierNameSyntax methodId,
        AttributeListSyntax? attributes,
        SyntaxToken[] modifiers,
        TypeSyntax resultType,
        params (TypeSyntax, string)[] args)
    {
        // Creating identifiers cache
        // TODO: create own IdGenerator
        // IdGenerator.reset();
        cache = new IdentifiersCache();

        // Creating method declaration
        MethodId = methodId;
        declaration = MethodDeclaration(resultType, methodToken);
        if (attributes != null)
            declaration = declaration.AddAttributeLists(attributes);
        var parameters = new ParameterSyntax[args.Length];
        ParametersIds = new IdentifierNameSyntax[args.Length];
        for (var i = 0; i < args.Length; i++)
        {
            var (type, varName) = args[i];
            var (arg, argId) = cache.GenerateIdentifier(varName);
            ParametersIds[i] = argId;
            parameters[i] = Parameter(arg).WithType(type);
        }
        var parameterList =
            ParameterList(
                SeparatedList<ParameterSyntax>()
                    .AddRange(parameters)
            );
        declaration =
            declaration
                .AddModifiers(modifiers)
                .WithParameterList(parameterList);
        Body = new BlockBuilder(cache);
    }

    private class BlockBuilder : IBlock
    {
        // Variables cache
        private readonly IdentifiersCache cache;

        private readonly List<StatementSyntax> _statements = new();

        public BlockBuilder(IdentifiersCache cache)
        {
            this.cache = cache;
        }

        public IdentifierNameSyntax AddDecl(
            string varName,
            TypeSyntax? type,
            ExpressionSyntax init,
            bool reuse = false)
        {
            // TODO: to check for equality of syntax nodes use 'AreEquivalent'
            string initializerString = init.ToString();
            if (reuse && cache.TryGetIdByInit(initializerString, out var result))
                return result;
            var (var, varId) = cache.GenerateIdentifier(varName);
            var varDecl = RenderVarDecl(type, var, init);
            _statements.Add(LocalDeclarationStatement(varDecl));
            cache.SetIdInit(varId, initializerString);
            return varId;
        }

        public IdentifierNameSyntax AddAssemblyViaPathDecl(Assembly assembly)
        {
            ExpressionSyntax assemblyId;
            if (AssemblyLoader == null)
            {
                assemblyId =
                    RenderCall(
                        IdentifierName("Assembly"), "LoadFrom",
                        RenderLiteral(assembly.Location)
                    );
            }
            else
            {
                assemblyId =
                    RenderCall(
                        AssemblyLoader,
                        RenderLiteral(assembly.Location),
                        RenderLiteral(assembly.FullName)
                    );
            }

            return AddDecl("assembly", AssemblyType, assemblyId, true);
        }

        public IdentifierNameSyntax AddAssemblyViaNameDecl(Assembly assembly)
        {
            ExpressionSyntax assemblyId;
            if (AssemblyLoader == null)
            {
                assemblyId =
                    RenderCall(
                        IdentifierName("Assembly"), "Load",
                        RenderLiteral(assembly.FullName)
                    );
            }
            else
            {
                assemblyId =
                    RenderCall(
                        AssemblyLoader,
                        RenderLiteral(assembly.Location),
                        RenderLiteral(assembly.FullName)
                    );
            }

            return AddDecl("assembly", AssemblyType, assemblyId, true);
        }

        public IdentifierNameSyntax AddModuleDecl(IdentifierNameSyntax assemblyId, Module module)
        {
            // TODO: care about dynamic modules (mocks and others)
            var getModule =
                RenderCall(
                    assemblyId, "GetModule",
                    RenderLiteral(module.Name)
                );
            return AddDecl("module", ModuleType, getModule, true);
        }

        public IdentifierNameSyntax AddModuleDecl(Module module)
        {
            var assemblyId = AddAssemblyViaPathDecl(module.Assembly);
            // TODO: care about dynamic modules (mocks and others)
            var getModule =
                RenderCall(
                    assemblyId, "GetModule",
                    RenderLiteral(module.Name)
                );
            return AddDecl("module", ModuleType, getModule, true);
        }

        public IdentifierNameSyntax AddTypeDecl(IdentifierNameSyntax moduleId, Type type)
        {
            var getType =
                RenderCall(
                    moduleId, "GetType",
                    RenderLiteral(type.FullName)
                );
            return AddDecl("type", SystemType, getType, true);
        }

        public IdentifierNameSyntax AddTypeDecl(Type type)
        {
            var moduleId = AddModuleDecl(type.Module);
            var getType =
                RenderCall(
                    moduleId, "GetType",
                    RenderLiteral(type.FullName)
                );
            return AddDecl("type", SystemType, getType, true);
        }

        public IdentifierNameSyntax AddMethodDecl(MethodBase method, IdentifierNameSyntax? moduleId = null!)
        {
            moduleId ??= AddModuleDecl(method.Module);
            var resolveMethod =
                RenderCall(
                    moduleId, "ResolveMethod",
                    RenderLiteral(method.MetadataToken)
                );

            return AddDecl("method", MethodBaseType, resolveMethod);
        }

        public void AddAssertEqual(ExpressionSyntax x, ExpressionSyntax y)
        {
            _statements.Add(ExpressionStatement(RenderAssertEqual(x, y)));
        }

        public void AddAssert(ExpressionSyntax condition)
        {
            _statements.Add(ExpressionStatement(RenderAssert(condition)));
        }

        public void AddTryCatch(BlockCreator tryBlockCreator, TypeSyntax catchType, string exVarName, BlockCreatorWithVar catchBlockCreator)
        {
            var (exceptionVar, exceptionId) = cache.GenerateIdentifier(exVarName);
            var declaration = CatchDeclaration(catchType, exceptionVar);
            var tryBlock = tryBlockCreator(new BlockBuilder(new IdentifiersCache(cache)));
            var catchBlock = catchBlockCreator(new BlockBuilder(new IdentifiersCache(cache)), exceptionId);
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

        public void AddIfIsPattern(ExpressionSyntax whatToCast, TypeSyntax type, string varName, BlockCreatorWithVar thenBranchCreator, StatementSyntax? elseBranch = null)
        {
            ElseClauseSyntax elseClause = null!;
            if (elseBranch != null) elseClause = ElseClause(elseBranch);
            var (isVar, isVarId) = cache.GenerateIdentifier(varName);
            var condition = RenderIsType(whatToCast, type, isVar);
            var thenBranch = thenBranchCreator(new BlockBuilder(new IdentifiersCache(cache)), isVarId);
            var ifExpr = IfStatement(condition, thenBranch, elseClause);
            _statements.Add(ifExpr);
        }

        public void AddFor(
            TypeSyntax? type,
            string iteratorName,
            ExprCreator condition,
            ExprCreator increment,
            BlockCreatorWithVar creator)
        {
            type ??= VarKeyword;
            var (iterator, iteratorId) = cache.GenerateIdentifier(iteratorName);
            var forStatement =
                ForStatement(
                    RenderVarDecl(type, iterator, Zero),
                    SeparatedList<ExpressionSyntax>(),
                    condition(iteratorId),
                    SingletonSeparatedList(increment(iteratorId)),
                    creator(new BlockBuilder(new IdentifiersCache(cache)), iteratorId)
                );
            _statements.Add(forStatement);
        }
        
        public void AddWhile(ExpressionSyntax condition, BlockCreator blockCreator)
        {
            var block = blockCreator(new BlockBuilder(new IdentifiersCache(cache)));
            var whileStatement = WhileStatement(condition, block);
            _statements.Add(whileStatement);
        }
        
        public void AddForEach(TypeSyntax? type, string varName, IdentifierNameSyntax where, BlockCreatorWithVar blockCreator)
        {
            type ??= VarKeyword;
            var (elem, elemId) = cache.GenerateIdentifier(varName);
            var block = blockCreator(new BlockBuilder(new IdentifiersCache(cache)), elemId);
            var forEach = ForEachStatement(type, elem, where, block);
            _statements.Add(forEach);
        }

        public void AddReturn(ExpressionSyntax? whatToReturn)
        {
            _statements.Add(ReturnStatement(whatToReturn));
        }

        private ExpressionSyntax RenderArray(Array obj)
        {
            var rank = obj.Rank;
            var elemTypeName = obj.GetType().GetElementType()?.ToString();
            if (elemTypeName == null)
                throw new ArgumentException();
            var type = RenderArrayType(elemTypeName, obj.Rank);
            var initializer = new List<ExpressionSyntax>();
            if (rank > 1)
            {
                throw new NotImplementedException("implement rendering for non-vector arrays");
                // for (int i = 0; i < obj.Rank; i++)
                // {
                //     var innerInitializer = new List<ExpressionSyntax>();
                //     for (int j = obj.GetLowerBound(i); j <= obj.GetUpperBound(i); j++)
                //     {
                //         
                //     }
                // }
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

        private ExpressionSyntax RenderFields(object obj)
        {
            var type = obj.GetType();
            var typeId = AddTypeDecl(type);
            // TODO: minimize
            var getUninitializedObjectId = RenderMemberAccess("FormatterServices", "GetUninitializedObject");
            var getUninitializedObject = RenderCall(getUninitializedObjectId, typeId);
            var objId = AddDecl("obj", ObjectType, getUninitializedObject);
            var fields = Reflection.fieldsOf(false, type);
            var bindingFlags = BindingFlags ?? CodeRenderer.BindingFlags;
                
            foreach (var (_, fieldInfo) in fields)
            {
                var getField =
                    RenderCall(typeId, "GetField", RenderLiteral(fieldInfo.Name), bindingFlags);
                var fieldValue = fieldInfo.GetValue(obj);
                var setValue =
                    RenderCall(getField, "SetValue", objId, RenderObject(fieldValue));
                _statements.Add(ExpressionStatement(setValue));
            }

            return objId;
        }

        public ExpressionSyntax RenderObject(object? obj) => obj switch
        {
            null => LiteralExpression(SyntaxKind.NullLiteralExpression),
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
            Array a => RenderArray(a),
            ValueType => RenderFields(obj),
            _ when obj.GetType().IsPointer => throw new NotImplementedException("implement rendering of pointers"),
            _ => RenderFields(obj)
        };

        public BlockSyntax GetBlock()
        {
            return Block(_statements);
        }
    }

    // TODO: add 'GenerateIdentifier' to API instead of lambdas?
    private IBlock NewBlock()
    {
        return new BlockBuilder(new IdentifiersCache(cache));
    }

    public (IdentifierNameSyntax, IdentifierNameSyntax) GetTwoArgs()
    {
        Debug.Assert(ParametersIds.Length == 2);
        return (ParametersIds[0], ParametersIds[1]);
    }

    public MethodDeclarationSyntax Render()
    {
        return declaration.WithBody(Body.GetBlock());
    }

    ~MethodRenderer()
    {
        // TODO: create own IdGenerator
        // IdGenerator.restore();
    }
}
