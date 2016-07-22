namespace VSharp.Core.Horn

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft
open VSharp.Core.Common
open VSharp.Core.Utils

module HornPrinter =

    let private __notImplemented__() = raise (new System.NotImplementedException())

    let printBinaryExpression (facade : HornFacade) (assertions : Assertions) (expression : IBinaryExpression) =
        match expression with
        | :? IAdditiveExpression as a -> __notImplemented__()
        | :? IBitwiseAndExpression as a -> __notImplemented__()
        | :? IBitwiseExclusiveOrExpression as a -> __notImplemented__()
        | :? IBitwiseInclusiveOrExpression as a -> __notImplemented__()
        | :? IConditionalAndExpression as a -> __notImplemented__()
        | :? IConditionalOrExpression as a -> __notImplemented__()
        | :? IEqualityExpression as a -> __notImplemented__()
        | :? IMultiplicativeExpression as a -> __notImplemented__()
        | :? INullCoalescingExpression as a -> __notImplemented__()
        | :? IRelationalExpression as a -> __notImplemented__()
        | :? IShiftExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    let printUnaryExpression (facade : HornFacade) (assertions : Assertions) (expression : IUnaryExpression) =
        match expression with
        | :? IReferenceExpression as a -> __notImplemented__()
        | :? ITypeofExpression as a -> __notImplemented__()
        | :? IUnaryOperatorExpression as a -> __notImplemented__()
        | :? IUncheckedExpression as a -> __notImplemented__()
        | :? IUnsafeCodeAddressOfExpression as a -> __notImplemented__()
        | :? IUnsafeCodePointerAccessExpression as a -> __notImplemented__()
        | :? IUnsafeCodePointerIndirectionExpression as a -> __notImplemented__()
        | :? IUnsafeCodeSizeOfExpression as a -> __notImplemented__()
        | :? I__ArglistExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    let printOperatorExpression (facade : HornFacade) (assertions : Assertions) (expression : IOperatorExpression) =
        match expression with
        | :? IAssignmentExpression as a -> __notImplemented__()
        | :? IBinaryExpression as binary -> printBinaryExpression facade assertions binary
        | :? IPostfixOperatorExpression as a -> __notImplemented__()
        | :? IPrefixOperatorExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    let printConditionalAccessExpression (facade : HornFacade) (assertions : Assertions) (expression : IConditionalAccessExpression) =
        match expression with
        | :? IElementAccessExpression as a -> __notImplemented__
        | :? IInvocationExpression as a -> __notImplemented__
        | :? IReferenceExpression as a -> __notImplemented__
        | _ -> __notImplemented__()

    let printCreationExpression(facade : HornFacade) (assertions : Assertions) (expression : ICreationExpression) =
        match expression with
        | :? IAnonymousObjectCreationExpression as a -> __notImplemented__()
        | :? IArrayCreationExpression as a -> __notImplemented__()
        | :? IObjectCreationExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    let printAnonymousFunctionExpression (facade : HornFacade) (assertions : Assertions) (expression : IAnonymousFunctionExpression) =
        match expression with
        | :? IAnonymousMethodExpression as a -> __notImplemented__
        | :? ILambdaExpression as a -> __notImplemented__
        | _ -> __notImplemented__()

    let printExpression (facade : HornFacade) (assertions : Assertions) (expression : ICSharpExpression) =
        match expression with
        | :? IAnonymousFunctionExpression as anonimous -> printAnonymousFunctionExpression facade assertions anonimous
        | :? IAsExpression as a -> __notImplemented__()
        | :? IAwaitExpression as a -> __notImplemented__()
        | :? IBaseExpression as a -> __notImplemented__()
        | :? ICastExpression as a -> __notImplemented__()
        | :? ICheckedExpression as a -> __notImplemented__()
        | :? IConditionalAccessExpression as conditional -> printConditionalAccessExpression facade assertions conditional
        | :? IConditionalTernaryExpression as a -> __notImplemented__()
        | :? ICreationExpression as creation -> printCreationExpression facade assertions creation
        | :? ICSharpLiteralExpression as a -> __notImplemented__()
        | :? IDefaultExpression as a -> __notImplemented__()
        | :? IInterpolatedStringExpression as a -> __notImplemented__()
        | :? IIsExpression as a -> __notImplemented__()
        | :? IOperatorExpression as operator -> printOperatorExpression facade assertions operator
        | :? IParenthesizedExpression as a -> __notImplemented__()
        | :? IPredefinedTypeExpression as a -> __notImplemented__()
        | :? IQueryExpression as a -> __notImplemented__()
        | :? IStringLiteralOwner as a -> __notImplemented__()
        | :? IThisExpression as a -> __notImplemented__()
        | :? IUnaryExpression as unary -> printUnaryExpression facade assertions unary
        | _ -> __notImplemented__()

    let printConstantDeclaration facade smtSymbol assertions (declaration : IConstantDeclaration) =
        let newAssertions = printExpression facade assertions declaration.ValueExpression
        newAssertions.With(facade.ctx.MkEq(smtSymbol, facade.symbols.[declaration.ValueExpression]))

    let printTypeOwnerDeclaration (facade : HornFacade) assertions (parameterDeclaration : ITypeOwnerDeclaration) =
        let sort = TypePrinter.printType facade.ctx parameterDeclaration.Type
        // TODO: MkBound??
        let smtConst = facade.ctx.MkConst(IdGenerator.startingWith parameterDeclaration.DeclaredName, sort)
        facade.symbols.AddExpr(parameterDeclaration.DeclaredName, smtConst)

        match parameterDeclaration with
        | :? IParameterDeclaration -> assertions
        | :? ICatchVariableDeclaration -> __notImplemented__()
        | :? IConstantDeclaration as decl -> printConstantDeclaration facade smtConst assertions decl
        | _ -> __notImplemented__()

    let printParametersDeclarations facade assertions parametersDeclarations =
        Seq.fold (printTypeOwnerDeclaration facade) assertions parametersDeclarations

    let printMultipleDeclarations facade assertions (declaration : IMultipleDeclaration) =
        Seq.fold (printTypeOwnerDeclaration facade) assertions declaration.Declarators

    let rec printStatement facade assertions (statement : ICSharpStatement) =
        match statement with
        | :? IBlock as block -> printBlock facade assertions block
        | :? IBreakStatement as breakStatement -> __notImplemented__()
        | :? ICheckedStatement as checkedStatement -> __notImplemented__()
        | :? IContinueStatement as continueStatement -> __notImplemented__()
        | :? IDeclarationStatement as declaration -> printMultipleDeclarations facade assertions declaration.Declaration
        | :? IDoStatement as doStatement -> __notImplemented__()
        | :? IEmptyStatement -> assertions
        | :? IExpressionStatement as expression -> __notImplemented__()
        | :? IGotoCaseStatement as gotoCase -> __notImplemented__()
        | :? IGotoStatement as goto -> __notImplemented__()
        | :? IIfStatement as fork -> __notImplemented__()
        | :? ILabelStatement as label -> __notImplemented__()
        | :? ILockStatement as lock -> __notImplemented__()
        | :? IForeachStatement as foreach -> __notImplemented__()
        | :? IForStatement as forStatement -> __notImplemented__()
        | :? IWhileStatement as whileStatement -> __notImplemented__()
        | :? ILoopStatement as otherLoop -> __notImplemented__()
        | :? IReturnStatement as returnStatement -> __notImplemented__()
        | :? ISwitchLabelStatement as switchLabel -> __notImplemented__()
        | :? ISwitchStatement as switch -> __notImplemented__()
        | :? IThrowStatement as throw -> __notImplemented__()
        | :? ITryStatement as tryStatement -> __notImplemented__()
        | :? IUncheckedStatement as unchecked -> __notImplemented__()
        | :? IUnsafeCodeFixedStatement as unsafe -> __notImplemented__()
        | :? IUnsafeCodeUnsafeStatement as unsafe -> __notImplemented__()
        | :? IUsingStatement as using -> __notImplemented__()
        | :? IYieldStatement as yieldStatement -> __notImplemented__()
        | _ -> __notImplemented__()

    and printBlock facade assertions (functionBody : IBlock) =
        Seq.fold (printStatement facade) assertions functionBody.Statements

    let printMethod (facade : HornFacade) (assertions : Assertions) (methodDeclaration : IMethodDeclaration) =
        let newAssertions = printParametersDeclarations facade methodDeclaration.ParameterDeclarationsEnumerable
        printBlock facade newAssertions methodDeclaration.Body
