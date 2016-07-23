namespace VSharp.Core.Horn

open JetBrains.ReSharper.Psi
open JetBrains.ReSharper.Psi.Tree
open JetBrains.ReSharper.Psi.CSharp.Tree
open Microsoft
open VSharp.Core.Common
open VSharp.Core.Utils

module HornPrinter =

    let private __notImplemented__() = raise (new System.NotImplementedException())
    // Partial active pattern. Match if field equals value.
    let (|Field|_|) field x = if field = x then Some () else None
    let infiniteDomain = VSharp.Core.Properties.Settings.InfiniteIntegers

    let rec printBinaryExpression (facade : HornFacade) (assertions : Assertions) (expression : IBinaryExpression) =
        let operatorToken = expression.OperatorSign.GetTokenType()
        let assertions1 = printExpression facade assertions expression.LeftOperand
        let assertions2 = printExpression facade assertions1 expression.RightOperand
        let leftResult = facade.symbols.[expression.LeftOperand]
        let rightResult = facade.symbols.[expression.RightOperand]
        let resultingSort = TypePrinter.printType facade.ctx (expression.Type())
        let resultingConst = facade.symbols.NewIntermediateVar(facade.ctx, resultingSort)

        let arithOrBitwise fun1 fun2 =
            if infiniteDomain then fun1 (leftResult :?> Z3.ArithExpr) (rightResult :?> Z3.ArithExpr) :> Z3.Expr
            else fun2 (leftResult :?> Z3.BitVecExpr) (rightResult :?> Z3.BitVecExpr) :> Z3.Expr
        let justBitwise func =
            if infiniteDomain then __notImplemented__()
            else func (leftResult :?> Z3.BitVecExpr) (rightResult :?> Z3.BitVecExpr) :> Z3.Expr

        let resultingExpression =
            match expression with
            | :? IAdditiveExpression ->
                match operatorToken with
                | Field CSharp.Parsing.CSharpTokenType.PLUS -> arithOrBitwise (fun a b -> facade.ctx.MkAdd(a, b)) (fun a b -> facade.ctx.MkBVAdd(a, b))
                | Field CSharp.Parsing.CSharpTokenType.MINUS -> arithOrBitwise (fun a b -> facade.ctx.MkSub(a, b)) (fun a b -> facade.ctx.MkBVSub(a, b))
                | _ -> __notImplemented__()
            | :? IBitwiseAndExpression -> justBitwise (fun a b -> facade.ctx.MkBVAND(a, b))
            | :? IBitwiseExclusiveOrExpression -> justBitwise (fun a b -> facade.ctx.MkBVXOR(a, b))
            | :? IBitwiseInclusiveOrExpression -> justBitwise (fun a b -> facade.ctx.MkBVOR(a, b))
            | :? IConditionalAndExpression -> facade.ctx.MkAnd(leftResult :?> Z3.BoolExpr, rightResult :?> Z3.BoolExpr) :> Z3.Expr
            | :? IConditionalOrExpression -> facade.ctx.MkOr(leftResult :?> Z3.BoolExpr, rightResult :?> Z3.BoolExpr) :> Z3.Expr
            | :? IEqualityExpression ->
                match operatorToken with
                | Field CSharp.Parsing.CSharpTokenType.EQEQ -> facade.ctx.MkEq(leftResult, rightResult) :> Z3.Expr
                | Field CSharp.Parsing.CSharpTokenType.NE -> facade.ctx.MkNot(facade.ctx.MkEq(leftResult, rightResult)) :> Z3.Expr
                | _ -> __notImplemented__()
            | :? IMultiplicativeExpression ->
                match operatorToken with
                | Field CSharp.Parsing.CSharpTokenType.ASTERISK -> arithOrBitwise (fun a b -> facade.ctx.MkMul(a, b)) (fun a b -> facade.ctx.MkBVMul(a, b))
                | Field CSharp.Parsing.CSharpTokenType.DIV -> arithOrBitwise (fun a b -> facade.ctx.MkDiv(a, b)) (fun a b -> facade.ctx.MkBVSDiv(a, b))
                | _ -> __notImplemented__()
            | :? INullCoalescingExpression -> __notImplemented__()
            | :? IRelationalExpression ->
                match operatorToken with
                | Field CSharp.Parsing.CSharpTokenType.GT -> arithOrBitwise (fun a b -> facade.ctx.MkGt(a, b)) (fun a b -> facade.ctx.MkBVSGT(a, b))
                | Field CSharp.Parsing.CSharpTokenType.GE -> arithOrBitwise (fun a b -> facade.ctx.MkGe(a, b)) (fun a b -> facade.ctx.MkBVSGE(a, b))
                | Field CSharp.Parsing.CSharpTokenType.LT -> arithOrBitwise (fun a b -> facade.ctx.MkLt(a, b)) (fun a b -> facade.ctx.MkBVSLT(a, b))
                | Field CSharp.Parsing.CSharpTokenType.LE -> arithOrBitwise (fun a b -> facade.ctx.MkLe(a, b)) (fun a b -> facade.ctx.MkBVSLE(a, b))
                | _ -> __notImplemented__()
            | :? IShiftExpression ->
                match operatorToken with
                | Field CSharp.Parsing.CSharpTokenType.LTLT -> justBitwise (fun a b -> facade.ctx.MkBVSHL(a, b))
                | Field CSharp.Parsing.CSharpTokenType.GTGT -> justBitwise (fun a b -> facade.ctx.MkBVASHR(a, b))
                | _ -> __notImplemented__()
            | _ -> __notImplemented__()

        facade.symbols.AddExpr(expression, resultingConst)
        assertions2.With(facade.ctx.MkEq(resultingConst, resultingExpression))


    and printUnaryExpression (facade : HornFacade) (assertions : Assertions) (expression : IUnaryExpression) =
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

    and printOperatorExpression (facade : HornFacade) (assertions : Assertions) (expression : IOperatorExpression) =
        match expression with
        | :? IAssignmentExpression as a -> __notImplemented__()
        | :? IBinaryExpression as binary -> printBinaryExpression facade assertions binary
        | :? IPostfixOperatorExpression as a -> __notImplemented__()
        | :? IPrefixOperatorExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    and printConditionalAccessExpression (facade : HornFacade) (assertions : Assertions) (expression : IConditionalAccessExpression) =
        match expression with
        | :? IElementAccessExpression as a -> __notImplemented__()
        | :? IInvocationExpression as a -> __notImplemented__()
        | :? IReferenceExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    and printCreationExpression(facade : HornFacade) (assertions : Assertions) (expression : ICreationExpression) =
        match expression with
        | :? IAnonymousObjectCreationExpression as a -> __notImplemented__()
        | :? IArrayCreationExpression as a -> __notImplemented__()
        | :? IObjectCreationExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    and printAnonymousFunctionExpression (facade : HornFacade) (assertions : Assertions) (expression : IAnonymousFunctionExpression) =
        match expression with
        | :? IAnonymousMethodExpression as a -> __notImplemented__()
        | :? ILambdaExpression as a -> __notImplemented__()
        | _ -> __notImplemented__()

    and printExpression (facade : HornFacade) (assertions : Assertions) (expression : ICSharpExpression) =
        match expression with
        | :? IAnonymousFunctionExpression as anonymous -> printAnonymousFunctionExpression facade assertions anonymous
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
        let newAssertions = printParametersDeclarations facade assertions methodDeclaration.ParameterDeclarationsEnumerable
        printBlock facade newAssertions methodDeclaration.Body
