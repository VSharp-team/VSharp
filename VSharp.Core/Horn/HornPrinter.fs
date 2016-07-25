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
    // Install this flag for using arithmetics over infinite domain rather then bitvectors
    let infiniteDomain = false

    let addRule (facade : HornFacade) (assertions : Assertions) returnValue =
        let func = facade.symbols.CurrentFunctionDeclaration()
        let smtFunc = facade.symbols.Function func.DeclaredElement
        let declarationToVariable (param : ICSharpParameterDeclaration) = facade.symbols.Expr(param.DeclaredElement :> IDeclaredElement)
        let arguments = Seq.map declarationToVariable func.ParameterDeclarationsEnumerable
        let argumentsAndReturnValue = Seq.append arguments (Seq.singleton returnValue) |> Seq.toArray
        let premise = assertions.Print facade.ctx
        let conclusion = smtFunc.Apply argumentsAndReturnValue :?> Z3.BoolExpr
        facade.fp.AddRule(facade.ctx.MkImplies(premise, conclusion))

    let rec printBinaryExpression (facade : HornFacade) (assertions : Assertions) (expression : IBinaryExpression) =
        let operatorToken = expression.OperatorSign.GetTokenType()
        let assertions1 = printExpression facade assertions expression.LeftOperand
        let assertions2 = printExpression facade assertions1 expression.RightOperand
        let leftResult = facade.symbols.Expr expression.LeftOperand
        let rightResult = facade.symbols.Expr expression.RightOperand
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

        facade.symbols.[expression] <- resultingConst
        assertions2.With(facade.ctx.MkEq(resultingConst, resultingExpression))

    and printReferenceExpression (facade : HornFacade) (assertions : Assertions) (reference : IReferenceExpression) =
        let declaredElement = reference.Reference.Resolve().DeclaredElement
        if declaredElement <> null && facade.symbols.Has declaredElement then
            facade.symbols.[reference] <- facade.symbols.[declaredElement]
        assertions

    and printUnaryExpression facade assertions (expression : IUnaryExpression) =
        match expression with
        | :? ITypeofExpression -> __notImplemented__()
        | :? IUnaryOperatorExpression as operatorExpression ->
            let operatorToken = operatorExpression.OperatorSign.GetTokenType()
            match operatorToken with
            | Field CSharp.Parsing.CSharpTokenType.MINUS -> __notImplemented__()
            | _ -> __notImplemented__()
        | :? IUncheckedExpression -> __notImplemented__()
        | :? IUnsafeCodeAddressOfExpression -> __notImplemented__()
        | :? IUnsafeCodePointerAccessExpression -> __notImplemented__()
        | :? IUnsafeCodePointerIndirectionExpression -> __notImplemented__()
        | :? IUnsafeCodeSizeOfExpression -> __notImplemented__()
        | :? I__ArglistExpression -> __notImplemented__()
        | _ -> __notImplemented__()

    and printOperatorExpression (facade : HornFacade) (assertions : Assertions) (expression : IOperatorExpression) =
        match expression with
        | :? IAssignmentExpression -> __notImplemented__()
        | :? IBinaryExpression as binary -> printBinaryExpression facade assertions binary
        | :? IPostfixOperatorExpression -> __notImplemented__()
        | :? IPrefixOperatorExpression -> __notImplemented__()
        | _ -> __notImplemented__()

    and printArgument facade assertions (argument : ICSharpArgument) = printExpression facade assertions argument.Value

    and printArgumentList facade assertions (expression : IArgumentList) =
        Seq.fold (printArgument facade) assertions expression.ArgumentsEnumerable

    and printInvocationExpression facade assertions (invocation : IInvocationExpression) =
        let invokedAssertions = printExpression facade assertions invocation.InvokedExpression
        let newAssertions = printArgumentList facade invokedAssertions invocation.ArgumentList
        let argToSmtExpr (arg : ICSharpArgument) = facade.symbols.Expr(arg.Value :> ITreeNode)
        let args = Seq.map argToSmtExpr invocation.ArgumentsEnumerable
        if facade.symbols.Has invocation.InvokedExpression then
            match facade.symbols.[invocation.InvokedExpression] with
            | :? Z3.FuncDecl as func ->
                let argsWithReturn =
                    if facade.symbols.HasReturnType func then
                        let returnType = Seq.last func.Domain
                        let resultVar = facade.symbols.NewIntermediateVar(facade.ctx, returnType)
                        facade.symbols.[invocation] <- resultVar
                        Seq.append args (Seq.singleton resultVar)
                    else
                        args
                newAssertions.With (func.Apply(argsWithReturn |> Seq.toArray) :?> Z3.BoolExpr)
            | _ -> __notImplemented__()
        else
            newAssertions

    and printConditionalAccessExpression facade assertions (expression : IConditionalAccessExpression) =
        match expression with
        | :? IElementAccessExpression -> __notImplemented__()
        | :? IInvocationExpression as invocation -> printInvocationExpression facade assertions invocation
        | :? IReferenceExpression as reference -> printReferenceExpression facade assertions reference
        | _ -> __notImplemented__()

    and printCreationExpression (facade : HornFacade) (assertions : Assertions) (expression : ICreationExpression) =
        match expression with
        | :? IAnonymousObjectCreationExpression -> __notImplemented__()
        | :? IArrayCreationExpression -> __notImplemented__()
        | :? IObjectCreationExpression -> __notImplemented__()
        | _ -> __notImplemented__()

    and printAnonymousFunctionExpression (facade : HornFacade) (assertions : Assertions) (expression : IAnonymousFunctionExpression) =
        match expression with
        | :? IAnonymousMethodExpression -> __notImplemented__()
        | :? ILambdaExpression -> __notImplemented__()
        | _ -> __notImplemented__()

    and printLiteralExpression (facade : HornFacade) assertions (literal : ICSharpLiteralExpression) =
        let sort = TypePrinter.printType facade.ctx literal.ConstantValue.Type
        let expr =
            match literal.ConstantValue with
            | b when b.IsBoolean() -> facade.ctx.MkBool(b.Value :?> bool) :> Z3.Expr
            | n when n.IsFloat() || n.IsDouble() || n.IsDecimal() -> facade.ctx.MkFP(n.Value :?> float, sort :?> Z3.FPSort) :> Z3.Expr
            | n when n.Type.IsPredefinedNumeric() ->
                if infiniteDomain then facade.ctx.MkInt(n.Value :?> int) :> Z3.Expr
                else facade.ctx.MkBV(n.Value :?> int, (sort :?> Z3.BitVecSort).Size) :> Z3.Expr
            // Strings are currently not supported.
            | s when s.IsString() -> facade.ctx.MkInt(0) :> Z3.Expr
            | _ -> __notImplemented__()
        facade.symbols.[literal] <- expr
        assertions

    and printConditionalTernaryExpression facade assertions (ternary : IConditionalTernaryExpression) =
//        let conditionAssertions = printExpression facade assertions ternary.ConditionOperand
//        let condition = facade.symbols.Boolean ternary.ConditionOperand
//        let thenBranch = (new Assertions()).With(condition)
//        let elseBranch = (new Assertions()).With(facade.ctx.MkNot condition)
//        let thenAssertions = printExpression facade thenBranch ternary.ThenResult
//        let elseAssertions = printExpression facade elseBranch ternary.ElseResult
//        conditionAssertions.Fork(thenAssertions, elseAssertions)
        assertions

    and printExpression facade assertions (expression : ICSharpExpression) =
        match expression with
        | :? IAnonymousFunctionExpression as anonymous -> printAnonymousFunctionExpression facade assertions anonymous
        | :? IAsExpression -> __notImplemented__()
        | :? IAwaitExpression -> __notImplemented__()
        | :? IBaseExpression -> __notImplemented__()
        | :? ICastExpression -> __notImplemented__()
        | :? ICheckedExpression -> __notImplemented__()
        | :? IConditionalAccessExpression as conditional -> printConditionalAccessExpression facade assertions conditional
        | :? IConditionalTernaryExpression as ternary -> printConditionalTernaryExpression facade assertions ternary
        | :? ICreationExpression as creation -> printCreationExpression facade assertions creation
        | :? ICSharpLiteralExpression as literal -> printLiteralExpression facade assertions literal
        | :? IDefaultExpression -> __notImplemented__()
        | :? IInterpolatedStringExpression -> __notImplemented__()
        | :? IIsExpression -> __notImplemented__()
        | :? IOperatorExpression as operator -> printOperatorExpression facade assertions operator
        | :? IParenthesizedExpression -> __notImplemented__()
        | :? IPredefinedTypeExpression -> __notImplemented__()
        | :? IQueryExpression -> __notImplemented__()
        | :? IStringLiteralOwner -> __notImplemented__()
        | :? IThisExpression -> __notImplemented__()
        | :? IUnaryExpression as unary -> printUnaryExpression facade assertions unary
        | _ -> __notImplemented__()

    and printVariableInitializer facade assertions (initializer : IVariableInitializer) =
        match initializer with
        | :? IArrayInitializer -> __notImplemented__()
        | :? IExpressionInitializer as init ->
            let result = printExpression facade assertions init.Value
            facade.symbols.[init] <- facade.symbols.[init.Value]
            result
        | :? IUnsafeCodeFixedPointerInitializer -> __notImplemented__()
        | :? IUnsafeCodeStackAllocInitializer -> __notImplemented__()
        | _ -> __notImplemented__()

    and printMemberInitializer facade assertions (initializer : IMemberInitializer) =
        match initializer with
        | :? IEventInitializer -> __notImplemented__()
        | :? IIndexerInitializer -> __notImplemented__()
        | :? IPropertyInitializer -> __notImplemented__()
        | _ -> __notImplemented__()

    and printInitializerElement facade assertions (initializer : IInitializerElement) =
        match initializer with
        | :? IAnonymousMemberDeclaration -> __notImplemented__()
        | :? ICollectionElementInitializer -> __notImplemented__()
        | :? IMemberInitializer as init -> printMemberInitializer facade assertions init
        | :? IVariableInitializer as init -> printVariableInitializer facade assertions init
        | _ -> __notImplemented__()

    and printMultipleDeclarationMember (facade : HornFacade) assertions (declaration : IMultipleDeclarationMember) =
        let sort = TypePrinter.printType facade.ctx declaration.Type
        // TODO: MkBound??
        let smtConst = facade.ctx.MkConst(IdGenerator.startingWith declaration.DeclaredName, sort)
        facade.symbols.[declaration.DeclaredElement] <- smtConst
        match declaration with
        | :? IConstantDeclaration as decl ->
            let newAssertions = printExpression facade assertions decl.ValueExpression
            newAssertions.With(facade.ctx.MkEq(smtConst, facade.symbols.Expr decl.ValueExpression))
        | :? IEventDeclaration -> __notImplemented__()
        | :? IFieldDeclaration -> __notImplemented__()
        | :? ILocalConstantDeclaration as decl ->
            let newAssertions = printExpression facade assertions decl.ValueExpression
            newAssertions.With(facade.ctx.MkEq(smtConst, facade.symbols.Expr decl.ValueExpression))
        | :? ILocalVariableDeclaration as decl ->
            let newAssertions = printVariableInitializer facade assertions decl.Initial
            newAssertions.With(facade.ctx.MkEq(smtConst, facade.symbols.Expr decl.Initial))
        | _ -> __notImplemented__()

    and printParameterDeclaration (facade : HornFacade) assertions (declaration : IParameterDeclaration) =
        let sort = TypePrinter.printType facade.ctx declaration.Type
        // TODO: MkBound??
        let smtConst = facade.ctx.MkConst(IdGenerator.startingWith declaration.DeclaredName, sort)
        declaration.DeclaredElement |> ignore
        facade.symbols.[declaration.DeclaredElement] <- smtConst
        assertions

    and printTypeOwnerDeclaration facade assertions (parameterDeclaration : ITypeOwnerDeclaration) =
        match parameterDeclaration with
        | :? IParameterDeclaration as decl -> printParameterDeclaration facade assertions decl
        | :? ICatchVariableDeclaration as decl -> __notImplemented__()
        | :? IMultipleDeclarationMember as decl -> printMultipleDeclarationMember facade assertions decl
        | _ -> __notImplemented__()

    and printParametersDeclarations facade assertions parametersDeclarations =
        Seq.fold (printParameterDeclaration facade) assertions parametersDeclarations

    and printMultipleDeclarations facade assertions (declaration : IMultipleDeclaration) =
        Seq.fold (printTypeOwnerDeclaration facade) assertions declaration.Declarators

    and printReturnStatement facade assertions (statement : IReturnStatement) =
        if statement.Value = null then assertions
        else
            let newAssertions = printExpression facade assertions statement.Value
            let resultingExpr = facade.symbols.Expr statement.Value
            addRule facade newAssertions resultingExpr
            newAssertions.Returned()

    and printIfStatement facade assertions (conditional : IIfStatement) =
        let conditionAssertions = printExpression facade assertions conditional.Condition
        let condition = facade.symbols.Boolean conditional.Condition
        let thenBranch = conditionAssertions.With(condition)
        let elseBranch = conditionAssertions.With(facade.ctx.MkNot condition)
        let thenAssertions : Assertions = printStatement facade thenBranch conditional.Then
        let elseAssertions : Assertions = printStatement facade elseBranch conditional.Else
        let thenReturned = thenAssertions.IsReturned()
        let elseReturned = elseAssertions.IsReturned()
        if (thenReturned && elseReturned) then conditionAssertions.Returned()
        else if (thenReturned) then elseAssertions
        else if (thenReturned) then thenAssertions
        else (new Assertions()).Fork(thenAssertions, elseAssertions)

    and printStatement facade assertions (statement : ICSharpStatement) =
        match statement with
        | s when s = null -> assertions
        | :? IBlock as block -> printBlock facade assertions block
        | :? IBreakStatement as breakStatement -> __notImplemented__()
        | :? ICheckedStatement as checkedStatement -> __notImplemented__()
        | :? IContinueStatement as continueStatement -> __notImplemented__()
        | :? IDeclarationStatement as declaration -> printMultipleDeclarations facade assertions declaration.Declaration
        | :? IDoStatement as doStatement -> __notImplemented__()
        | :? IEmptyStatement -> assertions
        | :? IExpressionStatement as expression -> printExpression facade assertions expression.Expression
        | :? IGotoCaseStatement as gotoCase -> __notImplemented__()
        | :? IGotoStatement as goto -> __notImplemented__()
        | :? IIfStatement as fork -> printIfStatement facade assertions fork
        | :? ILabelStatement as label -> __notImplemented__()
        | :? ILockStatement as lock -> __notImplemented__()
        | :? IForeachStatement as foreach -> __notImplemented__()
        | :? IForStatement as forStatement -> __notImplemented__()
        | :? IWhileStatement as whileStatement -> __notImplemented__()
        | :? ILoopStatement as otherLoop -> __notImplemented__()
        | :? IReturnStatement as returnStatement -> printReturnStatement facade assertions returnStatement
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

    and printFunctionDeclaration (facade : HornFacade) assertions (functionDeclaration : ICSharpFunctionDeclaration) =
        let printParametrizedFunction (declaration : ICSharpParametersOwnerDeclaration) (func : ICSharpFunctionDeclaration) =
            facade.symbols.EnterFunctionDeclaration declaration
            let newAssertions = printParametersDeclarations facade assertions declaration.ParameterDeclarationsEnumerable
            let sortOfParameter (decl : ICSharpParameterDeclaration) = facade.symbols.Expr(decl.DeclaredElement :> IDeclaredElement).Sort
            let signature = Seq.map sortOfParameter declaration.ParameterDeclarationsEnumerable
            let returnSort = TypePrinter.printType facade.ctx declaration.DeclaredElement.ReturnType
            let fullSignature = (if returnSort = null then signature else Seq.append signature (Seq.singleton returnSort)) |> Seq.toArray
            let smtFuncDecl = facade.ctx.MkFuncDecl(IdGenerator.startingWith functionDeclaration.DeclaredName, fullSignature, facade.ctx.MkBoolSort())
            facade.symbols.[functionDeclaration.DeclaredElement :> IDeclaredElement] <- smtFuncDecl
            let result = printBlock facade newAssertions func.Body
            facade.symbols.LeaveFunctionDeclaration()
            result

        match functionDeclaration with
        | :? IAccessorDeclaration -> __notImplemented__()
        | :? IOperatorDeclaration -> __notImplemented__()
        | :? ICSharpParametersOwnerDeclaration as parameterized -> printParametrizedFunction parameterized functionDeclaration
        | _ -> __notImplemented__()
