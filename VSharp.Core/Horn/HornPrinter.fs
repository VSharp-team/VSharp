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


    and printUnaryExpression (facade : HornFacade) (assertions : Assertions) (expression : IUnaryExpression) =
        match expression with
        | :? IReferenceExpression -> __notImplemented__()
        | :? ITypeofExpression -> __notImplemented__()
        | :? IUnaryOperatorExpression -> __notImplemented__()
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

    and printConditionalAccessExpression facade assertions (expression : IConditionalAccessExpression) =
        match expression with
        | :? IElementAccessExpression -> __notImplemented__()
        | :? IInvocationExpression as invocation ->
            let invokedAssertions = printExpression facade assertions invocation.InvokedExpression
            let newAssertions = printArgumentList facade invokedAssertions invocation.ArgumentList
            let args = Seq.map (fun arg -> facade.symbols.Expr(arg :> ITreeNode)) invocation.ArgumentsEnumerable
            match facade.symbols.[invocation.InvokedExpression] with
            | :? Z3.FuncDecl as func ->
                let argsWithReturn =
                    if facade.symbols.HasReturnType func then
                        let returnType = func.Parameters.[func.Parameters.Length - 1].Sort
                        let resultVar = facade.symbols.NewIntermediateVar(facade.ctx, returnType)
                        facade.symbols.[expression] <- resultVar
                        Seq.append args (Seq.singleton resultVar)
                    else
                        args
                assertions.With (func.Apply(argsWithReturn |> Seq.toArray) :?> Z3.BoolExpr)
            | _ -> __notImplemented__()
        | :? IReferenceExpression -> __notImplemented__()
        | _ -> __notImplemented__()

    and printCreationExpression(facade : HornFacade) (assertions : Assertions) (expression : ICreationExpression) =
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

    and printExpression (facade : HornFacade) (assertions : Assertions) (expression : ICSharpExpression) =
        match expression with
        | :? IAnonymousFunctionExpression as anonymous -> printAnonymousFunctionExpression facade assertions anonymous
        | :? IAsExpression -> __notImplemented__()
        | :? IAwaitExpression -> __notImplemented__()
        | :? IBaseExpression -> __notImplemented__()
        | :? ICastExpression -> __notImplemented__()
        | :? ICheckedExpression -> __notImplemented__()
        | :? IConditionalAccessExpression as conditional -> printConditionalAccessExpression facade assertions conditional
        | :? IConditionalTernaryExpression -> __notImplemented__()
        | :? ICreationExpression as creation -> printCreationExpression facade assertions creation
        | :? ICSharpLiteralExpression -> __notImplemented__()
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

    and printStatement facade assertions (statement : ICSharpStatement) =
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

    and printFunctionDeclaration facade assertions (functionDeclaration : ICSharpFunctionDeclaration) =
        let printParametrizedFunction (declaration : ICSharpParametersOwnerDeclaration) (func : ICSharpFunctionDeclaration) =
            let newAssertions = printParametersDeclarations facade assertions declaration.ParameterDeclarationsEnumerable
            let sortOfParameter (decl : ICSharpParameterDeclaration) = facade.symbols.Expr(decl.DeclaredElement :> IDeclaredElement).Sort
            let signature = Seq.map sortOfParameter declaration.ParameterDeclarationsEnumerable
            let returnSort = TypePrinter.printType facade.ctx declaration.DeclaredElement.ReturnType
            let fullSignature = (if returnSort = null then signature else Seq.append signature (Seq.singleton returnSort)) |> Seq.toArray
            let smtFuncDecl = facade.ctx.MkFuncDecl(IdGenerator.startingWith functionDeclaration.DeclaredName, fullSignature, facade.ctx.MkBoolSort())
            facade.symbols.[functionDeclaration.DeclaredElement :> IDeclaredElement] <- smtFuncDecl
            printBlock facade newAssertions func.Body

        match functionDeclaration with
        | :? IAccessorDeclaration -> __notImplemented__()
        | :? IOperatorDeclaration -> __notImplemented__()
        | :? ICSharpParametersOwnerDeclaration as parameterized -> printParametrizedFunction parameterized functionDeclaration
        | _ -> __notImplemented__()
