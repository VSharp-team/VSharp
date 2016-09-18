namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast

module Interpreter =

    let private __notImplemented__() = raise (new System.NotImplementedException())

    let rec dbg indent (ast : JetBrains.Decompiler.Ast.INode) =
        System.Console.Write(new System.String('\t', indent))
        System.Console.WriteLine(ast.GetType().ToString())
        ast.Children |> Seq.iter (dbg (indent + 1))

// ------------------------------- INode and inheritors -------------------------------

    let rec reduceNode env (ast : INode) =
        match ast with
        | :? IStatement as statement -> reduceStatement env statement
        | :? IExpression as expression -> reduceExpression env expression
        | :? ICatchClause as catch -> reduceCatchClause env catch
        | :? IFunctionSignature as signature -> reduceFunctionSignature env signature
        | :? ILocalVariableDeclarationScopeOwner as owner -> reduceLocalVariableDeclarationScopeOwner env owner
        | :? IMemberInitializer as initializer -> reduceMemberInitializer env initializer
        | :? ISwitchCase as switchCase -> reduceSwitchCase env switchCase
        | _ -> __notImplemented__()

    and reduceCatchClause env (ast : ICatchClause) =
        __notImplemented__()

    and reduceFunctionSignature env (ast : IFunctionSignature) =
        __notImplemented__()

    and reduceSwitchCase env (ast : ISwitchCase) =
        __notImplemented__()

// ------------------------------- ILocalVariableDeclarationScopeOwner and inheritors -------------------------------

    and reduceLocalVariableDeclarationScopeOwner env (ast : ILocalVariableDeclarationScopeOwner) =
        match ast with
        | :? IAnonymousMethodExpression as expression -> reduceAnonymousMethodExpression env expression
        | :? IDecompiledMethod as expression -> reduceDecompiledMethod env expression
        | :? ILambdaBlockExpression as expression -> reduceLambdaBlockExpression env expression
        | _ -> __notImplemented__()

    and reduceDecompiledMethod env (ast : IDecompiledMethod) =
        __notImplemented__()

// ------------------------------- IMemberInitializer and inheritors -------------------------------

    and reduceMemberInitializer env (ast : IMemberInitializer) =
        match ast with
        | :? IFieldMemberInitializer as initializer -> reduceFieldMemberInitializer env initializer
        | :? IPropertyMemberInitializer as initializer -> reducePropertyMemberInitializer env initializer
        | _ -> __notImplemented__()

    and reduceFieldMemberInitializer env (ast : IFieldMemberInitializer) =
        __notImplemented__()

    and reducePropertyMemberInitializer env (ast : IPropertyMemberInitializer) =
        __notImplemented__()


// ------------------------------- IStatement and inheritors -------------------------------

    and reduceStatement env (ast : IStatement) =
        match ast with
        | :? IAbstractGotoStatement as abstractGoto -> reduceAbstractGotoStatement env abstractGoto
        | :? IAbstractLoopStatement as abstractLoop -> reduceAbstractLoopStatement env abstractLoop
        | :? IBlockStatement as blockStatement -> reduceBlockStatement env blockStatement
        | :? ICommentStatement as commentStatement -> reduceCommentStatement env commentStatement
        | :? IEmptyStatement as emptyStatement -> reduceEmptyStatement env emptyStatement
        | :? IEndFinallyStatement as endFinally -> reduceEndFinallyStatement env endFinally
        | :? IExpressionStatement as expressionStatement -> reduceExpressionStatement env expressionStatement
        | :? IFixedStatement as fixedStatement -> reduceFixedStatement env fixedStatement
        | :? IIfStatement as ifStatement -> reduceIfStatement env ifStatement
        | :? IJumpStatement as jump -> reduceJumpStatement env jump
        | :? ILabelDeclarationStatement as labelDeclaration -> reduceLabelDeclarationStatement env labelDeclaration
        | :? ILocalVariableDeclarationStatement as localVariableDeclaration -> reduceLocalVariableDeclarationStatement env localVariableDeclaration
        | :? ILockStatement as lockStatement -> reduceLockStatement env lockStatement
        | :? IMemoryCopyStatement as memoryCopy -> reduceMemoryCopyStatement env memoryCopy
        | :? IMemoryInitializeStatement as memoryInitialize -> reduceMemoryInitializeStatement env memoryInitialize
        | :? IPinStatement as pinStatement -> reducePinStatement env pinStatement
        | :? IRethrowStatement as rethrowStatement -> reduceRethrowStatement env rethrowStatement
        | :? IReturnStatement as returnStatement -> reduceReturnStatement env returnStatement
        | :? ISuccessfulFilteringStatement as filtering -> reduceSuccessfulFilteringStatement env filtering
        | :? ISwitchStatement as switchStatement -> reduceSwitchStatement env switchStatement
        | :? IThrowStatement as throwStatement -> reduceThrowStatement env throwStatement
        | :? ITryStatement as tryStatement -> reduceTryStatement env tryStatement
        | :? IUnpinStatement as unpinStatement -> reduceUnpinStatement env unpinStatement
        | :? IUsingStatement as usingStatement -> reduceUsingStatement env usingStatement
        | :? IYieldReturnStatement as yieldReturn -> reduceYieldReturnStatement env yieldReturn
        | _ -> __notImplemented__()


// ------------------------------- IAbstractGotoStatement and inheritors -------------------------------
    and reduceAbstractGotoStatement env (ast : IAbstractGotoStatement) =
        match ast with
        | :? IBreakStatement as breakStatement -> reduceBreakStatement env breakStatement
        | :? IContinueStatement as continueStatement -> reduceContinueStatement env continueStatement
        | :? IGotoCaseStatement as gotoCaseStatement -> reduceGotoCaseStatement env gotoCaseStatement 
        | :? IGotoDefaultStatement as gotoDefaultStatement -> reduceGotoDefaultStatement env gotoDefaultStatement
        | :? IGotoStatement as gotoStatement -> reduceGotoStatement env gotoStatement
        | :? IYieldBreakStatement as yieldBreakStatement -> reduceYieldBreakStatement env yieldBreakStatement
        | _ -> __notImplemented__()

    and reduceBreakStatement env (ast : IBreakStatement) =
        __notImplemented__()

    and reduceContinueStatement env (ast : IContinueStatement) =
        __notImplemented__()

    and reduceGotoCaseStatement env (ast : IGotoCaseStatement) =
        __notImplemented__()

    and reduceGotoDefaultStatement env (ast : IGotoDefaultStatement) =
        __notImplemented__()

    and reduceGotoStatement env (ast : IGotoStatement) =
        __notImplemented__()

    and reduceYieldBreakStatement env (ast : IYieldBreakStatement) =
        __notImplemented__()

// ------------------------------- IAbstractLoopStatement and inheritors -------------------------------

    and reduceAbstractLoopStatement env (ast : IAbstractLoopStatement) =
        match ast with
        | :? IForEachStatement as forEach -> reduceForEachStatement env forEach
        | :? IForStatement as forStatement -> reduceForStatement env forStatement
        | :? ILoopStatement as loop -> reduceLoopStatement env loop
        | _ -> __notImplemented__()

    and reduceForEachStatement env (ast : IForEachStatement) =
        __notImplemented__()

    and reduceForStatement env (ast : IForStatement) =
        __notImplemented__()

    and reduceLoopStatement env (ast : ILoopStatement) =
        __notImplemented__()

// ------------------------------- Rest Statements-------------------------------
    and reduceBlockStatement env (ast : IBlockStatement) =
        __notImplemented__()

    and reduceCommentStatement env (ast : ICommentStatement) =
        __notImplemented__()

    and reduceEmptyStatement env (ast : IEmptyStatement) =
        __notImplemented__()

    and reduceEndFinallyStatement env (ast : IEndFinallyStatement) =
        __notImplemented__()

    and reduceExpressionStatement env (ast : IExpressionStatement) =
        __notImplemented__()

    and reduceFixedStatement env (ast : IFixedStatement) =
        __notImplemented__()

    and reduceIfStatement env (ast : IIfStatement) =
        __notImplemented__()

    and reduceJumpStatement env (ast : IJumpStatement) =
        __notImplemented__()

    and reduceLabelDeclarationStatement env (ast : ILabelDeclarationStatement) =
        __notImplemented__()

    and reduceLocalVariableDeclarationStatement env (ast : ILocalVariableDeclarationStatement) =
        __notImplemented__()

    and reduceLockStatement env (ast : ILockStatement) =
        __notImplemented__()

    and reduceMemoryCopyStatement env (ast : IMemoryCopyStatement) =
        __notImplemented__()

    and reduceMemoryInitializeStatement env (ast : IMemoryInitializeStatement) =
        __notImplemented__()

    and reducePinStatement env (ast : IPinStatement) =
        __notImplemented__()

    and reduceRethrowStatement env (ast : IRethrowStatement) =
        __notImplemented__()

    and reduceReturnStatement env (ast : IReturnStatement) =
        __notImplemented__()

    and reduceSuccessfulFilteringStatement env (ast : ISuccessfulFilteringStatement) =
        __notImplemented__()

    and reduceSwitchStatement env (ast : ISwitchStatement) =
        __notImplemented__()

    and reduceThrowStatement env (ast : IThrowStatement) =
        __notImplemented__()

    and reduceTryStatement env (ast : ITryStatement) =
        __notImplemented__()

    and reduceUnpinStatement env (ast : IUnpinStatement) =
        __notImplemented__()

    and reduceUsingStatement env (ast : IUsingStatement) =
        __notImplemented__()

    and reduceYieldReturnStatement env (ast : IYieldReturnStatement) =
        __notImplemented__()


// ------------------------------- IExpression and inheritors -------------------------------

    and reduceExpression env (ast : IExpression) =
        match ast with
        | :? IAbstractBinaryOperationExpression as expression -> reduceAbstractBinaryOperation env expression
        | :? IAbstractTypeCastExpression as expression -> reduceAbstractTypeCastExpression env expression
        | :? IAbstractUnaryOperationExpression as expression -> reduceAbstractUnaryOperationExpression env expression
        | :? IAddressOfExpression as expression -> reduceAddressOfExpression env expression
        | :? IArgListCreationExpression as expression -> reduceArgListCreationExpression env expression
        | :? IArgListReferenceExpression as expression -> reduceArgListReferenceExpression env expression
        | :? IArrayElementAccessExpression as expression -> reduceArrayElementAccessExpression env expression
        | :? IAwaitExpression as expression -> reduceAwaitExpression env expression
        | :? IBaseReferenceExpression as expression -> reduceBaseReferenceExpression env expression
        | :? IBoxExpression as expression -> reduceBoxExpression env expression
        | :? ICheckCastExpression as expression -> reduceCheckCastExpression env expression
        | :? ICheckFiniteExpression as expression -> reduceCheckFiniteExpression env expression
        | :? IConditionalExpression as expression -> reduceConditionalExpression env expression
        | :? ICreationExpression as expression -> reduceCreationExpression env expression
        | :? IDefaultValueExpression as expression -> reduceDefaultValueExpression env expression
        | :? IDelegateCallExpression as expression -> reduceDelegateCallExpression env expression
        | :? IDerefExpression as expression -> reduceDerefExpression env expression
        | :? IExpressionList as expression -> reduceExpressionList env expression
        | :? IFieldReferenceExpression as expression -> reduceFieldReferenceExpression env expression
        | :? IFunctionPointerCallExpression as expression -> reduceFunctionPointerCallExpression env expression
        | :? ILiteralExpression as expression -> reduceLiteralExpression env expression
        | :? ILocalVariableReferenceExpression as expression -> reduceLocalVariableReferenceExpression env expression
        | :? IMakeRefExpression as expression -> reduceMakeRefExpression env expression
        | :? IMemberAccessExpression as expression -> reduceMemberAccessExpression env expression
        | :? IMemberInitializerList as expression -> reduceMemberInitializerList env expression
        | :? IMethodPointerExpression as expression -> reduceMethodPointerExpression env expression
        | :? IMethodReferenceExpression as expression -> reduceMethodReferenceExpression env expression
        | :? INestedInitializer as expression -> reduceNestedInitializer env expression
        | :? IOverflowCheckExpression as expression -> reduceOverflowCheckExpression env expression
        | :? IParameterModifierExpression as expression -> reduceParameterModifierExpression env expression
        | :? IParameterReferenceExpression as expression -> reduceParameterReferenceExpression env expression
        | :? IPointerElementAccessExpression as expression -> reducePointerElementAccessExpression env expression
        | :? IPointerIndirectionExpression as expression -> reducePointerIndirectionExpression env expression
        | :? IRefExpression as expression -> reduceRefExpression env expression
        | :? IRefTypeExpression as expression -> reduceRefTypeExpression env expression
        | :? IRefTypeTokenExpression as expression -> reduceRefTypeTokenExpression env expression
        | :? IRefValueExpression as expression -> reduceRefValueExpression env expression
        | :? ISizeOfExpression as expression -> reduceSizeOfExpression env expression
        | :? IStackAllocExpression as expression -> reduceStackAllocExpression env expression
        | :? IThisReferenceExpression as expression -> reduceThisReferenceExpression env expression
        | :? ITryCastExpression as expression -> reduceTryCastExpression env expression
        | :? ITypeOfExpression as expression -> reduceTypeOfExpression env expression
        | :? ITypeReferenceExpression as expression -> reduceTypeReferenceExpression env expression
        | :? IUnboxExpression as expression -> reduceUnboxExpression env expression
        | :? IUntypedStackAllocExpression as expression -> reduceUntypedStackAllocExpression env expression
        | :? IVirtualMethodPointerExpression as expression -> reduceVirtualMethodPointerExpression env expression
        | _ -> __notImplemented__()

    and reduceAddressOfExpression env (ast : IAddressOfExpression) =
        __notImplemented__()

    and reduceArgListCreationExpression env (ast : IArgListCreationExpression) =
        __notImplemented__()

    and reduceArgListReferenceExpression env (ast : IArgListReferenceExpression) =
        __notImplemented__()

    and reduceArrayElementAccessExpression env (ast : IArrayElementAccessExpression) =
        __notImplemented__()

    and reduceAwaitExpression env (ast : IAwaitExpression) =
        __notImplemented__()

    and reduceBaseReferenceExpression env (ast : IBaseReferenceExpression) =
        __notImplemented__()

    and reduceBoxExpression env (ast : IBoxExpression) =
        __notImplemented__()

    and reduceCheckCastExpression env (ast : ICheckCastExpression) =
        __notImplemented__()

    and reduceCheckFiniteExpression env (ast : ICheckFiniteExpression) =
        __notImplemented__()

    and reduceConditionalExpression env (ast : IConditionalExpression) =
        __notImplemented__()

    and reduceDefaultValueExpression env (ast : IDefaultValueExpression) =
        __notImplemented__()

    and reduceDelegateCallExpression env (ast : IDelegateCallExpression) =
        __notImplemented__()

    and reduceDerefExpression env (ast : IDerefExpression) =
        __notImplemented__()

    and reduceExpressionList env (ast : IExpressionList) =
        __notImplemented__()

    and reduceFieldReferenceExpression env (ast : IFieldReferenceExpression) =
        __notImplemented__()

    and reduceFunctionPointerCallExpression env (ast : IFunctionPointerCallExpression) =
        __notImplemented__()

    and reduceLiteralExpression env (ast : ILiteralExpression) =
        __notImplemented__()

    and reduceLocalVariableReferenceExpression env (ast : ILocalVariableReferenceExpression) =
        __notImplemented__()

    and reduceMakeRefExpression env (ast : IMakeRefExpression) =
        __notImplemented__()

    and reduceMemberInitializerList env (ast : IMemberInitializerList ) =
        __notImplemented__()

    and reduceMethodPointerExpression env (ast : IMethodPointerExpression) =
        __notImplemented__()

    and reduceMethodReferenceExpression env (ast : IMethodReferenceExpression) =
        __notImplemented__()

    and reduceNestedInitializer env (ast : INestedInitializer) =
        __notImplemented__()

    and reduceParameterModifierExpression env (ast : IParameterModifierExpression) =
        __notImplemented__()

    and reduceParameterReferenceExpression env (ast : IParameterReferenceExpression) =
        __notImplemented__()

    and reducePointerElementAccessExpression env (ast : IPointerElementAccessExpression) =
        __notImplemented__()

    and reducePointerIndirectionExpression env (ast : IPointerIndirectionExpression) =
        __notImplemented__()

    and reduceRefExpression env (ast : IRefExpression) =
        __notImplemented__()

    and reduceRefTypeExpression env (ast : IRefTypeExpression) =
        __notImplemented__()

    and reduceRefTypeTokenExpression env (ast : IRefTypeTokenExpression) =
        __notImplemented__()

    and reduceRefValueExpression env (ast : IRefValueExpression) =
        __notImplemented__()

    and reduceSizeOfExpression env (ast : ISizeOfExpression) =
        __notImplemented__()

    and reduceStackAllocExpression env (ast : IStackAllocExpression) =
        __notImplemented__()

    and reduceThisReferenceExpression env (ast : IThisReferenceExpression) =
        __notImplemented__()

    and reduceTryCastExpression env (ast : ITryCastExpression) =
        __notImplemented__()

    and reduceTypeOfExpression env (ast : ITypeOfExpression) =
        __notImplemented__()

    and reduceTypeReferenceExpression env (ast : ITypeReferenceExpression) =
        __notImplemented__()

    and reduceUnboxExpression env (ast : IUnboxExpression) =
        __notImplemented__()

    and reduceUntypedStackAllocExpression env (ast : IUntypedStackAllocExpression) =
        __notImplemented__()

    and reduceVirtualMethodPointerExpression env (ast : IVirtualMethodPointerExpression) =
        __notImplemented__()

// ------------------------------- IAbstractBinaryOperationExpression and inheritors -------------------------------

    and reduceAbstractBinaryOperation env (ast : IAbstractBinaryOperationExpression) =
        match ast with
        | :? IBinaryOperationExpression as binOp -> reduceBinaryOperationExpression env binOp
        | :? IUserDefinedBinaryOperationExpression as userBinOp -> reduceUserDefinedBinaryOperationExpression env userBinOp
        | _ -> __notImplemented__()

    and reduceBinaryOperationExpression env (ast : IBinaryOperationExpression) =
        __notImplemented__()

    and reduceUserDefinedBinaryOperationExpression env (ast : IUserDefinedBinaryOperationExpression) =
        __notImplemented__()


// ------------------------------- IAbstractTypeCastExpression and inheritors -------------------------------

    and reduceAbstractTypeCastExpression env (ast : IAbstractTypeCastExpression) =
        match ast with
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression env expression
        | :? IUserDefinedTypeCastExpression as expression -> reduceUserDefinedTypeCastExpression env expression
        | _ -> __notImplemented__()

    and reduceTypeCastExpression env (ast : ITypeCastExpression) =
        __notImplemented__()

    and reduceUserDefinedTypeCastExpression env (ast : IUserDefinedTypeCastExpression) =
        __notImplemented__()

// ------------------------------- IAbstractUnaryOperationExpression and inheritors -------------------------------

    and reduceAbstractUnaryOperationExpression env (ast : IAbstractUnaryOperationExpression) =
        match ast with
        | :? IUnaryOperationExpression as expression -> reduceUnaryOperationExpression env expression
        | :? IUserDefinedUnaryOperationExpression as expression -> reduceUserDefinedUnaryOperationExpression env expression
        | _ -> __notImplemented__()

    and reduceUnaryOperationExpression env (ast : IUnaryOperationExpression) =
        __notImplemented__()

    and reduceUserDefinedUnaryOperationExpression env (ast : IUserDefinedUnaryOperationExpression) =
        __notImplemented__()

// ------------------------------- ICreationExpression and inheritors -------------------------------

    and reduceCreationExpression env (ast : ICreationExpression) =
        match ast with
        | :? IAnonymousMethodExpression as expression -> reduceAnonymousMethodExpression env expression
        | :? IAnonymousObjectCreationExpression as expression -> reduceAnonymousObjectCreationExpression env expression
        | :? IArrayCreationExpression as expression -> reduceArrayCreationExpression env expression
        | :? IDelegateCreationExpression as expression -> reduceDelegateCreationExpression env expression
        | :? ILambdaBlockExpression as expression -> reduceLambdaBlockExpression env expression
        | :? ILambdaExpression as expression -> reduceLambdaExpression env expression
        | :? IObjectCreationExpression as expression -> reduceObjectCreationExpression env expression
        | _ -> __notImplemented__()

    and reduceAnonymousMethodExpression env (ast : IAnonymousMethodExpression) =
        __notImplemented__()

    and reduceAnonymousObjectCreationExpression env (ast : IAnonymousObjectCreationExpression) =
        __notImplemented__()

    and reduceArrayCreationExpression env (ast : IArrayCreationExpression) =
        __notImplemented__()

    and reduceDelegateCreationExpression env (ast : IDelegateCreationExpression) =
        __notImplemented__()

    and reduceLambdaBlockExpression env (ast : ILambdaBlockExpression) =
        __notImplemented__()

    and reduceLambdaExpression env (ast : ILambdaExpression) =
        __notImplemented__()

    and reduceObjectCreationExpression env (ast : IObjectCreationExpression) =
        __notImplemented__()

// ------------------------------- IMemberAccessExpression and inheritors -------------------------------

    and reduceMemberAccessExpression env (ast : IMemberAccessExpression) =
        match ast with
        | :? IFieldAccessExpression as expression -> reduceFieldAccessExpression env expression
        | :? IMemberCallExpression as expression -> reduceMemberAccessExpression env expression
        | _ -> __notImplemented__()

    and reduceFieldAccessExpression env (ast : IFieldAccessExpression) =
        __notImplemented__()

// ------------------------------- IMemberCallExpression and inheritors -------------------------------

    and reduceMemberCallExpression env (ast : IMemberCallExpression) =
        match ast with
        | :? IEventAccessExpression as expression -> reduceEventAccessExpression env expression
        | :? IIndexerCallExpression as expression -> reduceIndexerCallExpression env expression
        | :? IMethodCallExpression as expression -> reduceMethodCallExpression env expression
        | :? IPropertyAccessExpression as expression -> reducePropertyAccessExpression env expression
        | _ -> __notImplemented__()

    and reduceEventAccessExpression env (ast : IEventAccessExpression) =
        __notImplemented__()

    and reduceIndexerCallExpression env (ast : IIndexerCallExpression) =
        __notImplemented__()

    and reduceMethodCallExpression env (ast : IMethodCallExpression) =
        __notImplemented__()

    and reducePropertyAccessExpression env (ast : IPropertyAccessExpression) =
        __notImplemented__()

// ------------------------------- IOverflowCheckExpression and inheritors -------------------------------

    and reduceOverflowCheckExpression env (ast : IOverflowCheckExpression) =
        match ast with
        | :? IBinaryOperationExpression as expression -> reduceBinaryOperationExpression env expression
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression env expression
        | :? IUnaryOperationExpression as expression -> reduceUnaryOperationExpression env expression
        | _ -> __notImplemented__()
