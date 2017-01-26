namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open System

// TODO: use CPS jedi...
module Interpreter =

    let assemblyLoader = new JetBrains.Metadata.Reader.API.MetadataLoader(JetBrains.Metadata.Access.MetadataProviderFactory.DefaultProvider)
    let private __notImplemented__() = raise (new System.NotImplementedException())

    let rec dbg indent (ast : JetBrains.Decompiler.Ast.INode) =
        System.Console.Write(new System.String('\t', indent))
        System.Console.WriteLine(ast.GetType().ToString())
        ast.Children |> Seq.iter (dbg (indent + 1))

// ------------------------------- Decompilation -------------------------------
    let rec decompileAndReduceMethod (meth : JetBrains.ReSharper.Psi.IMethod) =
        let isValid = (meth <> null) && (meth.GetContainingType() <> null) && (meth.GetContainingType().GetClrName() <> null)

        let qualifiedTypeName =
            if isValid then meth.GetContainingType().GetClrName().FullName
            else raise(new System.ArgumentException("What have I done..."))
            //else (invocation.InvokedExpression :?> IReferenceExpression).GetExtensionQualifier().GetText()
        let methodName = if isValid then meth.ShortName else "NahSon"//invocation.InvocationExpressionReference.GetName()

        let path =
            match meth.GetContainingType().Module with
            | :? JetBrains.ReSharper.Psi.Modules.IAssemblyPsiModule as assemblyModule -> assemblyModule.Assembly.Location
            | _ -> raise(new System.Exception("Shit happens"))

        let metadataAssembly = assemblyLoader.LoadFrom(path, fun x -> true)

        let rp = JetBrains.Util.FileSystemPath.Parse(typeof<System.String>.Assembly.Location).Parent
        metadataAssembly.ReferencedAssembliesNames |> Seq.iter (fun ass -> Console.WriteLine("Loaded from " + assemblyLoader.LoadFrom(JetBrains.Metadata.Utils.AssemblyNameMetadataExtensions.FindAssemblyFile(rp, ass), fun x -> true).Location.ToString()))
        let metadataTypeInfo = metadataAssembly.GetTypeInfoFromQualifiedName(qualifiedTypeName, false)
        System.Console.WriteLine("METADATA ASS: " + metadataAssembly.Location.FullPath + " " + metadataAssembly.IsResolved.ToString())
        System.Console.WriteLine("TYPE: " + metadataTypeInfo.ToString())
        let metadataMethod = metadataTypeInfo.GetMethods() |> Seq.pick (fun m -> if m.Name.Equals(methodName) then Some(m) else None)

        let lifetime = JetBrains.DataFlow.Lifetimes.Define()
        let methodCollector = new JetBrains.Metadata.Utils.MethodCollectorStub()
        let options = new JetBrains.Decompiler.ClassDecompilerOptions(true)
        let decompiler = new JetBrains.Decompiler.ClassDecompiler(lifetime.Lifetime, metadataAssembly, options, methodCollector)
        let decompiledMethod = decompiler.Decompile(metadataTypeInfo, metadataMethod)
        System.Console.WriteLine("DECOMPILED: " + JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
        System.Console.WriteLine("DECOMPILED BODY: " + JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod.Body))
        System.Console.WriteLine("NOW TRACING:")
        dbg 0 decompiledMethod
        reduceDecompiledMethod VSharp.Core.Symbolic.State.empty decompiledMethod

// ------------------------------- INode and inheritors -------------------------------

    and reduceNode state (ast : INode) =
        match ast with
        | :? IStatement as statement -> reduceStatement state statement
        | :? IExpression as expression -> reduceExpression state expression
        | :? ICatchClause as catch -> reduceCatchClause state catch
        | :? IFunctionSignature as signature -> (Nop, reduceFunctionSignature state signature)
        | :? ILocalVariableDeclarationScopeOwner as owner -> reduceLocalVariableDeclarationScopeOwner state owner
        | :? IMemberInitializer as initializer -> reduceMemberInitializer state initializer
        | :? ISwitchCase as switchCase -> reduceSwitchCase state switchCase
        | _ -> __notImplemented__()

    and reduceCatchClause state (ast : ICatchClause) =
        __notImplemented__()

    and reduceFunctionSignature state (ast : IFunctionSignature) =
        let foldParam state (param : IMethodParameter) =
            let freshConst = Terms.FreshConstant param.Name (System.Type.GetType(param.Type.FullName))
            State.addTerm state param.Name freshConst
        ast.Parameters |> Seq.fold foldParam state

    and reduceSwitchCase state (ast : ISwitchCase) =
        __notImplemented__()

// ------------------------------- ILocalVariableDeclarationScopeOwner and inheritors -------------------------------

    and reduceLocalVariableDeclarationScopeOwner state (ast : ILocalVariableDeclarationScopeOwner) =
        match ast with
        | :? IAnonymousMethodExpression as expression -> reduceAnonymousMethodExpression state expression
        | :? IDecompiledMethod as expression -> reduceDecompiledMethod state expression
        | :? ILambdaBlockExpression as expression -> reduceLambdaBlockExpression state expression
        | _ -> __notImplemented__()

    and reduceDecompiledMethod state (ast : IDecompiledMethod) =
        let newState = reduceFunctionSignature state ast.Signature
        reduceBlockStatement newState ast.Body

// ------------------------------- IMemberInitializer and inheritors -------------------------------

    and reduceMemberInitializer state (ast : IMemberInitializer) =
        match ast with
        | :? IFieldMemberInitializer as initializer -> reduceFieldMemberInitializer state initializer
        | :? IPropertyMemberInitializer as initializer -> reducePropertyMemberInitializer state initializer
        | _ -> __notImplemented__()

    and reduceFieldMemberInitializer state (ast : IFieldMemberInitializer) =
        __notImplemented__()

    and reducePropertyMemberInitializer state (ast : IPropertyMemberInitializer) =
        __notImplemented__()


// ------------------------------- IStatement and inheritors -------------------------------

    and reduceStatement state (ast : IStatement) =
        match ast with
        | :? IAbstractGotoStatement as abstractGoto -> reduceAbstractGotoStatement state abstractGoto
        | :? IAbstractLoopStatement as abstractLoop -> reduceAbstractLoopStatement state abstractLoop
        | :? IBlockStatement as blockStatement -> reduceBlockStatement state blockStatement
        | :? ICommentStatement as commentStatement -> reduceCommentStatement state commentStatement
        | :? IEmptyStatement as emptyStatement -> reduceEmptyStatement state emptyStatement
        | :? IEndFinallyStatement as endFinally -> reduceEndFinallyStatement state endFinally
        | :? IExpressionStatement as expressionStatement -> reduceExpressionStatement state expressionStatement
        | :? IFixedStatement as fixedStatement -> reduceFixedStatement state fixedStatement
        | :? IIfStatement as ifStatement -> reduceIfStatement state ifStatement
        | :? IJumpStatement as jump -> reduceJumpStatement state jump
        | :? ILabelDeclarationStatement as labelDeclaration -> reduceLabelDeclarationStatement state labelDeclaration
        | :? ILocalVariableDeclarationStatement as localVariableDeclaration -> reduceLocalVariableDeclarationStatement state localVariableDeclaration
        | :? ILockStatement as lockStatement -> reduceLockStatement state lockStatement
        | :? IMemoryCopyStatement as memoryCopy -> reduceMemoryCopyStatement state memoryCopy
        | :? IMemoryInitializeStatement as memoryInitialize -> reduceMemoryInitializeStatement state memoryInitialize
        | :? IPinStatement as pinStatement -> reducePinStatement state pinStatement
        | :? IRethrowStatement as rethrowStatement -> reduceRethrowStatement state rethrowStatement
        | :? IReturnStatement as returnStatement -> reduceReturnStatement state returnStatement
        | :? ISuccessfulFilteringStatement as filtering -> reduceSuccessfulFilteringStatement state filtering
        | :? ISwitchStatement as switchStatement -> reduceSwitchStatement state switchStatement
        | :? IThrowStatement as throwStatement -> reduceThrowStatement state throwStatement
        | :? ITryStatement as tryStatement -> reduceTryStatement state tryStatement
        | :? IUnpinStatement as unpinStatement -> reduceUnpinStatement state unpinStatement
        | :? IUsingStatement as usingStatement -> reduceUsingStatement state usingStatement
        | :? IYieldReturnStatement as yieldReturn -> reduceYieldReturnStatement state yieldReturn
        | _ -> __notImplemented__()


// ------------------------------- IAbstractGotoStatement and inheritors -------------------------------

    and reduceAbstractGotoStatement state (ast : IAbstractGotoStatement) =
        match ast with
        | :? IBreakStatement as breakStatement -> reduceBreakStatement state breakStatement
        | :? IContinueStatement as continueStatement -> reduceContinueStatement state continueStatement
        | :? IGotoCaseStatement as gotoCaseStatement -> reduceGotoCaseStatement state gotoCaseStatement 
        | :? IGotoDefaultStatement as gotoDefaultStatement -> reduceGotoDefaultStatement state gotoDefaultStatement
        | :? IGotoStatement as gotoStatement -> reduceGotoStatement state gotoStatement
        | :? IYieldBreakStatement as yieldBreakStatement -> reduceYieldBreakStatement state yieldBreakStatement
        | _ -> __notImplemented__()

    and reduceBreakStatement state (ast : IBreakStatement) =
        __notImplemented__()

    and reduceContinueStatement state (ast : IContinueStatement) =
        __notImplemented__()

    and reduceGotoCaseStatement state (ast : IGotoCaseStatement) =
        __notImplemented__()

    and reduceGotoDefaultStatement state (ast : IGotoDefaultStatement) =
        __notImplemented__()

    and reduceGotoStatement state (ast : IGotoStatement) =
        __notImplemented__()

    and reduceYieldBreakStatement state (ast : IYieldBreakStatement) =
        __notImplemented__()

// ------------------------------- IAbstractLoopStatement and inheritors -------------------------------

    and reduceAbstractLoopStatement state (ast : IAbstractLoopStatement) =
        match ast with
        | :? IForEachStatement as forEach -> reduceForEachStatement state forEach
        | :? IForStatement as forStatement -> reduceForStatement state forStatement
        | :? ILoopStatement as loop -> reduceLoopStatement state loop
        | _ -> __notImplemented__()

    and reduceForEachStatement state (ast : IForEachStatement) =
        __notImplemented__()

    and reduceForStatement state (ast : IForStatement) =
        __notImplemented__()

    and reduceLoopStatement state (ast : ILoopStatement) =
        __notImplemented__()

// ------------------------------- Rest Statements-------------------------------

    and reduceBlockStatement state (ast : IBlockStatement) =
        let foldStatement (curTerm, curState) statement =
            if Terms.IsVoid curTerm then reduceStatement curState statement
            else (curTerm, curState)
        ast.Statements |> Seq.fold foldStatement (Nop, state)
        // TODO: Remove local variables declarations
        // TODO: Actually branches of if statement may not be IBlockStatement, but arbitrary statement
        // (including int x = ...). We should get rid of that local declarations too.

    and reduceCommentStatement state (ast : ICommentStatement) =
        __notImplemented__()

    and reduceEmptyStatement state (ast : IEmptyStatement) =
        __notImplemented__()

    and reduceEndFinallyStatement state (ast : IEndFinallyStatement) =
        __notImplemented__()

    and reduceExpressionStatement state (ast : IExpressionStatement) =
        let term, newState = reduceExpression state ast.Expression
        ((if Terms.IsError term then term else Nop), newState)

    and reduceFixedStatement state (ast : IFixedStatement) =
        __notImplemented__()

    and reduceIfStatement state (ast : IIfStatement) =
        let condition, conditionState = reduceExpression state ast.Condition
        match condition with
        | Terms.True ->  reduceStatement conditionState ast.Then
        | Terms.False -> reduceStatement conditionState ast.Else
        | _ ->
            let thenVal, thenState = reduceStatement conditionState ast.Then
            let elseVal, elseState = reduceStatement conditionState ast.Else
            Merging.merge condition thenVal elseVal conditionState thenState elseState

    and reduceJumpStatement state (ast : IJumpStatement) =
        __notImplemented__()

    and reduceLabelDeclarationStatement state (ast : ILabelDeclarationStatement) =
        __notImplemented__()

    and reduceLocalVariableDeclarationStatement state (ast : ILocalVariableDeclarationStatement) =
        __notImplemented__()

    and reduceLockStatement state (ast : ILockStatement) =
        __notImplemented__()

    and reduceMemoryCopyStatement state (ast : IMemoryCopyStatement) =
        __notImplemented__()

    and reduceMemoryInitializeStatement state (ast : IMemoryInitializeStatement) =
        __notImplemented__()

    and reducePinStatement state (ast : IPinStatement) =
        __notImplemented__()

    and reduceRethrowStatement state (ast : IRethrowStatement) =
        __notImplemented__()

    and reduceReturnStatement state (ast : IReturnStatement) =
        reduceExpression state ast.Result

    and reduceSuccessfulFilteringStatement state (ast : ISuccessfulFilteringStatement) =
        __notImplemented__()

    and reduceSwitchStatement state (ast : ISwitchStatement) =
        __notImplemented__()

    and reduceThrowStatement state (ast : IThrowStatement) =
        __notImplemented__()

    and reduceTryStatement state (ast : ITryStatement) =
        __notImplemented__()

    and reduceUnpinStatement state (ast : IUnpinStatement) =
        __notImplemented__()

    and reduceUsingStatement state (ast : IUsingStatement) =
        __notImplemented__()

    and reduceYieldReturnStatement state (ast : IYieldReturnStatement) =
        __notImplemented__()


// ------------------------------- IExpression and inheritors -------------------------------

    and reduceExpression state (ast : IExpression) =
        match ast with
        | :? IAbstractBinaryOperationExpression as expression -> reduceAbstractBinaryOperation state expression
        | :? IAbstractTypeCastExpression as expression -> reduceAbstractTypeCastExpression state expression
        | :? IAbstractUnaryOperationExpression as expression -> reduceAbstractUnaryOperationExpression state expression
        | :? IAddressOfExpression as expression -> reduceAddressOfExpression state expression
        | :? IArgListCreationExpression as expression -> reduceArgListCreationExpression state expression
        | :? IArgListReferenceExpression as expression -> reduceArgListReferenceExpression state expression
        | :? IArrayElementAccessExpression as expression -> reduceArrayElementAccessExpression state expression
        | :? IAwaitExpression as expression -> reduceAwaitExpression state expression
        | :? IBaseReferenceExpression as expression -> reduceBaseReferenceExpression state expression
        | :? IBoxExpression as expression -> reduceBoxExpression state expression
        | :? ICheckCastExpression as expression -> reduceCheckCastExpression state expression
        | :? ICheckFiniteExpression as expression -> reduceCheckFiniteExpression state expression
        | :? IConditionalExpression as expression -> reduceConditionalExpression state expression
        | :? ICreationExpression as expression -> reduceCreationExpression state expression
        | :? IDefaultValueExpression as expression -> reduceDefaultValueExpression state expression
        | :? IDelegateCallExpression as expression -> reduceDelegateCallExpression state expression
        | :? IDerefExpression as expression -> reduceDerefExpression state expression
        | :? IExpressionList as expression -> reduceExpressionList state expression
        | :? IFieldReferenceExpression as expression -> reduceFieldReferenceExpression state expression
        | :? IFunctionPointerCallExpression as expression -> reduceFunctionPointerCallExpression state expression
        | :? ILiteralExpression as expression -> reduceLiteralExpression state expression
        | :? ILocalVariableReferenceExpression as expression -> reduceLocalVariableReferenceExpression state expression
        | :? IMakeRefExpression as expression -> reduceMakeRefExpression state expression
        | :? IMemberAccessExpression as expression -> reduceMemberAccessExpression state expression
        | :? IMemberInitializerList as expression -> reduceMemberInitializerList state expression
        | :? IMethodPointerExpression as expression -> reduceMethodPointerExpression state expression
        | :? IMethodReferenceExpression as expression -> reduceMethodReferenceExpression state expression
        | :? INestedInitializer as expression -> reduceNestedInitializer state expression
        | :? IParameterModifierExpression as expression -> reduceParameterModifierExpression state expression
        | :? IParameterReferenceExpression as expression -> reduceParameterReferenceExpression state expression
        | :? IPointerElementAccessExpression as expression -> reducePointerElementAccessExpression state expression
        | :? IPointerIndirectionExpression as expression -> reducePointerIndirectionExpression state expression
        | :? IRefExpression as expression -> reduceRefExpression state expression
        | :? IRefTypeExpression as expression -> reduceRefTypeExpression state expression
        | :? IRefTypeTokenExpression as expression -> reduceRefTypeTokenExpression state expression
        | :? IRefValueExpression as expression -> reduceRefValueExpression state expression
        | :? ISizeOfExpression as expression -> reduceSizeOfExpression state expression
        | :? IStackAllocExpression as expression -> reduceStackAllocExpression state expression
        | :? IThisReferenceExpression as expression -> reduceThisReferenceExpression state expression
        | :? ITryCastExpression as expression -> reduceTryCastExpression state expression
        | :? ITypeOfExpression as expression -> reduceTypeOfExpression state expression
        | :? ITypeReferenceExpression as expression -> reduceTypeReferenceExpression state expression
        | :? IUnboxExpression as expression -> reduceUnboxExpression state expression
        | :? IUntypedStackAllocExpression as expression -> reduceUntypedStackAllocExpression state expression
        | :? IVirtualMethodPointerExpression as expression -> reduceVirtualMethodPointerExpression state expression
        | _ -> __notImplemented__()

    and reduceAddressOfExpression state (ast : IAddressOfExpression) =
        __notImplemented__()

    and reduceArgListCreationExpression state (ast : IArgListCreationExpression) =
        __notImplemented__()

    and reduceArgListReferenceExpression state (ast : IArgListReferenceExpression) =
        __notImplemented__()

    and reduceArrayElementAccessExpression state (ast : IArrayElementAccessExpression) =
        __notImplemented__()

    and reduceAwaitExpression state (ast : IAwaitExpression) =
        __notImplemented__()

    and reduceBaseReferenceExpression state (ast : IBaseReferenceExpression) =
        __notImplemented__()

    and reduceBoxExpression state (ast : IBoxExpression) =
        __notImplemented__()

    and reduceCheckCastExpression state (ast : ICheckCastExpression) =
        __notImplemented__()

    and reduceCheckFiniteExpression state (ast : ICheckFiniteExpression) =
        __notImplemented__()

    and reduceConditionalExpression state (ast : IConditionalExpression) =
        __notImplemented__()

    and reduceDefaultValueExpression state (ast : IDefaultValueExpression) =
        __notImplemented__()

    and reduceDelegateCallExpression state (ast : IDelegateCallExpression) =
        __notImplemented__()

    and reduceDerefExpression state (ast : IDerefExpression) =
        __notImplemented__()

    and reduceExpressionList state (ast : IExpressionList) =
        __notImplemented__()

    and reduceFieldReferenceExpression state (ast : IFieldReferenceExpression) =
        __notImplemented__()

    and reduceFunctionPointerCallExpression state (ast : IFunctionPointerCallExpression) =
        __notImplemented__()

    and reduceLiteralExpression state (ast : ILiteralExpression) =
        (Terms.MakeConcrete ast.Value.Value (System.Type.GetType(ast.Value.Type.AssemblyQualifiedName)), state)

    and reduceLocalVariableReferenceExpression state (ast : ILocalVariableReferenceExpression) =
        __notImplemented__()

    and reduceMakeRefExpression state (ast : IMakeRefExpression) =
        __notImplemented__()

    and reduceMemberInitializerList state (ast : IMemberInitializerList ) =
        __notImplemented__()

    and reduceMethodPointerExpression state (ast : IMethodPointerExpression) =
        __notImplemented__()

    and reduceMethodReferenceExpression state (ast : IMethodReferenceExpression) =
        __notImplemented__()

    and reduceNestedInitializer state (ast : INestedInitializer) =
        __notImplemented__()

    and reduceParameterModifierExpression state (ast : IParameterModifierExpression) =
        __notImplemented__()

    and reduceParameterReferenceExpression state (ast : IParameterReferenceExpression) =
        (State.eval state ast.Parameter.Name, state)

    and reducePointerElementAccessExpression state (ast : IPointerElementAccessExpression) =
        __notImplemented__()

    and reducePointerIndirectionExpression state (ast : IPointerIndirectionExpression) =
        __notImplemented__()

    and reduceRefExpression state (ast : IRefExpression) =
        __notImplemented__()

    and reduceRefTypeExpression state (ast : IRefTypeExpression) =
        __notImplemented__()

    and reduceRefTypeTokenExpression state (ast : IRefTypeTokenExpression) =
        __notImplemented__()

    and reduceRefValueExpression state (ast : IRefValueExpression) =
        __notImplemented__()

    and reduceSizeOfExpression state (ast : ISizeOfExpression) =
        __notImplemented__()

    and reduceStackAllocExpression state (ast : IStackAllocExpression) =
        __notImplemented__()

    and reduceThisReferenceExpression state (ast : IThisReferenceExpression) =
        __notImplemented__()

    and reduceTryCastExpression state (ast : ITryCastExpression) =
        __notImplemented__()

    and reduceTypeOfExpression state (ast : ITypeOfExpression) =
        __notImplemented__()

    and reduceTypeReferenceExpression state (ast : ITypeReferenceExpression) =
        __notImplemented__()

    and reduceUnboxExpression state (ast : IUnboxExpression) =
        __notImplemented__()

    and reduceUntypedStackAllocExpression state (ast : IUntypedStackAllocExpression) =
        __notImplemented__()

    and reduceVirtualMethodPointerExpression state (ast : IVirtualMethodPointerExpression) =
        __notImplemented__()

// ------------------------------- IAbstractBinaryOperationExpression and inheritors -------------------------------

    and reduceAbstractBinaryOperation state (ast : IAbstractBinaryOperationExpression) =
        match ast with
        | :? IBinaryOperationExpression as binOp -> reduceBinaryOperationExpression state binOp
        | :? IUserDefinedBinaryOperationExpression as userBinOp -> reduceUserDefinedBinaryOperationExpression state userBinOp
        | _ -> __notImplemented__()

    and reduceBinaryOperationExpression state (ast : IBinaryOperationExpression) =
        let op = ast.OperationType
        let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled
        let left, state1 = reduceExpression state ast.LeftArgument
        let right, state2 = reduceExpression state1 ast.RightArgument
        let t = Types.GetTypeOfNode ast |> Types.FromPrimitiveDotNetType
        match t with
        | Bool -> __notImplemented__()
        | Numeric t -> Arithmetics.simplifyBinaryOperation op left right isChecked t state2
        | String -> (Strings.simplifyOperation op left right, state2)
        | _ -> __notImplemented__()


    and reduceUserDefinedBinaryOperationExpression state (ast : IUserDefinedBinaryOperationExpression) =
        __notImplemented__()


// ------------------------------- IAbstractTypeCastExpression and inheritors -------------------------------

    and reduceAbstractTypeCastExpression state (ast : IAbstractTypeCastExpression) =
        match ast with
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression state expression
        | :? IUserDefinedTypeCastExpression as expression -> reduceUserDefinedTypeCastExpression state expression
        | _ -> __notImplemented__()

    and reduceTypeCastExpression state (ast : ITypeCastExpression) =
        __notImplemented__()

    and reduceUserDefinedTypeCastExpression state (ast : IUserDefinedTypeCastExpression) =
        __notImplemented__()

// ------------------------------- IAbstractUnaryOperationExpression and inheritors -------------------------------

    and reduceAbstractUnaryOperationExpression state (ast : IAbstractUnaryOperationExpression) =
        match ast with
        | :? IUnaryOperationExpression as expression -> reduceUnaryOperationExpression state expression
        | :? IUserDefinedUnaryOperationExpression as expression -> reduceUserDefinedUnaryOperationExpression state expression
        | _ -> __notImplemented__()

    and reduceUnaryOperationExpression state (ast : IUnaryOperationExpression) =
        let op = ast.OperationType
        let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled
        let arg, newState = reduceExpression state ast.Argument
        let t = Types.GetTypeOfNode ast |> Types.FromPrimitiveDotNetType
        let result =
            match t with
            | Bool -> __notImplemented__()
            | Numeric t -> Arithmetics.simplifyUnaryOperation op arg isChecked t
            | String -> __notImplemented__()
            | _ -> __notImplemented__()
        (result, newState)

    and reduceUserDefinedUnaryOperationExpression state (ast : IUserDefinedUnaryOperationExpression) =
        __notImplemented__()

// ------------------------------- ICreationExpression and inheritors -------------------------------

    and reduceCreationExpression state (ast : ICreationExpression) =
        match ast with
        | :? IAnonymousMethodExpression as expression -> reduceAnonymousMethodExpression state expression
        | :? IAnonymousObjectCreationExpression as expression -> reduceAnonymousObjectCreationExpression state expression
        | :? IArrayCreationExpression as expression -> reduceArrayCreationExpression state expression
        | :? IDelegateCreationExpression as expression -> reduceDelegateCreationExpression state expression
        | :? ILambdaBlockExpression as expression -> reduceLambdaBlockExpression state expression
        | :? ILambdaExpression as expression -> reduceLambdaExpression state expression
        | :? IObjectCreationExpression as expression -> reduceObjectCreationExpression state expression
        | _ -> __notImplemented__()

    and reduceAnonymousMethodExpression state (ast : IAnonymousMethodExpression) =
        __notImplemented__()

    and reduceAnonymousObjectCreationExpression state (ast : IAnonymousObjectCreationExpression) =
        __notImplemented__()

    and reduceArrayCreationExpression state (ast : IArrayCreationExpression) =
        __notImplemented__()

    and reduceDelegateCreationExpression state (ast : IDelegateCreationExpression) =
        __notImplemented__()

    and reduceLambdaBlockExpression state (ast : ILambdaBlockExpression) =
        __notImplemented__()

    and reduceLambdaExpression state (ast : ILambdaExpression) =
        __notImplemented__()

    and reduceObjectCreationExpression state (ast : IObjectCreationExpression) =
        __notImplemented__()

// ------------------------------- IMemberAccessExpression and inheritors -------------------------------

    and reduceMemberAccessExpression state (ast : IMemberAccessExpression) =
        match ast with
        | :? IFieldAccessExpression as expression -> reduceFieldAccessExpression state expression
        | :? IMemberCallExpression as expression -> reduceMemberCallExpression state expression
        | _ -> __notImplemented__()

    and reduceFieldAccessExpression state (ast : IFieldAccessExpression) =
        __notImplemented__()

// ------------------------------- IMemberCallExpression and inheritors -------------------------------

    and reduceMemberCallExpression state (ast : IMemberCallExpression) =
        match ast with
        | :? IEventAccessExpression as expression -> reduceEventAccessExpression state expression
        | :? IIndexerCallExpression as expression -> reduceIndexerCallExpression state expression
        | :? IMethodCallExpression as expression -> reduceMethodCallExpression state expression
        | :? IPropertyAccessExpression as expression -> reducePropertyAccessExpression state expression
        | _ -> __notImplemented__()

    and reduceEventAccessExpression state (ast : IEventAccessExpression) =
        __notImplemented__()

    and reduceIndexerCallExpression state (ast : IIndexerCallExpression) =
        __notImplemented__()

    and reduceMethodCallExpression state (ast : IMethodCallExpression) =
        System.Console.WriteLine("reduceMethodCallExpression " + ast.ToString())
        match ast with
        | _ when ast.IsStatic ->
            let args, newState = Seq.mapFold reduceExpression state ast.Arguments
            System.Console.WriteLine("TARGET: " + (ast.Target = null).ToString())
            Seq.head args, newState
        | _ -> __notImplemented__()

    and reducePropertyAccessExpression state (ast : IPropertyAccessExpression) =
        __notImplemented__()
