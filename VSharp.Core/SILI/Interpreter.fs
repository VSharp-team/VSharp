namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open System
open VSharp.Core.Utils

// TODO: use CPS jedi...
module Interpreter =

    let assemblyLoader = new JetBrains.Metadata.Reader.API.MetadataLoader(JetBrains.Metadata.Access.MetadataProviderFactory.DefaultProvider)
    let private __notImplemented__() = raise (new System.NotImplementedException())

    let rec dbg indent (ast : JetBrains.Decompiler.Ast.INode) =
        System.Console.Write(new System.String('\t', indent))
        System.Console.WriteLine(ast.GetType().ToString())
        ast.Children |> Seq.iter (dbg (indent + 1))

// ------------------------------- Decompilation -------------------------------
    let rec decompileAndReduceMethod (meth : JetBrains.ReSharper.Psi.IMethod) k =
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
        reduceDecompiledMethod VSharp.Core.Symbolic.State.empty decompiledMethod k

// ------------------------------- INode and inheritors -------------------------------

    and reduceNode state (ast : INode) k =
        match ast with
        | :? IStatement as statement -> reduceStatement state statement k
        | :? IExpression as expression -> reduceExpression state expression k
        | :? ICatchClause as catch -> reduceCatchClause state catch k
        | :? IFunctionSignature as signature -> reduceFunctionSignature state signature (fun t -> k (Nop, t))
        | :? ILocalVariableDeclarationScopeOwner as owner -> reduceLocalVariableDeclarationScopeOwner state owner k
        | :? IMemberInitializer as initializer -> reduceMemberInitializer state initializer k
        | :? ISwitchCase as switchCase -> reduceSwitchCase state switchCase k
        | _ -> __notImplemented__()

    and reduceCatchClause state (ast : ICatchClause) k =
        __notImplemented__()

    and reduceFunctionSignature state (ast : IFunctionSignature) k =
        let foldParam state (param : IMethodParameter) =
            let freshConst = Terms.FreshConstant param.Name (System.Type.GetType(param.Type.FullName))
            State.addTerm state param.Name freshConst
        ast.Parameters |> Seq.fold foldParam state |> k


    and reduceSwitchCase state (ast : ISwitchCase) k =
        __notImplemented__()

// ------------------------------- ILocalVariableDeclarationScopeOwner and inheritors -------------------------------

    and reduceLocalVariableDeclarationScopeOwner state (ast : ILocalVariableDeclarationScopeOwner) k =
        match ast with
        | :? IAnonymousMethodExpression as expression -> reduceAnonymousMethodExpression state expression k
        | :? IDecompiledMethod as expression -> reduceDecompiledMethod state expression k
        | :? ILambdaBlockExpression as expression -> reduceLambdaBlockExpression state expression k
        | _ -> __notImplemented__()

    and reduceDecompiledMethod state (ast : IDecompiledMethod) k : unit = 
        reduceFunctionSignature state ast.Signature (fun newState -> reduceBlockStatement newState ast.Body k)

// ------------------------------- IMemberInitializer and inheritors -------------------------------

    and reduceMemberInitializer state (ast : IMemberInitializer) k =
        match ast with
        | :? IFieldMemberInitializer as initializer -> reduceFieldMemberInitializer state initializer k
        | :? IPropertyMemberInitializer as initializer -> reducePropertyMemberInitializer state initializer k
        | _ -> __notImplemented__()

    and reduceFieldMemberInitializer state (ast : IFieldMemberInitializer) k=
        __notImplemented__()

    and reducePropertyMemberInitializer state (ast : IPropertyMemberInitializer) k=
        __notImplemented__()


// ------------------------------- IStatement and inheritors -------------------------------

    and reduceStatement state (ast : IStatement) k =
        match ast with
        | :? IAbstractGotoStatement as abstractGoto -> reduceAbstractGotoStatement state abstractGoto k
        | :? IAbstractLoopStatement as abstractLoop -> reduceAbstractLoopStatement state abstractLoop k
        | :? IBlockStatement as blockStatement -> reduceBlockStatement state blockStatement k
        | :? ICommentStatement as commentStatement -> reduceCommentStatement state commentStatement k
        | :? IEmptyStatement as emptyStatement -> reduceEmptyStatement state emptyStatement k
        | :? IEndFinallyStatement as endFinally -> reduceEndFinallyStatement state endFinally k
        | :? IExpressionStatement as expressionStatement -> reduceExpressionStatement state expressionStatement k
        | :? IFixedStatement as fixedStatement -> reduceFixedStatement state fixedStatement k
        | :? IIfStatement as ifStatement -> reduceIfStatement state ifStatement k
        | :? IJumpStatement as jump -> reduceJumpStatement state jump k
        | :? ILabelDeclarationStatement as labelDeclaration -> reduceLabelDeclarationStatement state labelDeclaration k
        | :? ILocalVariableDeclarationStatement as localVariableDeclaration -> reduceLocalVariableDeclarationStatement state localVariableDeclaration k
        | :? ILockStatement as lockStatement -> reduceLockStatement state lockStatement k
        | :? IMemoryCopyStatement as memoryCopy -> reduceMemoryCopyStatement state memoryCopy k
        | :? IMemoryInitializeStatement as memoryInitialize -> reduceMemoryInitializeStatement state memoryInitialize k
        | :? IPinStatement as pinStatement -> reducePinStatement state pinStatement k
        | :? IRethrowStatement as rethrowStatement -> reduceRethrowStatement state rethrowStatement k
        | :? IReturnStatement as returnStatement -> reduceReturnStatement state returnStatement k
        | :? ISuccessfulFilteringStatement as filtering -> reduceSuccessfulFilteringStatement state filtering k
        | :? ISwitchStatement as switchStatement -> reduceSwitchStatement state switchStatement k
        | :? IThrowStatement as throwStatement -> reduceThrowStatement state throwStatement k
        | :? ITryStatement as tryStatement -> reduceTryStatement state tryStatement k
        | :? IUnpinStatement as unpinStatement -> reduceUnpinStatement state unpinStatement k
        | :? IUsingStatement as usingStatement -> reduceUsingStatement state usingStatement k
        | :? IYieldReturnStatement as yieldReturn -> reduceYieldReturnStatement state yieldReturn k
        | _ -> __notImplemented__()


// ------------------------------- IAbstractGotoStatement and inheritors -------------------------------

    and reduceAbstractGotoStatement state (ast : IAbstractGotoStatement) k =
        match ast with
        | :? IBreakStatement as breakStatement -> reduceBreakStatement state breakStatement k
        | :? IContinueStatement as continueStatement -> reduceContinueStatement state continueStatement k
        | :? IGotoCaseStatement as gotoCaseStatement -> reduceGotoCaseStatement state gotoCaseStatement k
        | :? IGotoDefaultStatement as gotoDefaultStatement -> reduceGotoDefaultStatement state gotoDefaultStatement k
        | :? IGotoStatement as gotoStatement -> reduceGotoStatement state gotoStatement k
        | :? IYieldBreakStatement as yieldBreakStatement -> reduceYieldBreakStatement state yieldBreakStatement k
        | _ -> __notImplemented__()

    and reduceBreakStatement state (ast : IBreakStatement) k =
        __notImplemented__()

    and reduceContinueStatement state (ast : IContinueStatement) k =
        __notImplemented__()

    and reduceGotoCaseStatement state (ast : IGotoCaseStatement) k =
        __notImplemented__()

    and reduceGotoDefaultStatement state (ast : IGotoDefaultStatement) k =
        __notImplemented__()

    and reduceGotoStatement state (ast : IGotoStatement) k =
        __notImplemented__()

    and reduceYieldBreakStatement state (ast : IYieldBreakStatement) k =
        __notImplemented__()

// ------------------------------- IAbstractLoopStatement and inheritors -------------------------------

    and reduceAbstractLoopStatement state (ast : IAbstractLoopStatement) k =
        match ast with
        | :? IForEachStatement as forEach -> reduceForEachStatement state forEach k
        | :? IForStatement as forStatement -> reduceForStatement state forStatement k
        | :? ILoopStatement as loop -> reduceLoopStatement state loop k
        | _ -> __notImplemented__()

    and reduceForEachStatement state (ast : IForEachStatement) k =
        __notImplemented__()

    and reduceForStatement state (ast : IForStatement) k =
        __notImplemented__()

    and reduceLoopStatement state (ast : ILoopStatement) k =
        __notImplemented__()

// ------------------------------- Rest Statements-------------------------------

    and reduceBlockStatement state (ast : IBlockStatement) k =
        let rec handleStatement (curTerm, curState) xs k =
            match xs with
            | SeqEmpty  -> k (curTerm, curState)
            | SeqNode(h, tail) -> 
                if Terms.IsVoid curTerm 
                then reduceStatement curState h (fun res -> handleStatement res tail k)
                else handleStatement (curTerm, curState) tail k

        handleStatement (Nop, state) ast.Statements k
        


    and reduceCommentStatement state (ast : ICommentStatement) k =
        __notImplemented__()

    and reduceEmptyStatement state (ast : IEmptyStatement) k =
        __notImplemented__()

    and reduceEndFinallyStatement state (ast : IEndFinallyStatement) k =
        __notImplemented__()

    and reduceExpressionStatement state (ast : IExpressionStatement) k =
        reduceExpression state ast.Expression (fun (term, newState) -> 
            k ((if Terms.IsError term then term else Nop), newState))
        

    and reduceFixedStatement state (ast : IFixedStatement) k =
        __notImplemented__()

    and reduceIfStatement state (ast : IIfStatement) k =
        reduceExpression state ast.Condition (fun (condition, conditionState) ->  
        match condition with
        | Terms.True ->  reduceStatement conditionState ast.Then k
        | Terms.False -> reduceStatement conditionState ast.Else k
        | _ ->
            reduceStatement conditionState ast.Then (fun (thenVal, thenState) -> 
            reduceStatement conditionState ast.Else (fun (elseVal, elseState) -> 
            Merging.merge condition thenVal elseVal conditionState thenState elseState 
            |> k)))

    and reduceJumpStatement state (ast : IJumpStatement) k =
        __notImplemented__()

    and reduceLabelDeclarationStatement state (ast : ILabelDeclarationStatement) k =
        __notImplemented__()

    and reduceLocalVariableDeclarationStatement state (ast : ILocalVariableDeclarationStatement) k =
        __notImplemented__()

    and reduceLockStatement state (ast : ILockStatement) k =
        __notImplemented__()

    and reduceMemoryCopyStatement state (ast : IMemoryCopyStatement) k =
        __notImplemented__()

    and reduceMemoryInitializeStatement state (ast : IMemoryInitializeStatement) k =
        __notImplemented__()

    and reducePinStatement state (ast : IPinStatement) k =
        __notImplemented__()

    and reduceRethrowStatement state (ast : IRethrowStatement) k =
        __notImplemented__()

    and reduceReturnStatement state (ast : IReturnStatement) k =
        reduceExpression state ast.Result k

    and reduceSuccessfulFilteringStatement state (ast : ISuccessfulFilteringStatement) k =
        __notImplemented__()

    and reduceSwitchStatement state (ast : ISwitchStatement) k =
        __notImplemented__()

    and reduceThrowStatement state (ast : IThrowStatement) k =
        __notImplemented__()

    and reduceTryStatement state (ast : ITryStatement) k =
        __notImplemented__()

    and reduceUnpinStatement state (ast : IUnpinStatement) k =
        __notImplemented__()

    and reduceUsingStatement state (ast : IUsingStatement) k =
        __notImplemented__()

    and reduceYieldReturnStatement state (ast : IYieldReturnStatement) k =
        __notImplemented__()


// ------------------------------- IExpression and inheritors -------------------------------

    and reduceExpression state (ast : IExpression) k =
        match ast with
        | :? IAbstractBinaryOperationExpression as expression -> reduceAbstractBinaryOperation state expression k
        | :? IAbstractTypeCastExpression as expression -> reduceAbstractTypeCastExpression state expression k
        | :? IAbstractUnaryOperationExpression as expression -> reduceAbstractUnaryOperationExpression state expression k
        | :? IAddressOfExpression as expression -> reduceAddressOfExpression state expression k
        | :? IArgListCreationExpression as expression -> reduceArgListCreationExpression state expression k
        | :? IArgListReferenceExpression as expression -> reduceArgListReferenceExpression state expression k
        | :? IArrayElementAccessExpression as expression -> reduceArrayElementAccessExpression state expression k
        | :? IAwaitExpression as expression -> reduceAwaitExpression state expression k
        | :? IBaseReferenceExpression as expression -> reduceBaseReferenceExpression state expression k
        | :? IBoxExpression as expression -> reduceBoxExpression state expression k
        | :? ICheckCastExpression as expression -> reduceCheckCastExpression state expression k
        | :? ICheckFiniteExpression as expression -> reduceCheckFiniteExpression state expression k
        | :? IConditionalExpression as expression -> reduceConditionalExpression state expression k
        | :? ICreationExpression as expression -> reduceCreationExpression state expression k
        | :? IDefaultValueExpression as expression -> reduceDefaultValueExpression state expression k
        | :? IDelegateCallExpression as expression -> reduceDelegateCallExpression state expression k
        | :? IDerefExpression as expression -> reduceDerefExpression state expression k
        | :? IExpressionList as expression -> reduceExpressionList state expression k
        | :? IFieldReferenceExpression as expression -> reduceFieldReferenceExpression state expression k
        | :? IFunctionPointerCallExpression as expression -> reduceFunctionPointerCallExpression state expression k
        | :? ILiteralExpression as expression -> reduceLiteralExpression state expression k
        | :? ILocalVariableReferenceExpression as expression -> reduceLocalVariableReferenceExpression state expression k
        | :? IMakeRefExpression as expression -> reduceMakeRefExpression state expression k
        | :? IMemberAccessExpression as expression -> reduceMemberAccessExpression state expression k
        | :? IMemberInitializerList as expression -> reduceMemberInitializerList state expression k
        | :? IMethodPointerExpression as expression -> reduceMethodPointerExpression state expression k
        | :? IMethodReferenceExpression as expression -> reduceMethodReferenceExpression state expression k
        | :? INestedInitializer as expression -> reduceNestedInitializer state expression k
        | :? IParameterModifierExpression as expression -> reduceParameterModifierExpression state expression k
        | :? IParameterReferenceExpression as expression -> reduceParameterReferenceExpression state expression k
        | :? IPointerElementAccessExpression as expression -> reducePointerElementAccessExpression state expression k
        | :? IPointerIndirectionExpression as expression -> reducePointerIndirectionExpression state expression k
        | :? IRefExpression as expression -> reduceRefExpression state expression k
        | :? IRefTypeExpression as expression -> reduceRefTypeExpression state expression k
        | :? IRefTypeTokenExpression as expression -> reduceRefTypeTokenExpression state expression k
        | :? IRefValueExpression as expression -> reduceRefValueExpression state expression k
        | :? ISizeOfExpression as expression -> reduceSizeOfExpression state expression k
        | :? IStackAllocExpression as expression -> reduceStackAllocExpression state expression k
        | :? IThisReferenceExpression as expression -> reduceThisReferenceExpression state expression k
        | :? ITryCastExpression as expression -> reduceTryCastExpression state expression k
        | :? ITypeOfExpression as expression -> reduceTypeOfExpression state expression k
        | :? ITypeReferenceExpression as expression -> reduceTypeReferenceExpression state expression k
        | :? IUnboxExpression as expression -> reduceUnboxExpression state expression k
        | :? IUntypedStackAllocExpression as expression -> reduceUntypedStackAllocExpression state expression k
        | :? IVirtualMethodPointerExpression as expression -> reduceVirtualMethodPointerExpression state expression k
        | _ -> __notImplemented__()

    and reduceAddressOfExpression state (ast : IAddressOfExpression) k =
        __notImplemented__()

    and reduceArgListCreationExpression state (ast : IArgListCreationExpression) k =
        __notImplemented__()

    and reduceArgListReferenceExpression state (ast : IArgListReferenceExpression) k =
        __notImplemented__()

    and reduceArrayElementAccessExpression state (ast : IArrayElementAccessExpression) k =
        __notImplemented__()

    and reduceAwaitExpression state (ast : IAwaitExpression) k =
        __notImplemented__()

    and reduceBaseReferenceExpression state (ast : IBaseReferenceExpression) k =
        __notImplemented__()

    and reduceBoxExpression state (ast : IBoxExpression) k =
        __notImplemented__()

    and reduceCheckCastExpression state (ast : ICheckCastExpression) k =
        __notImplemented__()

    and reduceCheckFiniteExpression state (ast : ICheckFiniteExpression) k =
        __notImplemented__()

    and reduceConditionalExpression state (ast : IConditionalExpression) k =
        __notImplemented__()

    and reduceDefaultValueExpression state (ast : IDefaultValueExpression) k =
        __notImplemented__()

    and reduceDelegateCallExpression state (ast : IDelegateCallExpression) k =
        __notImplemented__()

    and reduceDerefExpression state (ast : IDerefExpression) k =
        __notImplemented__()

    and reduceExpressionList state (ast : IExpressionList) k =
        __notImplemented__()

    and reduceFieldReferenceExpression state (ast : IFieldReferenceExpression) k =
        __notImplemented__()

    and reduceFunctionPointerCallExpression state (ast : IFunctionPointerCallExpression) k =
        __notImplemented__()

    and reduceLiteralExpression state (ast : ILiteralExpression) k =
        let mType = System.Type.GetType(ast.Value.Type.AssemblyQualifiedName) in
        k (Terms.MakeConcrete ast.Value.Value mType, state)


    and reduceLocalVariableReferenceExpression state (ast : ILocalVariableReferenceExpression) k =
        __notImplemented__()

    and reduceMakeRefExpression state (ast : IMakeRefExpression) k =
        __notImplemented__()

    and reduceMemberInitializerList state (ast : IMemberInitializerList ) k =
        __notImplemented__()

    and reduceMethodPointerExpression state (ast : IMethodPointerExpression) k =
        __notImplemented__()

    and reduceMethodReferenceExpression state (ast : IMethodReferenceExpression) k =
        __notImplemented__()

    and reduceNestedInitializer state (ast : INestedInitializer) k =
        __notImplemented__()

    and reduceParameterModifierExpression state (ast : IParameterModifierExpression) k =
        __notImplemented__()

    and reduceParameterReferenceExpression state (ast : IParameterReferenceExpression) k =
        let term = State.eval state ast.Parameter.Name in k (term, state)

    and reducePointerElementAccessExpression state (ast : IPointerElementAccessExpression) k =
        __notImplemented__()

    and reducePointerIndirectionExpression state (ast : IPointerIndirectionExpression) k =
        __notImplemented__()

    and reduceRefExpression state (ast : IRefExpression) k =
        __notImplemented__()

    and reduceRefTypeExpression state (ast : IRefTypeExpression) k =
        __notImplemented__()

    and reduceRefTypeTokenExpression state (ast : IRefTypeTokenExpression) k =
        __notImplemented__()

    and reduceRefValueExpression state (ast : IRefValueExpression) k =
        __notImplemented__()

    and reduceSizeOfExpression state (ast : ISizeOfExpression) k =
        __notImplemented__()

    and reduceStackAllocExpression state (ast : IStackAllocExpression) k =
        __notImplemented__()

    and reduceThisReferenceExpression state (ast : IThisReferenceExpression) k =
        __notImplemented__()

    and reduceTryCastExpression state (ast : ITryCastExpression) k =
        __notImplemented__()

    and reduceTypeOfExpression state (ast : ITypeOfExpression) k =
        __notImplemented__()

    and reduceTypeReferenceExpression state (ast : ITypeReferenceExpression) k =
        __notImplemented__()

    and reduceUnboxExpression state (ast : IUnboxExpression) k =
        __notImplemented__()

    and reduceUntypedStackAllocExpression state (ast : IUntypedStackAllocExpression) k =
        __notImplemented__()

    and reduceVirtualMethodPointerExpression state (ast : IVirtualMethodPointerExpression) k =
        __notImplemented__()

// ------------------------------- IAbstractBinaryOperationExpression and inheritors -------------------------------

    and reduceAbstractBinaryOperation state (ast : IAbstractBinaryOperationExpression) k =
        match ast with
        | :? IBinaryOperationExpression as binOp -> reduceBinaryOperationExpression state binOp k
        | :? IUserDefinedBinaryOperationExpression as userBinOp -> reduceUserDefinedBinaryOperationExpression state userBinOp k
        | _ -> __notImplemented__()


    and reduceBinaryOperationExpression state (ast : IBinaryOperationExpression) k =
        let op = ast.OperationType in
        let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled in
        reduceExpression state ast.LeftArgument (fun (left, state1) -> 
        reduceExpression state1 ast.RightArgument (fun (right, state2) ->
        let t = Types.GetTypeOfNode ast |> Types.FromPrimitiveDotNetType in
        match t with
        | Bool -> __notImplemented__()
        | Numeric t -> Arithmetics.simplifyBinaryOperation op left right isChecked t state2 k
        | String -> Strings.simplifyOperation op left right |> fun t -> k (t, state2)
        | _ -> __notImplemented__()))


    and reduceUserDefinedBinaryOperationExpression state (ast : IUserDefinedBinaryOperationExpression) k =
        __notImplemented__()


// ------------------------------- IAbstractTypeCastExpression and inheritors -------------------------------

    and reduceAbstractTypeCastExpression state (ast : IAbstractTypeCastExpression) k =
        match ast with
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression state expression k
        | :? IUserDefinedTypeCastExpression as expression -> reduceUserDefinedTypeCastExpression state expression k
        | _ -> __notImplemented__()

    and reduceTypeCastExpression state (ast : ITypeCastExpression) k =
        __notImplemented__()

    and reduceUserDefinedTypeCastExpression state (ast : IUserDefinedTypeCastExpression) k =
        __notImplemented__()

// ------------------------------- IAbstractUnaryOperationExpression and inheritors -------------------------------

    and reduceAbstractUnaryOperationExpression state (ast : IAbstractUnaryOperationExpression) k =
        match ast with
        | :? IUnaryOperationExpression as expression -> reduceUnaryOperationExpression state expression k
        | :? IUserDefinedUnaryOperationExpression as expression -> reduceUserDefinedUnaryOperationExpression state expression k
        | _ -> __notImplemented__()

    and reduceUnaryOperationExpression state (ast : IUnaryOperationExpression) k =
        let op = ast.OperationType in
        let isChecked = (ast.OverflowCheck = OverflowCheckType.Enabled) in
            reduceExpression state ast.Argument (fun (arg, newState) ->
            let t = Types.GetTypeOfNode ast |> Types.FromPrimitiveDotNetType in
                match t with
                | Bool -> __notImplemented__()
                | Numeric t -> Arithmetics.simplifyUnaryOperation op arg isChecked t (k << withSnd newState)
                | String -> __notImplemented__()
                | _ -> __notImplemented__())

    and reduceUserDefinedUnaryOperationExpression state (ast : IUserDefinedUnaryOperationExpression) k =
        __notImplemented__()

// ------------------------------- ICreationExpression and inheritors -------------------------------

    and reduceCreationExpression state (ast : ICreationExpression) k =
        match ast with
        | :? IAnonymousMethodExpression as expression -> reduceAnonymousMethodExpression state expression k
        | :? IAnonymousObjectCreationExpression as expression -> reduceAnonymousObjectCreationExpression state expression k
        | :? IArrayCreationExpression as expression -> reduceArrayCreationExpression state expression k
        | :? IDelegateCreationExpression as expression -> reduceDelegateCreationExpression state expression k
        | :? ILambdaBlockExpression as expression -> reduceLambdaBlockExpression state expression k
        | :? ILambdaExpression as expression -> reduceLambdaExpression state expression k
        | :? IObjectCreationExpression as expression -> reduceObjectCreationExpression state expression k
        | _ -> __notImplemented__()

    and reduceAnonymousMethodExpression state (ast : IAnonymousMethodExpression) k =
        __notImplemented__()

    and reduceAnonymousObjectCreationExpression state (ast : IAnonymousObjectCreationExpression) k =
        __notImplemented__()

    and reduceArrayCreationExpression state (ast : IArrayCreationExpression) k =
        __notImplemented__()

    and reduceDelegateCreationExpression state (ast : IDelegateCreationExpression) k =
        __notImplemented__()

    and reduceLambdaBlockExpression state (ast : ILambdaBlockExpression) k =
        __notImplemented__()

    and reduceLambdaExpression state (ast : ILambdaExpression) k =
        __notImplemented__()

    and reduceObjectCreationExpression state (ast : IObjectCreationExpression) k =
        __notImplemented__()

// ------------------------------- IMemberAccessExpression and inheritors -------------------------------

    and reduceMemberAccessExpression state (ast : IMemberAccessExpression) k =
        match ast with
        | :? IFieldAccessExpression as expression -> reduceFieldAccessExpression state expression k
        | :? IMemberCallExpression as expression -> reduceMemberCallExpression state expression k
        | _ -> __notImplemented__()

    and reduceFieldAccessExpression state (ast : IFieldAccessExpression) k =
        __notImplemented__()

// ------------------------------- IMemberCallExpression and inheritors -------------------------------

    and reduceMemberCallExpression state (ast : IMemberCallExpression) k =
        match ast with
        | :? IEventAccessExpression as expression -> reduceEventAccessExpression state expression k
        | :? IIndexerCallExpression as expression -> reduceIndexerCallExpression state expression k
        | :? IMethodCallExpression as expression -> reduceMethodCallExpression state expression k
        | :? IPropertyAccessExpression as expression -> reducePropertyAccessExpression state expression k
        | _ -> __notImplemented__()

    and reduceEventAccessExpression state (ast : IEventAccessExpression) k =
        __notImplemented__()

    and reduceIndexerCallExpression state (ast : IIndexerCallExpression) k =
        __notImplemented__()


    and reduceMethodCallExpression state (ast : IMethodCallExpression) k =
        System.Console.WriteLine("reduceMethodCallExpression " + ast.ToString())
        match ast with
        | _ when ast.IsStatic ->
            Cps.Seq.mapFoldk reduceExpression state ast.Arguments (fun (args, newState) ->
                System.Console.WriteLine("TARGET: " + (ast.Target = null).ToString())
                k (Seq.head args, newState))
        | _ -> __notImplemented__()


    and reducePropertyAccessExpression state (ast : IPropertyAccessExpression) k =
        __notImplemented__()
