namespace VSharp

open JetBrains.Decompiler.Ast
open System

module internal Interpreter =

    let rec dbg indent (ast : JetBrains.Decompiler.Ast.INode) =
        System.Console.Write(new System.String('\t', indent))
        System.Console.WriteLine(ast.GetType().ToString())
        ast.Children |> Seq.iter (dbg (indent + 1))


// ------------------------------- Decompilation -------------------------------

    let rec decompileAndReduceMethod state this parameters qualifiedTypeName metadataMethod assemblyPath k =
        let decompiledMethod = DecompilerServices.decompileMethod assemblyPath qualifiedTypeName metadataMethod
        match decompiledMethod with
        | None ->
            printfn "WARNING: Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name
            k (Error (new InvalidOperationException(sprintf "Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name)), state)
        | Some decompiledMethod ->
            // printfn "DECOMPILED:\n%s" (JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
            reduceDecompiledMethod state this parameters decompiledMethod k//(fun res -> printfn "For %s got %s" methodName (res.ToString()); k res)

// ------------------------------- INode and inheritors -------------------------------

    and reduceCatchClause state (ast : ICatchClause) k =
        __notImplemented__()

    and reduceFunctionSignature state (ast : IFunctionSignature) this values k =
        let rec map2 f xs1 xs2 =
            match xs1, xs2 with
            | SeqEmpty, [] -> []
            | SeqEmpty, x2::xs2' -> f None (Some x2) :: map2 f xs1 xs2'
            | SeqNode(x1, xs1'), [] -> f (Some x1) None :: map2 f xs1' xs2
            | SeqNode(x1, xs1'), x2::xs2' -> f (Some x1) (Some x2) :: map2 f xs1' xs2'
        let valueOrFreshConst (param : Option<IMethodParameter>) value =
            match param, value with
            | None, _ -> failwith "Internal error: parameters list is longer than expected!"
            | Some param, None ->
                if param.MetadataParameter.HasDefaultValue
                then (param.Name, Concrete(param.MetadataParameter.GetDefaultValue(), Types.FromMetadataType param.Type))
                else (param.Name, Term.Constant(param.Name, (Types.FromMetadataType param.Type)))
            | Some param, Some value -> (param.Name, value)
        let parameters = map2 valueOrFreshConst ast.Parameters values in
        let parametersAndThis =
            match this with
            | Some term -> ("this", term)::parameters
            | None -> parameters
        State.push state parametersAndThis |> k

    and reduceSwitchCase state (ast : ISwitchCase) k =
        __notImplemented__()

// ------------------------------- ILocalVariableDeclarationScopeOwner and inheritors -------------------------------

    and reduceFunction state this parameters (signature : IFunctionSignature) (body : IBlockStatement) k =
        reduceFunctionSignature state signature this parameters (fun state ->
        reduceBlockStatement state body (fun (result, state) -> (ControlFlow.consumeBreak result, State.pop state) |> k))

    and reduceDecompiledMethod state this parameters (ast : IDecompiledMethod) k =
        reduceFunction state (Some this) parameters ast.Signature ast.Body (fun (result, state) -> (ControlFlow.resultToTerm result, state) |> k)

// ------------------------------- IMemberInitializer and inheritors -------------------------------

    and reduceMemberInitializer this state (ast : IMemberInitializer) k =
        match ast with
        | :? IFieldMemberInitializer as initializer -> reduceFieldMemberInitializer this state initializer k
        | :? IPropertyMemberInitializer as initializer -> reducePropertyMemberInitializer this state initializer k
        | _ -> __notImplemented__()

    and reduceFieldMemberInitializer this state (ast : IFieldMemberInitializer) k =
        reduceExpression state ast.Value (fun (term, state) ->
        let fieldReference = Memory.referenceToField state ast.Field.Name this in
        Memory.mutate state fieldReference term |> snd |> k)

    and reducePropertyMemberInitializer this state (ast : IPropertyMemberInitializer) k =
        __notImplemented__()


// ------------------------------- IStatement and inheritors -------------------------------

    and reduceStatement state (ast : IStatement) k =
        match ast with
        | null -> k (NoResult, state)
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
        k (Break, state)

    and reduceContinueStatement state (ast : IContinueStatement) k =
        k (Continue, state)

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
        let lambdaBlock = Transformations.forStatementToRecursion ast in
        reduceBlockStatement state lambdaBlock k

    and reduceLoopStatement state (ast : ILoopStatement) k =
        __notImplemented__()

// ------------------------------- Rest Statements-------------------------------

    and reduceBlockStatement state (ast : IBlockStatement) k =
        let compose (result, state) statement k =
            if ControlFlow.calculationDone statement result then k (result, state)
            else
                let result =
                    if Transformations.isContinueConsumer statement
                    then ControlFlow.consumeContinue result
                    else result
                in
                reduceStatement state statement (fun (newRes, newState) -> k (ControlFlow.composeSequentially result newRes state newState))
        Cps.Seq.foldlk compose (NoResult, (State.push state [])) ast.Statements (fun (res, state) -> k (res, State.pop state))

    and reduceCommentStatement state (ast : ICommentStatement) k =
        __notImplemented__()

    and reduceEmptyStatement state (ast : IEmptyStatement) k =
        __notImplemented__()

    and reduceEndFinallyStatement state (ast : IEndFinallyStatement) k =
        __notImplemented__()

    and reduceExpressionStatement state (ast : IExpressionStatement) k =
        if Transformations.isInlinedCall ast
        then
            reduceInlinedDelegateCallStatement state (ast.Expression :?> IDelegateCallExpression) k
        else
            reduceExpression state ast.Expression (fun (term, newState) ->
                k (ControlFlow.throwOrIgnore term, newState))

    and reduceFixedStatement state (ast : IFixedStatement) k =
        __notImplemented__()

    and reduceIfStatement state (ast : IIfStatement) k =
        reduceExpression state ast.Condition (fun (condition, conditionState) ->
        match condition with
        | Terms.True ->  reduceStatement conditionState ast.Then k
        | Terms.False -> reduceStatement conditionState ast.Else k
        | e when Terms.Just Terms.IsError e -> k (Return e, conditionState)
        | _ ->
            reduceStatement conditionState ast.Then (fun (thenResult, thenState) ->
            reduceStatement conditionState ast.Else (fun (elseResult, elseState) ->
            let result = ControlFlow.mergeResults condition thenResult elseResult in
            let state = Merging.merge2States condition !!condition thenState elseState in
            k (result, state))))

    and reduceJumpStatement state (ast : IJumpStatement) k =
        __notImplemented__()

    and reduceLabelDeclarationStatement state (ast : ILabelDeclarationStatement) k =
        __notImplemented__()

    and reduceLocalVariableDeclarationStatement state (ast : ILocalVariableDeclarationStatement) k =
        reduceExpression state ast.Initializer (fun (initializer, state) ->
        let name = ast.VariableReference.Variable.Name in
        k (NoResult, Memory.allocateOnStack state name initializer))

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
        reduceExpression state ast.Result (fun (term, state) -> k (Return term, state))

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
        | null -> k (Nop, state)
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

    and reduceExpressionToRef state followHeapRefs (ast : IExpression) k =
        if ast = null then k (Concrete(null, VSharp.Object), state)
        else
            match ast with
            | :? ILocalVariableReferenceExpression as expression -> k (Memory.referenceToVariable state expression.Variable.Name followHeapRefs, state)
            | :? IParameterReferenceExpression as expression -> k (Memory.referenceToVariable state expression.Parameter.Name followHeapRefs, state)
            | :? IThisReferenceExpression as expression -> k (Memory.referenceToVariable state "this" followHeapRefs, state)
            | :? IFieldAccessExpression as expression ->
                reduceExpressionToRef state true expression.Target (fun (target, state) ->
                k (Memory.referenceToField state expression.FieldSpecification.Field.Name target, state))
            | :? IDerefExpression as expression -> reduceExpressionToRef state followHeapRefs expression.Argument k
            | _ -> reduceExpression state ast k

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
        reduceExpression state ast.Condition (fun (condition, conditionState) ->
        match condition with
        | Terms.True ->  reduceExpression conditionState ast.Then k
        | Terms.False -> reduceExpression conditionState ast.Else k
        | _ ->
            reduceExpression conditionState ast.Then (fun (thenResult, thenState) ->
            reduceExpression conditionState ast.Else (fun (elseResult, elseState) ->
            let result = Merging.merge2Terms condition !!condition thenResult elseResult in
            let state = Merging.merge2States condition !!condition thenState elseState in
            k (result, state))))

    and reduceDefaultValueExpression state (ast : IDefaultValueExpression) k =
        (Memory.defaultOf (Types.FromMetadataType ast.Type), state) |> k

    and reduceDelegateCallExpression state (ast : IDelegateCallExpression) k =
        reduceDelegateCall state ast (fun (result, state) -> (ControlFlow.resultToTerm result, state) |> k)

    and reduceInlinedDelegateCallStatement state (ast : IDelegateCallExpression) k =
        reduceDelegateCall state ast k

    and reduceDelegateCall state (ast : IDelegateCallExpression) k =
        Cps.Seq.mapFoldk reduceExpression state ast.Arguments (fun (args, state) ->
        reduceExpression state ast.Delegate (fun (deleg, state) ->
        let invoke deleg k =
            match deleg with
                | Terms.Lambda(signature, body) -> reduceFunction state None args signature body k
                | Concrete(obj, _) -> (Throw (Error(new InvalidCastException("Cannot apply non-function type"))), state) |> k
                | _ -> __notImplemented__()
        in
        match deleg with
        | Terms.GuardedValues(gs, vs) ->
            Cps.List.mapk invoke vs (fun results ->
            let terms, states = List.unzip results in
            let term = terms |> List.map ControlFlow.resultToTerm |> List.zip gs |> Merging.merge in
            let state = Merging.mergeStates gs states
            (Return term, state) |> k)
        | _ -> invoke deleg k))

    and reduceDerefExpression state (ast : IDerefExpression) k =
        reduceExpression state ast.Argument (fun (reference, state) ->
        k (Memory.deref state reference, state))

    and reduceExpressionList state (ast : IExpressionList) k =
        __notImplemented__()

    and reduceFieldReferenceExpression state (ast : IFieldReferenceExpression) k =
        __notImplemented__()

    and reduceFunctionPointerCallExpression state (ast : IFunctionPointerCallExpression) k =
        __notImplemented__()

    and reduceLiteralExpression state (ast : ILiteralExpression) k =
        let mType = Types.FromMetadataType ast.Value.Type in
        k (Concrete(ast.Value.Value, mType), state)

    and reduceLocalVariableReferenceExpression state (ast : ILocalVariableReferenceExpression) k =
        k (Memory.valueOf state ast.Variable.Name, state)

    and reduceMakeRefExpression state (ast : IMakeRefExpression) k =
        __notImplemented__()

    and reduceMemberInitializerList initializedObject state (ast : IMemberInitializerList) k =
        if ast = null then k state
        else Cps.Seq.foldlk (reduceMemberInitializer initializedObject) state ast.Initializers k

    and reduceMethodPointerExpression state (ast : IMethodPointerExpression) k =
        __notImplemented__()

    and reduceMethodReferenceExpression state (ast : IMethodReferenceExpression) k =
        __notImplemented__()

    and reduceNestedInitializer state (ast : INestedInitializer) k =
        __notImplemented__()

    and reduceParameterModifierExpression state (ast : IParameterModifierExpression) k =
        __notImplemented__()

    and reduceParameterReferenceExpression state (ast : IParameterReferenceExpression) k =
        k (Memory.valueOf state ast.Parameter.Name, state)

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
        k (Memory.valueOf state "this", state)

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
        match op with
        | OperationType.Assignment -> reduceAssignment state ast.LeftArgument ast.RightArgument k
        | _ when Operations.isOperationAssignment op ->
            Transformations.transformOperationAssignment ast
            assert(ast.OperationType = OperationType.Assignment)
            reduceAssignment state ast.LeftArgument ast.RightArgument k
        | _ ->
            let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled in
            reduceBinaryOperation state ast.OperationType ast.LeftArgument ast.RightArgument isChecked (Types.GetSystemTypeOfNode ast) k

    and reduceUserDefinedBinaryOperationExpression state (ast : IUserDefinedBinaryOperationExpression) k =
        __notImplemented__()

    and reduceAssignment state (left : IExpression) (right : IExpression) k =
        reduceExpression state right (fun (right, state) -> reduceAssignmentReduced state left right k)

    and reduceAssignmentReduced state (left : IExpression) right k =
        match left with
        | :? IParameterReferenceExpression
        | :? ILocalVariableReferenceExpression
        | :? IFieldAccessExpression as field ->
            reduceExpressionToRef state false left (fun (targetRef, state) ->
            Memory.mutate state targetRef right |> k)
        | :? IPropertyAccessExpression
        | :? IIndexerCallExpression
        | _ -> __notImplemented__()

    and reduceBinaryOperation state op leftArgument rightArgument isChecked t k =
        reduceExpression state leftArgument (fun (left, state) ->
        reduceExpression state rightArgument (fun (right, state) ->
        let t1 = Terms.TypeOf left in
        let t2 = Terms.TypeOf right in
        match op with
        | op when Propositional.isLogicalOperation op ->
            Propositional.simplifyBinaryConnective op left right (withSnd state >> k)
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation op left right isChecked t (withSnd state >> k)
        | op when Strings.isStringOperation op t1 t2 ->
            Strings.simplifyOperation op left right |> (withSnd state >> k)
        | _ -> __notImplemented__()))

// ------------------------------- IAbstractTypeCastExpression and inheritors -------------------------------

    and reduceAbstractTypeCastExpression state (ast : IAbstractTypeCastExpression) k =
        match ast with
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression state expression k
        | :? IUserDefinedTypeCastExpression as expression -> reduceUserDefinedTypeCastExpression state expression k
        | _ -> __notImplemented__()

    and reduceTypeCastExpression state (ast : ITypeCastExpression) k =
        let targetType = Types.FromMetadataType ast.TargetType in
        reduceExpression state ast.Argument (fun (argument, newState) ->
        typeCast newState argument targetType |> k)

    and typeCast state term targetType =
        // TODO: refs and structs should still be refs after cast!
        if Types.IsObject targetType then (term, state)
        else
            let cast src dst expr =
                if src = dst then expr
                else Expression(Cast(src, dst), [expr], dst)
            in
            let rec castSimple = function
                | Error _ -> term
                | Nop -> Error (new InvalidCastException(format1 "Internal error: casting void to {0}!" targetType))
                | Concrete(value, _) ->
                    if Terms.IsFunction term && Types.IsFunction targetType
                    then Concrete(value, targetType)
                    else Terms.MakeConcrete value (Types.ToDotNetType targetType)
                | Constant(name, t) -> cast t targetType term
                | Expression(operation, operands, t) -> cast t targetType term
                | _ -> __notImplemented__()
            in
            match term with
            | Union(gvs) ->
                let gs, vs = List.unzip gvs in
                let vs' = List.map castSimple vs in
                (Merging.merge (List.zip gs vs'), state)
            | _ -> (castSimple term, state)

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
        match op with
        | OperationType.PrefixIncrement
        | OperationType.PrefixDecrement -> reducePrefixIncrement state ast k
        | _ ->
            let isChecked = (ast.OverflowCheck = OverflowCheckType.Enabled) in
            reduceExpression state ast.Argument (fun (arg, newState) ->
            let dotNetType = Types.GetSystemTypeOfNode ast in
            match op with
            | OperationType.PostfixDecrement -> reducePostfixIncrement state ast.Argument arg (Terms.MakeConcrete -1 dotNetType) isChecked dotNetType k
            | OperationType.PostfixIncrement -> reducePostfixIncrement state ast.Argument arg (Terms.MakeConcrete 1 dotNetType) isChecked dotNetType k
            | _ ->
                let t = dotNetType |> Types.FromDotNetType in
                    match t with
                    | Bool -> Propositional.simplifyUnaryConnective op arg (withSnd newState >> k)
                    | Numeric t -> Arithmetics.simplifyUnaryOperation op arg isChecked t (withSnd newState >> k)
                    | String -> __notImplemented__()
                    | _ -> __notImplemented__())

    and reduceUserDefinedUnaryOperationExpression state (ast : IUserDefinedUnaryOperationExpression) k =
        __notImplemented__()

    and reducePrefixIncrement state ast k =
        let assignment = Transformations.transformPrefixCrement ast in
        reduceAssignment state assignment.LeftArgument assignment.RightArgument k

    and reducePostfixIncrement state leftAst left right isChecked t k =
        Arithmetics.simplifyBinaryOperation OperationType.Add left right isChecked t (fun sum ->
        reduceAssignmentReduced state leftAst sum (fun (_, state) ->
        (left, state) |> k))

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
        let typ = Types.FromFunctionSignature ast.Signature null in
        k (Concrete ((ast.Signature, ast.Body), typ), state)

    and reduceLambdaExpression state (ast : ILambdaExpression) k =
        __notImplemented__()

    and reduceObjectCreationExpression state (ast : IObjectCreationExpression) k =
        // TODO: support collection initializers
        let qualifiedTypeName = ast.ConstructedType.AssemblyQualifiedName in
        let assemblyPath = DecompilerServices.locationOfType qualifiedTypeName in
        let fields = DecompilerServices.getDefaultFieldValuesOf assemblyPath qualifiedTypeName in
        let names, typesAndInitializers = List.unzip fields in
        let types, initializers = List.unzip typesAndInitializers in
        Cps.List.mapFoldk reduceExpression state initializers (fun (initializers, state) ->
        let fields = List.map2 (fun t -> function | Nop -> Memory.defaultOf (Types.FromMetadataType t) | v -> v) types initializers
                        |> List.zip names |> Map.ofList in
        let t = Types.FromMetadataType ast.ConstructedType in
        let freshValue = Struct(fields, t) in
        let isReference = Types.IsReference t in
        let reference, state =
            if isReference
            then Memory.allocateInHeap state freshValue false
            else
                let tempVar = "constructed instance" in
                let state = State.push state [(tempVar, freshValue)] in
                (Memory.referenceToVariable state tempVar false, state)
        in
        let finish (_, state) =
            reduceMemberInitializerList reference state ast.ObjectInitializer (fun state ->
            if not isReference
            then k (Memory.deref state reference, State.pop state)
            else k (reference, state))
        in
        if ast.ConstructorSpecification = null
        then finish (Nop, state)
        else
            Cps.List.mapFoldk reduceExpression state (List.ofArray ast.Arguments) (fun (arguments, state) ->
            decompileAndReduceMethod state reference arguments qualifiedTypeName ast.ConstructorSpecification.Method assemblyPath finish))

// ------------------------------- IMemberAccessExpression and inheritors -------------------------------

    and reduceMemberAccessExpression state (ast : IMemberAccessExpression) k =
        match ast with
        | :? IFieldAccessExpression as expression -> reduceFieldAccessExpression state expression k
        | :? IMemberCallExpression as expression -> reduceMemberCallExpression state expression k
        | _ -> __notImplemented__()

    and reduceFieldAccessExpression state (ast : IFieldAccessExpression) k =
        reduceExpression state ast.Target (fun (target, state) ->
        k (Memory.fieldOf target ast.FieldSpecification.Field.Name, state))

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
        reduceExpressionToRef state true ast.Target (fun (target, state) ->
        Cps.Seq.mapFoldk reduceExpression state ast.Arguments (fun (args, state) ->
        let target = Memory.npeIfNull target in
        if Terms.Just Terms.IsError target then k (target, state)
        else
            let qualifiedTypeName = ast.MethodInstantiation.MethodSpecification.OwnerType.AssemblyQualifiedName in
            let metadataMethod = ast.MethodInstantiation.MethodSpecification.Method in
            let assemblyPath = ast.MethodInstantiation.MethodSpecification.OwnerType.Type.Assembly.Location in
            decompileAndReduceMethod state target args qualifiedTypeName metadataMethod assemblyPath k))

    and reducePropertyAccessExpression state (ast : IPropertyAccessExpression) k =
        __notImplemented__()
