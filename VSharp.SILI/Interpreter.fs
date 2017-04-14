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
        let decompiledMethod = DecompilerServices.decompileMethod assemblyPath qualifiedTypeName metadataMethod in
        match decompiledMethod with
        | None when DecompilerServices.isConstructor metadataMethod ->
            // Got default constructor ignored by decompiler
            k (NoResult, state)
        | None ->
            failwith (sprintf "WARNING: Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name)
            // k (Error (new InvalidOperationException(sprintf "Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name)), state)
        | Some decompiledMethod ->
            printfn "DECOMPILED %s:\n%s" qualifiedTypeName (JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
            reduceDecompiledMethod state this parameters decompiledMethod k//(fun res -> printfn "For %s got %s" methodName (res.ToString()); k res)

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

// ------------------------------- ILocalVariableDeclarationScopeOwner and inheritors -------------------------------

    and reduceFunction state this parameters (signature : IFunctionSignature) (body : IBlockStatement) k =
        reduceFunctionSignature state signature this parameters (fun state ->
        reduceBlockStatement state body (fun (result, state) -> (ControlFlow.consumeBreak result, State.pop state) |> k))

    and reduceDecompiledMethod state this parameters (ast : IDecompiledMethod) k =
        reduceFunction state (Some this) parameters ast.Signature ast.Body k

// ------------------------------- IMemberInitializer and inheritors -------------------------------

    and reduceMemberInitializer this state (ast : IMemberInitializer) k =
        match ast with
        | :? IFieldMemberInitializer as initializer -> reduceFieldMemberInitializer this state initializer k
        | :? IPropertyMemberInitializer as initializer -> reducePropertyMemberInitializer this state initializer k
        | _ -> __notImplemented__()

    and reduceFieldMemberInitializer this state (ast : IFieldMemberInitializer) k =
        reduceExpression state ast.Value (fun (term, state) ->
        let fieldReference = Memory.referenceToField state (DecompilerServices.idOfMetadataField ast.Field) this in
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
        k (NoResult, state)

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

    and reduceConditionalExecution state conditionInvocation thenBranch elseBranch k =
        conditionInvocation state (fun (condition, conditionState) ->
        match condition with
        | Terms.True ->  thenBranch conditionState k
        | Terms.False -> elseBranch conditionState k
        | e when Terms.Just Terms.IsError e -> k (Return e, conditionState)
        | _ ->
            thenBranch conditionState (fun (thenResult, thenState) ->
            elseBranch conditionState (fun (elseResult, elseState) ->
            let result = ControlFlow.mergeResults condition thenResult elseResult in
            let state = Merging.merge2States condition !!condition thenState elseState in
            k (result, state))))

    and npeOrInvokeStatement state isStatic reference statement k =
        if isStatic then statement state k
        else
            reduceConditionalExecution state
                (fun state k -> k (Memory.isNull reference, state))
                (fun state k -> k (ControlFlow.npe(), state))
                statement
                k

    and npeOrInvokeExpression state isStatic reference expression k =
        npeOrInvokeStatement state isStatic reference expression
            (fun (result, state) -> k (ControlFlow.resultToTerm result, state))

    and reduceIfStatement state (ast : IIfStatement) k =
        reduceConditionalExecution state
            (fun state k -> reduceExpression state ast.Condition k)
            (fun state k -> reduceStatement state ast.Then k)
            (fun state k -> reduceStatement state ast.Else k)
            k

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
        let rec findException (node : INode) =
            if node = null then failwith "Internal error: exception register not found for rethowing!"
            match DecompilerServices.getPropertyOfNode node "Thrown" null with
            | null -> findException node.Parent
            | exn -> exn :?> Term
        in
        let exn = findException ast in
        k (Throw exn, state)

    and reduceReturnStatement state (ast : IReturnStatement) k =
        reduceExpression state ast.Result (fun (term, state) -> k (ControlFlow.throwOrReturn term, state))

    and reduceSuccessfulFilteringStatement state (ast : ISuccessfulFilteringStatement) k =
        __notImplemented__()

    and reduceSwitchStatement state (ast : ISwitchStatement) k =
        __notImplemented__()

    and reduceSwitchCase state (ast : ISwitchCase) k =
        __notImplemented__()

    and reduceThrowStatement state (ast : IThrowStatement) k =
        reduceExpression state ast.Argument (fun (arg, state) ->
        k (Throw arg, state))

    and reduceTryStatement state (ast : ITryStatement) k =
        reduceBlockStatement state ast.Body (fun (result, state) ->
        reduceCatchBlock state result ast.CatchClauses (fun (result, state) ->
        reduceFinally state result ast.Finally (fun (result, state) ->
        reduceFault state result ast.Fault k)))

    and reduceCatchBlock state statementResult (clauses : ICatchClause[]) k =
        if Array.isEmpty clauses then k (statementResult, state)
        else
            let thrown, normal = ControlFlow.pickOutExceptions statementResult in
            match thrown with
            | None -> k (statementResult, state)
            | Some(guard, exn) ->
                reduceConditionalExecution state
                    (fun state k -> k (guard, state))
                    (fun state k -> reduceCatchClauses exn state (Seq.ofArray clauses) k)
                    (fun state k -> k (Guarded ((guard, NoResult)::normal), state))
                    k

    and reduceCatchClauses exn state clauses k =
        match clauses with
        | SeqEmpty -> k (Throw exn, state)
        | SeqNode(clause, rest) ->
            reduceConditionalExecution state
                (fun state k -> reduceCatchCondition exn state clause k)
                (fun state k -> reduceBlockStatement state clause.Body (fun (result, state) -> k (result, State.pop state)))
                (fun state k -> reduceCatchClauses exn (State.pop state) rest k)
                k

    and reduceCatchCondition exn state (ast : ICatchClause) k =
        if ast.VariableReference = null then k (Terms.MakeTrue, State.push state []) // just catch {...} case
        else
            DecompilerServices.setPropertyOfNode ast "Thrown" exn
            // catch (...) {...} case
            let typeMatches = is ast.VariableReference.Variable.Type exn in
            let state = State.push state [(ast.VariableReference.Variable.Name, exn)] in
            if ast.Filter = null then k (typeMatches, state)
            else
                let filteringExpression = Transformations.extractExceptionFilter ast.Filter in
                reduceConditionalExecution state
                    (fun state k -> k (typeMatches, state))
                    (fun state k -> reduceExpression state filteringExpression
                                        (fun (filterResult, state) ->
                                            k (ControlFlow.consumeErrorOrReturn
                                                (fun _ -> Return Terms.MakeFalse) filterResult, state)))
                    (fun state k -> k (Return typeMatches, state))
                    (fun (result, state) -> k (ControlFlow.resultToTerm result, state))

    and reduceFinally state statementResult (ast : IBlockStatement) k =
        if ast = null then k (statementResult, state)
        else reduceBlockStatement state ast (fun (_, state) -> k (statementResult, state))

    and reduceFault state statementResult (ast : IBlockStatement) k =
        if ast = null then k (statementResult, state)
        else
            let thrown, normal = ControlFlow.pickOutExceptions statementResult in
            match thrown with
            | None -> k (statementResult, state)
            | Some(guard, exn) ->
                reduceConditionalExecution state
                    (fun state k -> k (guard, state))
                    (fun state k -> reduceBlockStatement state ast (fun (_, state) -> k (NoResult, state)))
                    (fun state k -> k (NoResult, state))
                    (fun (_, state) -> k (statementResult, state))

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
        match ast with
        | null -> k (Concrete(null, VSharp.Object), state)
        | :? ILocalVariableReferenceExpression as expression -> k (Memory.referenceToVariable state expression.Variable.Name followHeapRefs, state)
        | :? IParameterReferenceExpression as expression -> k (Memory.referenceToVariable state expression.Parameter.Name followHeapRefs, state)
        | :? IThisReferenceExpression as expression -> k (Memory.referenceToVariable state "this" followHeapRefs, state)
        | :? IFieldAccessExpression as expression ->
            reduceExpressionToRef state true expression.Target (fun (target, state) ->
            referenceToField state target expression.FieldSpecification.Field k)
        | :? IDerefExpression as expression -> reduceExpressionToRef state followHeapRefs expression.Argument k
        | _ -> reduceExpression state ast k

    and referenceToField state target (field : JetBrains.Metadata.Reader.API.IMetadataField) k =
        let id = DecompilerServices.idOfMetadataField field in
        if field.IsStatic then
            k (Memory.referenceToStaticField state id field.DeclaringType.AssemblyQualifiedName, state)
        else
            k (Memory.referenceToField state id target, state)

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
                | Lambdas.Lambda(lambda) -> lambda state args k
                | Concrete(obj, _) ->
                    let exn = new InvalidCastException("Cannot apply non-function type") in
                    let exnTerm = Terms.MakeConcrete exn (exn.GetType()) in
                    (Throw exnTerm, state) |> k
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
        reduceExpressionToRef state false ast.Argument k

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
        reduceExpression state ast.Argument (fun (term, state) ->
        k (tryCast ast.Type term, state))

    and reduceTypeOfExpression state (ast : ITypeOfExpression) k =
        let instance = Types.MetadataToDotNetType ast.Type in
        k (Terms.MakeConcrete instance typedefof<Type>, state)

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
        | _ when Operations.isOperationAssignment op -> reduceOperationAssignment state ast k
        | _ when Propositional.isConditionalOperation op->
            reduceConditionalOperation state ast.OperationType ast.LeftArgument ast.RightArgument k
        | _ ->
            let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled in
            reduceBinaryOperation state ast.OperationType ast.LeftArgument ast.RightArgument isChecked (Types.GetSystemTypeOfNode ast) k

    and reduceUserDefinedBinaryOperationExpression state (ast : IUserDefinedBinaryOperationExpression) k =
        __notImplemented__()

    and reduceAssignment state (left : IExpression) (right : IExpression) k =
        let targetReducer =
            match left with
            | :? IParameterReferenceExpression
            | :? ILocalVariableReferenceExpression -> fun state k -> k (Nop, state)
            | :? IMemberAccessExpression as memberAccess -> fun state k -> reduceExpressionToRef state true memberAccess.Target k
            | _ -> __notImplemented__()
        in
        let rightReducer state k = reduceExpression state right k in
        mutate state left rightReducer targetReducer k

    and reduceOperationAssignment state (ast : IBinaryOperationExpression) k =
        let op = Operations.getAssignmentOperation ast.OperationType in
        let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled in
        let t = Types.GetSystemTypeOfNode ast
        let left = ast.LeftArgument in
        readTargeted state left (fun (targetRef, leftTerm, state) ->
        reduceExpression state ast.RightArgument (fun (rightTerm, state) ->
        performBinaryOperation state op leftTerm rightTerm isChecked t (fun (result, state) ->
        let obtainTarget state k = k (targetRef, state) in
        let obtainResult state k = k (result, state) in
        mutate state left obtainResult obtainTarget k)))

    and readTargeted state (ast : IExpression) k =
        match ast with
        | :? IParameterReferenceExpression
        | :? ILocalVariableReferenceExpression ->
            reduceExpression state ast (fun (result, state) ->
            k (Nop, result, state))
        | :? IFieldAccessExpression as field ->
            reduceExpressionToRef state true field.Target (fun (targetRef, state) ->
            readField state targetRef field.FieldSpecification.Field (fun (result, state) ->
            k (targetRef, result, state)))
        | :? IPropertyAccessExpression as property ->
            reduceExpressionToRef state true property.Target (fun (targetRef, state) ->
            let obtainTarget state k = k (targetRef, state) in
            reduceMethodCall state obtainTarget property.PropertySpecification.Property.Getter [] (fun (result, state) ->
            k (targetRef, result, state)))
        | :? IIndexerCallExpression
        | _ -> __notImplemented__()

    and mutate state (left : IExpression) right target k =
        // Pre-calculated term is used to support conceptually two different cases: "new A().N = 10" and "new A().N++".
        // In both we mutate fresh instance of A, but second one uses target of previous evaluation of "new A().N".
        // C# compiler generates "dup" instruction in that case.
        match left with
        | :? IParameterReferenceExpression
        | :? ILocalVariableReferenceExpression ->
            reduceExpressionToRef state false left (fun (targetRef, state) ->
            right state (fun (rightTerm, state) ->
            Memory.mutate state targetRef rightTerm |> k))
        | :? IFieldAccessExpression as field ->
            target state (fun (targetTerm, state) ->
            right state (fun (rightTerm, state) ->
            referenceToField state targetTerm field.FieldSpecification.Field (fun (fieldRef, state) ->
            Memory.mutate state fieldRef rightTerm |> k)))
        | :? IPropertyAccessExpression as property ->
            target state (fun (targetTerm, state) ->
            right state (fun (rightTerm, state) ->
            reduceMethodCall state target property.PropertySpecification.Property.Setter [right] k))
        | :? IIndexerCallExpression
        | _ -> __notImplemented__()

    and reduceBinaryOperation state op leftArgument rightArgument isChecked t k =
        reduceExpression state leftArgument (fun (left, state) ->
        reduceExpression state rightArgument (fun (right, state) ->
        performBinaryOperation state op left right isChecked t k))

    and performBinaryOperation state op left right isChecked t k =
        let t1 = Terms.TypeOf left in
        let t2 = Terms.TypeOf right in
        match op with
        | op when Propositional.isLogicalOperation op t1 t2 ->
            Propositional.simplifyBinaryConnective op left right (withSnd state >> k)
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation op left right isChecked t (withSnd state >> k)
        | op when Strings.isStringOperation op t1 t2 ->
            Strings.simplifyOperation op left right |> (withSnd state >> k)
        | _ ->
            match op with
            | OperationType.Equal -> Memory.referenceEqual left right |> (withSnd state >> k)
            | OperationType.NotEqual ->
                let equal = Memory.referenceEqual left right in
                Propositional.simplifyNegation equal (withSnd state >> k)
            | _ -> __notImplemented__()

    and reduceConditionalOperation state op leftArgument rightArgument k =
        let handleOp state op stopValue ignoreValue leftArgument rightArgument k =
            reduceExpression state leftArgument (fun (left, state') ->
                match left with
                | _ when left = ignoreValue -> reduceExpression state' rightArgument k
                | e when Terms.Just Terms.IsError e -> k (e, state')
                | _ when left = stopValue -> (stopValue, state') |> k
                | _ when op = OperationType.ConditionalAnd -> reduceExpression state' rightArgument (fun (right, state'') ->
                    let res = left &&& right in
                    let state = Merging.merge2States left !!left state'' state' in
                    k (res, state))
                | _ when op = OperationType.ConditionalOr -> reduceExpression state' rightArgument (fun (right, state'') ->
                    let res = left ||| right in
                    let state = Merging.merge2States left !!left state' state'' in
                    k (res, state))
                | _ -> __notImplemented__())
        in

        match op with
        | OperationType.ConditionalAnd -> handleOp state op Terms.MakeFalse Terms.MakeTrue leftArgument rightArgument k
        | OperationType.ConditionalOr  -> handleOp state op Terms.MakeTrue Terms.MakeFalse leftArgument rightArgument k
        | _ -> raise(System.ArgumentException("Wrong operator"))

// ------------------------------- IAbstractTypeCastExpression and inheritors -------------------------------

    and reduceAbstractTypeCastExpression state (ast : IAbstractTypeCastExpression) k =
        match ast with
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression state expression k
        | :? IUserDefinedTypeCastExpression as expression -> reduceUserDefinedTypeCastExpression state expression k
        | _ -> __notImplemented__()

    and reduceTypeCastExpression state (ast : ITypeCastExpression) k =
        let targetType = Types.FromMetadataType ast.TargetType in
        reduceExpression state ast.Argument (fun (argument, newState) ->
        typeCast (ast.OverflowCheck = OverflowCheckType.Enabled) newState argument targetType |> k)

    and is typ = function
        | Terms.GuardedValues(gs, vs) ->
            vs |> List.map (is typ) |> List.zip gs |> Merging.merge
        | term ->
            // TODO: here we should use more sophisticated type constraints processing, but for now...
            let justInherits = (Types.MetadataToDotNetType typ).IsAssignableFrom(Types.ToDotNetType (Terms.TypeOf term)) in
            Concrete(justInherits, Bool)

    and tryCast typ term =
        let casted = is typ term in
        Merging.merge2Terms casted !!casted term (Concrete(null, Types.FromMetadataType typ))

    and typeCast isChecked state term targetType =
        // TODO: refs and structs should still be refs after cast!
        if Types.IsObject targetType then (term, state)
        else
            let cast src dst expr =
                if src = dst then expr
                else Expression(Cast(src, dst, isChecked), [expr], dst)
            in
            let rec castSimple = function
                | Error _ -> term
                | Nop -> Terms.MakeError (new InvalidCastException(format1 "Internal error: casting void to {0}!" targetType))
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
        let reduceTarget state k = k (Terms.MakeNull typedefof<obj>, state) in
        let reduceArg state k = reduceExpression state ast.Argument k in
        reduceMethodCall state reduceTarget ast.MethodSpecification.Method [reduceArg] k

// ------------------------------- IAbstractUnaryOperationExpression and inheritors -------------------------------

    and reduceAbstractUnaryOperationExpression state (ast : IAbstractUnaryOperationExpression) k =
        match ast with
        | :? IUnaryOperationExpression as expression -> reduceUnaryOperationExpression state expression k
        | :? IUserDefinedUnaryOperationExpression as expression -> reduceUserDefinedUnaryOperationExpression state expression k
        | _ -> __notImplemented__()

    and reduceUnaryOperationExpression state (ast : IUnaryOperationExpression) k =
        let op = ast.OperationType in
        let isChecked = (ast.OverflowCheck = OverflowCheckType.Enabled) in
        let dotNetType = Types.GetSystemTypeOfNode ast in
        let t = dotNetType |> Types.FromDotNetType in
        match op with
        | OperationType.PrefixIncrement
        | OperationType.PrefixDecrement -> reducePrefixIncrement state ast k
        | OperationType.PostfixDecrement -> reducePostfixIncrement state ast.Argument (Terms.MakeConcrete -1 dotNetType) isChecked dotNetType k
        | OperationType.PostfixIncrement -> reducePostfixIncrement state ast.Argument (Terms.MakeConcrete 1 dotNetType) isChecked dotNetType k
        | _ ->
            reduceExpression state ast.Argument (fun (arg, newState) ->
            match t with
            | Bool -> Propositional.simplifyUnaryConnective op arg (withSnd newState >> k)
            | Numeric t -> Arithmetics.simplifyUnaryOperation op arg isChecked t (withSnd newState >> k)
            | String -> __notImplemented__()
            | _ -> __notImplemented__())

    and reduceUserDefinedUnaryOperationExpression state (ast : IUserDefinedUnaryOperationExpression) k =
        __notImplemented__()

    and reducePrefixIncrement state ast k =
        let assignment = Transformations.transformPrefixCrement ast in
        reduceOperationAssignment state assignment k

    and reducePostfixIncrement state leftAst right isChecked t k =
        let op = OperationType.Add in
        readTargeted state leftAst (fun (targetRef, left, state) ->
        performBinaryOperation state op left right isChecked t (fun (result, state) ->
        mutate state leftAst (fun state k -> k (result, state)) (fun state k -> k (targetRef, state)) (fun (_, state) ->
        k (left, state))))

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
        // TODO: implement it!
        k (Terms.MakeNull(Types.MetadataToDotNetType(ast.ArrayType)), state)

    and reduceDelegateCreationExpression state (ast : IDelegateCreationExpression) k =
        let metadataMethod = ast.MethodInstantiation.MethodSpecification.Method in
        let qualifiedTypeName = metadataMethod.DeclaringType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        reduceExpressionToRef state true ast.Target (fun (targetTerm, state) ->
        let invoke state args k =
            let assemblyPath = metadataMethod.DeclaringType.Assembly.Location in
            decompileAndReduceMethod state targetTerm args qualifiedTypeName metadataMethod assemblyPath k
        in
        let delegateTerm = Lambdas.Make metadataMethod invoke in
        let returnDelegateTerm state k = k (Return delegateTerm, state) in
        npeOrInvokeExpression state metadataMethod.IsStatic targetTerm returnDelegateTerm k))

    and reduceLambdaBlockExpression state (ast : ILambdaBlockExpression) k =
        let invoke state args k =
            reduceFunction state None args ast.Signature ast.Body k
        k (Lambdas.Make2 ast.Signature null invoke, state)

    and reduceLambdaExpression state (ast : ILambdaExpression) k =
        __notImplemented__()

    and initializeStaticMembersIfNeed state qualifiedTypeName k =
        if State.staticMembersInitialized state qualifiedTypeName then
            k state
        else
            let t = Types.FromQualifiedTypeName qualifiedTypeName in
            match Options.StaticFieldsValuation with
            | Options.Interpret ->
                let fields = DecompilerServices.getDefaultFieldValuesOf true qualifiedTypeName in
                let instance =
                    fields
                        |> List.map (fun (n, (t, _)) -> (n, Memory.defaultOf (Types.FromMetadataType t)))
                        |> Map.ofList
                        |> withSnd t
                        |> Struct
                in
                let state = Memory.allocateInStaticMemory state qualifiedTypeName instance in
                let initOneField state (name, (typ, expression)) k =
                    let address = Memory.referenceToStaticField state name qualifiedTypeName in
                    reduceExpression state expression (fun (value, state) ->
                    Memory.mutate state address value |> snd |> k)
                in
                Cps.List.foldlk initOneField state fields (fun state ->
                match DecompilerServices.getStaticConstructorOf qualifiedTypeName with
                | Some constr ->
                    reduceDecompiledMethod state (Concrete(null, t)) [] constr (snd >> k)
                | None -> k state)
            | Options.Overapproximate ->
                let instance, state = Memory.allocateSymbolicStruct true state t (System.Type.GetType(qualifiedTypeName)) in
                Memory.allocateInStaticMemory state qualifiedTypeName instance |> k

    and reduceObjectCreationExpression state (ast : IObjectCreationExpression) k =
        // TODO: support collection initializers
        let qualifiedTypeName = ast.ConstructedType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        let fields = DecompilerServices.getDefaultFieldValuesOf false qualifiedTypeName in
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
        let finish state =
            reduceMemberInitializerList reference state ast.ObjectInitializer (fun state ->
            if not isReference
            then k (Memory.deref state reference, State.pop state)
            else k (reference, state))
        in
        let baseClasses = DecompilerServices.baseClassesChain ast.ConstructedType in
        let reduceConstructor state (t : JetBrains.Metadata.Reader.API.IMetadataType) k =
            if ast.ConstructorSpecification = null
            then k state
            else
                Cps.List.mapFoldk reduceExpression state (List.ofArray ast.Arguments) (fun (arguments, state) ->
                let assemblyPath = DecompilerServices.locationOfType qualifiedTypeName in
                decompileAndReduceMethod state reference arguments qualifiedTypeName ast.ConstructorSpecification.Method assemblyPath (snd >> k))
        in
        Cps.List.foldlk reduceConstructor state baseClasses finish))

// ------------------------------- IMemberAccessExpression and inheritors -------------------------------

    and reduceMemberAccessExpression state (ast : IMemberAccessExpression) k =
        match ast with
        | :? IFieldAccessExpression as expression -> reduceFieldAccessExpression state expression k
        | :? IMemberCallExpression as expression -> reduceMemberCallExpression state expression k
        | _ -> __notImplemented__()

    and reduceFieldAccessExpression state (ast : IFieldAccessExpression) k =
        let qualifiedTypeName = ast.FieldSpecification.Field.DeclaringType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        reduceExpression state ast.Target (fun (target, state) ->
        readField state target ast.FieldSpecification.Field k))

    and readField state target (field : JetBrains.Metadata.Reader.API.IMetadataField) k =
        let fieldName = DecompilerServices.idOfMetadataField field in
        if field.IsStatic then
            let reference = Memory.referenceToStaticField state fieldName field.DeclaringType.AssemblyQualifiedName in
            k (Memory.deref state reference, state)
        else
            if (Terms.IsRef target) then
                Console.WriteLine("Warning: got field access without explicit dereferencing: " + (field.ToString()))
                k (Memory.fieldOf (Memory.deref state target) fieldName, state)
            else
                k (Memory.fieldOf target fieldName, state)

// ------------------------------- IMemberCallExpression and inheritors -------------------------------

    and reduceMemberCallExpression state (ast : IMemberCallExpression) k =
        match ast with
        | :? IEventAccessExpression as expression -> reduceEventAccessExpression state expression k
        | :? IIndexerCallExpression as expression -> reduceIndexerCallExpression state expression k
        | :? IMethodCallExpression as expression -> reduceMethodCallExpression state expression k
        | :? IPropertyAccessExpression as expression -> reducePropertyAccessExpression state expression k
        | _ -> __notImplemented__()

    and reduceEventAccessExpression state (ast : IEventAccessExpression) k =
        let qualifiedTypeName = ast.EventSpecification.Event.DeclaringType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        __notImplemented__())

    and reduceIndexerCallExpression state (ast : IIndexerCallExpression) k =
        let qualifiedTypeName = ast.PropertySpecification.Property.DeclaringType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        __notImplemented__())

    and reduceMethodCall state target (metadataMethod : JetBrains.Metadata.Reader.API.IMetadataMethod) arguments k =
        let qualifiedTypeName = metadataMethod.DeclaringType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        target state (fun (targetTerm, state) ->
        Cps.Seq.mapFoldk (fun state arg k -> arg state k) state arguments (fun (args, state) ->
        let invoke state k =
            let assemblyPath = metadataMethod.DeclaringType.Assembly.Location in
            decompileAndReduceMethod state targetTerm args qualifiedTypeName metadataMethod assemblyPath k
        in
        npeOrInvokeExpression state metadataMethod.IsStatic targetTerm invoke k)))

    and reduceMethodCallExpression state (ast : IMethodCallExpression) k =
        let reduceTarget state k = reduceExpressionToRef state true ast.Target k in
        let reduceArg arg = fun state k -> reduceExpression state arg k in
        let reduceArgs = ast.Arguments |> List.ofSeq |> List.map reduceArg in
        reduceMethodCall state reduceTarget ast.MethodInstantiation.MethodSpecification.Method reduceArgs k

    and reducePropertyAccessExpression state (ast : IPropertyAccessExpression) k =
        let obtainTarget state k = reduceExpressionToRef state true ast.Target k in
        reduceMethodCall state obtainTarget ast.PropertySpecification.Property.Getter [] (fun (result, state) ->
        k (result, state))
