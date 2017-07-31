namespace VSharp

open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API
open global.System
open System.Collections.Generic
open System.Reflection

type ImplementsAttribute(name : string) =
    inherit System.Attribute()
    member this.Name = name


module internal Interpreter =

// ------------------------------- Environment -------------------------------

    let getTokenBy = DecompilerServices.getTokenBy
    let getThisTokenBy = DecompilerServices.getThisTokenBy

    let externalImplementations =
        let dict = new Dictionary<string, MethodInfo>() in
        let (|||) = FSharp.Core.Operators.(|||) in
        let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public in
        Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule (Assembly.GetExecutingAssembly().GetTypes())
        |> Seq.iter (fun t -> t.GetMethods(bindingFlags) |> Seq.iter (fun m ->
            match m.GetCustomAttributes(typedefof<ImplementsAttribute>) with
            | SeqEmpty -> ()
            | SeqNode(attr, _) ->
                let key = (attr :?> ImplementsAttribute).Name in
                dict.Add(key, m)))
        dict

    let concreteExternalImplementations =
        let dict = new Dictionary<string, IDecompiledMethod>() in
        let (|||) = FSharp.Core.Operators.(|||) in
        let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public in
        [|Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Array")|]
        |> Seq.iter (fun t -> t.GetMethods(bindingFlags) |> Seq.iter (fun m ->
            match m.GetCustomAttributes(typedefof<CSharpUtils.ImplementsAttribute>) with
            | SeqEmpty -> ()
            | SeqNode(attr, _) ->
                let key = (attr :?> CSharpUtils.ImplementsAttribute).Name in
                let qualifiedTypeName = m.DeclaringType.AssemblyQualifiedName in
                let assemblyPath = JetBrains.Util.FileSystemPath.Parse m.DeclaringType.Assembly.Location in
                let metadataMethod = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m in
                match metadataMethod with
                | None ->
                    failwith (sprintf "WARNING: Could not decompile %s.%s" qualifiedTypeName m.Name)
                | Some metadataMethod ->
                    let decompiledMethod = DecompilerServices.decompileMethod assemblyPath qualifiedTypeName metadataMethod in
                    match decompiledMethod with
                    | None ->
                        failwith (sprintf "WARNING: Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name)
                    | Some decompiledMethod ->
                        dict.Add(key, decompiledMethod)))
        dict

    let rec internalCall metadataMethod argsAndThis ((s, h, m, f, p) as state : State.state) k =
        let fullMethodName = DecompilerServices.metadataMethodToString metadataMethod in
        let k' (result, state) = k (result, State.popStack state) in
        let methodInfo = externalImplementations.[fullMethodName] in
        let argsAndThis = List.map snd argsAndThis
        let parameters : obj[] =
            // Sometimes F# compiler merges tuple with the rest arguments!
            match methodInfo.GetParameters().Length with
            | 2 -> [| state; argsAndThis |]
            | 6 -> [| s; h; m; f; p; argsAndThis |]
            | _ -> __notImplemented__()
        let result = methodInfo.Invoke(null, parameters) in
        match result with
        | :? (StatementResult * State.state) as r -> k' r
        | _ -> internalfail "internal call should return tuple StatementResult * State!"

// ------------------------------- Member calls -------------------------------

    and decompileAndReduceMethod state this parameters qualifiedTypeName metadataMethod assemblyPath k =
        let decompiledMethod = DecompilerServices.decompileMethod assemblyPath qualifiedTypeName metadataMethod in
        match decompiledMethod with
        | None when DecompilerServices.isConstructor metadataMethod ->
            // Got default constructor ignored by decompiler
            k (NoResult, state)
        | None ->
            failwith (sprintf "WARNING: Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name)
        | Some decompiledMethod ->
            if metadataMethod.IsInternalCall then
                printfn "INTERNAL CALL OF %s.%s" qualifiedTypeName metadataMethod.Name
                let fullMethodName = DecompilerServices.metadataMethodToString metadataMethod in
                if externalImplementations.ContainsKey(fullMethodName) then
                    reduceFunctionSignature state decompiledMethod.Signature (Some this) parameters (fun (argsAndThis, state) ->
                    internalCall metadataMethod argsAndThis state k)
                elif concreteExternalImplementations.ContainsKey(fullMethodName) then
                    let parameters = this :: parameters
                    reduceDecompiledMethod state this parameters concreteExternalImplementations.[fullMethodName] k
                else __notImplemented__()
            else
                printfn "DECOMPILED %s:\n%s" qualifiedTypeName (JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
                reduceDecompiledMethod state this parameters decompiledMethod k

    and reduceFunctionSignature state (ast : IFunctionSignature) this values k =
        let valueOrFreshConst (param : Option<IMethodParameter>) value =
            match param, value with
            | None, _ -> internalfail "parameters list is longer than expected!"
            | Some param, None ->
                if param.MetadataParameter.HasDefaultValue
                then ((param.Name, getTokenBy (Choice1Of2 param)), Concrete(param.MetadataParameter.GetDefaultValue(), Types.FromConcreteMetadataType param.Type))
                else ((param.Name, getTokenBy (Choice1Of2 param)), Memory.makeSymbolicInstance false (Symbolization Nop) param.Name (Types.FromSymbolicMetadataType param.Type false))
            | Some param, Some value -> ((param.Name, getTokenBy (Choice1Of2 param)), value)
        let parameters = List.map2Different valueOrFreshConst ast.Parameters values in
        let parametersAndThis =
            match this with
            | Some term -> (("this", getThisTokenBy ast), term)::parameters
            | None -> parameters
        k (parametersAndThis, State.newStackFrame state parametersAndThis)

    and reduceFunction state this parameters returnType funcId (signature : IFunctionSignature) invoke k =
        reduceFunctionSignature state signature this parameters (fun (_, state) ->
        CallGraph.call state funcId invoke returnType (fun (result, state) -> (ControlFlow.consumeBreak result, state) |> k))

    and reduceFunctionWithBlockBody state this parameters returnType funcId (signature : IFunctionSignature) (body : IBlockStatement) k =
        let invoke state k = reduceBlockStatement state body k in
        reduceFunction state this parameters returnType funcId signature invoke k

    and reduceDecompiledMethod state this parameters (ast : IDecompiledMethod) k =
        let returnType = Types.FromSymbolicMetadataType (ast.MetadataMethod.Signature.ReturnType) false in
        reduceFunctionWithBlockBody state (Some this) parameters returnType (MetadataMethodIdentifier ast.MetadataMethod) ast.Signature ast.Body k

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

    and reduceArgListCreationExpression state (ast : IArgListCreationExpression) k =
        __notImplemented__()

    and reduceArgListReferenceExpression state (ast : IArgListReferenceExpression) k =
        __notImplemented__()

    and reduceParameterModifierExpression state (ast : IParameterModifierExpression) k =
        __notImplemented__()

// ------------------------------- Upper-level functions -------------------------------

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

    and reduceMemberAccessExpression state (ast : IMemberAccessExpression) k =
        match ast with
        | :? IFieldAccessExpression as expression -> reduceFieldAccessExpression state expression k
        | :? IMemberCallExpression as expression -> reduceMemberCallExpression state expression k
        | _ -> __notImplemented__()

    and reduceMemberCallExpression state (ast : IMemberCallExpression) k =
        match ast with
        | :? IEventAccessExpression as expression -> reduceEventAccessExpression state expression k
        | :? IIndexerCallExpression as expression -> reduceIndexerCallExpression state expression k
        | :? IMethodCallExpression as expression -> reduceMethodCallExpression state expression k
        | :? IPropertyAccessExpression as expression -> reducePropertyAccessExpression state expression k
        | _ -> __notImplemented__()

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

// ------------------------------- Delegates and lambdas -------------------------------

    and reduceDelegateCallExpression state (ast : IDelegateCallExpression) k =
        reduceDelegateCall state ast (fun (result, state) -> (ControlFlow.resultToTerm result, state) |> k)

    and reduceInlinedDelegateCallStatement state (ast : IDelegateCallExpression) k =
        reduceDelegateCall state ast k

    and reduceDelegateCall state (ast : IDelegateCallExpression) k =
        Cps.Seq.mapFoldk reduceExpression state ast.Arguments (fun (args, state) ->

        let curDelegate = Transformations.inlinedCallTarget ast |> function
            | None -> ast.Delegate
            | Some d -> d
        in

        reduceExpression state curDelegate (fun (deleg, state) ->
        let rec invoke state deleg k =
            match deleg with
                | HeapRef _ as r ->
                    let term, state = Memory.deref state r in
                    invoke state term k
                | Functions.Lambda(lambda) -> lambda state args k
                | Concrete(obj, _) ->
                    let message = Concrete("Cannot apply non-function type", VSharp.String) in
                    let term, state = State.activator.CreateInstance typeof<InvalidCastException> [message] state in
                    k (Throw term, state)
                | _ -> __notImplemented__()
        in
        match deleg with
        | Terms.GuardedValues(gs, vs) ->
            Cps.List.mapk (invoke state) vs (fun results ->
            let terms, states = List.unzip results in
            let term = terms |> List.map ControlFlow.resultToTerm |> List.zip gs |> Merging.merge in
            let state = Merging.mergeStates gs states
            (Return term, state) |> k)
        | _ -> invoke state deleg k))

    and reduceDelegateCreationExpression state (ast : IDelegateCreationExpression) k =
        let metadataMethod = ast.MethodInstantiation.MethodSpecification.Method in
        let qualifiedTypeName = metadataMethod.DeclaringType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        reduceExpressionToRef state true ast.Target (fun (targetTerm, state) ->
        let invoke state args k =
            let assemblyPath = metadataMethod.DeclaringType.Assembly.Location in
            decompileAndReduceMethod state targetTerm args qualifiedTypeName metadataMethod assemblyPath k
        in
        let delegateTerm, state = Functions.MakeLambda state metadataMethod invoke in
        let returnDelegateTerm state k = k (Return delegateTerm, state) in
        npeOrInvokeExpression state metadataMethod.IsStatic targetTerm returnDelegateTerm k))

    and reduceLambdaBlockExpression state (ast : ILambdaBlockExpression) k =
        let returnType = VSharp.Void in // TODO!!!
        let invoke state args k =
            reduceFunctionWithBlockBody state None args returnType (DelegateIdentifier ast) ast.Signature ast.Body k
        Functions.MakeLambda2 state ast.Signature null invoke |> k

    and reduceLambdaExpression state (ast : ILambdaExpression) k =
        let returnType = VSharp.Void in // TODO!!!
        let invokeBody state k =
            reduceExpression state ast.Body (fun (term, state) -> k (ControlFlow.throwOrReturn term, state))
        in
        let invoke state args k =
            reduceFunction state None args returnType (DelegateIdentifier ast) ast.Signature invokeBody k
        in
        Functions.MakeLambda2 state ast.Signature null invoke |> k

    and reduceAnonymousMethodExpression state (ast : IAnonymousMethodExpression) k =
        __notImplemented__()

// ------------------------------- Loops -------------------------------

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
        reduceExpressionStatement state lambdaBlock k

    and reduceLoopStatement state (ast : ILoopStatement) k =
        __notImplemented__()

    and reduceYieldReturnStatement state (ast : IYieldReturnStatement) k =
        __notImplemented__()

// ------------------------------- Linear control flow-------------------------------

    and composeSequentially curIStatement (result, state) statement k =
        let pathCondition = ControlFlow.currentCalculationPathCondition (curIStatement()) result in
        match pathCondition with
        | Terms.True -> statement state (fun (newRes, newState) -> k (ControlFlow.composeSequentially result newRes state newState))
        | Terms.False -> k (result, state)
        | _ ->
            statement
                (State.withPathCondition state pathCondition)
                (fun (newRes, newState) ->
                    let newState = State.popPathCondition newState in
                    k (ControlFlow.composeSequentially result newRes state newState))

    and reduceSequentially state statements k =
        Cps.Seq.foldlk
            (composeSequentially (fun () -> None))
            (NoResult, State.newStackFrame state [])
            statements
            (fun (res, state) -> k (res, State.popStack state))

    and reduceBlockStatement state (ast : IBlockStatement) k =
        let compose rs statement k =
            composeSequentially (fun () -> Some(statement)) rs (fun state -> reduceStatement state statement) k
        Cps.Seq.foldlk compose (NoResult, State.newStackFrame state []) ast.Statements (fun (res, state) -> k (res, State.popStack state))

    and reduceCommentStatement state (ast : ICommentStatement) k =
        k (NoResult, state)

    and reduceEmptyStatement state (ast : IEmptyStatement) k =
        k (NoResult, state)

    and reduceExpressionStatement state (ast : IExpressionStatement) k =
        if Transformations.isInlinedCall ast
        then
            reduceInlinedDelegateCallStatement state (ast.Expression :?> IDelegateCallExpression) k
        else
            reduceExpression state ast.Expression (fun (term, newState) ->
                k (ControlFlow.throwOrIgnore term, newState))

    and reduceLocalVariableDeclarationStatement state (ast : ILocalVariableDeclarationStatement) k =
        let name = ast.VariableReference.Variable.Name in
        let initialize k =
            let t = Types.FromConcreteMetadataType ast.VariableReference.Variable.Type in
            match t with
            | StructType _ when ast.Initializer = null -> k (Memory.defaultOf t, state)
            | _ -> reduceExpression state ast.Initializer k
        initialize (fun (initializer, state) -> k (NoResult, Memory.allocateOnStack state (name, getTokenBy (Choice2Of2 ast.VariableReference.Variable)) initializer))

    and reduceReturnStatement state (ast : IReturnStatement) k =
        reduceExpression state ast.Result (fun (term, state) -> k (ControlFlow.throwOrReturn term, state))

// ------------------------------- Conditional operations -------------------------------

    and reduceConditionalExecution state conditionInvocation thenBranch elseBranch k =
        conditionInvocation state (fun (condition, conditionState) ->
        match condition with
        | Terms.True ->  thenBranch conditionState k
        | Terms.False -> elseBranch conditionState k
        | e when Terms.Just Terms.IsError e -> k (Return e, conditionState)
        | _ ->
            thenBranch (State.withPathCondition conditionState   condition) (fun (thenResult, thenState) ->
            elseBranch (State.withPathCondition conditionState !!condition) (fun (elseResult, elseState) ->
            let result = ControlFlow.mergeResults condition thenResult elseResult in
            let state = Merging.merge2States condition !!condition (State.popPathCondition thenState) (State.popPathCondition elseState) in
            k (result, state))))

    and npeOrInvokeStatement state isStatic reference statement k =
        if isStatic then statement state k
        else
            reduceConditionalExecution state
                (fun state k -> k (Memory.isNull reference, state))
                (fun state k ->
                    let term, state = ControlFlow.npe state in
                    k (Throw term, state))
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

    and reduceSwitchStatement state (ast : ISwitchStatement) k =
        reduceExpression state ast.Expression (fun (arg, state) ->
        let reduceDefault state k = reduceBlockStatement state ast.Default (fun (result, state) -> k (ControlFlow.consumeBreak result, state)) in
        reduceSwitchCases state arg reduceDefault (List.ofArray ast.Cases) k)

    and reduceSwitchCases state arg dflt (cases : ISwitchCase list) k =
        let t = Terms.TypeOf arg |> Types.ToDotNetType in
        let compareArg (result, state) expression k =
            reduceExpression state expression (fun (value, state) ->
            performBinaryOperation state OperationType.Equal arg value false t (fun (equal, state) ->
            k (result ||| equal, state)))
        in
        match cases with
        | [] -> dflt state k
        | case::rest ->
            reduceConditionalExecution state
                (fun state k -> Cps.Seq.foldlk compareArg (Terms.MakeFalse, state) case.Values k)
                (fun state k -> reduceBlockStatement state case.Body (fun (result, state) -> k (ControlFlow.consumeBreak result, state)))
                (fun state k -> reduceSwitchCases state arg dflt rest k)
                k

// ------------------------------- Try-catch -------------------------------

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
                (fun state k -> reduceBlockStatement state clause.Body (fun (result, state) -> k (result, State.popStack state)))
                (fun state k -> reduceCatchClauses exn (State.popStack state) rest k)
                k

    and reduceCatchCondition exn state (ast : ICatchClause) k =
        if ast.VariableReference = null then k (Terms.MakeTrue, State.newStackFrame state []) // just catch {...} case
        else
            DecompilerServices.setPropertyOfNode ast "Thrown" exn
            // catch (...) {...} case
            let targetType = Types.FromSymbolicMetadataType ast.VariableReference.Variable.Type true in
            let typeMatches = checkCast exn targetType in
            let stackKey = ast.VariableReference.Variable.Name, getTokenBy (Choice2Of2 ast.VariableReference.Variable) in
            let state = State.newStackFrame state [(stackKey, exn)] in
            if ast.Filter = null then k (typeMatches, state)
            else
                let filteringExpression = Transformations.extractExceptionFilter ast.Filter in
                reduceConditionalExecution state
                    (fun state k -> k (typeMatches, state))
                    (fun state k -> reduceExpression state filteringExpression
                                        (fun (filterResult, state) ->
                                            k (ControlFlow.consumeErrorOrReturn
                                                (always (Return Terms.MakeFalse)) filterResult, state)))
                    (fun state k -> k (Return typeMatches, state))
                    (fun (result, state) -> k (ControlFlow.resultToTerm result, state))

    and reduceRethrowStatement state (ast : IRethrowStatement) k =
        let rec findException (node : INode) =
            if node = null then internalfail "exception register not found for rethowing!"
            match DecompilerServices.getPropertyOfNode node "Thrown" null with
            | null -> findException node.Parent
            | exn -> exn :?> Term
        in
        let exn = findException ast in
        k (Throw exn, state)

    and reduceFinally state statementResult (ast : IBlockStatement) k =
        if ast = null then k (statementResult, state)
        else reduceBlockStatement state ast (fun (_, state) -> k (statementResult, state))

    and reduceEndFinallyStatement state (ast : IEndFinallyStatement) k =
        __notImplemented__()

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

    and reduceSuccessfulFilteringStatement state (ast : ISuccessfulFilteringStatement) k =
        __notImplemented__()

    and reduceUsingStatement state (ast : IUsingStatement) k =
        __notImplemented__()

// ------------------------------- Memory access -------------------------------

    and reduceExpressionToRef state followHeapRefs (ast : IExpression) k =
        match ast with
        | null -> k (Concrete(null, VSharp.Object(IdGenerator.startingWith typedefof<obj>.FullName, [])), state) //TODO: Check
        | :? ILocalVariableReferenceExpression as expression -> k (Memory.referenceLocalVariable state (expression.Variable.Name, getTokenBy (Choice2Of2 expression.Variable)) followHeapRefs, state)
        | :? IParameterReferenceExpression as expression -> k (Memory.referenceLocalVariable state (expression.Parameter.Name, getTokenBy (Choice1Of2 expression.Parameter)) followHeapRefs, state)
        | :? IThisReferenceExpression as expression -> k (Memory.referenceLocalVariable state ("this", getThisTokenBy expression) followHeapRefs, state)
        | :? IFieldAccessExpression as expression ->
            reduceExpressionToRef state true expression.Target (fun (target, state) ->
            referenceToField state followHeapRefs target expression.FieldSpecification.Field k)
        | :? IDerefExpression as expression -> reduceExpressionToRef state followHeapRefs expression.Argument k
        | _ -> reduceExpression state ast k

    and referenceToField state followHeapRefs target (field : JetBrains.Metadata.Reader.API.IMetadataField) k =
        let id = DecompilerServices.idOfMetadataField field in
        if field.IsStatic then
            k (Memory.referenceStaticField state followHeapRefs id field.DeclaringType.AssemblyQualifiedName)
        else
            k (Memory.referenceField state followHeapRefs id target)

    and reduceArrayElementAccessExpression state (ast : IArrayElementAccessExpression) k =
        reduceExpression state ast.Array (fun (arrayRef, state) ->
        Cps.Seq.mapFoldk reduceExpression state ast.Indexes (fun (indices, state) ->
        let reference, state = Memory.referenceArrayIndex state arrayRef indices in
        k (Memory.deref state reference)))

    and reduceBaseReferenceExpression state (ast : IBaseReferenceExpression) k =
        k (Memory.derefLocalVariable state ("this", getThisTokenBy ast))

    and reduceBoxExpression state (ast : IBoxExpression) k =
        __notImplemented__()

    and reduceCheckFiniteExpression state (ast : ICheckFiniteExpression) k =
        __notImplemented__()

    and reduceDefaultValueExpression state (ast : IDefaultValueExpression) k =
        (Memory.defaultOf (Types.FromConcreteMetadataType ast.Type), state) |> k

    and reduceDerefExpression state (ast : IDerefExpression) k =
        reduceExpression state ast.Argument (fun (reference, state) ->
        k (Memory.deref state reference))

    and reduceFieldAccessExpression state (ast : IFieldAccessExpression) k =
        let qualifiedTypeName = ast.FieldSpecification.Field.DeclaringType.AssemblyQualifiedName in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        reduceExpression state ast.Target (fun (target, state) ->
        readField state target ast.FieldSpecification.Field k))

    and readField state target (field : JetBrains.Metadata.Reader.API.IMetadataField) k =
        let fieldName = DecompilerServices.idOfMetadataField field in
        if field.IsStatic then
            let reference, state = Memory.referenceStaticField state false fieldName field.DeclaringType.AssemblyQualifiedName in
            k (Memory.deref state reference)
        else
            if (Terms.IsRef target) then
                Console.WriteLine("Warning: got field access without explicit dereferencing: " + (field.ToString()))
                let term, state = Memory.deref state target in
                k (Memory.fieldOf term fieldName, state)
            else
                k (Memory.fieldOf target fieldName, state)

    and reduceLiteralExpression state (ast : ILiteralExpression) k =
        let mType = Types.FromConcreteMetadataType ast.Value.Type in
        k (Concrete(ast.Value.Value, mType), state)

    and reduceLocalVariableReferenceExpression state (ast : ILocalVariableReferenceExpression) k =
        k (Memory.derefLocalVariable state (ast.Variable.Name, getTokenBy (Choice2Of2 ast.Variable)))

    and reduceMakeRefExpression state (ast : IMakeRefExpression) k =
        __notImplemented__()

    and reduceParameterReferenceExpression state (ast : IParameterReferenceExpression) k =
        k (Memory.derefLocalVariable state (ast.Parameter.Name, getTokenBy (Choice1Of2 ast.Parameter)))

    and reduceThisReferenceExpression state (ast : IThisReferenceExpression) k =
        k (Memory.derefLocalVariable state ("this", getThisTokenBy ast))

// ------------------------------- Binary operations -------------------------------

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
        let reduceTarget state k = k (Terms.MakeNull typedefof<obj>, state) in
        let reduceLeftArg state k = reduceExpression state ast.LeftArgument k in
        let reduceRightArg state k = reduceExpression state ast.RightArgument k in
        reduceMethodCall state reduceTarget ast.MethodSpecification.Method [reduceLeftArg; reduceRightArg] k

    and reduceAssignment state (left : IExpression) (right : IExpression) k =
        let targetReducer =
            match left with
            | :? IParameterReferenceExpression
            | :? ILocalVariableReferenceExpression
            | :? IArrayElementAccessExpression ->
                fun state k -> k (Nop, state)
            | :? IMemberAccessExpression as memberAccess ->
                fun state k -> reduceExpressionToRef state true memberAccess.Target k
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
            referenceToField state false targetTerm field.FieldSpecification.Field (fun (fieldRef, state) ->
            Memory.mutate state fieldRef rightTerm |> k)))
        | :? IPropertyAccessExpression as property ->
            target state (fun (targetTerm, state) ->
            right state (fun (rightTerm, state) ->
            reduceMethodCall state target property.PropertySpecification.Property.Setter [right] k))
        | :? IArrayElementAccessExpression as arrayAccess ->
            reduceExpression state arrayAccess.Array (fun (array, state) ->
            Cps.Seq.mapFoldk reduceExpression state arrayAccess.Indexes (fun (indices, state) ->
            right state (fun (rightTerm, state) ->
            let reference, state = Memory.referenceArrayIndex state array indices in
            Memory.mutate state reference rightTerm |> k)))
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
            Arithmetics.simplifyBinaryOperation op state left right isChecked t k
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

// ------------------------------- Unary operations -------------------------------

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
            | Numeric t -> Arithmetics.simplifyUnaryOperation op newState arg isChecked t k
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

// ------------------------------- Type casting and type information -------------------------------

    and reduceAbstractTypeCastExpression state (ast : IAbstractTypeCastExpression) k =
        match ast with
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression state expression k
        | :? IUserDefinedTypeCastExpression as expression -> reduceUserDefinedTypeCastExpression state expression k
        | _ -> __notImplemented__()
    //TODO: make with generic
    and is (leftType : TermType) (rightType : TermType) =
        let makeBoolConst name termType = Terms.FreshConstant name (SymbolicConstantType termType) typedefof<bool>
        in
        let concreteIs (dotNetType : Type) =
            let b = makeBoolConst dotNetType.FullName (ClassType(dotNetType, [])) in
            function
            | Types.ReferenceType(t, _)
            | Types.StructureType(t, _) -> Terms.MakeBool (t = dotNetType)
            | SubType(t, name, _, _) as termType when t.IsAssignableFrom(dotNetType) ->
                makeBoolConst name termType ==> b
            | SubType(t, _, _, _) when not <| t.IsAssignableFrom(dotNetType) -> Terms.MakeFalse
            | ArrayType _ -> Terms.MakeBool <| dotNetType.IsAssignableFrom(typedefof<obj>)
            | Null -> Terms.MakeFalse
            | Object(name, _) as termType -> makeBoolConst name termType ==> b
            | _ -> __notImplemented__()
        in
        let subTypeIs (dotNetType: Type, rightName) =
            let b = makeBoolConst rightName (SubType(dotNetType, rightName, [], [])) in
            function
            | Types.ReferenceType(t, _) -> Terms.MakeBool <| dotNetType.IsAssignableFrom(t)
            | Types.StructureType(t, _) -> Terms.MakeBool (dotNetType = typedefof<obj> || dotNetType = typedefof<ValueType>)
            | SubType(t, name, [], []) when dotNetType.IsAssignableFrom(t) -> Terms.MakeTrue
            | SubType(t, name, [], []) as termType when t.IsAssignableFrom(dotNetType) ->
                makeBoolConst name termType ==> b
            | ArrayType _ -> Terms.MakeBool <| dotNetType.IsAssignableFrom(typedefof<obj>)
            | Null -> Terms.MakeFalse
            | Object(name, _) as termType -> makeBoolConst name termType ==> b
            | _ -> __notImplemented__()
        in
        match leftType, rightType with
        | PointerType left, PointerType right -> is left right
        | PointerType left, _ -> is left rightType
        | Void, _   | _, Void
        | Bottom, _ | _, Bottom -> Terms.MakeFalse
        | Func _, Func _ -> Terms.MakeTrue
        | ArrayType(t1, c1), ArrayType(Object("Array", []), 0) -> Terms.MakeTrue
        | ArrayType(t1, c1), ArrayType(t2, c2) -> Terms.MakeBool <| ((t1 = t2) && (c1 = c2))
        | leftType, Types.StructureType(t, _) when leftType <> Null -> concreteIs t leftType
        | leftType, Types.ReferenceType(t, _) -> concreteIs t leftType
        | leftType, SubType(t, name, _, _) -> subTypeIs (t, name) leftType
        | leftType, Object(name, _) -> subTypeIs (typedefof<obj>, name) leftType
        | _ -> __notImplemented__()

    and doCast (state : State.state) term targetType =
        let leftType = Terms.TypeOf term in
        let rec isUpCast l r =
            match l, r with
            | PointerType left, PointerType right -> isUpCast left right
            | PointerType left, _ -> isUpCast left r
            | Types.ComplexType(t1, _, _), Types.ComplexType(t2, _, _) -> t2.IsAssignableFrom(t2)
            | _, Object _ -> true
            | Object _, _ -> false
            | Func _, Func _ -> false
            | ArrayType _, _ -> true
            | _ -> __notImplemented__()
        in
        let result =
            match isUpCast leftType targetType with
            | true -> term
            | false ->
                match term with
                | Concrete(value, _) -> Concrete(value, targetType)
                | Constant(name, source, _) -> Term.Constant(name, source, targetType)
                | Expression(operation, operands, _) -> Expression(operation, operands, targetType)
                | StackRef(t, l, _) -> StackRef(t, l, targetType)
                | HeapRef(p, _) -> HeapRef(p, targetType) // TODO
                | Struct(m, _) -> Struct(m, targetType)
                | _ -> __notImplemented__()
        in
        Return result, state

    and throwInvalidCastException state term targetType =
        let result, state =
            match term with
            | Error t as error -> error, state
            | Nop ->
                let message = Concrete(sprintf "Internal error: casting void to %O!" targetType, VSharp.String) in
                let term, state = State.activator.CreateInstance typeof<InvalidCastException> [message] state in
                Error term, state
            | StackRef _ as r ->
                printfn "Warning: casting stack reference %s to %s!" (toString r) (toString targetType)
                r, state
            | _ ->
                let message = Concrete(sprintf "Internal error: casting %O to %O!" (Terms.TypeOf term) targetType, VSharp.String) in
                let term, state = State.activator.CreateInstance typeof<InvalidCastException> [message] state in
                Error term, state
        in
        ControlFlow.throwOrReturn result, state

    and reduceUserDefinedTypeCastExpression state (ast : IUserDefinedTypeCastExpression) k =
        let reduceTarget state k = k (Terms.MakeNull typedefof<obj>, state) in
        let reduceArg state k = reduceExpression state ast.Argument k in
        reduceMethodCall state reduceTarget ast.MethodSpecification.Method [reduceArg] k

    and reduceTryCastExpression state (ast : ITryCastExpression) k =
        let targetType = Types.FromSymbolicMetadataType ast.Type true in
        reduceExpression state ast.Argument (fun (term, state) ->
        let isCasted = checkCast term targetType in
        let mapper state term targetType =
            reduceConditionalExecution state
                (fun state k -> k (isCasted, state))
                (fun state k -> k (doCast state term targetType))
                (fun state k -> k (Return <| Concrete(null, targetType), state))
                (fun (statementResult, state) -> (ControlFlow.resultToTerm statementResult, state))
        in
        let term, state =
            match term with
            | Union gvs -> Merging.guardedStateMap (fun term -> mapper state term targetType) gvs state
            | _ -> mapper state term targetType
        in k (term, state))

    and reduceTypeCastExpression state (ast : ITypeCastExpression) k =
        let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled
        let cast src dst expr =
            if src = dst then expr
            else Expression(Cast(src, dst, isChecked), [expr], dst)
        in
        let targetType = Types.FromSymbolicMetadataType ast.TargetType true in
        let isCasted term = checkCast term targetType in
        let hierarchyCast state term targetType =
            reduceConditionalExecution state
                (fun state k -> k (isCasted term, state))
                (fun state k -> k (doCast state term targetType))
                (fun state k -> k (throwInvalidCastException state term targetType))
                (fun (statementResult, state) -> (ControlFlow.resultToTerm statementResult, state))
        in
        let rec primitiveCast state term targetType =
            match term with
            | Error _ -> (term, state)
            | Nop ->
                let message = Concrete(sprintf "Internal error: casting void to %O!" targetType, VSharp.String) in
                let term, state = State.activator.CreateInstance typeof<InvalidCastException> [message] state in
                (Error term, state)
            | Concrete(_, Null) as t -> (t, state)
            | Concrete(value, _) ->
                if Terms.IsFunction term && Types.IsFunction targetType
                then (Concrete(value, targetType), state)
                else (Terms.MakeConcrete value (Types.ToDotNetType targetType), state)
            | Constant(_, _, t) -> (cast t targetType term, state)
            | Expression(operation, operands, t) -> (cast t targetType term, state)
            | StackRef _ as r ->
                printfn "Warning: casting stack reference %s to %s!" (toString r) (toString targetType)
                hierarchyCast state r targetType
            | HeapRef _ as r -> hierarchyCast state r targetType
            | Struct _ as r -> hierarchyCast state r targetType
            | _ -> __notImplemented__()
        in
        reduceExpression state ast.Argument (fun (term, state) ->
        let term, state =
            match term with
            | Union gvs -> Merging.guardedStateMap (fun term -> primitiveCast state term targetType) gvs state
            | _ -> primitiveCast state term targetType
        in k (term, state))

    and checkCast term targetType =
        let mapper = fun left -> is (Terms.TypeOf left) targetType in
        match term with
        | Union gvs -> Merging.guardedMap mapper gvs
        | _ -> mapper term

    and reduceCheckCastExpression state (ast : ICheckCastExpression) k =
        let targetType = Types.FromSymbolicMetadataType ast.Type true in
        reduceExpression state ast.Argument (fun (term, state) ->
        checkCast term targetType |> withSnd state |> k)

    and reduceTypeOfExpression state (ast : ITypeOfExpression) k =
        let instance = Types.MetadataToDotNetType ast.Type in
        k (Terms.MakeConcrete instance typedefof<Type>, state)

// ------------------------------- Objects construction -------------------------------

    and reduceAnonymousObjectCreationExpression state (ast : IAnonymousObjectCreationExpression) k =
        __notImplemented__()

    and reduceArrayCreationExpression state (ast : IArrayCreationExpression) k =
        let typ = Types.FromConcreteMetadataType ast.ArrayType in
        Cps.Seq.mapFoldk reduceExpression state ast.Dimensions (fun (dimensions, state) ->
        reduceExpressionList state ast.Initializer (fun (initializer, state) ->
        let result =
            match initializer with
            | Concrete(null, _) -> Array.makeDefault Memory.defaultOf dimensions typ (Array.zeroLowerBound dimensions.Length)
            | _ -> Array.fromInitializer (int(ast.ArrayType.Rank)) typ initializer
        Memory.allocateInHeap state result |> k))

    and initializeStaticMembersIfNeed state qualifiedTypeName k =
        if State.staticMembersInitialized state qualifiedTypeName then
            k state
        else
            Console.WriteLine("Initializing static members of " + qualifiedTypeName)
            let t = Types.FromQualifiedTypeName qualifiedTypeName in
            match Options.StaticFieldsValuation() with
            | Options.DefaultStaticFields ->
                let fields = DecompilerServices.getDefaultFieldValuesOf true qualifiedTypeName in
                let instance =
                    fields
                        |> List.map (fun (n, (t, _)) -> (Concrete(n, VSharp.String), Memory.defaultOf (Types.FromConcreteMetadataType t)))
                        |> Heap.ofSeq
                        |> withSnd t
                        |> Struct
                in
                let state = Memory.allocateInStaticMemory state qualifiedTypeName instance in
                let initOneField state (name, (typ, expression)) k =
                    let address, state = Memory.referenceStaticField state false name qualifiedTypeName in
                    reduceExpression state expression (fun (value, state) ->
                    Memory.mutate state address value |> snd |> k)
                in
                Cps.List.foldlk initOneField state fields (fun state ->
                match DecompilerServices.getStaticConstructorOf qualifiedTypeName with
                | Some constr ->
                    reduceDecompiledMethod state (Concrete(null, t)) [] constr (snd >> k)
                | None -> k state)
            | Options.SymbolizeStaticFields ->
                let addr = StaticRef(qualifiedTypeName, [], t) in
                let instance = Memory.makeSymbolicStruct true (Symbolization addr) t (System.Type.GetType(qualifiedTypeName)) in
                Memory.allocateInStaticMemory state qualifiedTypeName instance |> k

    and reduceObjectCreation state constructedType objectInitializerList collectionInitializerList (constructorSpecification : MethodSpecification) invokeArguments k =
        let qualifiedTypeName = DecompilerServices.assemblyQualifiedName constructedType in
        initializeStaticMembersIfNeed state qualifiedTypeName (fun state ->
        let fields = DecompilerServices.getDefaultFieldValuesOf false qualifiedTypeName in
        let names, typesAndInitializers = List.unzip fields in
        let types, initializers = List.unzip typesAndInitializers in
        Cps.List.mapFoldk reduceExpression state initializers (fun (initializers, state) ->
        let fields = List.map2 (fun t -> function | Nop -> Memory.defaultOf (Types.FromConcreteMetadataType t) | v -> v) types initializers
                        |> List.zip (List.map Terms.MakeConcreteString names) |> Heap.ofSeq in
        let t = Types.FromConcreteMetadataType constructedType in
        let freshValue = Struct(fields, t) in
        let isReference = Types.IsReferenceType t in
        let reference, state =
            if isReference
            then Memory.allocateInHeap state freshValue
            else
                let tempVar = "constructed instance" in
                let state = State.newStackFrame state [((tempVar, tempVar), freshValue)] in
                (Memory.referenceLocalVariable state (tempVar, tempVar) false, state)
        in
        let finish r =
            composeSequentially (fun () -> None) r
                (fun state k ->
                    if isReference
                    then k (Return reference, state)
                    else
                        let term, state = Memory.deref state reference in
                        k (Return term, State.popStack state))
                (fun (result, state) -> k (ControlFlow.resultToTerm result, state))
        in
        let invokeInitializers r =
            composeSequentially (fun () -> None) r (fun state k ->
                if objectInitializerList <> null then
                    reduceMemberInitializerList reference state objectInitializerList k
                else if collectionInitializerList <> null then
                    reduceCollectionInitializerList constructedType reference state collectionInitializerList k
                else k (NoResult, state)
            ) finish
        in
        let baseClasses = DecompilerServices.baseClassesChain constructedType in
        let reduceConstructor (t : JetBrains.Metadata.Reader.API.IMetadataType) state k =
            if constructorSpecification = null
            then k (NoResult, state)
            else
                invokeArguments state (fun (arguments, state) ->
                    let assemblyPath = DecompilerServices.locationOfType qualifiedTypeName in
                    decompileAndReduceMethod state reference arguments qualifiedTypeName constructorSpecification.Method assemblyPath k)
        in
        let reduceConstructors = baseClasses |> Seq.map reduceConstructor in
        reduceSequentially state reduceConstructors invokeInitializers))

    and reduceObjectCreationExpression state (ast : IObjectCreationExpression) k =
        let arguments state = Cps.List.mapFoldk reduceExpression state (List.ofArray ast.Arguments) in
        reduceObjectCreation state ast.ConstructedType ast.ObjectInitializer ast.CollectionInitializer ast.ConstructorSpecification arguments k

    and reduceMemberInitializerList initializedObject state (ast : IMemberInitializerList) k =
        let initializers = ast.Initializers |> Seq.map (reduceMemberInitializer initializedObject) in
        reduceSequentially state initializers k

    and reduceMemberInitializer this (ast : IMemberInitializer) state k =
        match ast with
        | :? IFieldMemberInitializer as initializer -> reduceFieldMemberInitializer this state initializer k
        | :? IPropertyMemberInitializer as initializer -> reducePropertyMemberInitializer this state initializer k
        | _ -> __notImplemented__()

    and reduceFieldMemberInitializer this state (ast : IFieldMemberInitializer) k =
        reduceExpression state ast.Value (fun (value, state) ->
        let fieldReference, state = Memory.referenceField state false (DecompilerServices.idOfMetadataField ast.Field) this in
        let result, state = Memory.mutate state fieldReference value in
        k (ControlFlow.throwOrIgnore result, state))

    and reducePropertyMemberInitializer this state (ast : IPropertyMemberInitializer) k =
        __notImplemented__()

    and reduceCollectionInitializerList constructedType initializedObject state (ast : IExpressionList) k =
        let intializers = ast.Expressions |> Seq.map (reduceCollectionInitializer constructedType initializedObject) in
        reduceSequentially state intializers k

    and reduceCollectionInitializer constructedType initializedObject (ast : IExpression) state k =
        let args =
            match ast with
            | :? IExpressionList as es -> es.Expressions :> seq<IExpression>
            | e -> [e] :> seq<IExpression>
        in
        let argTypes = Seq.map DecompilerServices.getTypeOfNode args in
        Cps.Seq.mapFoldk reduceExpression state args (fun (argValues, state) ->
        let bestOverload = DecompilerServices.resolveAdd argTypes constructedType in
        let reduceTarget state k = k (initializedObject, state) in
        let reduceArg arg = (fun state k -> k (arg, state)) in
        let reduceArgs = argValues |> List.ofSeq |> List.map reduceArg in
        reduceMethodCall state reduceTarget bestOverload reduceArgs (fun (result, state) ->
        k (ControlFlow.throwOrIgnore result, state)))

    and reduceExpressionList state (ast : IExpressionList) k =
        if ast = null then k (Concrete(null, VSharp.Void), state)
        else Cps.Seq.mapFoldk reduceExpression state ast.Expressions (fun (terms, state) ->
        k (Concrete(terms, VSharp.Void), state))

    and reduceNestedInitializer state (ast : INestedInitializer) k =
        __notImplemented__()

// ------------------------------- Concurrency -------------------------------

    and reduceLockStatement state (ast : ILockStatement) k =
        __notImplemented__()

    and reduceAwaitExpression state (ast : IAwaitExpression) k =
        __notImplemented__()

// ------------------------------- Unsafe code -------------------------------

    and reduceAddressOfExpression state (ast : IAddressOfExpression) k =
        reduceExpressionToRef state true ast.Argument k

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

    and reduceFixedStatement state (ast : IFixedStatement) k =
        __notImplemented__()

    and reduceTypeReferenceExpression state (ast : ITypeReferenceExpression) k =
        __notImplemented__()

    and reduceUnboxExpression state (ast : IUnboxExpression) k =
        __notImplemented__()

    and reduceUntypedStackAllocExpression state (ast : IUntypedStackAllocExpression) k =
        __notImplemented__()

    and reduceVirtualMethodPointerExpression state (ast : IVirtualMethodPointerExpression) k =
        __notImplemented__()

    and reduceMemoryCopyStatement state (ast : IMemoryCopyStatement) k =
        __notImplemented__()

    and reduceMemoryInitializeStatement state (ast : IMemoryInitializeStatement) k =
        __notImplemented__()

    and reducePinStatement state (ast : IPinStatement) k =
        __notImplemented__()


    and reduceUnpinStatement state (ast : IUnpinStatement) k =
        __notImplemented__()

    and reduceFieldReferenceExpression state (ast : IFieldReferenceExpression) k =
        __notImplemented__()

    and reduceFunctionPointerCallExpression state (ast : IFunctionPointerCallExpression) k =
        __notImplemented__()

    and reduceMethodPointerExpression state (ast : IMethodPointerExpression) k =
        __notImplemented__()

    and reduceMethodReferenceExpression state (ast : IMethodReferenceExpression) k =
        __notImplemented__()

// ------------------------------- Goto statements -------------------------------

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

    and reduceJumpStatement state (ast : IJumpStatement) k =
        __notImplemented__()

    and reduceLabelDeclarationStatement state (ast : ILabelDeclarationStatement) k =
        __notImplemented__()


type Activator() =
    interface State.ActivatorInterface with
        member this.CreateInstance exceptionType arguments state =
            let assemblyQualifiedName = exceptionType.AssemblyQualifiedName in
            let assemblyLocation = exceptionType.Assembly.Location in
            let decompiledClass = DecompilerServices.decompileClass (DecompilerServices.jetBrainsFileSystemPath assemblyLocation) assemblyQualifiedName in
            let methods = decompiledClass.TypeInfo.GetMethods() in
            let invokeArguments state k = k (arguments, state) in
            let argumentsLength = List.length arguments in
            let argumentsTypes = List.map Terms.TypeOf arguments in
            let ctorMethods =
                methods
                |> List.ofArray
                |> List.filter (fun (m : IMetadataMethod)
                                    -> m.Name = ".ctor"
                                        && m.Parameters.Length = argumentsLength
                                        && m.Parameters
                                            |> Seq.forall2 (fun p1 p2 -> Types.FromConcreteMetadataType (p2.Type) = p1) argumentsTypes) in

            assert(List.length ctorMethods = 1)
            let ctor = List.head ctorMethods in
            let methodSpecification = new MethodSpecification(ctor, Array.map (fun (p : IMetadataParameter) -> p.Type) ctor.Parameters)
            Interpreter.reduceObjectCreation state (DecompilerServices.resolveType exceptionType) null null methodSpecification invokeArguments id
