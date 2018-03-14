﻿namespace VSharp.Interpreter

open VSharp
open VSharp.Core
open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API
open global.System
open System.Collections.Generic
open System.Reflection

type ImplementsAttribute(name : string) =
    inherit System.Attribute()
    member x.Name = name

module internal Interpreter =

// ------------------------------- Utilities -------------------------------

    let private getTokenBy = DecompilerServices.getTokenBy
    let private getThisTokenBy = DecompilerServices.getThisTokenBy

    let restoreAfter k x = let r = k x in Restore(); r
    let restoreBefore k x = Restore(); k x

// ------------------------------- Environment interaction -------------------------------

    let externalImplementations =
        let dict = new Dictionary<string, MethodInfo>()
        let (|||) = FSharp.Core.Operators.(|||)
        let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
        Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule (Assembly.GetExecutingAssembly().GetTypes())
        |> Seq.iter (fun t -> t.GetMethods(bindingFlags) |> Seq.iter (fun m ->
            match m.GetCustomAttributes(typedefof<ImplementsAttribute>) with
            | Seq.Empty -> ()
            | Seq.Cons(attr, _) ->
                let key = (attr :?> ImplementsAttribute).Name
                dict.Add(key, m)))
        dict

    let concreteExternalImplementations =
        let dict = new Dictionary<string, IDecompiledMethod>()
        let (|||) = FSharp.Core.Operators.(|||)
        let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
        [|Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Array")|]
        |> Seq.iter (fun t -> t.GetMethods(bindingFlags) |> Seq.iter (fun m ->
            match m.GetCustomAttributes(typedefof<CSharpUtils.ImplementsAttribute>) with
            | Seq.Empty -> ()
            | Seq.Cons(attr, _) ->
                let key = (attr :?> CSharpUtils.ImplementsAttribute).Name
                let qualifiedTypeName = (safeGenericTypeDefinition m.DeclaringType).AssemblyQualifiedName
                let assemblyPath = JetBrains.Util.FileSystemPath.Parse m.DeclaringType.Assembly.Location
                let metadataMethod = DecompilerServices.methodInfoToMetadataMethod assemblyPath qualifiedTypeName m
                match metadataMethod with
                | None ->
                    failwith (sprintf "WARNING: Could not decompile %s.%s" qualifiedTypeName m.Name)
                | Some metadataMethod ->
                    let decompiledMethod = DecompilerServices.decompileMethod assemblyPath qualifiedTypeName metadataMethod
                    match decompiledMethod with
                    | DecompilerServices.DecompilationResult.DecompilationError ->
                        failwith (sprintf "WARNING: Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name)
                    | DecompilerServices.DecompilationResult.MethodWithoutInitializer decompiledMethod
                    | DecompilerServices.DecompilationResult.MethodWithExplicitInitializer decompiledMethod
                    | DecompilerServices.DecompilationResult.MethodWithImplicitInitializer decompiledMethod
                    | DecompilerServices.DecompilationResult.ObjectConstuctor decompiledMethod ->
                        dict.Add(key, decompiledMethod)
                    | _ -> __unreachable__()))
        dict

    let rec internalCall metadataMethod argsAndThis (s : state) k =
        let fullMethodName = DecompilerServices.metadataMethodToString metadataMethod
        let k' (result, state) = k (result, Memory.PopStack state)
        let methodInfo = externalImplementations.[fullMethodName]
        let extractArgument (_, value, _) =
            match value with
            | Specified term -> term
            | _ -> internalfail "internal call with unspecified parameter!"
        let argsAndThis = List.map extractArgument argsAndThis
        let parameters : obj[] =
            // Sometimes F# compiler merges tuple with the rest arguments!
            match methodInfo.GetParameters().Length with
            | 2 -> [| s; argsAndThis |]
            | 6 -> [| s.stack; s.heap; s.statics; s.frames; s.pc; argsAndThis |]
            | _ -> __notImplemented__()
        let result = methodInfo.Invoke(null, parameters)
        match result with
        | :? (statementResult * state) as r -> k' r
        | _ -> internalfail "internal call should return tuple StatementResult * State!"

// ------------------------------- Preparation -------------------------------

    and initialize state k =
        Reset()
        let k = Enter null state k
        let stringTypeName = typeof<string>.AssemblyQualifiedName
        let emptyString, state = MakeString 0 String.Empty |> Memory.AllocateInHeap state
        initializeStaticMembersIfNeed null state stringTypeName (fun (result, state) ->
        let emptyFieldRef, state = Memory.ReferenceStaticField state false "System.String.Empty" Core.String stringTypeName
        Memory.Mutate state emptyFieldRef emptyString |> snd |> restoreAfter k)

// ------------------------------- Member calls -------------------------------

    and decompileAndReduceMethod caller state this parameters qualifiedTypeName metadataMethod assemblyPath k =
        let decompiledMethod = DecompilerServices.decompileMethod assemblyPath qualifiedTypeName metadataMethod
        match decompiledMethod with
        | DecompilerServices.DecompilationResult.MethodWithoutInitializer decompiledMethod ->
            printfn "DECOMPILED %s:\n%s" qualifiedTypeName (JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
            reduceDecompiledMethod caller state this parameters decompiledMethod (fun state k' -> k' (NoComputation, state)) k
        | DecompilerServices.DecompilationResult.MethodWithExplicitInitializer _
        | DecompilerServices.DecompilationResult.MethodWithImplicitInitializer _
        | DecompilerServices.DecompilationResult.ObjectConstuctor _
        | DecompilerServices.DecompilationResult.DefaultConstuctor ->
            reduceBaseOrThisConstuctorCall caller state this parameters qualifiedTypeName metadataMethod assemblyPath decompiledMethod k
        | DecompilerServices.DecompilationResult.DecompilationError ->
            failwith (sprintf "WARNING: Could not decompile %s.%s" qualifiedTypeName metadataMethod.Name)

    and reduceFunctionSignature funcId state (ast : IFunctionSignature) this paramValues k =
        let k = Enter ast state k
        let values, areParametersSpecified =
            match paramValues with
            | Specified values -> values, true
            | Unspecified -> [], false
        let valueOrFreshConst (param : Option<IMethodParameter>) value =
            match param, value with
            | None, _ -> internalfail "parameters list is longer than expected!"
            | Some param, None ->
                let stackKey = (param.Name, getTokenBy (Choice1Of2 param))
                if areParametersSpecified then
                    if param.MetadataParameter.HasDefaultValue
                    then
                        let typ = MetadataTypes.variableFromMetadataType param.Type
                        (stackKey, Specified(Concrete (param.MetadataParameter.GetDefaultValue()) typ), Some typ)
                    else internalfail "parameters list is shorter than expected!"
                else (stackKey, Unspecified, MetadataTypes.variableFromMetadataType param.Type |> Types.WrapReferenceType |> Some)
            | Some param, Some value -> ((param.Name, getTokenBy (Choice1Of2 param)), Specified value, None)
        let parameters = List.map2Different valueOrFreshConst ast.Parameters values
        let parametersAndThis =
            match this with
            | Some thisValue ->
                let thisKey = ("this", getThisTokenBy ast)
                (thisKey, Specified thisValue, None)::parameters
            | None -> parameters
        k (parametersAndThis, Memory.NewStackFrame state funcId parametersAndThis)

    and reduceFunction state this parameters funcId (signature : IFunctionSignature) invoke k =
        reduceFunctionSignature funcId state signature this parameters (fun (_, state) ->
        Call funcId state invoke (fun (result, state) -> (ControlFlow.ConsumeBreak result, state) |> k))

    and reduceDecompiledMethod caller state this parameters (ast : IDecompiledMethod) initializerInvoke k =
        let metadataMethod = ast.MetadataMethod
        let invoke (ast : IDecompiledMethod) state k =
            initializerInvoke state (fun (result, state) ->
            reduceBlockStatement state ast.Body (fun (result', state') ->
            ControlFlow.ComposeSequentially result result' state state' |> k))
        let k = Enter caller state k
        if metadataMethod.IsInternalCall then
            // TODO: internal calls should pass throught CallGraph.call too
//            printfn "INTERNAL CALL OF %s.%s" ast.MetadataMethod.DeclaringType.AssemblyQualifiedName metadataMethod.Name
            let fullMethodName = DecompilerServices.metadataMethodToString metadataMethod
            if externalImplementations.ContainsKey(fullMethodName) then
                reduceFunctionSignature {metadataMethod = metadataMethod} state ast.Signature this parameters (fun (argsAndThis, state) ->
                internalCall metadataMethod argsAndThis state k)
            elif concreteExternalImplementations.ContainsKey(fullMethodName) then
                match parameters with
                | Specified parameters ->
                    let parameters' =
                        match this with
                        | Some term -> term::parameters
                        | None -> parameters
                    let extrn = concreteExternalImplementations.[fullMethodName]
                    reduceFunction state None (Specified parameters') {metadataMethod = extrn.MetadataMethod} extrn.Signature (invoke extrn) k
                | _ -> internalfail "internal call with unspecified parameters!"
            else __notImplemented__()
        else
            reduceFunction state this parameters {metadataMethod = ast.MetadataMethod} ast.Signature (invoke ast) k

    and reduceEventAccessExpression state (ast : IEventAccessExpression) k =
        let qualifiedTypeName = ast.EventSpecification.Event.DeclaringType.AssemblyQualifiedName
        initializeStaticMembersIfNeed ast state qualifiedTypeName (fun (result, state) ->
        __notImplemented__())

    and reduceIndexerCallExpression state (ast : IIndexerCallExpression) k =
        let qualifiedTypeName = ast.PropertySpecification.Property.DeclaringType.AssemblyQualifiedName
        initializeStaticMembersIfNeed ast state qualifiedTypeName (fun (result, state) ->
        __notImplemented__())

    and reduceMethodCall (caller : locationBinding) state target (metadataMethod : JetBrains.Metadata.Reader.API.IMetadataMethod) arguments k =
        let qualifiedTypeName = metadataMethod.DeclaringType.AssemblyQualifiedName
        initializeStaticMembersIfNeed caller state qualifiedTypeName (fun (result, state) ->
        target state (fun (targetTerm, state) ->
        Cps.Seq.mapFoldk (fun state arg k -> arg state k) state arguments (fun (args, state) ->
        let invoke state k =
            let assemblyPath = metadataMethod.DeclaringType.Assembly.Location
            let target = if metadataMethod.IsStatic then None else Some targetTerm
            decompileAndReduceMethod caller state target (Specified args) qualifiedTypeName metadataMethod assemblyPath (fun (result', state') ->
            ControlFlow.ComposeSequentially result result' state state' |> k)
        npeOrInvokeExpression caller state metadataMethod.IsStatic targetTerm invoke k)))

    and reduceMethodCallExpression state (ast : IMethodCallExpression) k =
        let reduceTarget state k = reduceExpressionToRef state true ast.Target k
        let reduceArg arg = fun state k -> reduceExpression state arg k
        let reduceArgs = ast.Arguments |> List.ofSeq |> List.map reduceArg
        reduceMethodCall ast state reduceTarget ast.MethodInstantiation.MethodSpecification.Method reduceArgs k

    and reducePropertyAccessExpression state (ast : IPropertyAccessExpression) k =
        let obtainTarget state k = reduceExpressionToRef state true ast.Target k
        reduceMethodCall ast state obtainTarget ast.PropertySpecification.Property.Getter [] (fun (result, state) ->
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
        | null -> k (NoComputation, state)
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
        | :? ICreationExpression as expression -> reduceCreationExpression false state expression k
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

    and reduceCreationExpression toRef state (ast : ICreationExpression) k =
        match ast with
        | :? IAnonymousMethodExpression as expression -> reduceAnonymousMethodExpression state expression k
        | :? IAnonymousObjectCreationExpression as expression -> reduceAnonymousObjectCreationExpression state expression k
        | :? IArrayCreationExpression as expression -> reduceArrayCreationExpression state expression k
        | :? IDelegateCreationExpression as expression -> reduceDelegateCreationExpression state expression k
        | :? ILambdaBlockExpression as expression -> reduceLambdaBlockExpression state expression k
        | :? ILambdaExpression as expression -> reduceLambdaExpression state expression k
        | :? IObjectCreationExpression as expression -> reduceObjectCreationExpression toRef state expression k
        | _ -> __notImplemented__()

// ------------------------------- Delegates and lambdas -------------------------------

    and reduceDelegateCallExpression state (ast : IDelegateCallExpression) k =
        reduceDelegateCall state ast (fun (result, state) -> (ControlFlow.ResultToTerm result, state) |> k)

    and reduceInlinedDelegateCallStatement state (ast : IDelegateCallExpression) k =
        reduceDelegateCall state ast k

    and reduceDelegateCall state (ast : IDelegateCallExpression) k =
        Cps.Seq.mapFoldk reduceExpression state ast.Arguments (fun (args, state) ->
        let curDelegate = Transformations.inlinedCallTarget ast |?? ast.Delegate
        reduceExpression state curDelegate (fun (deleg, state) ->
        let rec invoke state deleg k =
            let k = Enter ast state k
            match deleg.term with
                | HeapRef _ ->
                    let term, state = Memory.Dereference state deleg
                    invoke state term k
                | Lambda(lambda) -> lambda ast state (Specified args) k
                | _ -> __notImplemented__()
        GuardedApplyStatement state deleg invoke k))

    and reduceDelegateCreationExpression state (ast : IDelegateCreationExpression) k =
        let metadataMethod = ast.MethodInstantiation.MethodSpecification.Method
        let qualifiedTypeName = metadataMethod.DeclaringType.AssemblyQualifiedName
        initializeStaticMembersIfNeed ast state qualifiedTypeName (fun (result, state) ->
        reduceExpressionToRef state true ast.Target (fun (targetTerm, state) ->
        let invoke caller state args k =
            let assemblyPath = metadataMethod.DeclaringType.Assembly.Location
            decompileAndReduceMethod caller state (Some targetTerm) args qualifiedTypeName metadataMethod assemblyPath (fun (result', state') ->
            ControlFlow.ComposeSequentially result result' state state' |> k)
        let k = Enter ast state k
        let delegateTerm, state = Functions.MakeLambda state metadataMethod invoke
        let returnDelegateTerm state k = k (Return delegateTerm, state)
        npeOrInvokeExpression ast state metadataMethod.IsStatic targetTerm returnDelegateTerm k))

    and makeLambdaBlockInterpreter (ast : ILambdaBlockExpression) =
        fun caller state args k ->
            let k = Enter caller state k
            let invoke state k = reduceBlockStatement state ast.Body k
            reduceFunction state None args {metadataDelegate = ast} ast.Signature invoke k

    and reduceLambdaBlockExpression state (ast : ILambdaBlockExpression) k =
        let k = Enter ast state k
        Functions.MakeLambda2 state ast.Signature null (makeLambdaBlockInterpreter ast) |> k

    and makeLambdaInterpreter (ast : ILambdaExpression) =
        let invokeBody state k =
            reduceExpression state ast.Body (fun (term, state) -> k (ControlFlow.ThrowOrReturn term, state))
        fun caller state args k ->
            let k = Enter caller state k
            reduceFunction state None args {metadataDelegate = ast} ast.Signature invokeBody k

    and reduceLambdaExpression state (ast : ILambdaExpression) k =
        let k = Enter ast state k
        Functions.MakeLambda2 state ast.Signature null (makeLambdaInterpreter ast) |> k

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
        let lambdaBlock = Transformations.forStatementToRecursion ast
        reduceExpressionStatement state lambdaBlock k

    and reduceLoopStatement state (ast : ILoopStatement) k =
        __notImplemented__()

    and reduceYieldReturnStatement state (ast : IYieldReturnStatement) k =
        __notImplemented__()

// ------------------------------- Linear control flow-------------------------------

    and reduceSequentially state statements k =
        Cps.Seq.foldlk
            (InvokeAfter false)
            (NoResult(), Memory.NewScope state [])
            statements
            (fun (res, state) -> k (res, Memory.PopStack state))

    and reduceBlockStatement state (ast : IBlockStatement) k =
        let compose rs statement k =
            InvokeAfter (Transformations.isContinueConsumer statement) rs (fun state -> reduceStatement state statement) k
        let k = Enter ast state k
        Cps.Seq.foldlk compose (NoResult(), Memory.NewScope state []) ast.Statements (fun (res, state) -> k (res, Memory.PopStack state))

    and reduceCommentStatement state (ast : ICommentStatement) k =
        k (NoComputation, state)

    and reduceEmptyStatement state (ast : IEmptyStatement) k =
        k (NoComputation, state)

    and reduceExpressionStatement state (ast : IExpressionStatement) k =
        if Transformations.isInlinedCall ast
        then
            reduceInlinedDelegateCallStatement state (ast.Expression :?> IDelegateCallExpression) k
        else
            reduceExpression state ast.Expression (fun (term, newState) ->
                k (ControlFlow.ThrowOrIgnore term, newState))

    and reduceLocalVariableDeclarationStatement state (ast : ILocalVariableDeclarationStatement) k =
        let name = ast.VariableReference.Variable.Name
        let k = Enter ast state k
        let initialize k =
            let t = MetadataTypes.fromMetadataType ast.VariableReference.Variable.Type
            match t with
            | StructType _ when ast.Initializer = null -> k (MakeDefault t, state)
            | _ -> reduceExpression state ast.Initializer k
        initialize (fun (initializer, state) ->
            let statementResult = ControlFlow.ThrowOrIgnore initializer
            let allocate state value statementResult k =
                let state' = Memory.AllocateOnStack state (name, getTokenBy (Choice2Of2 ast.VariableReference.Variable)) initializer
                k (statementResult, state')
            failOrInvoke
                statementResult
                state
                (fun () -> allocate state initializer (NoResult()) k)
                (fun _ _ _ state k -> allocate state Nop statementResult k)
                (fun _ _ normal state k -> allocate state (Guarded normal) statementResult k)
                k)

    and reduceReturnStatement state (ast : IReturnStatement) k =
        if ast.Result = null then k (Return Nop, state)
        else reduceExpression state ast.Result (fun (term, state) -> k (ControlFlow.ThrowOrReturn term, state))

// ------------------------------- Conditional operations -------------------------------

    and npeOrInvokeStatement caller state isStatic reference statement k =
        if isStatic then statement state k
        else
            let k = Enter caller state k
            BranchStatementsOnNull state reference
                (fun state k -> RuntimeExceptions.NullReferenceException state Throw |> k)
                statement
                k

    and npeOrInvokeExpression caller state isStatic reference expression k =
        npeOrInvokeStatement caller state isStatic reference expression
            (fun (result, state) -> k (ControlFlow.ResultToTerm result, state))

    and reduceIfStatement state (ast : IIfStatement) k =
        BranchStatements state
            (fun state k -> reduceExpression state ast.Condition k)
            (fun state k -> reduceStatement state ast.Then k)
            (fun state k -> reduceStatement state ast.Else k)
            k

    and reduceConditionalExpression state (ast : IConditionalExpression) k =
        BranchExpressions state
            (fun state k -> reduceExpression state ast.Condition k)
            (fun state k -> reduceExpression state ast.Then k)
            (fun state k -> reduceExpression state ast.Else k)
            k

    and reduceSwitchStatement state (ast : ISwitchStatement) k =
        reduceExpression state ast.Expression (fun (arg, state) ->
        let reduceDefault state k = reduceBlockStatement state ast.Default (fun (result, state) -> k (ControlFlow.ConsumeBreak result, state))
        reduceSwitchCases state arg reduceDefault (List.ofArray ast.Cases) k)

    and reduceSwitchCases state arg dflt (cases : ISwitchCase list) k =
        let t = TypeOf arg |> Types.ToDotNetType
        let compareArg caller (result, state) expression k =
            reduceExpression state expression (fun (value, state) ->
            let k = Enter caller state k
            let equal = arg === value
            k (result ||| equal, state))
        match cases with
        | [] -> dflt state k
        | case::rest ->
            BranchStatements state
                (fun state k -> Cps.Seq.foldlk (compareArg case) (False, state) case.Values k)
                (fun state k -> reduceBlockStatement state case.Body (fun (result, state) -> k (ControlFlow.ConsumeBreak result, state)))
                (fun state k -> reduceSwitchCases state arg dflt rest k)
                k

// ------------------------------- Try-catch -------------------------------

    and failOrInvoke statementResult state notExn trueBranch elseBranch k =
        let thrown, normal = ControlFlow.PickOutExceptions statementResult
        match thrown with
        | None -> notExn()
        | Some(guard, exn) ->
            BranchStatements state
                (fun state k -> k (guard, state))
                (fun state k -> trueBranch guard exn normal state k)
                (fun state k -> elseBranch guard exn normal state k)
                k

    and reduceThrowStatement state (ast : IThrowStatement) k =
        reduceExpression state ast.Argument (fun (arg, state) ->
        let k = Enter ast state k
        BranchStatementsOnNull state arg
            (fun state k -> RuntimeExceptions.NullReferenceException state Throw |> k)
            (fun state k -> k (Throw arg, state))
            k)

    and reduceTryStatement state (ast : ITryStatement) k =
        reduceBlockStatement state ast.Body (fun (result, state) ->
        let k = Enter ast state k
        reduceCatchBlock state result ast.CatchClauses (fun (result, state) ->
        reduceFinally state result ast.Finally (fun (result, state) ->
        reduceFault state result ast.Fault k)))

    and reduceCatchBlock state statementResult (clauses : ICatchClause[]) k =
        if Array.isEmpty clauses then k (statementResult, state)
        else
            failOrInvoke
                statementResult
                state
                (fun () -> k (statementResult, state))
                (fun _ exn _ state k -> reduceCatchClauses exn state (Seq.ofArray clauses) k)
                (fun guard _ restOfUnion state k -> k (Guarded ((guard, NoResult ())::restOfUnion), state))
                k

    and reduceCatchClauses exn state clauses k =
        match clauses with
        | Seq.Empty -> k (Throw exn, state)
        | Seq.Cons(clause, rest) ->
            BranchStatements state
                (fun state k -> reduceCatchCondition exn state clause k)
                (fun state k -> reduceBlockStatement state clause.Body (fun (result, state) -> k (result, Memory.PopStack state)))
                (fun state k -> reduceCatchClauses exn (Memory.PopStack state) rest k)
                k

    and reduceCatchCondition exn state (ast : ICatchClause) k =
        let k = Enter ast state k
        let typeMatches, state =
            if ast.VariableReference = null then (True, Memory.NewScope state []) // just catch {...} case
            else
                DecompilerServices.setPropertyOfNode ast "Thrown" exn
                // catch (...) {...} case
                let targetType = MetadataTypes.fromMetadataType ast.VariableReference.Variable.Type
                let typeMatches, state = Types.CanCast state targetType exn
                let stackKey = ast.VariableReference.Variable.Name, getTokenBy (Choice2Of2 ast.VariableReference.Variable)
                let state = Memory.NewScope state [(stackKey, Specified exn, None)]
                typeMatches, state
        if ast.Filter = null then k (typeMatches, state)
        else
            let k = Enter ast.Filter state k
            let filteringExpression = Transformations.extractExceptionFilter ast.Filter
            BranchStatements state
                (fun state k -> k (typeMatches, state))
                (fun state k -> reduceExpression state filteringExpression
                                    (fun (filterResult, state) ->
                                        k (ControlFlow.ConsumeErrorOrReturn
                                             (always (Return False)) filterResult, state)))
                (fun state k -> k (Return typeMatches, state))
                (fun (result, state) -> k (ControlFlow.ResultToTerm result, state))

    and reduceRethrowStatement state (ast : IRethrowStatement) k =
        let rec findException (node : INode) =
            if node = null then internalfail "exception register not found for rethowing!"
            match DecompilerServices.getPropertyOfNode node "Thrown" null with
            | null -> findException node.Parent
            | exn -> exn :?> term
        let exn = findException ast
        k (Throw exn, state)

    and reduceFinally state statementResult (ast : IBlockStatement) k =
        if ast = null then k (statementResult, state)
        else reduceBlockStatement state ast (fun (_, state) -> k (statementResult, state))

    and reduceEndFinallyStatement state (ast : IEndFinallyStatement) k =
        __notImplemented__()

    and reduceFault state statementResult (ast : IBlockStatement) k =
        if ast = null then k (statementResult, state)
        else
            failOrInvoke
                statementResult
                state
                (fun () -> k (statementResult, state))
                (fun _ _ _ state k -> reduceBlockStatement state ast (fun (_, state) -> k (NoComputation, state)))
                (fun _ _ _ state k -> k (NoComputation, state))
                (fun (_, state) -> k (statementResult, state))

    and reduceSuccessfulFilteringStatement state (ast : ISuccessfulFilteringStatement) k =
        __notImplemented__()

    and reduceUsingStatement state (ast : IUsingStatement) k =
        __notImplemented__()

// ------------------------------- Memory access -------------------------------

    and reduceExpressionToRef state followHeapRefs (ast : IExpression) k =
        let k = Enter ast state k
        match ast with
        | null -> k (MakeNullRef Null, state)
        | :? ILocalVariableReferenceExpression as expression ->
            k (Memory.ReferenceLocalVariable state (expression.Variable.Name, getTokenBy (Choice2Of2 expression.Variable)) followHeapRefs, state)
        | :? IParameterReferenceExpression as expression ->
            k (Memory.ReferenceLocalVariable state (expression.Parameter.Name, getTokenBy (Choice1Of2 expression.Parameter))  followHeapRefs, state)
        | :? IThisReferenceExpression as expression ->
            k (Memory.ReferenceLocalVariable state ("this", getThisTokenBy expression) followHeapRefs, state)
        | :? IFieldAccessExpression as expression ->
            reduceExpressionToRef state true expression.Target (fun (target, state) ->
            referenceToField ast state followHeapRefs target expression.FieldSpecification.Field k)
        | :? IDerefExpression as expression -> reduceExpressionToRef state followHeapRefs expression.Argument k
        | :? ICreationExpression as expression -> reduceCreationExpression true state expression k
        | :? ILiteralExpression as expression -> reduceLiteralExpressionToRef state expression k
        | :? IAddressOfExpression as expression -> reduceAddressOfExpressionToRef state expression k
        | :? ITryCastExpression
        | :? IAbstractTypeCastExpression -> reduceExpression state ast k
        | _ -> __notImplemented__()

    and referenceToField caller state followHeapRefs target (field : JetBrains.Metadata.Reader.API.IMetadataField) k =
        let id = DecompilerServices.idOfMetadataField field
        let typ = MetadataTypes.fromMetadataType field.Type
        let k = Enter caller state k
        if field.IsStatic then
            k (Memory.ReferenceStaticField state followHeapRefs id typ field.DeclaringType.AssemblyQualifiedName)
        else
            k (Memory.ReferenceField state followHeapRefs id typ target)

    and reduceArrayElementAccessExpression state (ast : IArrayElementAccessExpression) k =
        reduceExpression state ast.Array (fun (arrayRef, state) ->
        Cps.Seq.mapFoldk reduceExpression state ast.Indexes (fun (indices, state) ->
        let k = Enter ast state k
        let reference, state = Memory.ReferenceArrayIndex state arrayRef indices
        k (Memory.Dereference state reference)))

    and reduceBaseReferenceExpression state (ast : IBaseReferenceExpression) k =
        let k = Enter ast state k
        k (Memory.DereferenceLocalVariable state ("this", getThisTokenBy ast))

    and reduceBoxExpression state (ast : IBoxExpression) k =
        __notImplemented__()

    and reduceCheckFiniteExpression state (ast : ICheckFiniteExpression) k =
        __notImplemented__()

    and reduceDefaultValueExpression state (ast : IDefaultValueExpression) k =
        let k = Enter ast state k
        (ast.Type |> MetadataTypes.fromMetadataType |> MakeDefault, state) |> k

    and reduceDerefExpression state (ast : IDerefExpression) k =
        reduceExpression state ast.Argument (fun (reference, state) ->
        let k = Enter ast state k
        k (Memory.Dereference state reference))

    and reduceFieldAccessExpression state (ast : IFieldAccessExpression) k =
        let qualifiedTypeName = ast.FieldSpecification.Field.DeclaringType.AssemblyQualifiedName
        initializeStaticMembersIfNeed ast state qualifiedTypeName (fun (statementResult, state) ->
            let readFieldLocal () =
                reduceExpressionToRef state true ast.Target (fun (target, state) ->
                readField ast state target ast.FieldSpecification.Field k)
            failOrInvoke
                statementResult
                state
                (fun () -> readFieldLocal ())
                (fun _ _ _ state k -> k (statementResult, state))
                (fun _ _ _ state k -> readFieldLocal ())
                (fun (r, s) -> k ((ControlFlow.ResultToTerm r), s)))

    and readField caller state target (field : JetBrains.Metadata.Reader.API.IMetadataField) k =
        let fieldName = DecompilerServices.idOfMetadataField field
        let fieldType = MetadataTypes.fromMetadataType field.Type
        let k = Enter caller state k
        if field.IsStatic then
            let reference, state = Memory.ReferenceStaticField state false fieldName fieldType field.DeclaringType.AssemblyQualifiedName
            k (Memory.Dereference state reference)
        else
            let reference, state = Memory.ReferenceField state false fieldName fieldType target
            Memory.Dereference state reference |> k

    and reduceLiteralExpression state (ast : ILiteralExpression) k =
        let mType = MetadataTypes.fromMetadataType ast.Value.Type
        let k = Enter ast state k
        let obj = ast.Value.Value
        match mType with
        | Core.String ->
            let stringLength = String.length (obj.ToString())
            MakeString stringLength obj |> Memory.AllocateInHeap state |> k
        | Core.Null -> k (Terms.MakeNullRef Null, state)
        | _ -> k (Concrete obj mType, state)

    and reduceLocalVariableReferenceExpression state (ast : ILocalVariableReferenceExpression) k =
        let k = Enter ast state k
        k (Memory.DereferenceLocalVariable state (ast.Variable.Name, getTokenBy (Choice2Of2 ast.Variable)))

    and reduceParameterReferenceExpression state (ast : IParameterReferenceExpression) k =
        let k = Enter ast state k
        k (Memory.DereferenceLocalVariable state (ast.Parameter.Name, getTokenBy (Choice1Of2 ast.Parameter)))

    and reduceThisReferenceExpression state (ast : IThisReferenceExpression) k =
        let k = Enter ast state k
        k (Memory.DereferenceLocalVariable state ("this", getThisTokenBy ast))

// ------------------------------- Binary operations -------------------------------

    and reduceAbstractBinaryOperation state (ast : IAbstractBinaryOperationExpression) k =
        match ast with
        | :? IBinaryOperationExpression as binOp -> reduceBinaryOperationExpression state binOp k
        | :? IUserDefinedBinaryOperationExpression as userBinOp -> reduceUserDefinedBinaryOperationExpression state userBinOp k
        | _ -> __notImplemented__()

    and reduceBinaryOperationExpression state (ast : IBinaryOperationExpression) k =
        let op = ast.OperationType
        match op with
        | OperationType.Assignment -> reduceAssignment ast state ast.LeftArgument ast.RightArgument k
        | _ when DecompilerServices.isOperationAssignment op -> reduceOperationAssignment state ast k
        | _ when DecompilerServices.isConditionalOperation op ->
            reduceConditionalOperation state ast.OperationType ast.LeftArgument ast.RightArgument k
        | _ ->
            let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled
            reduceBinaryOperation ast state (DecompilerServices.convertOperation ast.OperationType) ast.LeftArgument ast.RightArgument isChecked (MetadataTypes.getSystemTypeOfNode ast) k

    and reduceUserDefinedBinaryOperationExpression state (ast : IUserDefinedBinaryOperationExpression) k =
        let k = Enter ast state k
        let reduceTarget state k = k (Terms.MakeNullRef (Types.FromDotNetType typedefof<obj>), state)
        let reduceLeftArg state k = reduceExpression state ast.LeftArgument k
        let reduceRightArg state k = reduceExpression state ast.RightArgument k
        reduceMethodCall ast state reduceTarget ast.MethodSpecification.Method [reduceLeftArg; reduceRightArg] k

    and reduceAssignment caller state (left : IExpression) (right : IExpression) k =
        let targetReducer =
            match left with
            | :? IParameterReferenceExpression
            | :? ILocalVariableReferenceExpression
            | :? IPointerIndirectionExpression
            | :? IArrayElementAccessExpression ->
                fun state k -> k (Nop, state)
            | :? IMemberAccessExpression as memberAccess ->
                fun state k -> reduceExpressionToRef state true memberAccess.Target k
            | _ -> __notImplemented__()
        let rightReducer state k = reduceExpression state right k
        mutate caller state left rightReducer targetReducer k

    and reduceOperationAssignment state (ast : IBinaryOperationExpression) k =
        let op = DecompilerServices.getAssignmentOperation ast.OperationType
        let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled
        let t = MetadataTypes.getSystemTypeOfNode ast
        let left = ast.LeftArgument
        readTargeted state left (fun (targetRef, leftTerm, state) ->
        reduceExpression state ast.RightArgument (fun (rightTerm, state) ->
        let k = Enter ast state k
        PerformBinaryOperation op isChecked state t leftTerm rightTerm (fun (result, state) ->
        let obtainTarget state k = k (targetRef, state)
        let obtainResult state k = k (result, state)
        mutate ast state left obtainResult obtainTarget k)))

    and readTargeted state (ast : IExpression) k =
        match ast with
        | :? IParameterReferenceExpression
        | :? ILocalVariableReferenceExpression ->
            reduceExpression state ast (fun (result, state) ->
            k (Nop, result, state))
        | :? IFieldAccessExpression as field ->
            reduceExpressionToRef state true field.Target (fun (targetRef, state) ->
            readField field state targetRef field.FieldSpecification.Field (fun (result, state) ->
            k (targetRef, result, state)))
        | :? IPropertyAccessExpression as property ->
            reduceExpressionToRef state true property.Target (fun (targetRef, state) ->
            let obtainTarget state k = k (targetRef, state)
            reduceMethodCall ast state obtainTarget property.PropertySpecification.Property.Getter [] (fun (result, state) ->
            k (targetRef, result, state)))
        | :? IIndexerCallExpression
        | _ -> __notImplemented__()

    and mutate (caller : locationBinding) state (left : IExpression) right target k =
        // Pre-calculated term is used to support conceptually two different cases: "new A().N = 10" and "new A().N++".
        // In both we mutate fresh instance of A, but second one uses target of previous evaluation of "new A().N".
        // C# compiler generates "dup" instruction in that case.
        match left with
        | :? IParameterReferenceExpression
        | :? ILocalVariableReferenceExpression ->
            reduceExpressionToRef state false left (fun (targetRef, state) ->
            right state (fun (rightTerm, state) ->
            let k = Enter caller state k
            Memory.Mutate state targetRef rightTerm |> k))
        | :? IFieldAccessExpression as field ->
            target state (fun (targetTerm, state) ->
            right state (fun (rightTerm, state) ->
            referenceToField field state false targetTerm field.FieldSpecification.Field (fun (fieldRef, state) ->
            let k = Enter caller state k
            Memory.Mutate state fieldRef rightTerm |> k)))
        | :? IPropertyAccessExpression as property ->
            target state (fun (targetTerm, state) ->
            right state (fun (rightTerm, state) ->
            reduceMethodCall property state target property.PropertySpecification.Property.Setter [right] k))
        | :? IArrayElementAccessExpression as arrayAccess ->
            reduceExpressionToRef state true arrayAccess.Array (fun (array, state) ->
            Cps.Seq.mapFoldk reduceExpression state arrayAccess.Indexes (fun (indices, state) ->
            right state (fun (rightTerm, state) ->
            let leaveArrayAccess = Enter arrayAccess state (fun () -> ())
            let reference, state = Memory.ReferenceArrayIndex state array indices
            leaveArrayAccess()
            let k = Enter caller state k
            Memory.Mutate state reference rightTerm |> k)))
        | :? IPointerIndirectionExpression as pointerIndirection ->
            reduceExpression state pointerIndirection.Argument (fun (targetRef, state) ->
            right state (fun (rightTerm, state) ->
            let k = Enter caller state k
            Memory.Mutate state targetRef rightTerm |> k))
        | :? IIndexerCallExpression
        | _ -> __notImplemented__()

    and reduceBinaryOperation caller state op leftArgument rightArgument isChecked t k =
        reduceExpression state leftArgument (fun (left, state) ->
        reduceExpression state rightArgument (fun (right, state) ->
        let k = Enter caller state k
        PerformBinaryOperation op isChecked state t left right k))

    and reduceConditionalOperation state op leftArgument rightArgument k =
        reduceExpression state leftArgument (fun (left, state') ->
            let condition, combine =
                match op with
                | OperationType.ConditionalAnd ->
                    (fun state k -> k (left, state)), (fun (right, state) -> k (left &&& right, state))
                | OperationType.ConditionalOr ->
                    (fun state k -> k (!!left, state)), (fun (right, state) -> k (left ||| right, state))
                | _ -> internalfailf "unexpected conditional operation %O" op
            BranchExpressions state condition
                (fun state k -> reduceExpression state rightArgument combine)
                (fun state k -> k (left, state))
                k)

// ------------------------------- Unary operations -------------------------------

    and reduceAbstractUnaryOperationExpression state (ast : IAbstractUnaryOperationExpression) k =
        match ast with
        | :? IUnaryOperationExpression as expression -> reduceUnaryOperationExpression state expression k
        | :? IUserDefinedUnaryOperationExpression as expression -> reduceUserDefinedUnaryOperationExpression state expression k
        | _ -> __notImplemented__()

    and reduceUnaryOperationExpression state (ast : IUnaryOperationExpression) k =
        let op = ast.OperationType
        let isChecked = (ast.OverflowCheck = OverflowCheckType.Enabled)
        let dotNetType = MetadataTypes.getSystemTypeOfNode ast
        let t = Types.FromDotNetType dotNetType
        let k = Enter ast state k
        match op with
        | OperationType.PrefixIncrement
        | OperationType.PrefixDecrement -> reducePrefixIncrement state ast k
        | OperationType.PostfixDecrement -> reducePostfixIncrement ast state ast.Argument (Types.CastConcrete -1 dotNetType) isChecked dotNetType k
        | OperationType.PostfixIncrement -> reducePostfixIncrement ast state ast.Argument (Types.CastConcrete  1 dotNetType) isChecked dotNetType k
        | _ ->
            reduceExpression state ast.Argument (fun (arg, newState) ->
            PerformUnaryOperation (DecompilerServices.convertOperation op) isChecked newState t arg k)

    and reduceUserDefinedUnaryOperationExpression state (ast : IUserDefinedUnaryOperationExpression) k =
        __notImplemented__()

    and reducePrefixIncrement state ast k =
        let assignment = Transformations.transformPrefixCrement ast
        reduceOperationAssignment state assignment k

    and reducePostfixIncrement caller state leftAst right isChecked t k =
        let op = Core.OperationType.Add
        readTargeted state leftAst (fun (targetRef, left, state) ->
        let k = Enter caller state k
        PerformBinaryOperation op isChecked state t left right (fun (result, state) ->
        mutate caller state leftAst (fun state k -> k (result, state)) (fun state k -> k (targetRef, state)) (fun (_, state) ->
        k (left, state))))

// ------------------------------- Type casting and type information -------------------------------

    and reduceAbstractTypeCastExpression state (ast : IAbstractTypeCastExpression) k =
        match ast with
        | :? ITypeCastExpression as expression -> reduceTypeCastExpression state expression k
        | :? IUserDefinedTypeCastExpression as expression -> reduceUserDefinedTypeCastExpression state expression k
        | _ -> __notImplemented__()

    and reduceUserDefinedTypeCastExpression state (ast : IUserDefinedTypeCastExpression) k =
        let reduceTarget state k = k (MakeNullRef (Types.FromDotNetType typedefof<obj>), state)
        let reduceArg state k = reduceExpression state ast.Argument k
        reduceMethodCall ast state reduceTarget ast.MethodSpecification.Method [reduceArg] k

    and reduceTryCastExpression state (ast : ITryCastExpression) k =
        let k = Enter ast state k
        let targetType = MetadataTypes.fromMetadataType ast.Type
        reduceExpression state ast.Argument (fun (term, state) ->
        Types.HierarchyCast state term targetType (fun state term typ -> typ |> MakeNullRef |> Return, state) k)

    and reduceTypeCastExpression state (ast : ITypeCastExpression) k =
        let k = Enter ast state k
        let targetType = MetadataTypes.fromMetadataType ast.TargetType
        let isChecked = ast.OverflowCheck = OverflowCheckType.Enabled
        reduceExpression state ast.Argument (fun (term, state) ->
        Types.Cast state term targetType isChecked (fun state _ _ -> RuntimeExceptions.InvalidCastException state Throw) k)

    and reduceCheckCastExpression state (ast : ICheckCastExpression) k =
        let targetType = MetadataTypes.fromMetadataType ast.Type
        let k = Enter ast state k
        reduceExpression state ast.Argument (fun (term, state) ->
        Types.CanCast state targetType term |> k)

    and reduceTypeOfExpression state (ast : ITypeOfExpression) k =
        let instance = MetadataTypes.metadataToDotNetType ast.Type
        let k = Enter ast state k
        k (Types.CastConcrete instance typedefof<Type>, state)

// ------------------------------- Objects construction -------------------------------

    and reduceAnonymousObjectCreationExpression state (ast : IAnonymousObjectCreationExpression) k =
        __notImplemented__()

    and reduceArrayCreationExpression state (ast : IArrayCreationExpression) k =
        let typ = MetadataTypes.fromMetadataType ast.ArrayType
        let k = Enter ast state k
        Cps.Seq.mapFoldk reduceExpression state ast.Dimensions (fun (dimensions, state) ->
        reduceExpressionList state ast.Initializer (fun (initializer, state) ->
        let result =
            match initializer.term with
            | Concrete(null, _) -> MakeDefaultArray dimensions typ
            | _ -> MakeInitializedArray (int ast.ArrayType.Rank) typ initializer
        Memory.AllocateInHeap state result |> k))

    and initializeStaticMembersIfNeed (caller : locationBinding) state qualifiedTypeName k =
        let k = Enter caller state k
        BranchStatements state
            (fun state k -> k (Memory.IsTypeNameInitialized qualifiedTypeName state, state))
            (fun state k ->
                k (NoComputation, state))
            (fun state k ->
                let state = Memory.AllocateDefaultStatic state qualifiedTypeName
                let fieldInitializerExpressions = DecompilerServices.getDefaultFieldValuesOf true false qualifiedTypeName
                let initOneField (name, (typ, expression)) state k =
                    if expression = null then k (NoComputation, state)
                    else
                        let k = Enter expression state k
                        let address, state = Memory.ReferenceStaticField state false name (MetadataTypes.fromMetadataType typ) qualifiedTypeName
                        reduceExpression state expression (fun (value, state) ->
                        let statementResult = ControlFlow.ThrowOrIgnore value
                        let mutate value k =
                            let term, state = Memory.Mutate state address value
                            k (ControlFlow.ThrowOrIgnore term, state)
                        failOrInvoke
                            statementResult
                            state
                            (fun () -> mutate value k)
                            (fun _ exn _ state k ->
                                // TODO: uncomment it when ref and out will be Implemented
                                (* RuntimeExceptions.TypeInitializerException qualifiedTypeName exn state Throw |> k*)
                                k (Throw exn, state))
                            (fun _ _ normal _ k -> mutate (ControlFlow.ResultToTerm (Guarded normal)) k)
                            k)
                let fieldInitializers = Seq.map initOneField fieldInitializerExpressions
                reduceSequentially state fieldInitializers (fun (result, state) ->
                match DecompilerServices.getStaticConstructorOf qualifiedTypeName with
                | Some constr ->
                    reduceDecompiledMethod null state None (Specified []) constr (fun state k -> k (result, state)) k
                | None -> k (result, state)))
            k

    and reduceBaseOrThisConstuctorCall caller state this parameters qualifiedTypeName metadataMethod assemblyPath decompiledMethod k =
        let rec mutateFields this names types values initializers state =
            match names, types, values, initializers with
            | [], [], [], [] -> this, state
            | name::names, typ::types, value::values, initializer::initializers ->
                match value.term with
                | Nop -> mutateFields this names types values initializers state
                | _ ->
                    let leave = Enter initializer state (fun () -> ())
                    let reference, state = Memory.ReferenceField state false name (MetadataTypes.fromMetadataType typ) this
                    let _, state = Memory.Mutate state reference value
                    leave()
                    mutateFields this names types values initializers state
            | _ -> internalfail "unexpected number of initializers"
        let initializeFieldsIfNeed state firstClassTypeInfo secondClassTypeInfo qualifiedTypeName k =
            if firstClassTypeInfo <> secondClassTypeInfo
            then
                let fields = DecompilerServices.getDefaultFieldValuesOf false false qualifiedTypeName
                let names, typesAndInitializers = List.unzip fields
                let types, initializers = List.unzip typesAndInitializers
                match this with
                | Some this ->
                    Cps.List.mapFoldk reduceExpression state initializers (fun (values, state) ->
                    mutateFields this names types values initializers state |> snd |> k)
                | _ -> k state
            else k state
        let baseCtorInfo (metadataMethod : IMetadataMethod) =
            let baseQualifiedTypeName = metadataMethod.DeclaringType.Base.AssemblyQualifiedName
            baseQualifiedTypeName, DecompilerServices.getBaseCtorWithoutArgs baseQualifiedTypeName, DecompilerServices.locationOfType baseQualifiedTypeName
        let composeResult result state k (result', state') = ControlFlow.ComposeSequentially result result' state state' |> k
        match decompiledMethod with
        | DecompilerServices.DecompilationResult.MethodWithExplicitInitializer decompiledMethod ->
            printfn "DECOMPILED MethodWithExplicitInitializer %s:\n%s" qualifiedTypeName (JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
            let initializerMethod = decompiledMethod.Initializer.MethodInstantiation.MethodSpecification.Method
            let initializerQualifiedTypeName = initializerMethod.DeclaringType.AssemblyQualifiedName
            let initializerAssemblyPath = initializerMethod.DeclaringType.Assembly.Location
            let args = decompiledMethod.Initializer.Arguments
            reduceDecompiledMethod caller state this parameters decompiledMethod (fun state k' ->
            initializeFieldsIfNeed state (decompiledMethod.MetadataMethod.DeclaringType) (initializerMethod.DeclaringType) qualifiedTypeName (fun state ->
            Cps.Seq.mapFoldk reduceExpression state args (fun (args, state) ->
            initializeStaticMembersIfNeed caller state initializerQualifiedTypeName (fun (result, state) ->
            decompileAndReduceMethod decompiledMethod state this (Specified args) initializerQualifiedTypeName initializerMethod initializerAssemblyPath (composeResult result state k'))))) k
        | DecompilerServices.DecompilationResult.MethodWithImplicitInitializer decompiledMethod ->
            printfn "DECOMPILED MethodWithImplicitInitializer %s:\n%s" qualifiedTypeName (JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(decompiledMethod))
            let initializerQualifiedTypeName, initializerMethod, initializerAssemblyPath = baseCtorInfo metadataMethod
            reduceDecompiledMethod caller state this parameters decompiledMethod (fun state k' ->
            initializeFieldsIfNeed state (decompiledMethod.MetadataMethod.DeclaringType) (initializerMethod.DeclaringType) qualifiedTypeName (fun state ->
            initializeStaticMembersIfNeed caller state initializerQualifiedTypeName (fun (result, state) ->
            decompileAndReduceMethod caller state this (Specified []) initializerQualifiedTypeName initializerMethod initializerAssemblyPath (composeResult result state k')))) k
        | DecompilerServices.DecompilationResult.DefaultConstuctor ->
            printfn "DECOMPILED default ctor %s" qualifiedTypeName
            let baseCtorQualifiedTypeName, baseCtorMethod, baseCtorAssemblyPath = baseCtorInfo metadataMethod
            initializeFieldsIfNeed state (metadataMethod.DeclaringType) (baseCtorMethod.DeclaringType) qualifiedTypeName (fun state ->
            initializeStaticMembersIfNeed caller state qualifiedTypeName (fun (result, state) ->
            decompileAndReduceMethod caller state this (Specified []) baseCtorQualifiedTypeName baseCtorMethod baseCtorAssemblyPath (composeResult result state k)))
        | DecompilerServices.DecompilationResult.ObjectConstuctor objCtor ->
            printfn "DECOMPILED %s:\n%s" qualifiedTypeName (JetBrains.Decompiler.Ast.NodeEx.ToStringDebug(objCtor))
            initializeFieldsIfNeed state (metadataMethod.DeclaringType) null qualifiedTypeName (fun state ->
            reduceDecompiledMethod caller state this parameters objCtor (fun state k' -> k' (NoComputation, state)) k)
        | _ -> __unreachable__()

    and reduceObjectCreation returnRef (caller : locationBinding) state constructedType objectInitializerList collectionInitializerList (constructorSpecification : MethodSpecification) invokeArguments k =
        let k = Enter caller state k
        let qualifiedTypeName = DecompilerServices.assemblyQualifiedName constructedType
        let freshValue = Memory.MakeDefaultStruct qualifiedTypeName
        let isReferenceType = MetadataTypes.isReferenceType constructedType
        let reference, state =
            if isReferenceType
            then Memory.AllocateInHeap state freshValue
            else
                let tempVar = "constructed instance"
                let state = Memory.NewScope state [((tempVar, tempVar), Specified freshValue, None)]
                (Memory.ReferenceLocalVariable state (tempVar, tempVar) false, state)
        initializeStaticMembersIfNeed caller state qualifiedTypeName (fun (result, state) ->
        let finish r =
            InvokeAfter false r
                (fun state k ->
                    if isReferenceType || returnRef
                    then k (Return reference, state)
                    else
                        let term, state = Memory.Dereference state reference
                        k (Return term, Memory.PopStack state))
                (fun (result, state) -> k (ControlFlow.ResultToTerm result, state))
        let invokeInitializers result state (result', state') =
            let r = ControlFlow.ComposeSequentially result result' state state'
            InvokeAfter false r (fun state k ->
                if objectInitializerList <> null then
                    reduceMemberInitializerList reference state objectInitializerList k
                elif collectionInitializerList <> null then
                    reduceCollectionInitializerList constructedType reference state collectionInitializerList k
                else k (NoComputation, state)
            ) finish
        if constructorSpecification = null
            then invokeInitializers result state (NoResult(), state)
            else
                invokeArguments state (fun (arguments, state) ->
                let assemblyPath = DecompilerServices.locationOfType qualifiedTypeName
                decompileAndReduceMethod caller state (Some reference) (Specified arguments) qualifiedTypeName constructorSpecification.Method assemblyPath (invokeInitializers result state)))

    and reduceObjectCreationExpression toRef state (ast : IObjectCreationExpression) k =
        let arguments state = Cps.List.mapFoldk reduceExpression state (List.ofArray ast.Arguments)
        reduceObjectCreation toRef ast state ast.ConstructedType ast.ObjectInitializer ast.CollectionInitializer ast.ConstructorSpecification arguments k

    and reduceMemberInitializerList initializedObject state (ast : IMemberInitializerList) k =
        let initializers = ast.Initializers |> Seq.map (reduceMemberInitializer initializedObject)
        let k = Enter ast state k
        reduceSequentially state initializers k

    and reduceMemberInitializer this (ast : IMemberInitializer) state k =
        match ast with
        | :? IFieldMemberInitializer as initializer -> reduceFieldMemberInitializer this state initializer k
        | :? IPropertyMemberInitializer as initializer -> reducePropertyMemberInitializer this state initializer k
        | _ -> __notImplemented__()

    and reduceFieldMemberInitializer this state (ast : IFieldMemberInitializer) k =
        reduceExpression state ast.Value (fun (value, state) ->
        let typ = MetadataTypes.fromMetadataType ast.Field.Type
        let k = Enter ast state k
        let fieldReference, state = Memory.ReferenceField state false (DecompilerServices.idOfMetadataField ast.Field) typ this
        let result, state = Memory.Mutate state fieldReference value
        k (ControlFlow.ThrowOrIgnore result, state))

    and reducePropertyMemberInitializer this state (ast : IPropertyMemberInitializer) k =
        reduceMethodCall ast state (fun state k -> k (this, state)) ast.Property.Setter [fun state k -> reduceExpression state ast.Value k] (fun (result, state) -> k (ControlFlow.ThrowOrReturn result, state))

    and reduceCollectionInitializerList constructedType initializedObject state (ast : IExpressionList) k =
        let intializers = ast.Expressions |> Seq.map (reduceCollectionInitializer constructedType initializedObject)
        let k = Enter ast state k
        reduceSequentially state intializers k

    and reduceCollectionInitializer constructedType initializedObject (ast : IExpression) state k =
        let args =
            match ast with
            | :? IExpressionList as es -> es.Expressions :> seq<IExpression>
            | e -> [e] :> seq<IExpression>
        let argTypes = Seq.map DecompilerServices.getTypeOfNode args
        Cps.Seq.mapFoldk reduceExpression state args (fun (argValues, state) ->
        let bestOverload = DecompilerServices.resolveAdd argTypes constructedType
        let reduceTarget state k = k (initializedObject, state)
        let reduceArg arg = (fun state k -> k (arg, state))
        let reduceArgs = argValues |> List.ofSeq |> List.map reduceArg
        reduceMethodCall ast state reduceTarget bestOverload reduceArgs (fun (result, state) ->
        k (ControlFlow.ThrowOrIgnore result, state)))

    and reduceExpressionList state (ast : IExpressionList) k =
        let k = Enter ast state k
        if ast = null then k (Concrete null Core.Void, state)
        else Cps.Seq.mapFoldk reduceExpression state ast.Expressions (fun (terms, state) ->
        k (Concrete terms Core.Void, state))

    and reduceNestedInitializer state (ast : INestedInitializer) k =
        __notImplemented__()

// ------------------------------- Concurrency -------------------------------

    and reduceLockStatement state (ast : ILockStatement) k =
        __notImplemented__()

    and reduceAwaitExpression state (ast : IAwaitExpression) k =
        __notImplemented__()

// ------------------------------- Unsafe code -------------------------------

    and reduceCompileOptimizedExpressionToRef state (ast : IExpression) prefixForGenerator k =
        let uniqueName = IdGenerator.startingWith prefixForGenerator
        let variableName = (uniqueName, uniqueName)
        reduceExpression state ast (fun (term, state) ->
        let k = Enter ast state k
        let state = Memory.AllocateOnStack state variableName term
        let reference = Memory.ReferenceLocalVariable state variableName true
        k (reference, state))

    and reduceLiteralExpressionToRef state (ast : ILiteralExpression) k =
        reduceCompileOptimizedExpressionToRef state ast "literalPtr#!" k

    and reduceAddressOfExpressionToRef state (ast : IAddressOfExpression) k =
        reduceCompileOptimizedExpressionToRef state ast "addressOfPtr#!" k

    and reduceAddressOfExpression state (ast : IAddressOfExpression) k =
        reduceExpressionToRef state true ast.Argument (fun (reference, state) ->
        let k = Enter ast state k
        Types.CastReferenceToPointer state reference k)

    and reduceRefExpression state (ast : IRefExpression) k =
        reduceExpressionToRef state false ast.Argument k

    and reducePointerElementAccessExpression state (ast : IPointerElementAccessExpression) k =
        __notImplemented__()

    and reducePointerIndirectionExpression state (ast : IPointerIndirectionExpression) k =
        let k = Enter ast state k
        reduceExpression state ast.Argument (fun (term, state) -> Memory.Dereference state term |> k)

    and reduceMakeRefExpression state (ast : IMakeRefExpression) k =
        __notImplemented__() // TODO: [C#] __makeref(_) = [IL] mkrefany

    and reduceRefTypeExpression state (ast : IRefTypeExpression) k =
        __notImplemented__() // TODO: [C#] __reftype(_) = [IL] refanytype

    and reduceRefTypeTokenExpression state (ast : IRefTypeTokenExpression) k =
        __notImplemented__() // TODO: what is it?

    and reduceRefValueExpression state (ast : IRefValueExpression) k =
        __notImplemented__() // TODO: [C#] __refvalue(_) = [IL] refanyval

    and reduceSizeOfExpression state (ast : ISizeOfExpression) k =
        let k = Enter ast state k
        let result = ast.Type |> MetadataTypes.fromMetadataType |> Types.SizeOf |> MakeNumber
        k (result, state)

    and reduceStackAllocExpression state (ast : IStackAllocExpression) k =
        __notImplemented__()

    and reduceUntypedStackAllocExpression state (ast : IUntypedStackAllocExpression) k =
        __notImplemented__()

    and reduceFixedStatement state (ast : IFixedStatement) k =
        __notImplemented__()

    and reduceTypeReferenceExpression state (ast : ITypeReferenceExpression) k =
        __notImplemented__()

    and reduceUnboxExpression state (ast : IUnboxExpression) k =
        __notImplemented__()

    and reduceMemoryCopyStatement state (ast : IMemoryCopyStatement) k =
        __notImplemented__() // TODO: [IL] cpblk

    and reduceMemoryInitializeStatement state (ast : IMemoryInitializeStatement) k =
        __notImplemented__() // TODO: [IL] initblk

    and reducePinStatement state (ast : IPinStatement) k =
        __notImplemented__() // TODO: what's this?

    and reduceUnpinStatement state (ast : IUnpinStatement) k =
        __notImplemented__() // TODO: what's this?

    and reduceFieldReferenceExpression state (ast : IFieldReferenceExpression) k =
        __notImplemented__()

    and reduceMethodReferenceExpression state (ast : IMethodReferenceExpression) k =
        __notImplemented__()

    and reduceFunctionPointerCallExpression state (ast : IFunctionPointerCallExpression) k =
        __notImplemented__()

    and reduceVirtualMethodPointerExpression state (ast : IVirtualMethodPointerExpression) k =
        __notImplemented__()

    and reduceMethodPointerExpression state (ast : IMethodPointerExpression) k =
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
        let k = Enter ast state k
        k (Break(), state)

    and reduceContinueStatement state (ast : IContinueStatement) k =
        let k = Enter ast state k
        k (Continue(), state)

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


type internal Activator() =
    interface IActivator with
        member x.CreateInstance caller exceptionType arguments state =
            let assemblyQualifiedName = exceptionType.AssemblyQualifiedName
            let assemblyLocation = exceptionType.Assembly.Location
            let decompiledClass = DecompilerServices.decompileClass (DecompilerServices.jetBrainsFileSystemPath assemblyLocation) assemblyQualifiedName
            let methods = decompiledClass.TypeInfo.GetMethods()
            let invokeArguments state k = k (arguments, state)
            let argumentsLength = List.length arguments
            let argumentsTypes =
                List.map (TypeOf >> function | Reference t -> t | t -> t) arguments
            let ctorMethods =
                methods
                |> List.ofArray
                |> List.filter (fun (m : IMetadataMethod)
                                    -> m.Name = ".ctor"
                                        && m.Parameters.Length = argumentsLength
                                        && m.Parameters
                                            |> Seq.forall2 (fun p1 p2 -> (MetadataTypes.metadataToDotNetType p2.Type).IsAssignableFrom(Types.ToDotNetType p1)) argumentsTypes)
            assert(List.length ctorMethods = 1)
            let ctor = List.head ctorMethods
            let methodSpecification = new MethodSpecification(ctor, Array.map (fun (p : IMetadataParameter) -> p.Type) ctor.Parameters)
            Interpreter.reduceObjectCreation false caller state (DecompilerServices.resolveType exceptionType) null null methodSpecification invokeArguments id

type internal SymbolicInterpreter() =
    interface IInterpreter with
        member x.Reset() = API.Reset()
        member x.InitEntryPoint state epDeclaringType k =
            Interpreter.initialize state (fun state ->
            Interpreter.initializeStaticMembersIfNeed null state epDeclaringType (snd >> k))
        member x.Invoke funcId state this k =
            API.SaveConfiguration()
            let k = Interpreter.restoreBefore k
            match funcId with
            | :? MetadataMethodIdentifier as m ->
                let mm = m.metadataMethod
                Interpreter.decompileAndReduceMethod null state this Unspecified mm.DeclaringType.AssemblyQualifiedName mm mm.Assembly.Location k
            | :? DelegateIdentifier as d ->
                let ast = d.metadataDelegate
                match ast with
                | :? ILambdaBlockExpression as lbe -> Interpreter.makeLambdaBlockInterpreter lbe ast state Unspecified k
                | :? ILambdaExpression as le -> Interpreter.makeLambdaInterpreter le ast state Unspecified k
                | _ -> __notImplemented__()
            | _ -> __notImplemented__()
