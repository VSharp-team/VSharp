namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections
open CilStateOperations
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open ipOperations
open Instruction

type cfg = CfgInfo

module internal TypeUtils =
    open Types

    // TODO: get all this functions from Core #mbdo
    let float64Type = Numeric typedefof<double>
    let float32Type = Numeric typedefof<float32>
    let int8Type    = Numeric typedefof<int8>
    let int16Type   = Numeric typedefof<int16>
    let int32Type   = Numeric typedefof<int32>
    let int64Type   = Numeric typedefof<int64>
    let uint8Type   = Numeric typedefof<uint8>
    let uint16Type  = Numeric typedefof<uint16>
    let uint32Type  = Numeric typedefof<uint32>
    let uint64Type  = Numeric typedefof<uint64>
    let charType    = Numeric typedefof<char>
    let nativeint   = Numeric typedefof<nativeint>

    let isEnum typ = typ |> ToDotNetType |> (fun t -> t.IsEnum)

    // [NOTE] there is no enums, because pushing to evaluation stack causes cast
    let rec signed2unsignedOrId = function
        | Numeric (Id typ) when typ = typedefof<int32> || typ = typedefof<uint32> -> uint32Type
        | Numeric (Id typ) when typ = typedefof<int8>  || typ = typedefof<uint8>  -> uint8Type
        | Numeric (Id typ) when typ = typedefof<int16> || typ = typedefof<uint16> -> uint16Type
        | Numeric (Id typ) when typ = typedefof<int64> || typ = typedefof<uint64> -> uint64Type
        | _ -> __unreachable__()

    let integers = [charType; int8Type; int16Type; int32Type; int64Type; uint8Type; uint16Type; uint32Type; uint64Type]
    let longs = [int64Type; uint64Type]

    let isIntegerTermType typ = integers |> List.contains typ || isEnum typ
    let isFloatTermType typ = typ = float32Type || typ = float64Type
    let isInteger = Terms.TypeOf >> isIntegerTermType
    let isLong term = List.contains (TypeOf term) longs
    let isBool = Terms.TypeOf >> IsBool
    let (|Int8|_|) t = if Terms.TypeOf t = int8Type then Some() else None
    let (|UInt8|_|) t = if Terms.TypeOf t = uint8Type then Some() else None
    let (|Int16|_|) t = if Terms.TypeOf t = int16Type then Some() else None
    let (|UInt16|_|) t = if Terms.TypeOf t = uint16Type then Some() else None
    let (|Int32|_|) t = if Terms.TypeOf t = int32Type then Some() else None
    let (|UInt32|_|) t = if Terms.TypeOf t = uint32Type then Some() else None
    let (|Int64|_|) t = if Terms.TypeOf t = int64Type then Some() else None
    let (|UInt64|_|) t = if Terms.TypeOf t = uint64Type then Some() else None
    let (|Bool|_|) t = if isBool t then Some() else None
    let (|Float32|_|) t = if Terms.TypeOf t = float32Type then Some() else None
    let (|Float64|_|) t = if Terms.TypeOf t = float64Type then Some() else None
    let (|Float|_|) t = if Terms.TypeOf t |> isFloatTermType then Some() else None

    module Char =
        let Zero = MakeNumber Unchecked.defaultof<char>
    module Int8 =
        let Zero = MakeNumber 0y
    module UInt8 =
        let Zero = MakeNumber 0uy
    module Int16 =
        let Zero = MakeNumber 0s
    module UInt16 =
        let Zero = MakeNumber 0us
    module Int32 =
        let Zero = MakeNumber 0
        let One = MakeNumber 1
        let MinusOne = MakeNumber -1
        let MinValue = MakeNumber System.Int32.MinValue
        let MaxValue = MakeNumber System.Int32.MaxValue
    module UInt32 =
        let Zero = MakeNumber 0u
        let MaxValue = MakeNumber UInt32.MaxValue
    module Int64 =
        let Zero = MakeNumber 0L
        let MinusOne = MakeNumber -1L
        let MinValue = MakeNumber System.Int64.MinValue
        let MaxValue = MakeNumber System.Int64.MaxValue
    module UInt64 =
        let Zero = MakeNumber 0UL
        let MaxValue = MakeNumber UInt64.MaxValue

module internal InstructionsSet =

    let mutable reportError : cilState -> unit = fun _ -> ()

    let idTransformation term k = k term

    // --------------------------------------- Metadata Interaction ----------------------------------------

    let resolveFieldFromMetadata (cfg : cfg) = TokenResolver.resolveFieldFromMetadata cfg.MethodBase cfg.IlBytes
    let resolveTypeFromMetadata (cfg : cfg) = TokenResolver.resolveTypeFromMetadata cfg.MethodBase cfg.IlBytes
    let resolveTermTypeFromMetadata (cfg : cfg) = resolveTypeFromMetadata cfg >> Types.FromDotNetType
    let resolveMethodFromMetadata (cfg : cfg) = TokenResolver.resolveMethodFromMetadata cfg.MethodBase cfg.IlBytes
    let resolveTokenFromMetadata (cfg : cfg) = TokenResolver.resolveTokenFromMetadata cfg.MethodBase cfg.IlBytes

    // ------------------------------- Environment interaction -------------------------------

    let rec internalCall (methodInfo : MethodInfo) (argsAndThis : term list) cilState k =
        let s = cilState.state
        let parameters : obj [] =
            // Sometimes F# compiler merges tuple with the rest arguments!
            match methodInfo.GetParameters().Length with
            | 2 -> [| s; argsAndThis |]
// TODO: this should be now broken!
//            | 6 -> [| s.stack; s.heap; s.statics; s.frames; s.pc; argsAndThis |]
            | _ -> __notImplemented__()
        let result =
            try
                methodInfo.Invoke(null, parameters)
            with
            | :? TargetInvocationException as targetException ->
                Logger.trace "InternalCall got TargetInvocationException %s" targetException.Message
                let actualException = targetException.GetBaseException()
                Logger.trace "TargetInvocationException.GetBaseException %s" actualException.Message
                raise actualException
            | e ->
                Logger.trace "InternalCall got exception %s" e.Message
                reraise()

        let pushOnEvaluationStack (term : term, cilState : cilState) =
            match term.term with
            | Nop -> ()
            | _ -> push term cilState
        match result with
        | :? term as r ->
            let cilState = changeState cilState s
            pushOnEvaluationStack(r, cilState); k [cilState]
        | :? ((term * state) list) as r ->
            r |> List.map (fun (t, s) -> let s' = changeState cilState s in pushOnEvaluationStack(t, s'); s') |> k
        | _ -> internalfail "internal call should return tuple term * state!"

    let isFSharpInternalCall method = Map.containsKey (Reflection.fullGenericMethodName method) Loader.FSharpImplementations
    // ------------------------------- CIL instructions -------------------------------

    let referenceLocalVariable index (methodBase : MethodBase) =
        let lvi = methodBase.GetMethodBody().LocalVariables.[index]
        let stackKey = LocalVariableKey(lvi, methodBase)
        Ref (PrimitiveStackLocation stackKey)
    let getArgTerm index (methodBase : MethodBase) =
        let pi = methodBase.GetParameters().[index]
        PrimitiveStackLocation (ParameterKey pi) |> Ref

    let castUnchecked typ term : term = Types.Cast term typ
    let ldc numberCreator t (cfg : cfg) shiftedOffset (cilState : cilState) =
        let num = numberCreator cfg.IlBytes shiftedOffset
        let termType = Types.FromDotNetType t
        push (Concrete num termType) cilState

    let ldloc numberCreator (cfg : cfg) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.IlBytes shiftedOffset
        let reference = referenceLocalVariable index cfg.MethodBase
        let term = Memory.Read cilState.state reference
        push term cilState

    let ldarg numberCreator (cfg : cfg) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.IlBytes shiftedOffset
        let arg =
            let state = cilState.state
            let this = if cfg.MethodBase.IsStatic then None else Some <| Memory.ReadThis state cfg.MethodBase
            match this, cfg.MethodBase.IsStatic with
            | None, _
            | Some _, true ->
                let term = getArgTerm argumentIndex cfg.MethodBase
                Memory.Read state term
            | Some this, _ when argumentIndex = 0 -> this
            | Some _, false ->
                let term = getArgTerm (argumentIndex - 1) cfg.MethodBase
                Memory.Read state term
        push arg cilState
    let ldarga numberCreator (cfg : cfg) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.IlBytes shiftedOffset
        let address =
            let this = if cfg.MethodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.MethodBase
            match this with
            | None -> getArgTerm argumentIndex cfg.MethodBase
            | Some _ when argumentIndex = 0 -> internalfail "can't load address of ``this''"
            | Some _ -> getArgTerm (argumentIndex - 1) cfg.MethodBase
        push address cilState
    let stloc numberCreator (cfg : cfg) shiftedOffset (cilState : cilState) =
        let variableIndex = numberCreator cfg.IlBytes shiftedOffset
        let right = pop cilState
        let location = referenceLocalVariable variableIndex cfg.MethodBase
        let typ = TypeOfLocation location
        let value = Types.Cast right typ
        ConfigureErrorReporter (changeState cilState >> reportError)
        let states = Memory.Write cilState.state location value
        states |> List.map (changeState cilState)
    let private simplifyConditionResult state res k =
        if Contradicts state !!res then k True
        elif Contradicts state res then k False
        else k res
    let performCILUnaryOperation op (cilState : cilState) =
        let x = pop cilState
        API.PerformUnaryOperation op x (fun interimRes ->
        let res = if Terms.TypeOf x |> Types.IsBool then simplifyConditionResult cilState.state interimRes id else interimRes
        push res cilState)

    let performCILBinaryOperation op operand1Transform operand2Transform resultTransform (cilState : cilState) =
        let arg2, arg1 = pop2 cilState
        operand1Transform arg1 (fun arg1 ->
        operand2Transform arg2 (fun arg2 ->
        API.PerformBinaryOperation op arg1 arg2 (fun interimRes ->
        resultTransform interimRes (fun res ->
        push res cilState))))
    let standardPerformBinaryOperation op =
        performCILBinaryOperation op idTransformation idTransformation idTransformation
    let dup (cilState : cilState) =
        let x = pop cilState
        push x cilState
        push x cilState

    let isCallIp (ip : ip) =
        match ip with
        | Instruction(offset, m) ->
            let opCode = parseInstruction m offset
            isDemandingCallOpCode opCode
        | _ -> false

    let ret (cfg : cfg) (cilState : cilState) =
        let resultTyp = Reflection.getMethodReturnType cfg.MethodBase |> Types.FromDotNetType
        if resultTyp <> Void then
            let res = pop cilState
            let castedResult = Types.Cast res resultTyp
            push castedResult cilState
        match cilState.ipStack with
        | _ :: ips -> cilState.ipStack <- (Exit cfg.MethodBase) :: ips
        | [] -> __unreachable__()

    let transform2BooleanTerm pc (term : term) =
        let check term =
            match TypeOf term with
            | Bool -> term
            | t when t = TypeUtils.charType -> term !== TypeUtils.Char.Zero
            | t when t = TypeUtils.int8Type -> term !== TypeUtils.Int8.Zero
            | t when t = TypeUtils.uint8Type -> term !== TypeUtils.UInt8.Zero
            | t when t = TypeUtils.int16Type -> term !== TypeUtils.Int16.Zero
            | t when t = TypeUtils.uint16Type -> term !== TypeUtils.UInt16.Zero
            | t when t = TypeUtils.int32Type -> term !== TypeUtils.Int32.Zero
            | t when t = TypeUtils.uint32Type -> term !== TypeUtils.UInt32.Zero
            | t when t = TypeUtils.int64Type -> term !== TypeUtils.Int64.Zero
            | t when t = TypeUtils.uint64Type -> term !== TypeUtils.UInt64.Zero
            | Numeric(Id t) when t.IsEnum ->
                term !== MakeNumber (Activator.CreateInstance t)
            | _ when IsReference term -> !!(IsNullReference term)
            | _ -> __notImplemented__()
        GuardedApplyExpressionWithPC pc term check

    let binaryOperationWithBoolResult op operand1Transformation operand2Transformation (cilState : cilState) =
        performCILBinaryOperation op operand1Transformation operand2Transformation (simplifyConditionResult cilState.state) cilState

    let ceq (cilState : cilState) =
        let y, x = peek2 cilState
        let transform =
            if TypeUtils.isBool x || TypeUtils.isBool y
            then fun t k -> k (transform2BooleanTerm cilState.state.pc t)
            else idTransformation
        binaryOperationWithBoolResult OperationType.Equal transform transform cilState
    let starg numCreator (cfg : cfg) offset (cilState : cilState) =
        let argumentIndex = numCreator cfg.IlBytes offset
        let argTerm =
           let this = if cfg.MethodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.MethodBase
           match this with
           | None -> getArgTerm argumentIndex cfg.MethodBase
           | Some this when argumentIndex = 0 -> this
           | Some _ -> getArgTerm (argumentIndex - 1) cfg.MethodBase
        let value = pop cilState
        let states = Memory.Write cilState.state argTerm value
        states |> List.map (changeState cilState)
    let brcommon condTransform (cfgData : cfg) (offset : offset) (cilState : cilState) =
        let cond = pop cilState
        let m = cfgData.MethodBase
        let ipThen, ipElse =
           match conditionalBranchTarget m offset with
           | offsetThen, [offsetElse] -> instruction m offsetThen, instruction m offsetElse
           | _ -> __unreachable__()
        StatedConditionalExecutionCIL cilState
           (fun state k -> k (condTransform <| transform2BooleanTerm state.pc cond, state))
           (fun cilState k -> setCurrentIp ipThen cilState; k [cilState])
           (fun cilState k -> setCurrentIp ipElse cilState; k [cilState])
           id
    let brfalse = brcommon id
    let brtrue = brcommon (!!)
    let applyAndBranch additionalFunction brtrueFunction (cfg : cfg) offset (cilState : cilState) =
        additionalFunction cilState
        brtrueFunction cfg offset cilState
    let boolToInt b =
        BranchExpressions (fun k -> k b) (fun k -> k TypeUtils.Int32.One) (fun k -> k TypeUtils.Int32.Zero) id
    let bitwiseOrBoolOperation bitwiseOp boolOp (cilState : cilState) =
        let arg2, arg1 = peek2 cilState
        let typ1, typ2 = TypeOf arg1, TypeOf arg2
        match typ1, typ2 with
        | Bool, Bool ->
            binaryOperationWithBoolResult boolOp idTransformation idTransformation cilState
        | _ when TypeUtils.isIntegerTermType typ1 && TypeUtils.isIntegerTermType typ2 ->
            standardPerformBinaryOperation bitwiseOp cilState
        | Bool, typ2 when TypeUtils.isIntegerTermType typ2 ->
            let newArg1 = boolToInt arg1
            performCILBinaryOperation bitwiseOp (fun _ k -> k newArg1) idTransformation idTransformation cilState
        | typ1, Bool when TypeUtils.isIntegerTermType typ1 ->
            let newArg2 = boolToInt arg2
            performCILBinaryOperation bitwiseOp idTransformation (fun _ k -> k newArg2) idTransformation cilState
        | typ1, typ2 -> internalfailf "unhandled case for Bitwise operation %O and types: %O %O" bitwiseOp typ1 typ2
    let bitwiseOrBoolNot (cilState : cilState) =
        let arg = peek cilState
        let op =
            match TypeOf arg with
            | Bool -> OperationType.LogicalNot
            | _ -> OperationType.BitwiseNot
        performCILUnaryOperation op cilState
    let retrieveActualParameters (methodBase : MethodBase) (cilState : cilState) =
        let paramsNumber = methodBase.GetParameters().Length
        let parameters, evaluationStack = EvaluationStack.PopMany paramsNumber cilState.state.evaluationStack
        let castParameter parameter (parInfo : ParameterInfo) =
            if Reflection.isDelegateConstructor methodBase && TypeUtils.isPointer parInfo.ParameterType then parameter
            else Types.FromDotNetType parInfo.ParameterType |> Types.Cast parameter
        setEvaluationStack evaluationStack cilState
        Seq.map2 castParameter (List.rev parameters) (methodBase.GetParameters()) |> List.ofSeq

    let makeUnsignedInteger term k =
        match TypeOf term with
        | Bool -> k <| Types.Cast term TypeUtils.uint32Type
        | Numeric (Id t) when t = typeof<double> || t = typeof<float> -> k term
        | Numeric _ as t -> k <| Types.Cast term (TypeUtils.signed2unsignedOrId t) // no specs found about overflows
        | _ -> k term
    let performUnsignedIntegerOperation op (cilState : cilState) =
        let arg2, arg1 = peek2 cilState
        if TypeUtils.isInteger arg1 && TypeUtils.isInteger arg2 then
            performCILBinaryOperation op makeUnsignedInteger makeUnsignedInteger idTransformation cilState
        else internalfailf "arguments for %O are not Integers!" op
    let ldstr (cfg : cfg) offset (cilState : cilState) =
        let stringToken = NumberCreator.extractInt32 cfg.IlBytes (offset + OpCodes.Ldstr.Size)
        let string = cfg.MethodBase.Module.ResolveString stringToken
        let reference = Memory.AllocateString string cilState.state
        push reference cilState
    let allocateValueTypeInHeap v (cilState : cilState) =
        let address = Memory.BoxValueType cilState.state v
        push address cilState
    let ldnull (cilState : cilState) = push NullRef cilState
    let convu (cilState : cilState) =
        let ptr = pop cilState |> MakeIntPtr
        push ptr cilState
    let convi = convu
    let castTopOfOperationalStack targetType (cilState : cilState) =
        let t = pop cilState
        let termForStack = Types.Cast t targetType
        push termForStack cilState
    let ldloca numberCreator (cfg : cfg) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.IlBytes shiftedOffset
        let term = referenceLocalVariable index cfg.MethodBase
        push term cilState
    let switch (cfgData : cfg) offset (cilState : cilState) =
        let m = cfgData.MethodBase
        let value = pop cilState
        let origPc = cilState.state.pc
        let value = makeUnsignedInteger value id
        let checkOneCase (guard, newIp) cilState kRestCases =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (guard, state))
                (fun cilState k -> setCurrentIp newIp cilState; k [cilState])
                (fun otherState k ->
                    otherState.state.pc <- origPc // ignore pc because we always know that cases do not overlap
                    kRestCases otherState k)
        let fallThroughIp, restIps =
            match conditionalBranchTarget m offset with
            | fall, rests -> instruction m fall, List.map (instruction m) rests
        let casesAndOffsets = List.mapi (fun i offset -> value === MakeNumber i, offset) restIps
        let fallThroughGuard = Arithmetics.(>>=) value (makeUnsignedInteger (List.length restIps |> MakeNumber) id)
        Cps.List.foldrk checkOneCase cilState ((fallThroughGuard, fallThroughIp)::casesAndOffsets) (fun _ k -> k []) id
    let ldtoken (cfg : cfg) offset (cilState : cilState) =
        let memberInfo = resolveTokenFromMetadata cfg (offset + OpCodes.Ldtoken.Size)
        let res =
            match memberInfo with // TODO: should create real RuntimeHandle struct #hack
            | :? FieldInfo as fi -> Terms.Concrete fi.FieldHandle (Types.FromDotNetType typeof<RuntimeFieldHandle>)
            | :? Type as t -> Terms.Concrete t.TypeHandle (Types.FromDotNetType typeof<RuntimeTypeHandle>)
            | :? MethodInfo as mi -> Terms.Concrete mi.MethodHandle (Types.FromDotNetType typeof<RuntimeMethodHandle>)
            | _ -> internalfailf "Could not resolve token"
        push res cilState
    let ldftn (cfg : cfg) offset (cilState : cilState) =
        let methodInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Ldftn.Size)
        let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType (methodInfo.GetType()))
        push methodPtr cilState
    let initobj (cfg : cfg) offset (cilState : cilState) =
        let targetAddress = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Initobj.Size)
        let states = Memory.Write cilState.state targetAddress (Memory.DefaultOf typ)
        states |> List.map (changeState cilState)
    let ldind t reportError (cilState : cilState) =
        // TODO: what about null pointers?
        let address = pop cilState
        let castedAddress = if TypeOfLocation address = t then address else Types.Cast address (Pointer t)
        ConfigureErrorReporter (changeState cilState >> reportError)
        let value = Memory.Read cilState.state castedAddress
        push value cilState

    let clt = binaryOperationWithBoolResult OperationType.Less idTransformation idTransformation
    let cgt = binaryOperationWithBoolResult OperationType.Greater idTransformation idTransformation
    let cltun = binaryOperationWithBoolResult OperationType.Less_Un makeUnsignedInteger makeUnsignedInteger
    let bgeHelper (cilState : cilState) =
        let arg1, arg2 = peek2 cilState
        let typ1, typ2 = Terms.TypeOf arg1, Terms.TypeOf arg2
        if Types.IsInteger typ1 && Types.IsInteger typ2 then clt cilState
        elif Types.IsReal typ1 && Types.IsReal typ2 then cltun cilState
        else __notImplemented__()
    let isinst (cfg : cfg) offset (cilState : cilState) =
        let object = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Isinst.Size)
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference object, state))
            (fun cilState k -> push NullRef cilState; k [cilState])
            (fun cilState k ->
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.IsCast cilState.state object typ, state))
                    (fun cilState k -> push object cilState; k [cilState])
                    (fun cilState k -> push NullRef cilState; k [cilState])
                    k)
            id
    let cgtun (cilState : cilState) =
        let arg2, arg1 = peek2 cilState
        if IsReference arg2 && IsReference arg1 then
            binaryOperationWithBoolResult OperationType.NotEqual idTransformation idTransformation cilState
        else binaryOperationWithBoolResult OperationType.Greater_Un makeUnsignedInteger makeUnsignedInteger cilState
    let ldobj (cfg : cfg) offset (cilState : cilState) =
        let address = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldobj.Size)
        let value = Memory.Read cilState.state address
        let typedValue = Types.Cast value typ
        push typedValue cilState
    let stobj reportError (cfg : cfg) offset (cilState : cilState) =
        let src, dest = pop2 cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Stobj.Size)
        let value = Types.Cast src typ
        ConfigureErrorReporter (changeState cilState >> reportError)
        let states = Memory.Write cilState.state dest value
        states |> List.map (changeState cilState)
    let stind valueCast reportError (cilState : cilState) = // TODO: do like ldind #do
        let value, address = pop2 cilState
        let value = valueCast value
        ConfigureErrorReporter (changeState cilState >> reportError)
        let states = Memory.Write cilState.state address value
        states |> List.map (changeState cilState)
    let sizeofInstruction (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Sizeof.Size)
        let size = Types.SizeOf typ
        push (MakeNumber size) cilState
    let leave (cfg : cfg) offset (cilState : cilState) =
        let m = cfg.MethodBase
        let dst = unconditionalBranchTarget m offset
        let ehcs =
            getEHSBytes m
            |> Seq.filter isFinallyClause
            |> Seq.filter (shouldExecuteFinallyClause offset dst)
            |> Seq.sortWith (fun ehc1 ehc2 -> ehc1.handlerOffset - ehc2.handlerOffset)
            |> List.ofSeq
        let currentIp =
            match ehcs with
            | [] -> Instruction(dst, m)
            | e :: ehcs -> leave (Instruction(e.handlerOffset, m)) ehcs dst m
        setCurrentIp currentIp cilState
    let rethrow (cilState : cilState) =
        let state = cilState.state
        assert(Option.isSome state.exceptionsRegister.ExceptionTerm)
        state.exceptionsRegister <- state.exceptionsRegister.TransformToUnhandled()
    let endfilter (cilState : cilState) =
        let value, restStack = EvaluationStack.Pop cilState.state.evaluationStack
        if restStack = EvaluationStack.EmptyStack then
            cilState.filterResult <- Some value
        else __notImplemented__()
    let endfinally _ =
        // Should be handled in makeStep function
        __unreachable__()
    let br (cfgData : cfg) offset (cilState : cilState) =
        let m = cfgData.MethodBase
        let newIp = instruction m (unconditionalBranchTarget m offset)
        setCurrentIp newIp cilState

    // TODO: implement fully (using information about calling method):
    // TODO: - if thisType is a value type and thisType implements method then ptr is passed unmodified
    // TODO: - if thisType is a value type and thisType does not implement method then ptr is dereferenced and boxed
    let constrained (cfg : cfg) offset (cilState : cilState) =
        match findNextInstructionOffsetAndEdges OpCodes.Constrained cfg.IlBytes offset with
        | FallThrough offset ->
            let method = resolveMethodFromMetadata cfg (offset + OpCodes.Callvirt.Size)
            let n = method.GetParameters().Length
            let args, evaluationStack = EvaluationStack.PopMany n cilState.state.evaluationStack
            setEvaluationStack evaluationStack cilState
            let thisForCallVirt = pop cilState
            match thisForCallVirt.term with
            | HeapRef _ -> ()
            | Ref _ when TypeOfLocation thisForCallVirt |> Types.IsValueType ->
                let thisStruct = Memory.Read cilState.state thisForCallVirt
                let heapRef = Memory.BoxValueType cilState.state thisStruct
                push heapRef cilState
                pushMany args cilState
            | Ref _ ->
                let this = Memory.Read cilState.state thisForCallVirt
                push this cilState
                pushMany args cilState
            | _ -> __unreachable__()
        | _ -> __unreachable__()
    let localloc (cilState : cilState) =
        // [NOTE] localloc usually is used for Span
        // So, pushing nullptr, because array will be allocated in Span constructor
        pop cilState |> ignore
        push (MakeNullPtr Void) cilState

    let private fallThroughImpl stackSizeBefore newIp cilState =
        // if not constructing runtime exception
        if not <| isUnhandledError cilState && Memory.CallStackSize cilState.state = stackSizeBefore then
            setCurrentIp newIp cilState

    let fallThrough (cfgData : cfg) offset cilState op =
        assert(not <| isUnhandledError cilState)
        let stackSizeBefore = Memory.CallStackSize cilState.state
        let m = cfgData.MethodBase
        let newIp = instruction m (fallThroughTarget m offset)
        op cfgData offset cilState
        fallThroughImpl stackSizeBefore newIp cilState
        [cilState]

    let forkThrough (cfgData : cfg) offset cilState op =
        assert(not <| isUnhandledError cilState)
        let stackSizeBefore = Memory.CallStackSize cilState.state
        let m = cfgData.MethodBase
        let newIp = instruction m (fallThroughTarget m offset)
        let cilStates = op cfgData offset cilState
        List.iter (fallThroughImpl stackSizeBefore newIp) cilStates
        cilStates

open InstructionsSet

type internal ILInterpreter(isConcolicMode : bool) as this =

    let cilStateImplementations : Map<string, cilState -> term option -> term list -> cilState list> =
        Map.ofList [
            "System.Int32 System.Array.GetLength(this, System.Int32)", this.CommonGetArrayLength
            "System.Int32 System.Array.GetLowerBound(this, System.Int32)", this.GetArrayLowerBound
            "System.Void System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)", this.CommonInitializeArray
            "System.Void System.String.FillStringChecked(System.String, System.Int32, System.String)", this.FillStringChecked
            "System.Void System.Array.Clear(System.Array, System.Int32, System.Int32)", this.ClearArray
            "System.Void System.Array.Copy(System.Array, System.Int32, System.Array, System.Int32, System.Int32, System.Boolean)", this.CopyArrayExtendedForm1
            "System.Void System.Array.Copy(System.Array, System.Int32, System.Array, System.Int32, System.Int32)", this.CopyArrayExtendedForm2
            "System.Void System.Array.Copy(System.Array, System.Array, System.Int32)", this.CopyArrayShortForm
            "System.Char System.String.get_Chars(this, System.Int32)", this.GetChars
        ]

    member x.ConfigureErrorReporter reporter =
        reportError <- reporter

    member private x.Raise createException (cilState : cilState) k =
        createException cilState
        k [cilState]

    member private x.AccessMultidimensionalArray accessor (cilState : cilState) upperBounds indices (k : cilState list -> 'a) =
        let checkArrayBounds upperBounds indices =
            let checkOneBound acc (upperBound, index) =
                let lowerBound = Concrete 0 Types.TLength
                let notTooSmall = Arithmetics.(>>=) index lowerBound
                let notTooLarge = Arithmetics.(<<) index upperBound
                acc &&& notTooSmall &&& notTooLarge
            assert(List.length upperBounds = List.length indices)
            let upperBoundsAndIndices = List.zip upperBounds indices
            List.fold checkOneBound True upperBoundsAndIndices
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (checkArrayBounds upperBounds indices, state))
            accessor
            (x.Raise x.IndexOutOfRangeException)
            k

    member private x.AccessArray accessor (cilState : cilState) upperBound index k =
        x.AccessMultidimensionalArray accessor cilState [upperBound] [index] k

    member private x.AccessArrayDimension accessor (cilState : cilState) (this : term) (dimension : term) =
        let upperBound = Memory.ArrayRank cilState.state this
        x.AccessArray (accessor this dimension) cilState upperBound dimension id

    member private x.CommonGetArrayLength (cilState : cilState) thisOption args =
        match args with
        | [dimensionsKey] ->
            let arrayLengthByDimension arrayRef index cilState (k : cilState list -> 'a) =
                push (Memory.ArrayLengthByDimension cilState.state arrayRef index) cilState
                k [cilState]
            x.AccessArrayDimension arrayLengthByDimension cilState (Option.get thisOption) dimensionsKey
        | _ -> internalfail "unexpected number of arguments"

    member private x.GetArrayLowerBound (cilState : cilState) (this : term option) args =
        match args with
        | [dimension] ->
            let arrayLowerBoundByDimension arrayRef index (cilState : cilState) k =
                push (Memory.ArrayLowerBoundByDimension cilState.state arrayRef index) cilState
                k [cilState]
            x.AccessArrayDimension arrayLowerBoundByDimension cilState (Option.get this) dimension
        | _ -> internalfail "unexpected number of arguments"

    member private x.NpeOrInvokeStatementCIL (cilState : cilState) (this : term) statement (k : cilState list -> 'a) =
         StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference this, state))
            (x.Raise x.NullReferenceException)
            statement
            k

    member private x.CommonInitializeArray (cilState : cilState) _ (args : term list) =
        match args with
        | [arrayRef; handleTerm] ->
            let initArray (cilState : cilState) k =
                Memory.InitializeArray cilState.state arrayRef handleTerm
                k [cilState]
            x.NpeOrInvokeStatementCIL cilState arrayRef (fun cilState k ->
            x.NpeOrInvokeStatementCIL cilState handleTerm initArray k) id
        | _ -> internalfail "unexpected number of arguments"

    member private x.FillStringChecked (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let state = cilState.state
        let dest, destPos, src = args.[0], args.[1], args.[2]
        let srcPos = MakeNumber 0
        let srcLength = Memory.StringLength state src
        let destLength = Memory.StringLength state dest
        let (<<=) = Arithmetics.(<<=)
        let check = srcLength <<= (Arithmetics.Sub destLength destPos)
        let copy (cilState : cilState) k =
            Memory.CopyStringArray cilState.state src srcPos dest destPos srcLength
            k [cilState]
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (check, state))
            copy
            (x.Raise x.IndexOutOfRangeException)
            id

    member private x.ClearArray (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let array, index, length = args.[0], args.[1], args.[2]
        let (>>) = API.Arithmetics.(>>)
        let (<<) = API.Arithmetics.(<<)
        let clearCase (cilState : cilState) k =
            Memory.ClearArray cilState.state array index length
            k [cilState]
        let nonNullCase (cilState : cilState) k =
            let zero = MakeNumber 0
            let lb = Memory.ArrayLowerBoundByDimension cilState.state array zero
            let numOfAllElements = Memory.CountOfArrayElements cilState.state array
            let check = index << lb ||| (Arithmetics.Add index length) >> numOfAllElements ||| length << zero
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (check, state))
                (x.Raise x.IndexOutOfRangeException)
                clearCase
                k
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference array, state))
            (x.Raise x.ArgumentNullException)
            nonNullCase
            id

    member private x.CommonCopyArray (cilState : cilState) src srcIndex dst dstIndex length =
        let state = cilState.state
        let srcType = MostConcreteTypeOfHeapRef state src
        let dstType = MostConcreteTypeOfHeapRef state dst
        let (>>) = API.Arithmetics.(>>)
        let (<<) = API.Arithmetics.(<<)
        let add = Arithmetics.Add
        let zero = TypeUtils.Int32.Zero
        let srcLB = Memory.ArrayLowerBoundByDimension state src zero
        let dstLB = Memory.ArrayLowerBoundByDimension state dst zero
        let srcNumOfAllElements = Memory.CountOfArrayElements state src
        let dstNumOfAllElements = Memory.CountOfArrayElements state dst
        let defaultCase (cilState : cilState) k =
            Memory.CopyArray cilState.state src srcIndex srcType dst dstIndex dstType length
            k [cilState]
        let lengthCheck (cilState : cilState) =
            let check = ((add srcIndex length) >> srcNumOfAllElements) ||| ((add dstIndex length) >> dstNumOfAllElements)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (check, state))
                (x.Raise x.ArgumentException)
                defaultCase
        let indicesCheck (cilState : cilState) =
            // TODO: extended form needs
            let primitiveLengthCheck = (length << zero) ||| (if TypeUtils.isLong length then length >> TypeUtils.Int32.MaxValue else False)
            let srcIndexCheck = (srcIndex << srcLB) ||| (if TypeUtils.isLong srcIndex then srcIndex >> srcNumOfAllElements else False)
            let dstIndexCheck = (dstIndex << dstLB) ||| (if TypeUtils.isLong dstIndex then dstIndex >> dstNumOfAllElements else False)

            StatedConditionalExecutionCIL cilState
                (fun state k -> k (primitiveLengthCheck ||| srcIndexCheck ||| dstIndexCheck, state))
                (x.Raise x.ArgumentOutOfRangeException)
                lengthCheck
        let assignableCheck (cilState : cilState) =
            let srcElemType = Types.ElementType srcType
            let dstElemType = Types.ElementType dstType
            let condition =
                if Types.IsValueType srcElemType then True
                else Types.TypeIsType srcElemType dstElemType
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (condition, state))
                indicesCheck
                (x.Raise x.InvalidCastException)
        let rankCheck (cilState : cilState) =
            if Types.RankOf srcType = Types.RankOf dstType then assignableCheck cilState
            else x.Raise x.RankException cilState
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference src ||| IsNullReference dst, state))
            (x.Raise x.ArgumentNullException)
            rankCheck
            id

    member private x.CopyArrayExtendedForm1 (cilState : cilState) _ (args : term list) =
        assert(List.length args = 6)
        let src, srcIndex, dst, dstIndex, length = args.[0], args.[1], args.[2], args.[3], args.[4]
        x.CommonCopyArray cilState src srcIndex dst dstIndex length

    member private x.CopyArrayExtendedForm2 (cilState : cilState) _ (args : term list) =
        assert(List.length args = 5)
        let src, srcIndex, dst, dstIndex, length = args.[0], args.[1], args.[2], args.[3], args.[4]
        x.CommonCopyArray cilState src srcIndex dst dstIndex length

    member private x.CopyArrayShortForm (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let src, dst, length = args.[0], args.[1], args.[2]
        let state = cilState.state
        let zero = TypeUtils.Int32.Zero
        let srcLB = Memory.ArrayLowerBoundByDimension state src zero
        let dstLB = Memory.ArrayLowerBoundByDimension state src zero
        x.CommonCopyArray cilState src srcLB dst dstLB length

    member private x.GetChars (cilState : cilState) this (args : term list) =
        assert(List.length args = 1)
        match this with
        | Some this ->
            let index = List.item 0 args
            let length = Memory.StringLength cilState.state this
            let getChar cilState k =
                let char = Memory.ReadStringChar cilState.state this index
                push char cilState
                List.singleton cilState |> k
            x.AccessArray getChar cilState length index id
        | None -> internalfailf "String.GetChars: unexpected this %O" this

    member private x.TrustedIntrinsics =
        let intPtr = Reflection.getAllMethods typeof<IntPtr> |> Array.map Reflection.getFullMethodName
        let volatile = Reflection.getAllMethods typeof<System.Threading.Volatile> |> Array.map Reflection.getFullMethodName
//        let comparerType = typeof<System.Collections.Generic.Comparer<obj>>.GetGenericTypeDefinition()
//        let defaultComparer = Reflection.getAllMethods comparerType |> Array.map (fun mi -> mi :> MethodBase)
        let defaultComparer = [|"System.Collections.Generic.Comparer`1[T] System.Collections.Generic.Comparer`1[T].get_Default()"|];
        Array.concat [intPtr; volatile; defaultComparer]

    member private x.IsNotImplementedIntrinsic (methodBase : MethodBase) fullMethodName =
        let isIntrinsic =
            let intrinsicAttr = "System.Runtime.CompilerServices.IntrinsicAttribute"
            methodBase.CustomAttributes |> Seq.exists (fun m -> m.AttributeType.ToString() = intrinsicAttr)
        isIntrinsic && (Array.contains fullMethodName x.TrustedIntrinsics |> not)

    member private x.IsExternalMethod (methodBase : MethodBase) =
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        let isInternalCall = methodBase.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall
        let isPInvokeImpl = methodBase.Attributes.HasFlag(MethodAttributes.PinvokeImpl)
        int isInternalCall <> 0 || isPInvokeImpl

    member private x.InstantiateThisIfNeed state thisOption (methodBase : MethodBase) =
        match thisOption with
        | Some this ->
            let thisType = TypeOfLocation this
            if Types.IsValueType thisType && methodBase.IsConstructor then
                let newThis = Memory.DefaultOf thisType
                let states = Memory.Write state this newThis
                assert(List.length states = 1 && LanguagePrimitives.PhysicalEquality state (List.head states))
        | None -> ()

    member private x.GetFullMethodNameArgsAndThis state (methodBase : MethodBase) =
        let fullyGenericMethod, genericArgs, _ = Reflection.generalizeMethodBase methodBase
        let fullGenericMethodName = Reflection.getFullMethodName fullyGenericMethod
        let wrapType arg = Concrete arg (Types.FromDotNetType typeof<Type>)
        let typeArgs = genericArgs |> Seq.map wrapType |> List.ofSeq
        let termArgs = methodBase.GetParameters() |> Seq.map (Memory.ReadArgument state) |> List.ofSeq
        let args = typeArgs @ termArgs
        let thisOption = if methodBase.IsStatic then None else Some <| Memory.ReadThis state methodBase
        x.InstantiateThisIfNeed state thisOption methodBase
        fullGenericMethodName, args, thisOption

    member private x.InvokeCSharpImplementation (cilState : cilState) fullMethodName thisOption args =
        // TODO: check that all parameters were specified
        let methodInfo = Loader.CSharpImplementations.[fullMethodName]
        let thisOption, args =
            match thisOption, methodInfo.IsStatic with
            | Some this, true -> None, this :: args
            | None, false -> internalfail "Calling non-static concrete implementation for static method"
            | _ -> thisOption, args
        Memory.PopFrame cilState.state
        ILInterpreter.InitFunctionFrame cilState.state methodInfo thisOption (Some args)
        x.InitializeStatics cilState methodInfo.DeclaringType (fun cilState ->
            setCurrentIp (instruction methodInfo 0) cilState
            [cilState])

    member private x.IsArrayGetOrSet (methodBase : MethodBase) =
        let name = methodBase.Name
        (name = "Set" || name = "Get") && typeof<Array>.IsAssignableFrom(methodBase.DeclaringType)

    static member InitFunctionFrame state (method : MethodBase) this paramValues =
        let parameters = method.GetParameters()
        let getParameterType (param : ParameterInfo) = Types.FromDotNetType param.ParameterType
        let values, areParametersSpecified =
            match paramValues with
            | Some values -> values, true
            | None -> [], false
        let localVarsDecl (lvi : LocalVariableInfo) =
            let stackKey = LocalVariableKey(lvi, method)
            (stackKey, None, Types.FromDotNetType lvi.LocalType)
        let locals =
            match method.GetMethodBody() with
            | null -> []
            | body -> body.LocalVariables |> Seq.map localVarsDecl |> Seq.toList
        let valueOrFreshConst (param : ParameterInfo option) value =
            match param, value with
            | None, _ -> internalfail "parameters list is longer than expected!"
            | Some param, None ->
                let stackKey = ParameterKey param
                match areParametersSpecified with
                | true when param.HasDefaultValue ->
                    let typ = getParameterType param
                    (stackKey, Some(Concrete param.DefaultValue typ), typ)
                | true -> internalfail "parameters list is shorter than expected!"
                | _ -> (stackKey, None, getParameterType param)
            | Some param, Some value -> (ParameterKey param, Some value, getParameterType param)
        let parameters = List.map2Different valueOrFreshConst parameters values
        let parametersAndThis =
            match this with
            | Some thisValue ->
                let thisKey = ThisKey method
                (thisKey, Some thisValue, TypeOfLocation thisValue) :: parameters // TODO: incorrect type when ``this'' is Ref to stack
            | None -> parameters
        Memory.NewStackFrame state method (parametersAndThis @ locals)

    member x.InitFunctionFrameCIL (cilState : cilState) (methodBase : MethodBase) this paramValues =
        ILInterpreter.InitFunctionFrame cilState.state methodBase this paramValues
        pushToIp (instruction methodBase 0) cilState

    member private x.InitStaticFieldWithDefaultValue state (f : FieldInfo) =
        assert f.IsStatic
        let fieldType = Types.FromDotNetType f.FieldType
        let value =
            if f.IsLiteral then
                match f.GetValue(null) with // argument means class with field f, so we have null, because f is a static field
                | null -> NullRef
                | :? string as str -> Memory.AllocateString str state
                | v when f.FieldType.IsPrimitive || f.FieldType.IsEnum -> Concrete v fieldType
                | _ -> __unreachable__()
            else Memory.DefaultOf fieldType
        let targetType = Types.FromDotNetType f.DeclaringType
        let fieldId = Reflection.wrapField f
        Memory.WriteStaticField state targetType fieldId value

    member private x.InvokeArrayGetOrSet (cilState : cilState) (methodBase : MethodBase) thisOption args =
        let name = methodBase.Name
        match thisOption with
        | Some arrayRef when name = "Get" ->
            let typ = Reflection.getMethodReturnType methodBase |> Types.FromDotNetType |> Some
            x.LdElemCommon typ cilState arrayRef args
        | Some arrayRef when name = "Set" ->
            let value, indices = List.lastAndRest args
            let typ = TypeOf value |> Some
            x.StElemCommon typ cilState arrayRef indices value
        | _ -> __unreachable__()

    // NOTE: When executing ldsfld, call and so on, we should previously initialize statics.
    // NOTE: Continuation 'whenInitializedCont' contains exploration of current instruction 'I' on ipStack.
    // NOTE: If statics was already initialized or we have no static constructor,
    //       then we can continue to explore instructions in continuation 'whenInitializedCont'.
    // NOTE: Otherwise, we need to add static constructor to ipStack (so queue will explore it instruction by instruction)
    //       without calling continuation 'whenInitializedCont'.
    // NOTE: After exploration of static constructor, queue will contain the same instruction 'I',
    //       that caused statics initialization, so queue will try again to explore this instruction,
    //       but at that moment statics will be already initialized

    member x.InitializeStatics (cilState : cilState) (t : Type) whenInitializedCont =
        let fields = t.GetFields(Reflection.staticBindingFlags)
        match t with
        | _ when t.IsGenericParameter -> whenInitializedCont cilState
        | _ ->
            let termType = Types.FromDotNetType t
            let typeInitialized = Memory.IsTypeInitialized cilState.state termType
            match typeInitialized with
            | True -> whenInitializedCont cilState
            | _ ->
                let staticConstructor = t.GetConstructors(Reflection.staticBindingFlags) |> Array.tryHead
                Seq.iter (x.InitStaticFieldWithDefaultValue cilState.state) fields
                Memory.InitializeStaticMembers cilState.state termType
                match staticConstructor with
                | Some cctor ->
                    // TODO: use InlineMethodBaseCallIfNeed instead (union Interpreter and InterpreterBase)
                    let name = Reflection.getFullMethodName cctor
                    if (name = "System.Void JetBrains.Diagnostics.Log..cctor()"
                        || name = "System.Void System.Environment..cctor()"
                        || name = "System.Void System.Globalization.CultureInfo..cctor()")
                    then whenInitializedCont cilState
                    else
                        x.InitFunctionFrameCIL cilState cctor None (Some [])
                        [cilState]
                | None -> whenInitializedCont cilState
                // TODO: make assumption ``Memory.withPathCondition state (!!typeInitialized)''

    member private x.InlineMethodBaseCallIfNeeded (methodBase : MethodBase) (cilState : cilState) k =
        // [NOTE] Asserting correspondence between ips and frames
        assert(currentMethod cilState = methodBase && currentOffset cilState = Some 0)
        let fullMethodName, args, thisOption = x.GetFullMethodNameArgsAndThis cilState.state methodBase
        let moveIpToExit (cilState : cilState) =
            // [NOTE] else current method non method
            if currentMethod cilState = methodBase then
                setCurrentIp (Exit methodBase) cilState
            cilState
        if Map.containsKey fullMethodName cilStateImplementations then
            let states = cilStateImplementations.[fullMethodName] cilState thisOption args
            List.map moveIpToExit states |> k
        elif Map.containsKey fullMethodName Loader.FSharpImplementations then
            let thisAndArguments = optCons args thisOption
            let moveIp states = List.map moveIpToExit states |> k
            internalCall Loader.FSharpImplementations.[fullMethodName] thisAndArguments cilState moveIp
        elif Map.containsKey fullMethodName Loader.CSharpImplementations then
            x.InvokeCSharpImplementation cilState fullMethodName thisOption args |> k
        // TODO: add Address function for array and return Ptr #do
        elif x.IsArrayGetOrSet methodBase then
            let cilStates = x.InvokeArrayGetOrSet cilState methodBase thisOption args
            List.map moveIpToExit cilStates |> k
        elif x.IsExternalMethod methodBase then
            let stackTrace = Memory.StackTrace cilState.state.stack
            internalfailf "new extern method: %s\nStackTrace:\n%s" fullMethodName stackTrace
        elif x.IsNotImplementedIntrinsic methodBase fullMethodName then
            let stackTrace = Memory.StackTrace cilState.state.stack
            internalfailf "new intrinsic method: %s\nStackTrace:\n%s" fullMethodName stackTrace
        elif methodBase.GetMethodBody() <> null then cilState |> List.singleton |> k
        else internalfailf "non-extern method %s without body!" (Reflection.getFullMethodName methodBase)

    member private x.ArrayMethods (arrayType : Type) =
        let methodsFromHelper = Type.GetType("System.SZArrayHelper") |> Reflection.getAllMethods
        let makeSuitable (m : MethodInfo) =
            if m.IsGenericMethod then m.MakeGenericMethod(arrayType.GetElementType()) else m
        let concreteMethods = Array.map makeSuitable methodsFromHelper
        Array.concat [concreteMethods; Reflection.getAllMethods typeof<Array>; Reflection.getAllMethods arrayType]

    member private x.FindSuitableForInterfaceMethod (targetType : Type) (method : MethodInfo) =
        let interfaceType = method.DeclaringType
        assert interfaceType.IsInterface
        let createSignature (m : MethodInfo) =
            m.GetParameters()
            |> Seq.map (fun p -> p.ParameterType |> Reflection.getFullTypeName)
            |> join ","
        let onlyLastName (m : MethodInfo) =
            match m.Name.LastIndexOf('.') with
            | i when i < 0 -> m.Name
            | i -> m.Name.Substring(i + 1)
        let sign = createSignature method
        let lastName = onlyLastName method
        let methods =
            match targetType with
            | _ when targetType.IsArray -> x.ArrayMethods targetType
            | _ -> targetType.GetInterfaceMap(interfaceType).TargetMethods
        methods |> Seq.find (fun mi -> createSignature mi = sign && onlyLastName mi = lastName)

    member private x.InvokeVirtualMethod (cilState : cilState) calledMethod targetMethod k =
        // Getting this and arguments values by old keys
        let this = Memory.ReadThis cilState.state calledMethod
        let args = calledMethod.GetParameters() |> Seq.map (Memory.ReadArgument cilState.state) |> List.ofSeq
        // Popping frame created for ancestor calledMethod
        popFrameOf cilState
        // Creating valid frame with stackKeys corresponding to actual targetMethod
        x.InitFunctionFrameCIL cilState targetMethod (Some this) (Some args)
        x.InlineMethodBaseCallIfNeeded targetMethod cilState k

    member x.CallVirtualMethodFromTermType (cilState : cilState) termType (calledMethod : MethodInfo) k =
        let targetType = termType |> Types.ToDotNetType
        let genericCalledMethod = if calledMethod.IsGenericMethod then calledMethod.GetGenericMethodDefinition() else calledMethod
        let genericMethodInfo =
            match genericCalledMethod.DeclaringType with
            | i when i.IsInterface -> x.FindSuitableForInterfaceMethod targetType genericCalledMethod
            | _ ->
                let allMethods = Reflection.getAllMethods targetType
                allMethods |> Seq.find (fun mi -> mi.GetBaseDefinition() = genericCalledMethod.GetBaseDefinition())
        let targetMethod =
            if genericMethodInfo.IsGenericMethodDefinition then
                genericMethodInfo.MakeGenericMethod(calledMethod.GetGenericArguments())
            else genericMethodInfo
        if targetMethod.IsAbstract
            then x.CallAbstract targetMethod cilState k
            else x.InvokeVirtualMethod cilState calledMethod targetMethod k

    member x.CallVirtualMethod (ancestorMethod : MethodInfo) (cilState : cilState) (k : cilState list -> 'a) =
        let this = Memory.ReadThis cilState.state ancestorMethod
        let callVirtual (cilState : cilState) this k =
            let baseType = MostConcreteTypeOfHeapRef cilState.state this
            let callForConcreteType typ state k =
                x.CallVirtualMethodFromTermType state typ ancestorMethod k
            let tryToCallForBaseType (cilState : cilState) (k : cilState list -> 'a) =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (API.Types.TypeIsRef state baseType this, state))
                    (callForConcreteType baseType)
                    (fun s k -> x.CallAbstract ancestorMethod s k)
                    k
            let baseDotNetType = Types.ToDotNetType baseType
            if baseDotNetType.IsInterface
                then x.CallAbstract ancestorMethod cilState k
                else tryToCallForBaseType cilState k
        GuardedApplyCIL cilState this callVirtual k

    member x.CallAbstract method cilState k =
        let iie = createInsufficientInformation "Can't call abstract method %O, need more information about the object type" method
        cilState.iie <- Some iie
        k (List.singleton cilState)

    member private x.ConvOvf targetType (cilState : cilState) =
        let supersetsOf =
            PersistentDict.ofSeq [
                TypeUtils.int8Type,    [|TypeUtils.int8Type; TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int16Type,   [|TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int32Type,   [|TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int64Type,   [|TypeUtils.int64Type|]
                TypeUtils.uint8Type,   [|TypeUtils.uint8Type; TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint16Type,  [|TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint32Type,  [|TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint64Type,  [|TypeUtils.uint64Type|]
                TypeUtils.float32Type, [|TypeUtils.float32Type; TypeUtils.float64Type|]
                TypeUtils.float64Type, [|TypeUtils.float64Type|] ]
        let isSubset leftTyp rightTyp = Array.contains rightTyp supersetsOf.[leftTyp]
        let minMaxOf = // TODO: implement big numbers, instead of double #hack
            PersistentDict.ofSeq [
                TypeUtils.int8Type,    (SByte.MinValue  |> double, SByte.MaxValue  |> double)
                TypeUtils.int16Type,   (Int16.MinValue  |> double, Int16.MaxValue  |> double)
                TypeUtils.int32Type,   (Int32.MinValue  |> double, Int32.MaxValue  |> double)
                TypeUtils.int64Type,   (Int64.MinValue  |> double, Int64.MaxValue  |> double)
                TypeUtils.uint8Type,   (Byte.MinValue   |> double, Byte.MaxValue   |> double)
                TypeUtils.uint16Type,  (UInt16.MinValue |> double, UInt16.MaxValue |> double)
                TypeUtils.uint32Type,  (UInt32.MinValue |> double, UInt32.MaxValue |> double)
                TypeUtils.uint64Type,  (UInt64.MinValue |> double, UInt64.MaxValue |> double)
                TypeUtils.float32Type, (Single.MinValue |> double, Single.MaxValue |> double)
                TypeUtils.float64Type, (Double.MinValue |> double, Double.MaxValue |> double) ]
        let getSegment leftTyp rightTyp =
            let min1, max1 = minMaxOf.[leftTyp]
            let min2, max2 = minMaxOf.[rightTyp]
            match min1 < min2, max1 < max2 with
            | true, true   -> min2, max1
            | true, false  -> min2, max2
            | false, true  -> min1, max1
            | false, false -> min1, max2
        let canCastWithoutOverflow term targetTermType =
            let (<<=) = API.Arithmetics.(<<=)
            assert(Terms.TypeOf term |> Types.IsNumeric)
            let termType = Terms.TypeOf term
            if isSubset termType targetTermType then True
            elif termType = TypeUtils.int64Type && targetTermType = TypeUtils.uint64Type then
                let int64Zero = MakeNumber (0 |> int64)
                int64Zero <<= term
            elif termType = TypeUtils.uint64Type && targetTermType = TypeUtils.int64Type then
                let uint64RightBorder = MakeNumber (Int64.MaxValue |> uint64)
                term <<= uint64RightBorder
            else
                let min, max = getSegment termType targetTermType
                let leftBorder  = Concrete min termType // must save type info, because min is int64
                let rightBorder = Concrete max termType // must save type info, because max is int64
                (leftBorder <<= term) &&& (term <<= rightBorder)
        let t = pop cilState
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (canCastWithoutOverflow t targetType, state))
            (fun cilState k ->
                let castedResult = Types.Cast t targetType
                push castedResult cilState
                k [cilState])
            (x.Raise x.OverflowException)
            id

    member private x.ConvOvfUn unsignedSightType targetType (cilState : cilState) =
        let t = pop cilState
        let unsignedT = Types.Cast t unsignedSightType
        push unsignedT cilState
        x.ConvOvf targetType cilState

    member private x.CommonCastClass (cilState : cilState) (term : term) (typ : symbolicType) k =
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference term ||| Types.IsCast state term typ, state))
            (fun cilState k ->
                push (Types.Cast term typ) cilState
                k [cilState])
            (x.Raise x.InvalidCastException)
            k
    member private x.CastClass (cfg : cfg) offset (cilState : cilState) : cilState list =
        let term = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Castclass.Size)
        x.CommonCastClass cilState term typ id

    member private x.PushNewObjResultOnEvaluationStack (cilState : cilState) reference (calledMethod : MethodBase) =
        let valueOnStack =
            if calledMethod.DeclaringType.IsValueType then
                  Memory.Read cilState.state reference
            else reference
        push valueOnStack cilState

    member x.CommonCall (calledMethodBase : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let call cilState k = x.InlineMethodBaseCallIfNeeded calledMethodBase cilState k
        match calledMethodBase.IsStatic with
        | true -> call cilState k
        | false ->
            let this = Memory.ReadThis cilState.state calledMethodBase
            x.NpeOrInvokeStatementCIL cilState this call k
    member x.RetrieveCalledMethodAndArgs (opCode : OpCode) (calledMethodBase : MethodBase) (cilState : cilState) =
        let args = retrieveActualParameters calledMethodBase cilState
        let hasThis = not (calledMethodBase.IsStatic || opCode = OpCodes.Newobj)
        let this = if hasThis then pop cilState |> Some else None
        this, args

    member x.Call (cfg : cfg) offset (cilState : cilState) =
        let calledMethod = resolveMethodFromMetadata cfg (offset + OpCodes.Call.Size)
        let getArgsAndCall cilState =
            let this, args = x.RetrieveCalledMethodAndArgs OpCodes.Call calledMethod cilState
            x.InitFunctionFrameCIL cilState calledMethod this (Some args)
            x.CommonCall calledMethod cilState id
        if isConcolicMode then getArgsAndCall cilState
        else x.InitializeStatics cilState calledMethod.DeclaringType getArgsAndCall
    member x.CommonCallVirt (ancestorMethodBase : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let this = Memory.ReadThis cilState.state ancestorMethodBase
        let call (cilState : cilState) k =
            if ancestorMethodBase.IsVirtual && not ancestorMethodBase.IsFinal then
                let methodInfo = ancestorMethodBase :?> MethodInfo
                x.CallVirtualMethod methodInfo cilState k
            else
                x.InlineMethodBaseCallIfNeeded ancestorMethodBase cilState k
        x.NpeOrInvokeStatementCIL cilState this call k
    member x.CallVirt (cfg : cfg) offset (cilState : cilState) =
        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? Tuple<MethodInfo, term> as tuple, _) -> snd tuple, (fst tuple :> MethodBase)
            | _ -> __unreachable__()
        let ancestorMethod = resolveMethodFromMetadata cfg (offset + OpCodes.Call.Size)
        let thisOption, args = x.RetrieveCalledMethodAndArgs OpCodes.Callvirt ancestorMethod cilState
        let this, methodToCall =
            match thisOption with
            | Some this when Reflection.isDelegate ancestorMethod && this <> NullRef ->
                let deleg = Memory.ReadDelegate cilState.state this
                let target, mi = retrieveMethodInfo deleg
                // [NOTE] target is ref to closure: when we have it, 'this' = target, otherwise 'this' = thisOption
                if target = NullRef then thisOption, mi else Some target, mi
            | _ -> thisOption, ancestorMethod
        // NOTE: there is no need to initialize statics, because they were initialized before ``newobj'' execution
        // NOTE: It is not quite strict to InitFunctionFrame here because, but it does not matter because signatures of virtual methods are the same
        x.InitializeStatics cilState methodToCall.DeclaringType (fun cilState ->
        // [NOTE] If DeclaringType has static constructor, InitializeStatics will add new state to queue.
        //        But arguments and this was already popped, so when execution will return to callvirt,
        //        evaluation stack won't contain arguments and this.
        //        For this purpose initializing statics on cilState with this and arguments,
        //        after that popping them again.
///// TODO: can we pop args BEFORE calling static constructor? if yes, remove the comment above. If not, this code should be overwritten
//        let _, _ = x.RetrieveCalledMethodAndArgs OpCodes.Callvirt ancestorMethod cilState
        x.InitFunctionFrameCIL cilState methodToCall this (Some args)
        x.CommonCallVirt methodToCall cilState id)

    member x.ReduceArrayCreation (arrayType : Type) (cilState : cilState) (lengths : term list) k =
        let arrayTyp = Types.FromDotNetType arrayType
        Memory.AllocateDefaultArray cilState.state lengths arrayTyp |> k

    member x.CommonCreateDelegate (ctor : ConstructorInfo) (cilState : cilState) (args : term list) k =
        let target, methodPtr =
            assert(List.length args = 2)
            args.[0], args.[1]
        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? MethodInfo as mi, _) -> mi
            | _ -> __unreachable__()
        let typ = Types.FromDotNetType ctor.DeclaringType
        let lambda = Lambdas.make (retrieveMethodInfo methodPtr, target) typ
        Memory.AllocateDelegate cilState.state lambda |> k

    member x.CommonNewObj isCallNeeded (constructorInfo : ConstructorInfo) (cilState : cilState) (args : term list) (k : cilState list -> 'a) : 'a =
        __notImplemented__()
//        let typ = constructorInfo.DeclaringType
//        let constructedTermType = typ |> Types.FromDotNetType cilState.state
//        let blockCase (cilState : cilState) =
//            let callConstructor (cilState : cilState) reference afterCall =
//                if isCallNeeded then
//                    methodInterpreter.ReduceFunctionSignatureCIL cilState constructorInfo (Some reference) (Specified args) false (fun cilState ->
//                    x.InlineMethodBaseCallIfNeeded constructorInfo cilState afterCall)
//                else withResultState reference cilState.state |> changeState cilState |> List.singleton
//            let referenceTypeCase (cilState : cilState) =
//                let ref, state = Memory.AllocateDefaultClass cilState.state constructedTermType
//                callConstructor (withState state cilState) ref (List.map (pushToOpStack ref))
//            let valueTypeCase (cilState : cilState) =
//                let freshValue = Memory.DefaultOf constructedTermType
//                let ref, state = Memory.AllocateTemporaryLocalVariable cilState.state typ freshValue
//                let modifyResult (cilState : cilState) =
//                    let value = Memory.ReadSafe cilState.state ref
//                    pushToOpStack value cilState
//                callConstructor (withState state cilState) ref (List.map modifyResult)
//            if Types.IsValueType constructedTermType then valueTypeCase cilState
//            else referenceTypeCase cilState
//        let nonDelegateCase (cilState : cilState) =
//            if typ.IsArray && constructorInfo.GetMethodBody() = null
//                then x.ReduceArrayCreation typ cilState args id
//                else blockCase cilState
//        if Reflection.IsDelegateConstructor constructorInfo
//            then x.CommonCreateDelegate constructorInfo cilState args k
//            else nonDelegateCase cilState |> k

    member x.NewObj (cfg : cfg) offset (cilState : cilState) =
        let calledMethod = resolveMethodFromMetadata cfg (offset + OpCodes.Newobj.Size)
        assert calledMethod.IsConstructor
        let constructorInfo = calledMethod :?> ConstructorInfo
        let typ = constructorInfo.DeclaringType
        x.InitializeStatics cilState constructorInfo.DeclaringType (fun cilState ->
        let this, args = x.RetrieveCalledMethodAndArgs OpCodes.Newobj calledMethod cilState
        assert(Option.isNone this)
        let constructedTermType = Types.FromDotNetType typ
        let wasConstructorInlined stackSizeBefore (afterCall : cilState) =
            // [NOTE] For example, if constructor is external call, it will be inlined and executed simultaneously
            Memory.CallStackSize afterCall.state = stackSizeBefore
        let modifyValueResultIfConstructorWasCalled stackSizeBefore (afterCall : cilState) =
            if wasConstructorInlined stackSizeBefore afterCall then pushNewObjForValueTypes afterCall
            else ()

        let blockCase (cilState : cilState) =
            let callConstructor (cilState : cilState) reference afterCall =
                x.InitFunctionFrameCIL cilState constructorInfo (Some reference) (Some args)
                x.InlineMethodBaseCallIfNeeded constructorInfo cilState afterCall

            if Types.IsValueType constructedTermType || TypeUtils.isPointer typ then
                let freshValue = Memory.DefaultOf constructedTermType
                let ref = Memory.AllocateTemporaryLocalVariable cilState.state typ freshValue
                push ref cilState // NOTE: ref is used to retrieve constructed struct
                let stackSizeBefore = Memory.CallStackSize cilState.state
                callConstructor cilState ref (List.map (fun afterCall -> modifyValueResultIfConstructorWasCalled stackSizeBefore afterCall; afterCall))
            else
                let ref = Memory.AllocateDefaultClass cilState.state constructedTermType
                push ref cilState // NOTE: ref is used as result afterCall
                callConstructor cilState ref id

        let k reference =
            let newIp = moveInstruction (fallThroughTarget cfg.MethodBase offset) (currentIp cilState)
            push reference cilState
            setCurrentIp newIp cilState
            [cilState]

        if Reflection.isDelegateConstructor constructorInfo then
            x.CommonCreateDelegate constructorInfo cilState args k
        elif typ.IsArray && constructorInfo.GetMethodBody() = null then
            try
                // TODO: can lower bounds be specified via newobj?
                x.ReduceArrayCreation typ cilState args k
            with
            | :? OutOfMemoryException -> x.Raise x.OutOfMemoryException cilState id
        else blockCase cilState)

    member x.LdsFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let newIp = moveInstruction (fallThroughTarget cfg.MethodBase offset) (currentIp cilState)
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldsfld.Size)
        assert fieldInfo.IsStatic
        x.InitializeStatics cilState fieldInfo.DeclaringType (fun cilState ->
        let declaringTermType = Types.FromDotNetType fieldInfo.DeclaringType
        let fieldId = Reflection.wrapField fieldInfo
        let value = if addressNeeded
                    then StaticField(declaringTermType, fieldId) |> Ref
                    else Memory.ReadStaticField cilState.state declaringTermType fieldId
        push value cilState
        setCurrentIp newIp cilState
        [cilState])
    member private x.StsFld (cfg : cfg) offset (cilState : cilState) =
        let newIp = moveInstruction (fallThroughTarget cfg.MethodBase offset) (currentIp cilState) // TODO: remove this copy-paste
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stsfld.Size)
        assert fieldInfo.IsStatic
        x.InitializeStatics cilState fieldInfo.DeclaringType (fun cilState ->
        let declaringTermType = Types.FromDotNetType fieldInfo.DeclaringType
        let fieldId = Reflection.wrapField fieldInfo
        let value = pop cilState
        let fieldType = Types.FromDotNetType fieldInfo.FieldType
        let value = Types.Cast value fieldType
        Memory.WriteStaticField cilState.state declaringTermType fieldId value
        setCurrentIp newIp cilState
        [cilState])
    member x.LdFldWithFieldInfo (fieldInfo : FieldInfo) addressNeeded (cilState : cilState) =
        assert(not fieldInfo.IsStatic)
        let target = pop cilState
        let loadWhenTargetIsNotNull (cilState : cilState) k =
            let createCilState value =
                push value cilState
                k [cilState]
            let fieldId = Reflection.wrapField fieldInfo
            ConfigureErrorReporter (changeState cilState >> reportError)
            if TypeUtils.isPointer fieldInfo.DeclaringType then
                if addressNeeded then createCilState target
                else Memory.Read cilState.state target |> createCilState
            else
                if addressNeeded then Memory.ReferenceField cilState.state target fieldId |> createCilState
                else Memory.ReadField cilState.state target fieldId |> createCilState
        x.NpeOrInvokeStatementCIL cilState target loadWhenTargetIsNotNull id
    member x.LdFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldfld.Size)
        x.LdFldWithFieldInfo fieldInfo addressNeeded cilState
    member x.StFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stfld.Size)
        assert(not fieldInfo.IsStatic)
        let value, targetRef = pop2 cilState
        let storeWhenTargetIsNotNull (cilState : cilState) k =
            let fieldType = Types.FromDotNetType fieldInfo.FieldType
            let value = Types.Cast value fieldType
            let reference =
                if TypeUtils.isPointer fieldInfo.DeclaringType then targetRef
                else Reflection.wrapField fieldInfo |> Memory.ReferenceField cilState.state targetRef
            ConfigureErrorReporter (changeState cilState >> reportError)
            Memory.Write cilState.state reference value |> List.map (changeState cilState) |> k
        x.NpeOrInvokeStatementCIL cilState targetRef storeWhenTargetIsNotNull id
    member private x.LdElemCommon (typ : symbolicType option) (cilState : cilState) arrayRef indices =
        let arrayType = MostConcreteTypeOfHeapRef cilState.state arrayRef
        let uncheckedLdElem (cilState : cilState) k =
            ConfigureErrorReporter (changeState cilState >> reportError)
            let value = Memory.ReadArrayIndex cilState.state arrayRef indices typ
            push value cilState
            k [cilState]
        let checkedLdElem (cilState : cilState) k =
            let dims = List.init (Types.RankOf arrayType) MakeNumber
            let lengths = List.map (Memory.ArrayLengthByDimension cilState.state arrayRef) dims
            x.AccessMultidimensionalArray uncheckedLdElem cilState lengths indices k // TODO: if ptr, do net use check #do
        x.NpeOrInvokeStatementCIL cilState arrayRef checkedLdElem id
    member private x.LdElemWithCast typ (cilState : cilState) : cilState list =
        let index, arrayRef = pop2 cilState
        x.LdElemCommon typ cilState arrayRef [index]
    member private x.LdElemTyp typ (cilState : cilState) = x.LdElemWithCast (Some typ) cilState
    member private x.LdElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldelem.Size)
        x.LdElemTyp typ cilState
    member private x.LdElemRef = x.LdElemWithCast None
    member private x.LdElema (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldelema.Size)
        let index, arrayRef = pop2 cilState
        let referenceLocation (cilState : cilState) k =
            let value = Memory.ReferenceArrayIndex cilState.state arrayRef [index] (Some typ)
            push value cilState
            k [cilState]
        let checkTypeMismatch (cilState : cilState) (k : cilState list -> 'a) =
            let elementType = MostConcreteTypeOfHeapRef cilState.state arrayRef |> Types.ElementType
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.TypeIsType typ elementType &&& Types.TypeIsType elementType typ, state))
                referenceLocation
                (x.Raise x.ArrayTypeMismatchException)
                k
        let checkIndex (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            x.AccessArray checkTypeMismatch cilState length index k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkIndex id
    member private x.StElemCommon (typ : symbolicType option) (cilState : cilState) arrayRef indices value =
        let arrayType = MostConcreteTypeOfHeapRef cilState.state arrayRef
        let baseType = Types.ElementType arrayType
        let checkedStElem (cilState : cilState) (k : cilState list -> 'a) =
            let typeOfValue = TypeOf value
            let uncheckedStElem (cilState : cilState) (k : cilState list -> 'a) =
                ConfigureErrorReporter (changeState cilState >> reportError)
                Memory.WriteArrayIndex cilState.state arrayRef indices value typ |> List.map (changeState cilState) |> k
            let checkTypeMismatch (cilState : cilState) (k : cilState list -> 'a) =
                let condition =
                    if Types.IsValueType typeOfValue then True
                    else Types.RefIsAssignableToType cilState.state value baseType
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (condition, state))
                    uncheckedStElem
                    (x.Raise x.ArrayTypeMismatchException)
                    k
            let dims = List.init (Types.RankOf arrayType) MakeNumber
            let lengths = List.map (Memory.ArrayLengthByDimension cilState.state arrayRef) dims
            x.AccessMultidimensionalArray checkTypeMismatch cilState lengths indices k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkedStElem id
    member private x.StElemWithCast typ (cilState : cilState) =
        let value, index, arrayRef = pop3 cilState
        x.StElemCommon typ cilState arrayRef [index] value
    member private x.StElemTyp typ (cilState : cilState) =
        x.StElemWithCast (Some typ) cilState
    member private x.StElem cfg offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Stelem.Size)
        x.StElemWithCast (Some typ) cilState
    member private x.StElemRef = x.StElemWithCast None
    member private x.LdLen (cilState : cilState) =
        let arrayRef = pop cilState
        let ldlen (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            push length cilState
            k [cilState]
        x.NpeOrInvokeStatementCIL cilState arrayRef ldlen id
    member private x.LdVirtFtn (cfg : cfg) offset (cilState : cilState) =
        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Ldvirtftn.Size)
        let this = pop cilState
        let ldvirtftn (cilState : cilState) k =
            assert(IsReference this)
            let thisType = this |> MostConcreteTypeOfHeapRef cilState.state |> Types.ToDotNetType
            let methodInfo = thisType.GetMethod(ancestorMethodBase.Name, Reflection.allBindingFlags)
            let methodInfoType = methodInfo.GetType() |> Types.FromDotNetType
            let methodPtr = Terms.Concrete methodInfo methodInfoType
            push methodPtr cilState
            k [cilState]
        x.NpeOrInvokeStatementCIL cilState this ldvirtftn id

    member x.BoxNullable (t : Type) (v : term) (cilState : cilState) : cilState list =
        // TODO: move it to Reflection.fs; add more validation in case if .NET implementation does not have these fields
        let boxValue (cilState : cilState) =
            let value = pop cilState
            let address = Memory.BoxValueType cilState.state value
            push address cilState
        let hasValueCase (cilState : cilState) k =
            let valueFieldInfo = t.GetField("value", Reflection.instanceBindingFlags)
            push v cilState
            let cilStates = x.LdFldWithFieldInfo valueFieldInfo false cilState
            List.iter boxValue cilStates
            k cilStates
        let boxNullable (hasValue, cilState : cilState) k =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (hasValue, state))
                hasValueCase
                (fun cilState k ->
                    push NullRef cilState
                    k [cilState])
                k
        let hasValueFieldInfo = t.GetField("hasValue", Reflection.instanceBindingFlags)
        let hasValueResults =
            push v cilState
            x.LdFldWithFieldInfo hasValueFieldInfo false cilState  |> List.map (fun cilState -> (pop cilState, cilState))
        Cps.List.mapk boxNullable hasValueResults List.concat

    member x.Box (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Box.Size)
        let termType = Types.FromDotNetType t
        if Types.IsValueType termType then
            let v = pop cilState
            if Types.IsNullable termType then x.BoxNullable t v cilState
            else
                allocateValueTypeInHeap v cilState
                [cilState]
        else [cilState]
    member private x.UnboxCommon cilState obj t handleRestResults k =
        let nonExceptionCont (cilState : cilState) res k =
            push res cilState
            k [cilState]
        let termType = Types.FromDotNetType t
        assert(IsReference obj)
        assert(Types.IsValueType termType)
        let nullCase (cilState : cilState) k =
            if Types.IsNullable termType then
                let nullableTerm = Memory.DefaultOf termType
                let address = Memory.BoxValueType cilState.state nullableTerm
                let res = handleRestResults cilState (HeapReferenceToBoxReference address)
                nonExceptionCont cilState res k
            else
                x.Raise x.NullReferenceException cilState k
        let nullableCase (cilState : cilState) =
            let underlyingTypeOfNullableT = Nullable.GetUnderlyingType t
            ConfigureErrorReporter (changeState cilState >> reportError)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.RefIsType state obj (Types.FromDotNetType underlyingTypeOfNullableT), state))
                (fun cilState k ->
                    let value = HeapReferenceToBoxReference obj |> Memory.Read cilState.state
                    let nullableTerm = Memory.DefaultOf termType
                    let valueField, hasValueField = Reflection.fieldsOfNullable t
                    let nullableTerm = Memory.WriteStructField nullableTerm valueField value
                    let nullableTerm = Memory.WriteStructField nullableTerm hasValueField (MakeBool true)
                    let address = Memory.BoxValueType cilState.state nullableTerm
                    let res = handleRestResults cilState (HeapReferenceToBoxReference address)
                    nonExceptionCont cilState res k)
                (x.Raise x.InvalidCastException)
        let nonNullCase (cilState : cilState) =
            if Types.IsNullable termType then
                nullableCase cilState
            else
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.IsCast state obj termType, state))
                    (fun cilState k ->
                        let res = handleRestResults cilState (Types.Cast obj termType |> HeapReferenceToBoxReference)
                        push res cilState
                        k [cilState])
                    (x.Raise x.InvalidCastException)
        BranchOnNullCIL cilState obj
            nullCase
            nonNullCase
            k

    member private x.Throw (cilState : cilState) =
        let error = peek cilState
        BranchOnNullCIL cilState error
            (x.Raise x.NullReferenceException)
            (fun cilState k ->
                let codeLocations = List.map (Option.get << ip2codeLocation) cilState.ipStack
                setCurrentIp (SearchingForHandler(codeLocations, List.empty)) cilState
                setException (Unhandled error) cilState
                clearEvaluationStackLastFrame cilState
                k [cilState])
            id
    member private x.Unbox (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox.Size)
        let obj = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then
            let iie = createInsufficientInformation "Unboxing generic parameter"
            cilState.iie <- Some iie
            [cilState]
        else
            x.UnboxCommon cilState obj t (fun _ x -> x) id
    member private x.UnboxAny (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox_Any.Size)
        let termType = Types.FromDotNetType t
        let valueType = Types.FromDotNetType typeof<ValueType>
        let obj = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then
            let iie = createInsufficientInformation "Can't introduce generic type X for equation: T = Nullable<X>"
            cilState.iie <- Some iie
            [cilState]
        else
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.TypeIsType termType valueType, state))
                (fun cilState k ->
                    let handleRestResults cilState address = Memory.Read cilState.state address
                    x.UnboxCommon cilState obj t handleRestResults k)
                (fun state k -> x.CommonCastClass state obj termType k)
                id

    member private this.CommonDivRem performAction (cilState : cilState) =
        let integerCase (cilState : cilState) x y minusOne minValue =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k ((x === minValue) &&& (y === minusOne), state))
                        (this.Raise this.ArithmeticException)
                        (fun cilState k ->
                            push (performAction x y) cilState
                            k [cilState]))
                id
        let y, x = pop2 cilState
        match y, x with
        | TypeUtils.Float, TypeUtils.Float ->
            push (performAction x y) cilState
            [cilState]
        | TypeUtils.Int64, _
        | TypeUtils.UInt64, _
        | _, TypeUtils.Int64
        | _, TypeUtils.UInt64 -> integerCase cilState x y TypeUtils.Int64.MinusOne TypeUtils.Int64.MinValue
        | _ -> integerCase cilState x y TypeUtils.Int32.MinusOne TypeUtils.Int32.MinValue
        | _ -> __unreachable__()
    member private this.Div (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonDivRem div cilState

    member private this.Rem (cilState : cilState) =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonDivRem rem cilState

    member private this.CommonUnsignedDivRem isRem performAction (cilState : cilState) =
        let y, x = pop2 cilState
        match y, x with
        | _ when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState k ->
                    push (performAction x y) cilState
                    k [cilState])
                id
        | TypeUtils.Float, _
        | _, TypeUtils.Float when isRem -> internalfailf "Rem.Un is unspecified for Floats"
        | _ -> internalfailf "incompatible operands for %s" (if isRem then "Rem.Un" else "Div.Un")

    member private this.DivUn (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide_Un x y id
        this.CommonUnsignedDivRem false div cilState

    member private this.RemUn cilState =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder_Un x y id
        this.CommonUnsignedDivRem true rem cilState

    member private this.UnsignedCheckOverflow checkOverflowForUnsigned (cilState : cilState) =
        let y, x = pop2 cilState
        match y, x with
        | TypeUtils.Int64, _
        | _, TypeUtils.Int64
        | TypeUtils.UInt64, _
        | _, TypeUtils.UInt64 ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            let max = TypeUtils.UInt64.MaxValue
            let zero = TypeUtils.UInt64.Zero
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x, y = makeUnsignedInteger x id, makeUnsignedInteger y id
            let max = TypeUtils.UInt32.MaxValue
            let zero = TypeUtils.UInt32.Zero
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> internalfailf "incompatible operands for UnsignedCheckOverflow"
    member private this.SignedCheckOverflow checkOverflow (cilState : cilState) =
        let y, x = pop2 cilState
        match y, x with
        | TypeUtils.Int64, _
        | _, TypeUtils.Int64 ->
            let min = TypeUtils.Int64.MinValue
            let max = TypeUtils.Int64.MaxValue
            let zero = TypeUtils.Int64.Zero
            let minusOne = TypeUtils.Int64.MinusOne
            checkOverflow min max zero minusOne x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | TypeUtils.UInt64, _
        | _, TypeUtils.UInt64 -> __unreachable__() // instead of add_ovf should be called add_ovf_un
        | TypeUtils.Float, _
        | _, TypeUtils.Float -> __unreachable__() // only integers
        | _ ->
            let min = TypeUtils.Int32.MinValue
            let max = TypeUtils.Int32.MaxValue
            let zero = TypeUtils.Int32.Zero
            let minusOne = TypeUtils.Int32.MinusOne
            checkOverflow min max zero minusOne x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
    member private this.Add_ovf (cilState : cilState) =
        // min <= x + y <= max
        let checkOverflow min max zero _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let xMoreThan0 state k = k (x >>= zero, state)
            let yMoreThan0 state k = k (y >>= zero, state)
            let checkOverflowWhenMoreThan0 (state : state) k = // x >= 0 && y >= 0
                PerformBinaryOperation OperationType.Subtract max y (fun diff ->
                k (diff >>= x, state))
            let checkOverflowWhenLessThan0 (state : state) k =
                PerformBinaryOperation OperationType.Subtract min y (fun diff ->
                k (x >>= diff, state))
            let add (cilState : cilState) k = // no overflow
                PerformBinaryOperation OperationType.Add x y (fun sum ->
                    push sum cilState
                    k [cilState])
            StatedConditionalExecutionCIL cilState xMoreThan0
                (fun cilState -> // x >= 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        (fun cilState -> // y >= 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenMoreThan0
                                add
                                (this.Raise this.OverflowException))
                        add)
                (fun cilState -> // x < 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        add
                        (fun cilState -> // x < 0 && y < 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenLessThan0
                                add
                                (this.Raise this.OverflowException)))
                id
        this.SignedCheckOverflow checkOverflow cilState
    member private this.Mul_ovf (cilState : cilState) =
        // min <= x * y <= max
        let checkOverflow min max zero _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let (>>) = API.Arithmetics.(>>)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            let xMoreThan0 state k = k (x >> zero, state)
            let yMoreThan0 state k = k (y >> zero, state)
            let checkOverflowWhenXM0YM0 (state : state) k = // x > 0 && y > 0
                PerformBinaryOperation OperationType.Divide max y (fun quotient ->
                k (quotient >>= x, state))
            let checkOverflowWhenXL0YL0 (state : state) k = // x < 0 && y < 0
                PerformBinaryOperation OperationType.Divide max y (fun quotient ->
                k (x >>= quotient, state))
            let checkOverflowWhenXM0YL0 (state : state) k = // x > 0 && y < 0
                PerformBinaryOperation OperationType.Divide min x (fun quotient ->
                k (y >>= quotient, state))
            let checkOverflowWhenXL0YM0 (state : state) k = // x < 0 && y > 0
                PerformBinaryOperation OperationType.Divide min y (fun quotient ->
                k (x >>= quotient, state))
            let mul (cilState : cilState) k = // no overflow
                PerformBinaryOperation OperationType.Multiply x y (fun res ->
                    push res cilState
                    k [cilState])
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k ->
                    push zero cilState
                    k [cilState])
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        xMoreThan0
                        (fun cilState -> // x > 0
                            StatedConditionalExecutionCIL cilState yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YM0
                                        mul
                                        (this.Raise this.OverflowException))
                                (fun cilState -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YL0
                                        mul
                                        (this.Raise this.OverflowException)))
                        (fun cilState -> // x < 0
                            StatedConditionalExecutionCIL cilState
                                yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YM0
                                        mul
                                        (this.Raise this.OverflowException))
                                (fun cilState k -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YL0
                                        mul
                                        (this.Raise this.OverflowException)
                                        k)))
                id
        this.SignedCheckOverflow checkOverflow cilState
    member private this.Add_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k ->
                    PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                    k (diff >>= y, state)))
                (fun cilState k -> PerformBinaryOperation OperationType.Add x y (fun res ->
                    push res cilState
                    k [cilState]))
                (this.Raise this.OverflowException)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Mul_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned zero max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k ->
                    push zero cilState
                    k [cilState])
                (fun cilState k ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k ->
                            PerformBinaryOperation OperationType.Divide max x (fun quotient ->
                            k (quotient >>= y, state)))
                        (fun cilState k ->
                            PerformBinaryOperation OperationType.Multiply x y (fun res ->
                                push res cilState
                                k [cilState]))
                        (this.Raise this.OverflowException)
                        k)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (x >>= y, state))
                (fun (cilState : cilState) k -> // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    push res cilState
                    k [cilState]))
                (this.Raise this.OverflowException)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf (cilState : cilState) =
        // there is no way to reduce current operation to [x `Add_Ovf` (-y)]
        // min <= x - y <= max
        let checkOverflowForSigned min max zero minusOne x y cilState =
                let (>>=) = API.Arithmetics.(>>=)
                let xGreaterEqualZero state k = k (x >>= zero, state)
                let sub (cilState : cilState) k = // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    push res cilState
                    k [cilState])
                StatedConditionalExecutionCIL cilState
                    xGreaterEqualZero
                    (fun cilState -> // x >= 0 => max - x >= 0 => no overflow for [-1 * (max - x)]
                        StatedConditionalExecutionCIL cilState
                            (fun state k ->
                                PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (y >>= minusDiff, state)))) // y >= -(max - x)
                            sub
                            (this.Raise this.OverflowException))
                    (fun cilState -> // x < 0 => no overflow for [min - x] # x < 0 => [min - x] != min => no overflow for (-1) * [min - x]
                        StatedConditionalExecutionCIL cilState
                           (fun state k ->
                                PerformBinaryOperation OperationType.Subtract min x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (minusDiff >>= y, state)))) // -(min - x) >= y
                            sub
                            (this.Raise this.OverflowException))
                    id
        this.SignedCheckOverflow checkOverflowForSigned cilState
    member private x.Newarr (cfg : cfg) offset (cilState : cilState) =
        let (>>=) = API.Arithmetics.(>>=)
        let elemType = resolveTermTypeFromMetadata cfg (offset + OpCodes.Newarr.Size)
        let numElements = pop cilState
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (numElements >>= TypeUtils.Int32.Zero, state))
            (fun cilState k ->
                let ref = Memory.AllocateVectorArray cilState.state numElements elemType
                push ref cilState
                k [cilState])
            (this.Raise this.OverflowException)
            id


    member x.CreateException (exceptionType : Type) arguments cilState =
        assert (not <| exceptionType.IsValueType)
        Logger.printLog Logger.Info "%O!\nStack trace:\n%O" exceptionType (Memory.StackTrace cilState.state.stack)
        clearEvaluationStackLastFrame cilState
        let constructors = exceptionType.GetConstructors()
        let argumentsLength = List.length arguments
        let argumentsTypes =
            List.map (TypeOf >> Types.ToDotNetType) arguments
        let ctors =
            constructors
            |> List.ofArray
            |> List.filter (fun (ci : ConstructorInfo)
                             -> ci.GetParameters().Length = argumentsLength
                                && ci.GetParameters()
                                   |> Seq.forall2 (fun p1 p2 -> p2.ParameterType.IsAssignableFrom(p1)) argumentsTypes)
        assert(List.length ctors = 1)
        let ctor = List.head ctors
        let fullConstructorName = Reflection.getFullMethodName ctor
        assert (Loader.hasRuntimeExceptionsImplementation fullConstructorName)
        let proxyCtor = Loader.getRuntimeExceptionsImplementation fullConstructorName
        x.InitFunctionFrameCIL cilState proxyCtor None (Some arguments)

    member x.InvalidProgramException cilState =
        x.CreateException typeof<InvalidProgramException> [] cilState
    member x.NullReferenceException cilState =
        x.CreateException typeof<NullReferenceException> [] cilState
    member x.ArgumentException cilState =
        x.CreateException typeof<ArgumentException> [] cilState
    member x.ArgumentNullException cilState =
        x.CreateException typeof<ArgumentNullException> [] cilState
    member x.ArgumentOutOfRangeException cilState =
        x.CreateException typeof<ArgumentOutOfRangeException> [] cilState
    member x.IndexOutOfRangeException cilState =
        x.CreateException typeof<IndexOutOfRangeException> [] cilState
    member x.ArrayTypeMismatchException cilState =
        x.CreateException typeof<ArrayTypeMismatchException> [] cilState
    member x.RankException cilState =
        x.CreateException typeof<RankException> [] cilState
    member x.DivideByZeroException cilState =
        x.CreateException typeof<DivideByZeroException> [] cilState
    member x.OverflowException cilState =
        x.CreateException typeof<OverflowException> [] cilState
    member x.ArithmeticException cilState =
        x.CreateException typeof<ArithmeticException> [] cilState
    member x.TypeInitializerException qualifiedTypeName innerException (cilState : cilState) =
        let typeName = Memory.AllocateString qualifiedTypeName cilState.state
        let args = [typeName; innerException]
        x.CreateException typeof<TypeInitializationException> args cilState
    member x.InvalidCastException (cilState : cilState) =
        let message = Memory.AllocateString "Specified cast is not valid." cilState.state
        x.CreateException typeof<InvalidCastException> [message] cilState
    member x.OutOfMemoryException (cilState : cilState) =
        x.CreateException typeof<OutOfMemoryException> [] cilState

    // -------------------------------- ExplorerBase operations -------------------------------------

    member x.ExecuteAllInstructionsForCFGEdges (cfg : cfg) (cilState : cilState) : cilState list * cilState list * cilState list =
        let ip = currentIp cilState
        assert(ip.CanBeExpanded())
        let startingOffset = ip.Offset()
        let endOffset =
            let lastOffset = Seq.last cfg.SortedOffsets
            let rec binarySearch l r =
                if l + 1 = r then l
                else
                    let mid = (l + r) / 2
                    if cfg.SortedOffsets.[mid] <= startingOffset then binarySearch mid r
                    else binarySearch l mid
            let index = binarySearch 0 (Seq.length cfg.SortedOffsets)
            if cfg.SortedOffsets.[index] = lastOffset then cfg.IlBytes.Length
            else cfg.SortedOffsets.[index + 1]

        let isIpOfCurrentBasicBlock (ip : ip) =
            match ip with
            | Instruction(i, m) when m = cfg.MethodBase -> startingOffset <= i && i < endOffset
            | _ -> false
        x.ExecuteAllInstructionsWhile isIpOfCurrentBasicBlock cilState

    member x.ExecuteOneInstruction (cilState : cilState) : cilState list * cilState list * cilState list =
        x.ExecuteAllInstructionsWhile (always false) cilState

    // returns finishedStates, incompleteStates, erroredStates
    member x.ExecuteAllInstructionsWhile (condition : ip -> bool) (cilState : cilState) =
        let rec executeAllInstructions (finishedStates, incompleteStates, errors) cilState k =
            let ip = currentIp cilState
            try
                cilState.iie <- None
                let allStates = x.MakeStep cilState
                let newErrors, restStates = List.partition isUnhandledError allStates
                let errors = errors @ newErrors
                let newIieStates, goodStates = List.partition isIIEState restStates
                let incompleteStates = newIieStates @ incompleteStates
                match goodStates with
                | _ when List.forall (currentIp >> condition) goodStates ->
                    Cps.List.foldlk executeAllInstructions (finishedStates, incompleteStates, errors) goodStates k
                | _ -> (goodStates @ finishedStates, incompleteStates, errors) |> k
            with
            | :? InsufficientInformationException as iie ->
                cilState.iie <- Some iie
                setCurrentIp ip cilState
                (finishedStates, cilState :: incompleteStates, errors) |> k
        executeAllInstructions ([],[],[]) cilState id

    member private x.IncrementLevelIfNeeded (cfg : cfg) (offset : offset) (cilState : cilState) =
        let isVertex offset = cfg.SortedOffsets.BinarySearch(offset) >= 0
        if offset = 0 || cfg.IsLoopEntry offset then
            incrementLevel cilState {offset = offset; method = cfg.MethodBase}
        if cfg.HasSiblings offset then
            addIntoHistory cilState {offset = offset; method = cfg.MethodBase}

    member private x.DecrementMethodLevel (cilState : cilState) method =
        let key = {offset = 0; method = method}
        decrementLevel cilState key

    member private x.FindNeededEHCBlock offset moreSuitable (ehcs : ExceptionHandlingClause seq) =
        let findBlock acc (x : ExceptionHandlingClause) =
            // NOTE: need to execute the most wide finally block
            match acc with
            | Some block when moreSuitable x block -> Some x
            | Some _ -> acc
            | None ->
                // NOTE: check that this block protects current ip
                if x.tryOffset < offset && x.tryOffset + x.tryLength > offset then Some x else None
        Seq.fold findBlock None ehcs

    member x.MakeStep (cilState : cilState) =
        cilState.stepsNumber <- cilState.stepsNumber + 1u
        let exit m =
            x.DecrementMethodLevel cilState m
            Logger.printLogLazy Logger.Info "Done with method %s" (lazy Reflection.getFullMethodName m)
            match cilState.ipStack with
            // NOTE: the whole method is executed
//            | [ Exit _ ] when startsFromMethodBeginning cilState ->
//                setCurrentTime [] cilState
//                popFrameOf cilState
            // NOTE: some part of method is executed
            | [ Exit _ ] -> ()
            | Exit m :: ips' when Reflection.isStaticConstructor m ->
                popFrameOf cilState
                setIpStack ips' cilState
            | Exit _ :: (InstructionEndingIp(offset, caller) as ip) :: _ ->
                // TODO: assert (isCallIp ip)
                let newIp = moveInstruction (fallThroughTarget caller offset) ip
                popFrameOf cilState
                setCurrentIp newIp cilState
                let callSite = parseCallSite caller offset
                if callSite.opCode = OpCodes.Newobj && callSite.calledMethod.DeclaringType.IsValueType then
                    pushNewObjForValueTypes cilState
            | Exit _ :: Exit _ :: _ -> __unreachable__()
            | _ -> __unreachable__()
        let rec makeStep' ip k =
            match ip with
            | Instruction(offset, m) ->
                if offset = 0 then Logger.printLogLazy Logger.Info "Starting to explore method %O" (lazy Reflection.getFullMethodName m)
                x.ExecuteInstruction m offset cilState |> k
            | Exit m ->
                exit m
                k [cilState]
            | Leave(EndFinally, [],  dst, m) ->
                setCurrentIp (instruction m dst) cilState
                clearEvaluationStackLastFrame cilState
                k [cilState]
            | Leave(EndFinally, ehc :: ehcs,  dst, m) ->
                assert(isFinallyClause ehc)
                let ip' = ipOperations.leave (instruction m ehc.handlerOffset) ehcs dst m
                setCurrentIp ip' cilState
                clearEvaluationStackLastFrame cilState
                k [cilState]
            | Leave(ip, ehcs, dst, m) ->
                let oldLength = List.length cilState.ipStack
                let makeLeaveIfNeeded (result : cilState) =
                    if List.length result.ipStack = oldLength then
                        let ip = ipOperations.leave (currentIp result) ehcs dst m
                        setCurrentIp ip result
                makeStep' ip (fun states ->
                List.iter makeLeaveIfNeeded states
                k states)
            | SearchingForHandler([], framesToPop) ->
                let popFrameWithContents _ =
                    clearEvaluationStackLastFrame cilState
                    popFrameOf cilState
                if List.length framesToPop > 1 then List.iter popFrameWithContents (List.tail framesToPop)
                clearEvaluationStackLastFrame cilState
                setCurrentIp (SearchingForHandler([], [])) cilState
                k [cilState]
            | SearchingForHandler(location :: otherLocations, framesToPop) ->
                let method = location.method
                let ehcs = getEHSBytes method
                let filter = ehcs |> Seq.filter isFilterClause // TODO: use
                let exceptionType = MostConcreteTypeOfHeapRef cilState.state (cilState.state.exceptionsRegister.GetError()) |> Types.ToDotNetType
                let isSuitable ehc =
                    match ehc.ehcType with
                    | Catch t -> t = exceptionType
                    | _ -> false
                let suitableCatchBlocks = ehcs |> Seq.filter isSuitable
                let isNarrower (x : ExceptionHandlingClause) (y : ExceptionHandlingClause) =
                    y.handlerOffset < x.handlerOffset && y.handlerOffset + y.handlerLength > x.handlerOffset + x.handlerLength
                let neededBlock = x.FindNeededEHCBlock location.offset isNarrower suitableCatchBlocks
                match neededBlock with
                | Some ehc ->
                    let ip = SecondBypass(None, framesToPop, {method = method; offset = ehc.handlerOffset})
                    cilState.state.exceptionsRegister <- cilState.state.exceptionsRegister.TransformToCaught()
                    setCurrentIp ip cilState
                | None ->
                    let ip = SearchingForHandler(otherLocations, framesToPop @ [location])
                    setCurrentIp ip cilState
                k [cilState]
            | SecondBypass(Some EndFinally, locations, codeLocation) ->
                popFrameOf cilState
                let ip = SecondBypass(None, locations, codeLocation)
                setCurrentIp ip cilState
                k [cilState]
            | SecondBypass(_, [], codeLocation) ->
                // NOTE: starting to explore catch clause
                let ip = Instruction(codeLocation.offset, codeLocation.method)
                setCurrentIp ip cilState
                push (cilState.state.exceptionsRegister.GetError()) cilState
                k [cilState]
            | SecondBypass(Some ip, locations, codeLocation) ->
                makeStep' ip (fun states ->
                let makeIp (cilState : cilState) =
                    let ip = SecondBypass(Some (currentIp cilState), locations, codeLocation)
                    setCurrentIp ip cilState
                List.iter makeIp states
                k states)
            | SecondBypass(None, location :: otherLocations, codeLocation) ->
                let ehcs = getEHSBytes location.method
                let finallyBlocks = ehcs |> Seq.filter isFinallyClause
                let isWider (x : ExceptionHandlingClause) (y : ExceptionHandlingClause) =
                    x.handlerOffset < y.handlerOffset && x.handlerOffset + x.handlerLength > y.handlerOffset + y.handlerLength
                let neededBlock = x.FindNeededEHCBlock location.offset isWider finallyBlocks
                let finallyHandlerIp = neededBlock |> Option.map (fun b -> Instruction(b.handlerOffset, location.method))
                let ip = SecondBypass(finallyHandlerIp, otherLocations, codeLocation)
                clearEvaluationStackLastFrame cilState
                popFrameOf cilState
                setCurrentIp ip cilState
                k [cilState]
            | _ -> __notImplemented__()
        makeStep' (currentIp cilState) id

    member private x.ExecuteInstruction m offset (cilState : cilState) =
//        Logger.trace "ExecuteInstruction:\n%s" (dump cilState)
        let cfg = CFG.applicationGraph.GetCfg m
        let opCode = parseInstruction m offset
//        let newIps = moveIp cilState |> List.map (fun cilState -> cilState.ipStack)

        let opcodeValue = LanguagePrimitives.EnumOfValue opCode.Value
        let newSts =
            match opcodeValue with
            | OpCodeValues.Br
            | OpCodeValues.Br_S -> br cfg offset cilState; [cilState]
            | OpCodeValues.Add ->
                (fun _ _ -> standardPerformBinaryOperation OperationType.Add) |> fallThrough cfg offset cilState // TODO: check float overflow [spec]
            | OpCodeValues.Mul -> (fun _ _ -> standardPerformBinaryOperation OperationType.Multiply) |> fallThrough cfg offset cilState
            | OpCodeValues.Sub -> (fun _ _ -> standardPerformBinaryOperation OperationType.Subtract) |> fallThrough cfg offset cilState
            | OpCodeValues.Shl -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftLeft) |> fallThrough cfg offset cilState
            | OpCodeValues.Shr -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight) |> fallThrough cfg offset cilState
            | OpCodeValues.Shr_Un -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight_Un) |> fallThrough cfg offset cilState
            | OpCodeValues.Ceq -> (fun _ _ -> ceq) |> fallThrough cfg offset cilState
            | OpCodeValues.Cgt -> (fun _ _ -> cgt) |> fallThrough cfg offset cilState
            | OpCodeValues.Cgt_Un -> (fun _ _ -> cgtun) |> fallThrough cfg offset cilState
            | OpCodeValues.Clt -> (fun _ _ -> clt) |> fallThrough cfg offset cilState
            | OpCodeValues.Clt_Un -> (fun _ _ -> cltun) |> fallThrough cfg offset cilState
            | OpCodeValues.And -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseAnd OperationType.LogicalAnd) |> fallThrough cfg offset cilState
            | OpCodeValues.Or -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseOr OperationType.LogicalOr) |> fallThrough cfg offset cilState
            | OpCodeValues.Xor -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseXor OperationType.LogicalXor) |> fallThrough cfg offset cilState
            | OpCodeValues.Neg -> (fun _ _ -> performCILUnaryOperation OperationType.UnaryMinus) |> fallThrough cfg offset cilState
            | OpCodeValues.Not -> (fun _ _ -> bitwiseOrBoolNot) |> fallThrough cfg offset cilState
            | OpCodeValues.Stloc -> stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Stloc.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_0 -> stloc (fun _ _ -> 0) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_1 -> stloc (fun _ _ -> 1) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_2 -> stloc (fun _ _ -> 2) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_3 -> stloc (fun _ _ -> 3) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_S -> stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Stloc_S.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Starg -> starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Starg.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Starg_S -> starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Starg_S.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldc_I4 -> ldc (fun ilBytes offset -> NumberCreator.extractInt32 ilBytes (offset + OpCodes.Ldc_I4.Size)) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_0 -> ldc (fun _ _ -> 0) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_1 -> ldc (fun _ _ -> 1) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_2 -> ldc (fun _ _ -> 2) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_3 -> ldc (fun _ _ -> 3) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_4 -> ldc (fun _ _ -> 4) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_5 -> ldc (fun _ _ -> 5) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_6 -> ldc (fun _ _ -> 6) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_7 -> ldc (fun _ _ -> 7) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_8 -> ldc (fun _ _ -> 8) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_M1 -> ldc (fun _ _ -> -1) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_S -> ldc (fun ilBytes offset -> NumberCreator.extractInt8 ilBytes (offset + OpCodes.Ldc_I4_S.Size)) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I8 -> ldc (fun ilBytes offset -> NumberCreator.extractInt64 ilBytes (offset + OpCodes.Ldc_I8.Size)) typedefof<int64> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_R4 -> ldc (fun ilBytes offset -> NumberCreator.extractFloat32 ilBytes (offset + OpCodes.Ldc_R4.Size)) typedefof<float32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_R8 -> ldc (fun ilBytes offset -> NumberCreator.extractFloat64 ilBytes (offset + OpCodes.Ldc_R8.Size)) typedefof<double> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg -> ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarg.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_0 -> ldarg (fun _ _ -> 0) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_1 -> ldarg (fun _ _ -> 1) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_2 -> ldarg (fun _ _ -> 2) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_3 -> ldarg (fun _ _ -> 3) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_S -> ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarg_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Nop -> fallThrough cfg offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Ldloc -> ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloc.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_0 -> ldloc (fun _ _ -> 0) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_1 -> ldloc (fun _ _ -> 1) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_2 -> ldloc (fun _ _ -> 2) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_3 -> ldloc (fun _ _ -> 3) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_S -> ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloc_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloca -> ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloca.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloca_S -> ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloca_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ret -> ret cfg cilState; [cilState]
            | OpCodeValues.Dup -> (fun _ _ -> dup) |> fallThrough cfg offset cilState

            // branching
            | OpCodeValues.Brfalse -> brfalse cfg offset cilState
            | OpCodeValues.Brfalse_S -> brfalse cfg offset cilState
            | OpCodeValues.Brtrue -> brtrue cfg offset cilState
            | OpCodeValues.Brtrue_S -> brtrue cfg offset cilState
            | OpCodeValues.Beq -> applyAndBranch ceq brtrue cfg offset cilState
            | OpCodeValues.Beq_S -> applyAndBranch ceq brtrue cfg offset cilState
            | OpCodeValues.Bge -> applyAndBranch bgeHelper brfalse cfg offset cilState
            | OpCodeValues.Bge_S -> applyAndBranch bgeHelper brfalse cfg offset cilState

            | OpCodeValues.Bgt -> applyAndBranch cgt brtrue cfg offset cilState
            | OpCodeValues.Bgt_S -> applyAndBranch cgt brtrue cfg offset cilState
            | OpCodeValues.Bgt_Un -> applyAndBranch cgtun brtrue cfg offset cilState
            | OpCodeValues.Bgt_Un_S -> applyAndBranch cgtun brtrue cfg offset cilState
            | OpCodeValues.Ble -> applyAndBranch cgt brfalse cfg offset cilState
            | OpCodeValues.Ble_S -> applyAndBranch cgt brfalse cfg offset cilState
            | OpCodeValues.Ble_Un -> applyAndBranch cgtun brfalse cfg offset cilState
            | OpCodeValues.Ble_Un_S -> applyAndBranch cgtun brfalse cfg offset cilState
            | OpCodeValues.Blt -> applyAndBranch clt brtrue cfg offset cilState
            | OpCodeValues.Blt_S -> applyAndBranch clt brtrue cfg offset cilState
            | OpCodeValues.Blt_Un -> applyAndBranch cltun brtrue cfg offset cilState
            | OpCodeValues.Blt_Un_S -> applyAndBranch cltun brtrue cfg offset cilState
            | OpCodeValues.Bne_Un -> applyAndBranch ceq brfalse cfg offset cilState
            | OpCodeValues.Bne_Un_S -> applyAndBranch ceq brfalse cfg offset cilState
            | OpCodeValues.Bge_Un -> applyAndBranch cltun brfalse cfg offset cilState
            | OpCodeValues.Bge_Un_S -> applyAndBranch cltun brfalse cfg offset cilState

            | OpCodeValues.Ldstr -> ldstr |> fallThrough cfg offset cilState
            | OpCodeValues.Ldnull -> (fun _ _ -> ldnull) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I1 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int8Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I2 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int16Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I4 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int32Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I8 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_R4 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.float32Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_R8 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.float64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U1 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint8Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U2 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint16Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U4 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint32Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U8 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I -> (fun _ _ -> convi) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U -> (fun _ _ -> convu) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_R_Un -> (fun _ _ -> castTopOfOperationalStack TypeUtils.float64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Switch -> switch cfg offset cilState
            | OpCodeValues.Ldtoken -> ldtoken |> fallThrough cfg offset cilState
            | OpCodeValues.Ldftn -> ldftn |> fallThrough cfg offset cilState
            | OpCodeValues.Pop -> (fun _ _ -> pop >> ignore) |> fallThrough cfg offset cilState
            | OpCodeValues.Initobj -> initobj |> forkThrough cfg offset cilState
            | OpCodeValues.Ldarga -> ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarga.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarga_S -> ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarga_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I4 -> (fun _ _ -> ldind TypeUtils.int32Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I1 -> (fun _ _ -> ldind TypeUtils.int8Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I2 -> (fun _ _ -> ldind TypeUtils.int16Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I8 -> (fun _ _ -> ldind TypeUtils.int64Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_U1 -> (fun _ _ -> ldind TypeUtils.uint8Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_U2 -> (fun _ _ -> ldind TypeUtils.uint16Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_U4 -> (fun _ _ -> ldind TypeUtils.uint32Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_R4 -> (fun _ _ -> ldind TypeUtils.float32Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_R8 -> (fun _ _ -> ldind TypeUtils.float64Type reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_Ref -> (fun _ _ -> ldind TypeUtils.nativeint reportError) |> fallThrough cfg offset cilState
            // TODO: need to cast to nativeint? #do
            | OpCodeValues.Ldind_I -> (fun _ _ -> ldind TypeUtils.nativeint reportError) |> fallThrough cfg offset cilState
            | OpCodeValues.Isinst -> isinst |> forkThrough cfg offset cilState
            | OpCodeValues.Stobj -> (stobj reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldobj -> ldobj |> fallThrough cfg offset cilState
            | OpCodeValues.Stind_I1 -> (fun _ _ -> stind (castUnchecked TypeUtils.int8Type) reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I2 -> (fun _ _ -> stind (castUnchecked TypeUtils.int16Type) reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I4 -> (fun _ _ -> stind (castUnchecked TypeUtils.int32Type) reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I8 -> (fun _ _ -> stind (castUnchecked TypeUtils.int64Type) reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_R4 -> (fun _ _ -> stind (castUnchecked TypeUtils.float32Type) reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_R8 -> (fun _ _ -> stind (castUnchecked TypeUtils.float64Type) reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_Ref -> (fun _ _ -> stind id reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I -> (fun _ _ -> stind MakeIntPtr reportError) |> forkThrough cfg offset cilState
            | OpCodeValues.Sizeof -> sizeofInstruction |> fallThrough cfg offset cilState
            | OpCodeValues.Leave
            | OpCodeValues.Leave_S -> leave cfg offset cilState; [cilState]
            | OpCodeValues.Endfinally -> (fun _ _ -> endfinally) |> fallThrough cfg offset cilState
            | OpCodeValues.Rethrow -> (fun _ _ -> rethrow) |> fallThrough cfg offset cilState
            | OpCodeValues.Endfilter -> (fun _ _ -> endfilter) |> fallThrough cfg offset cilState
            | OpCodeValues.Localloc -> (fun _ _ -> localloc) |> fallThrough cfg offset cilState
            // TODO: notImplemented instructions
            | OpCodeValues.Stelem_I -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldelem_I -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Arglist -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Jmp -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Break -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Calli -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Ckfinite -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Constrained_ -> constrained |> fallThrough cfg offset cilState
            | OpCodeValues.Cpblk -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Cpobj -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Mkrefany -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix1 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix2 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix3 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix4 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix5 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix6 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix7 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefixref -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Readonly_ -> fallThrough cfg offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Refanytype -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Refanyval -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Tail_ -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Unaligned_ -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Volatile_ -> fallThrough cfg offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Initblk -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState

            | OpCodeValues.Call -> this.Call cfg offset cilState
            | OpCodeValues.Callvirt -> this.CallVirt cfg offset cilState
            | OpCodeValues.Newobj -> this.NewObj cfg offset cilState
            | OpCodeValues.Ldsfld -> this.LdsFld false cfg offset cilState
            | OpCodeValues.Ldsflda -> this.LdsFld true cfg offset cilState
            | OpCodeValues.Stsfld -> this.StsFld cfg offset cilState
            | OpCodeValues.Ldfld -> this.LdFld false |> forkThrough cfg offset cilState
            | OpCodeValues.Ldflda -> this.LdFld true |> forkThrough cfg offset cilState
            | OpCodeValues.Stfld -> this.StFld |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem -> this.LdElem |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelema -> this.LdElema |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I1 -> (fun _ _ -> this.LdElemTyp TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I2 -> (fun _ _ -> this.LdElemTyp TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I4 -> (fun _ _ -> this.LdElemTyp TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I8 -> (fun _ _ -> this.LdElemTyp TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_R4 -> (fun _ _ -> this.LdElemTyp TypeUtils.float32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_R8 -> (fun _ _ -> this.LdElemTyp TypeUtils.float64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_U1 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_U2 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_U4 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_Ref -> (fun _ _ -> this.LdElemRef) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem -> (fun _ _ -> this.StElem cfg offset) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I1 -> (fun _ _ -> this.StElemTyp TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I2 -> (fun _ _ -> this.StElemTyp TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I4 -> (fun _ _ -> this.StElemTyp TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I8 -> (fun _ _ -> this.StElemTyp TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_R4 -> (fun _ _ -> this.StElemTyp TypeUtils.float32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_R8 -> (fun _ _ -> this.StElemTyp TypeUtils.float64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_Ref -> (fun _ _ -> this.StElemRef) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I1 -> (fun _ _ -> this.ConvOvf TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I2 -> (fun _ _ -> this.ConvOvf TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I4 -> (fun _ _ -> this.ConvOvf TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I8 -> (fun _ _ -> this.ConvOvf TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I -> (fun _ _ -> convi) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U1 -> (fun _ _ -> this.ConvOvf TypeUtils.uint8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U2 -> (fun _ _ -> this.ConvOvf TypeUtils.uint16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U4 -> (fun _ _ -> this.ConvOvf TypeUtils.uint32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U8 -> (fun _ _ -> this.ConvOvf TypeUtils.uint64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U -> (fun _ _ -> convu) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I1_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I2_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I4_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I8_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I_Un -> (fun _ _ -> convi) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U1_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U2_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U4_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U8_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.uint64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U_Un -> (fun _ _ -> convu) |> fallThrough cfg offset cilState
            | OpCodeValues.Castclass -> this.CastClass |> forkThrough cfg offset cilState
            | OpCodeValues.Ldlen -> (fun _ _ -> this.LdLen) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldvirtftn -> this.LdVirtFtn |> forkThrough cfg offset cilState
            | OpCodeValues.Box -> this.Box |> forkThrough cfg offset cilState
            | OpCodeValues.Unbox -> this.Unbox |> forkThrough cfg offset cilState
            | OpCodeValues.Unbox_Any -> this.UnboxAny |> forkThrough cfg offset cilState
            | OpCodeValues.Add_Ovf_Un -> (fun _ _ -> this.Add_ovf_un) |> forkThrough cfg offset cilState
            | OpCodeValues.Sub_Ovf_Un -> (fun _ _ -> this.Sub_ovf_un) |> forkThrough cfg offset cilState
            | OpCodeValues.Mul_Ovf_Un -> (fun _ _ -> this.Mul_ovf_un) |> forkThrough cfg offset cilState
            | OpCodeValues.Add_Ovf -> (fun _ _ -> this.Add_ovf) |> forkThrough cfg offset cilState
            | OpCodeValues.Sub_Ovf -> (fun _ _ -> this.Sub_ovf) |> forkThrough cfg offset cilState
            | OpCodeValues.Mul_Ovf -> (fun _ _ -> this.Mul_ovf) |> forkThrough cfg offset cilState
            | OpCodeValues.Div -> (fun _ _ -> this.Div) |> forkThrough cfg offset cilState
            | OpCodeValues.Div_Un -> (fun _ _ -> this.DivUn) |> forkThrough cfg offset cilState
            | OpCodeValues.Rem -> (fun _ _ -> this.Rem) |> forkThrough cfg offset cilState
            | OpCodeValues.Rem_Un -> (fun _ _ -> this.RemUn) |> forkThrough cfg offset cilState
            | OpCodeValues.Newarr -> this.Newarr |> forkThrough cfg offset cilState
            | OpCodeValues.Throw -> this.Throw cilState
            | _ -> __unreachable__()

        let renewInstructionsInfo cilState =
            if not <| isUnhandledError cilState then
                x.IncrementLevelIfNeeded cfg offset cilState
        newSts |> List.iter renewInstructionsInfo
        newSts
