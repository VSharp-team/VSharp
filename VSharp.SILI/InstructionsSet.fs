namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Reflection.Emit

open VSharp
open VSharp.Core
open CFG

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

    let isEnum typ = typ |> ToDotNetType |> (fun t -> t.IsEnum)

    // [NOTE] there is no enums, because pushing to evaluation stack causes cast
    let rec signed2unsignedOrId = function
        | Numeric (Id typ) when typ = typedefof<int32> || typ = typedefof<uint32> -> uint32Type
        | Numeric (Id typ) when typ = typedefof<int8>  || typ = typedefof<uint8>  -> uint8Type
        | Numeric (Id typ) when typ = typedefof<int16> || typ = typedefof<uint16> -> uint16Type
        | Numeric (Id typ) when typ = typedefof<int64> || typ = typedefof<uint64> -> uint64Type
        | _ -> __unreachable__()

    let integers = [charType; int8Type; int16Type; int32Type; int64Type; uint8Type; uint16Type; uint32Type; uint64Type]

    let isIntegerTermType typ = integers |> List.contains typ || isEnum typ
    let isFloatTermType typ = typ = float32Type || typ = float64Type
    let isInteger = Terms.TypeOf >> isIntegerTermType
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
        let Zero = MakeNumber '\000'
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
        let MinValue = MakeNumber Int32.MinValue
        let MaxValue = MakeNumber Int32.MaxValue
    module UInt32 =
        let Zero = MakeNumber 0u
        let MaxValue = MakeNumber UInt32.MaxValue
    module Int64 =
        let Zero = MakeNumber 0L
        let MinusOne = MakeNumber -1L
        let MinValue = MakeNumber Int64.MinValue
        let MaxValue = MakeNumber Int64.MaxValue
    module UInt64 =
        let Zero = MakeNumber 0UL
        let MaxValue = MakeNumber UInt64.MaxValue

module internal InstructionsSet =
    open CilStateOperations
    open ipOperations

    let idTransformation term k = k term

    // --------------------------------------- Metadata Interaction ----------------------------------------

    let resolveFieldFromMetadata (cfg : cfgData) = TokenResolver.resolveFieldFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTypeFromMetadata (cfg : cfgData) = TokenResolver.resolveTypeFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTermTypeFromMetadata (cfg : cfgData) = resolveTypeFromMetadata cfg >> Types.FromDotNetType
    let resolveMethodFromMetadata (cfg : cfgData) = TokenResolver.resolveMethodFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTokenFromMetadata (cfg : cfgData) = TokenResolver.resolveTokenFromMetadata cfg.methodBase cfg.ilBytes

    // ------------------------------- Environment interaction -------------------------------

    let rec internalCall (methodInfo : MethodInfo) (argsAndThis : term list) (s : state) (k : state list -> 'a) =
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

        let pushOnEvaluationStack (term : term, state : state) =
            match term.term with
            | Nop -> ()
            | _ -> state.evaluationStack <- EvaluationStack.Push term state.evaluationStack
        match result with
        | :? term as r -> pushOnEvaluationStack(r, s); k [s]
        | :? ((term * state) list) as r -> List.iter pushOnEvaluationStack r; k (r |> List.unzip |> snd)
        | _ -> internalfail "internal call should return tuple term * state!"

    // ------------------------------- CIL instructions -------------------------------

    let referenceLocalVariable index (methodBase : MethodBase) =
        let lvi = methodBase.GetMethodBody().LocalVariables.[index]
        let stackKey = LocalVariableKey(lvi, methodBase)
        Ref (PrimitiveStackLocation stackKey)
    let getArgTerm index (methodBase : MethodBase) =
        let pi = methodBase.GetParameters().[index]
        PrimitiveStackLocation (ParameterKey pi) |> Ref

    let castUnchecked typ term : term =
        Types.Cast term typ
    let ldc numberCreator t (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let num = numberCreator cfg.ilBytes shiftedOffset
        let termType = Types.FromDotNetType t
        push (Concrete num termType) cilState

    let ldloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let reference = referenceLocalVariable index cfg.methodBase
        let term = Memory.Read cilState.state reference
        push term cilState

    let ldarg numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let arg =
            let state = cilState.state
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis state cfg.methodBase
            match this, cfg.methodBase.IsStatic with
            | None, _
            | Some _, true ->
                let term = getArgTerm argumentIndex cfg.methodBase
                Memory.Read state term
            | Some this, _ when argumentIndex = 0 -> this
            | Some _, false ->
                let term = getArgTerm (argumentIndex - 1) cfg.methodBase
                Memory.Read state term
        push arg cilState
    let ldarga numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let address =
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.methodBase
            match this with
            | None -> getArgTerm argumentIndex cfg.methodBase
            | Some _ when argumentIndex = 0 -> internalfail "can't load address of ``this''"
            | Some _ -> getArgTerm (argumentIndex - 1) cfg.methodBase
        push address cilState
    let stloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let variableIndex = numberCreator cfg.ilBytes shiftedOffset
        let right = pop cilState
        let location = referenceLocalVariable variableIndex cfg.methodBase
        let typ = TypeOfLocation location
        let value = castUnchecked typ right
        let states = Memory.WriteSafe cilState.state location value
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
            let opCode = Instruction.parseInstruction m offset
            Instruction.isDemandingCallOpCode opCode
        | _ -> false

    let ret (cfg : cfgData) (cilState : cilState) =
        let resultTyp = Reflection.getMethodReturnType cfg.methodBase |> Types.FromDotNetType
        if resultTyp <> Void then
            let res = pop cilState
            let castedResult = castUnchecked resultTyp res
            push castedResult cilState
        match cilState.ipStack with
        | _ :: ips -> cilState.ipStack <- (Exit cfg.methodBase) :: ips
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
    let starg numCreator (cfg : cfgData) offset (cilState : cilState) =
        let argumentIndex = numCreator cfg.ilBytes offset
        let argTerm =
           let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.methodBase
           match this with
           | None -> getArgTerm argumentIndex cfg.methodBase
           | Some this when argumentIndex = 0 -> this
           | Some _ -> getArgTerm (argumentIndex - 1) cfg.methodBase
        let value = pop cilState
        let states = Memory.WriteSafe cilState.state argTerm value
        states |> List.map (changeState cilState)
    let brcommon condTransform (cfgData : cfgData) (offset : offset) (cilState : cilState) =
        let cond = pop cilState
        let m = cfgData.methodBase
        let ipThen, ipElse =
           match Instruction.conditionalBranchTarget m offset with
           | offsetThen, [offsetElse] -> instruction m offsetThen, instruction m offsetElse
           | _ -> __unreachable__()
        StatedConditionalExecutionCIL cilState
           (fun state k -> k (condTransform <| transform2BooleanTerm state.pc cond, state))
           (fun cilState k -> setCurrentIp ipThen cilState; k [cilState])
           (fun cilState k -> setCurrentIp ipElse cilState; k [cilState])
           id
    let brfalse = brcommon id
    let brtrue = brcommon (!!)
    let applyAndBranch additionalFunction brtrueFunction (cfg : cfgData) offset (cilState : cilState) =
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
            else
                let typ = Types.FromDotNetType parInfo.ParameterType
                castUnchecked typ parameter
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
    let ldstr (cfg : cfgData) offset (cilState : cilState) =
        let stringToken = NumberCreator.extractInt32 cfg.ilBytes (offset + OpCodes.Ldstr.Size)
        let string = cfg.methodBase.Module.ResolveString stringToken
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
    let castTopOfOperationalStackUnchecked targetType (cilState : cilState) =
        let t = pop cilState
        let termForStack = castUnchecked targetType t
        push termForStack cilState
    let ldloca numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let term = referenceLocalVariable index cfg.methodBase
        push term cilState
    let switch (cfgData : cfgData) offset (cilState : cilState) =
        let m = cfgData.methodBase
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
            match Instruction.conditionalBranchTarget m offset with
            | fall,  rests -> instruction m fall, List.map (instruction m) rests
        let casesAndOffsets = List.mapi (fun i offset -> value === MakeNumber i, offset) restIps
        let fallThroughGuard = Arithmetics.(>>=) value (List.length restIps |> MakeNumber)
        Cps.List.foldrk checkOneCase cilState ((fallThroughGuard, fallThroughIp)::casesAndOffsets) (fun _ k -> k []) id
    let ldtoken (cfg : cfgData) offset (cilState : cilState) =
        let memberInfo = resolveTokenFromMetadata cfg (offset + OpCodes.Ldtoken.Size)
        let res =
            match memberInfo with // TODO: should create real RuntimeHandle struct #hack
            | :? FieldInfo as fi -> Terms.Concrete fi.FieldHandle (Types.FromDotNetType typeof<RuntimeFieldHandle>)
            | :? Type as t -> Terms.Concrete t.TypeHandle (Types.FromDotNetType typeof<RuntimeTypeHandle>)
            | :? MethodInfo as mi -> Terms.Concrete mi.MethodHandle (Types.FromDotNetType typeof<RuntimeMethodHandle>)
            | _ -> internalfailf "Could not resolve token"
        push res cilState
    let ldftn (cfg : cfgData) offset (cilState : cilState) =
        let methodInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Ldftn.Size)
        let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType (methodInfo.GetType()))
        push methodPtr cilState
    let initobj (cfg : cfgData) offset (cilState : cilState) =
        let targetAddress = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Initobj.Size)
        let states = Memory.WriteSafe cilState.state targetAddress (Memory.DefaultOf typ)
        states |> List.map (changeState cilState)
    let ldind valueCast (cilState : cilState) =
        // TODO: what about null pointers?
        let address = pop cilState
        let value = Memory.Read cilState.state address |> valueCast
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
    let isinst (cfg : cfgData) offset (cilState : cilState) =
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
    let ldobj (cfg : cfgData) offset (cilState : cilState) =
        let address = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldobj.Size)
        let value = Memory.Read cilState.state address
        let typedValue = castUnchecked typ value
        push typedValue cilState
    let stobj (cfg : cfgData) offset (cilState : cilState) =
        let src, dest = pop2 cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Stobj.Size)
        let value = castUnchecked typ src
        let states = Memory.WriteSafe cilState.state dest value
        states |> List.map (changeState cilState)
    let stind valueCast (cilState : cilState) =
        let value, address = pop2 cilState
        let value = valueCast value
        let states = Memory.WriteSafe cilState.state address value
        states |> List.map (changeState cilState)
    let sizeofInstruction (cfg : cfgData) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Sizeof.Size)
        let size = Types.SizeOf typ
        push (MakeNumber size) cilState
    let leave (cfg : cfgData) offset (cilState : cilState) =
        let m = cfg.methodBase
        let dst = Instruction.unconditionalBranchTarget m offset
        let ehcs = m.GetMethodBody().ExceptionHandlingClauses
                    |> Seq.filter Instruction.isFinallyClause
                    |> Seq.filter (Instruction.shouldExecuteFinallyClause offset dst)
                    |> Seq.sortWith (fun ehc1 ehc2 -> ehc1.HandlerOffset - ehc2.HandlerOffset)
                    |> List.ofSeq
        let currentIp =
            match ehcs with
            | [] -> Instruction(dst, m)
            | e :: ehcs -> leave (Instruction(e.HandlerOffset, m)) ehcs dst m
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
    let br (cfgData : cfgData) offset (cilState : cilState) =
        let m = cfgData.methodBase
        let newIp = instruction m (Instruction.unconditionalBranchTarget m offset)
        setCurrentIp newIp cilState

    // TODO: implement fully (using information about calling method):
    // TODO: - if thisType is a value type and thisType implements method then ptr is passed unmodified
    // TODO: - if thisType is a value type and thisType does not implement method then ptr is dereferenced and boxed
    let constrained cfg offset (cilState : cilState) =
        match Instruction.findNextInstructionOffsetAndEdges OpCodes.Constrained cfg.ilBytes offset with
        | FallThrough offset ->
            let method = resolveMethodFromMetadata cfg (offset + OpCodes.Callvirt.Size)
            let n = method.GetParameters().Length
            let args, evaluationStack = EvaluationStack.PopMany n cilState.state.evaluationStack
            setEvaluationStack evaluationStack cilState
            let thisForCallVirt = pop cilState
            match thisForCallVirt.term with
            | HeapRef _ -> ()
            | Ref _ when TypeOf thisForCallVirt |> Types.IsValueType ->
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

    let inline private fallThroughImpl stackSizeBefore newIp cilState =
        // if not constructing runtime exception
        if not <| isError cilState && Memory.CallStackSize cilState.state = stackSizeBefore then
            setCurrentIp newIp cilState

    let fallThrough (cfgData : cfgData) offset cilState op =
        assert(not <| isError cilState)
        let stackSizeBefore = Memory.CallStackSize cilState.state
        let m = cfgData.methodBase
        let newIp = instruction m (Instruction.fallThroughTarget m offset)
        op cfgData offset cilState
        fallThroughImpl stackSizeBefore newIp cilState
        [cilState]

    let forkThrough (cfgData : cfgData) offset cilState op =
        assert(not <| isError cilState)
        let stackSizeBefore = Memory.CallStackSize cilState.state
        let m = cfgData.methodBase
        let newIp = instruction m (Instruction.fallThroughTarget m offset)
        let cilStates = op cfgData offset cilState
        List.iter (fallThroughImpl stackSizeBefore newIp) cilStates
        cilStates
