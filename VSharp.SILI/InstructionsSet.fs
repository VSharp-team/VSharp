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

    let hashFunction (opcode : OpCode) =
        let v = opcode.Value |> int
        let offsetToMakeReadonlyOpcode256 = 226
        if v >= 0 then v
        else -v - offsetToMakeReadonlyOpcode256

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
            | Nop -> state
            | _ -> {state with evaluationStack = EvaluationStack.Push term state.evaluationStack}
        match result with
        | :? (term * state) as r -> pushOnEvaluationStack r |> List.singleton |> k
        | :? ((term * state) list) as r -> List.map pushOnEvaluationStack r |> k
        | _ -> internalfail "internal call should return tuple term * state!"

    // ------------------------------- CIL instructions -------------------------------

    let referenceLocalVariable index (methodBase : MethodBase) =
        let lvi = methodBase.GetMethodBody().LocalVariables.[index]
        let stackKey = LocalVariableKey(lvi, methodBase)
        Ref (PrimitiveStackLocation stackKey)
    let getArgTerm index (methodBase : MethodBase) =
        let pi = methodBase.GetParameters().[index]
        PrimitiveStackLocation (ParameterKey pi) |> Ref

    let castReferenceToPointerIfNeeded term typ state =
        if IsReference term && Types.IsPointer typ
        then Types.CastReferenceToPointer state term
        else term
    let castUnchecked typ term (state : state) : term =
        let term = castReferenceToPointerIfNeeded term typ state
        Types.Cast term typ
    let ldc numberCreator t (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let num = numberCreator cfg.ilBytes shiftedOffset
        let termType = Types.FromDotNetType t
        push (Concrete num termType) cilState |> List.singleton

    let ldloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let reference = referenceLocalVariable index cfg.methodBase
        let term = Memory.ReadSafe cilState.state reference
        push term cilState |> List.singleton

    let ldarg numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let arg =
            let state = cilState.state
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis state cfg.methodBase
            match this, cfg.methodBase.IsStatic with
            | None, _
            | Some _, true ->
                let term = getArgTerm argumentIndex cfg.methodBase
                Memory.ReadSafe state term
            | Some this, _ when argumentIndex = 0 -> this
            | Some _, false ->
                let term = getArgTerm (argumentIndex - 1) cfg.methodBase
                Memory.ReadSafe state term
        push arg cilState |> List.singleton
    let ldarga numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let address =
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.methodBase
            match this with
            | None -> getArgTerm argumentIndex cfg.methodBase
            | Some _ when argumentIndex = 0 -> internalfail "can't load address of ``this''"
            | Some _ -> getArgTerm (argumentIndex - 1) cfg.methodBase
        push address cilState |> List.singleton
    let stloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let variableIndex = numberCreator cfg.ilBytes shiftedOffset
        let right, cilState = pop cilState
        let left = referenceLocalVariable variableIndex cfg.methodBase
        let typ = TypeOf left
        let state = cilState.state
        let value = castUnchecked typ right state
        let states = Memory.WriteSafe state left value
        states |> List.map (fun state -> cilState |> withState state)
    let private simplifyConditionResult state res k =
        if WithPathCondition state !!res |> IsFalsePathCondition then k True
        elif WithPathCondition state res |> IsFalsePathCondition then k False
        else k res
    let performCILUnaryOperation op (cilState : cilState) =
        let x, cilState = pop cilState
        API.PerformUnaryOperation op x (fun interimRes ->
        let res = if Terms.TypeOf x |> Types.IsBool then simplifyConditionResult cilState.state interimRes id else interimRes
        push res cilState |> List.singleton)

    let performCILBinaryOperation op operand1Transform operand2Transform resultTransform (cilState : cilState) =
        let arg2, arg1, cilState = pop2 cilState
        operand1Transform arg1 (fun arg1 ->
        operand2Transform arg2 (fun arg2 ->
        API.PerformBinaryOperation op arg1 arg2 (fun interimRes ->
        resultTransform interimRes (fun res ->
        push res cilState |> List.singleton))))
    let standardPerformBinaryOperation op =
        performCILBinaryOperation op idTransformation idTransformation idTransformation
    let dup (cilState : cilState) =
        let x, cilState = pop cilState
        cilState |> push x |> push x |> List.singleton

    let isCallIp (ip : ip) =
        match ip with
        | Instruction(offset, m) ->
            let opCode = Instruction.parseInstruction m offset
            Instruction.isDemandingCallOpCode opCode
        | _ -> false

    let ret (cfg : cfgData) _ (cilState : cilState) : cilState list =
        let resultTyp = Reflection.getMethodReturnType cfg.methodBase |> Types.FromDotNetType
        let cilState =
            if resultTyp = Void then cilState
            else
                let res, cilState = pop cilState
                let castedResult = castUnchecked resultTyp res cilState.state
                push castedResult cilState
        match cilState.ipStack with
        | _ :: ips -> {cilState with ipStack = (Exit cfg.methodBase) :: ips} |> List.singleton
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
        let y, x, _ = pop2 cilState
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
        let value, cilState = pop cilState
        let states = Memory.WriteSafe cilState.state argTerm value
        states |> List.map (fun state -> cilState |> withState state)
    let brcommon condTransform (cfgData : cfgData) (offset : offset) (cilState : cilState) =
        let cond, cilState = pop cilState
        let m = cfgData.methodBase
        let ipThen, ipElse =
           match Instruction.conditionalBranchTarget m offset with
           | offsetThen, [offsetElse] -> instruction m offsetThen, instruction m offsetElse
           | _ -> __unreachable__()
        StatedConditionalExecutionCIL cilState
           (fun state k -> k (condTransform <| transform2BooleanTerm state.pc cond, state))
           (fun cilState k -> k [setCurrentIp ipThen cilState])
           (fun cilState k -> k [setCurrentIp ipElse cilState])
           id
    let brfalse = brcommon id
    let brtrue = brcommon (!!)
    let applyAndBranch errorStr additionalFunction brtrueFunction (cfg : cfgData) offset (cilState : cilState) =
        match additionalFunction cilState with
        | [st] -> brtrueFunction cfg offset st
        | _ -> internalfail errorStr
    let boolToInt b =
        BranchExpressions (fun k -> k b) (fun k -> k TypeUtils.Int32.One) (fun k -> k TypeUtils.Int32.Zero) id
    let bitwiseOrBoolOperation bitwiseOp boolOp (cilState : cilState) =
        let arg2, arg1, _ = pop2 cilState
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
        let arg, _ = pop cilState
        let op =
            match TypeOf arg with
            | Bool -> OperationType.LogicalNot
            | _ -> OperationType.BitwiseNot
        performCILUnaryOperation op cilState
    let retrieveActualParameters (methodBase : MethodBase) (cilState : cilState) =
        let paramsNumber = methodBase.GetParameters().Length
        let parameters, evaluationStack = EvaluationStack.PopArguments paramsNumber cilState.state.evaluationStack
        let castParameter parameter (parInfo : ParameterInfo) =
            if Reflection.isDelegateConstructor methodBase && parInfo.ParameterType = typeof<IntPtr> then parameter
            else
                let typ = Types.FromDotNetType parInfo.ParameterType
                castUnchecked typ parameter cilState.state
        let parameters = Seq.map2 castParameter (List.rev parameters) (methodBase.GetParameters()) |> List.ofSeq
        parameters, withEvaluationStack evaluationStack cilState

    let makeUnsignedInteger term k =
        match TypeOf term with
        | Bool -> k <| Types.Cast term TypeUtils.uint32Type
        | Numeric (Id t) when t = typeof<double> || t = typeof<float> -> k term
        | Numeric _ as t -> k <| Types.Cast term (TypeUtils.signed2unsignedOrId t) // no specs found about overflows
        | _ -> k term
    let performUnsignedIntegerOperation op (cilState : cilState) =
        let arg2, arg1, _ = pop2 cilState
        if TypeUtils.isInteger arg1 && TypeUtils.isInteger arg2 then
            performCILBinaryOperation op makeUnsignedInteger makeUnsignedInteger idTransformation cilState
        else internalfailf "arguments for %O are not Integers!" op
    let ldstr (cfg : cfgData) offset (cilState : cilState) =
        let stringToken = NumberCreator.extractInt32 cfg.ilBytes (offset + OpCodes.Ldstr.Size)
        let string = cfg.methodBase.Module.ResolveString stringToken
        let reference, state = Memory.AllocateString string cilState.state
        cilState |> withState state |> push reference |> List.singleton
    let allocateValueTypeInHeap v (cilState : cilState) =
        let address, state = Memory.BoxValueType cilState.state v
        cilState |> withState state |> push address |> List.singleton
    let ldnull (cilState : cilState) = push NullRef cilState |> List.singleton
    let convu (initialCilState : cilState) =
        let value, cilState = pop initialCilState
        let ptr = MakeIntPtr value cilState.state
        cilState |> push ptr |> List.singleton
    let convi = convu
    let castTopOfOperationalStackUnchecked targetType (cilState : cilState) =
        let t, cilState = pop cilState
        let termForStack = castUnchecked targetType t cilState.state
        cilState |> push termForStack |> List.singleton
    let ldloca numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let term = referenceLocalVariable index cfg.methodBase
        push term cilState |> List.singleton
    let switch (cfgData : cfgData) offset (cilState : cilState) =
        let m = cfgData.methodBase
        let value, cilState = pop cilState
        let value = makeUnsignedInteger value id
        let checkOneCase (guard, newIp) cilState kRestCases =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (guard, state))
                (fun cilState k -> k [setCurrentIp newIp cilState])
                (fun _ k -> kRestCases cilState k) // ignore pc because we always know that cases do not overlap
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
        push res cilState |> List.singleton
    let ldftn (cfg : cfgData) offset (cilState : cilState) =
        let methodInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Ldftn.Size)
        let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType (methodInfo.GetType()))
        push methodPtr cilState |> List.singleton
    let initobj (cfg : cfgData) offset (cilState : cilState) =
        let targetAddress, cilState = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Initobj.Size)
        let states = Memory.WriteSafe cilState.state targetAddress (Memory.DefaultOf typ)
        states |> List.map (fun state -> cilState |> withState state)
    let ldind valueCast (cilState : cilState) =
        let address, cilState = pop cilState
        let value = Memory.ReadSafe cilState.state address
        let value = valueCast value cilState.state
        cilState |> push value |> List.singleton
    let clt = binaryOperationWithBoolResult OperationType.Less idTransformation idTransformation
    let cgt = binaryOperationWithBoolResult OperationType.Greater idTransformation idTransformation
    let cltun = binaryOperationWithBoolResult OperationType.Less_Un makeUnsignedInteger makeUnsignedInteger
    let bgeHelper (cilState : cilState) =
        let arg1, arg2, _ = pop2 cilState
        let typ1, typ2 = Terms.TypeOf arg1, Terms.TypeOf arg2
        if Types.IsInteger typ1 && Types.IsInteger typ2 then clt cilState
        elif Types.IsReal typ1 && Types.IsReal typ2 then cltun cilState
        else __notImplemented__()
    let isinst (cfg : cfgData) offset (cilState : cilState) =
        let object, cilState = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Isinst.Size)
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference object, state))
            (fun cilState k -> cilState |> push NullRef |> List.singleton |> k)
            (fun cilState k ->
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.IsCast cilState.state object typ, state))
                    (fun cilState k -> cilState |> push object |> List.singleton |> k)
                    (fun cilState k -> cilState |> push NullRef |> List.singleton |> k)
                    k)
            id
    let cgtun (cilState : cilState) =
        let arg2, arg1, _ = pop2 cilState
        if IsReference arg2 && IsReference arg1 then
            binaryOperationWithBoolResult OperationType.NotEqual idTransformation idTransformation cilState
        else binaryOperationWithBoolResult OperationType.Greater_Un makeUnsignedInteger makeUnsignedInteger cilState
    let ldobj (cfg : cfgData) offset (cilState : cilState) =
        let address, cilState = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldobj.Size)
        let value = Memory.ReadSafe cilState.state address
        let typedValue = castUnchecked typ value cilState.state
        cilState |> push typedValue |> List.singleton
    let stobj (cfg : cfgData) offset (cilState : cilState) =
        let src, dest, cilState = pop2 cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Stobj.Size)
        let value = castUnchecked typ src cilState.state
        let states = Memory.WriteSafe cilState.state dest value
        states |> List.map (fun state -> cilState |> withState state)
    let stind valueCast (cilState : cilState) =
        let value, address, cilState = pop2 cilState
        let value = valueCast value cilState.state
        let states = Memory.WriteSafe cilState.state address value
        states |> List.map (fun state -> cilState |> withState state)
    let sizeofInstruction (cfg : cfgData) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Sizeof.Size)
        let size = Types.SizeOf typ
        cilState |> push (MakeNumber size) |> List.singleton
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
        setCurrentIp currentIp cilState :: []
    let rethrow _ _ (cilState : cilState) =
        let state = cilState.state
        assert(Option.isSome state.exceptionsRegister.ExceptionTerm)
        let state = {state with exceptionsRegister = state.exceptionsRegister.TransformToUnhandled()}
        { cilState with state = state} |> List.singleton
    let endfilter _ _ (cilState : cilState) =
        let value, tmp = pop cilState
        if tmp.state.evaluationStack = emptyEvaluationStack then {cilState with filterResult = Some value} :: []
        else __notImplemented__()
    let endfinally _ _ _ =
        // Should be handled in makeStep function
        __unreachable__()
    let br (cfgData : cfgData) offset (cilState : cilState) =
        let m = cfgData.methodBase
        let newIp = instruction m (Instruction.unconditionalBranchTarget m offset)
        setCurrentIp newIp cilState |> List.singleton

    // TODO: implement fully (using information about calling method):
    // TODO: - if thisType is a value type and thisType implements method then ptr is passed unmodified
    // TODO: - if thisType is a value type and thisType does not implement method then ptr is dereferenced and boxed
    let constrained cfg offset (initialCilState : cilState) =
        match Instruction.findNextInstructionOffsetAndEdges OpCodes.Constrained cfg.ilBytes offset with
        | FallThrough offset ->
            let method = resolveMethodFromMetadata cfg (offset + OpCodes.Callvirt.Size)
            let n = method.GetParameters().Length
            let args, evaluationStack = EvaluationStack.PopArguments n initialCilState.state.evaluationStack
            let cilState = withEvaluationStack evaluationStack initialCilState
            let thisForCallVirt, cilState = pop cilState
            match thisForCallVirt.term with
            | HeapRef _ -> List.singleton initialCilState
            | Ref _ when TypeOf thisForCallVirt |> Types.IsValueType ->
                let thisStruct = Memory.ReadSafe cilState.state thisForCallVirt
                let heapRef, state = Memory.BoxValueType cilState.state thisStruct
                let cilStateWithNewThis = withState state cilState |> push heapRef
                List.foldBack push args cilStateWithNewThis |> List.singleton
            | Ref _ ->
                let this = Memory.ReadSafe cilState.state thisForCallVirt
                let cilStateWithNewThis = push this cilState
                List.foldBack push args cilStateWithNewThis |> List.singleton
            | _ -> __unreachable__()
        | _ -> __unreachable__()
    let localloc _ _ (cilState : cilState) =
        // [NOTE] locallloc usually is used for Span
        // So, pushing nullptr, because array will be allocated in Span constructor
        let _, cilState = pop cilState
        push (Ptr None Void None) cilState |> List.singleton
    let zipWithOneOffset op (cfgData : cfgData) offset cilState =
        assert(not <| isError cilState)
        let m = cfgData.methodBase
        let newIp = instruction m (Instruction.fallThroughTarget m offset)
        let cilStates = op cfgData offset cilState
        let errors, goods = cilStates |> List.partition isError
        let changeIpIfNeeded (newState : cilState) =
            if Memory.CallStackSize newState.state = Memory.CallStackSize cilState.state then setCurrentIp newIp newState
            else newState // case when constructing runtime exception
        errors @ List.map changeIpIfNeeded goods

    let opcode2Function : (cfgData -> offset -> cilState -> cilState list) [] = Array.create 300 (fun _ _ _ -> internalfail "Interpreter is not ready")
    opcode2Function.[hashFunction OpCodes.Br]                 <- br
    opcode2Function.[hashFunction OpCodes.Br_S]               <- br
    opcode2Function.[hashFunction OpCodes.Add]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Add // TODO: check float overflow [spec]
    opcode2Function.[hashFunction OpCodes.Mul]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Multiply
    opcode2Function.[hashFunction OpCodes.Sub]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Subtract
    opcode2Function.[hashFunction OpCodes.Shl]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.ShiftLeft
    opcode2Function.[hashFunction OpCodes.Shr]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight
    opcode2Function.[hashFunction OpCodes.Shr_Un]             <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight_Un
    opcode2Function.[hashFunction OpCodes.Ceq]                <- zipWithOneOffset <| fun _ _ -> ceq
    opcode2Function.[hashFunction OpCodes.Cgt]                <- zipWithOneOffset <| fun _ _ -> cgt
    opcode2Function.[hashFunction OpCodes.Cgt_Un]             <- zipWithOneOffset <| fun _ _ -> cgtun
    opcode2Function.[hashFunction OpCodes.Clt]                <- zipWithOneOffset <| fun _ _ -> clt
    opcode2Function.[hashFunction OpCodes.Clt_Un]             <- zipWithOneOffset <| fun _ _ -> cltun
    opcode2Function.[hashFunction OpCodes.And]                <- zipWithOneOffset <| fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseAnd OperationType.LogicalAnd
    opcode2Function.[hashFunction OpCodes.Or]                 <- zipWithOneOffset <| fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseOr OperationType.LogicalOr
    opcode2Function.[hashFunction OpCodes.Xor]                <- zipWithOneOffset <| fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseXor OperationType.LogicalXor
    opcode2Function.[hashFunction OpCodes.Neg]                <- zipWithOneOffset <| fun _ _ -> performCILUnaryOperation OperationType.UnaryMinus
    opcode2Function.[hashFunction OpCodes.Not]                <- zipWithOneOffset <| fun _ _ -> bitwiseOrBoolNot
    opcode2Function.[hashFunction OpCodes.Stloc]              <- zipWithOneOffset <| stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Stloc.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Stloc_0]            <- zipWithOneOffset <| stloc (fun _ _ -> 0)
    opcode2Function.[hashFunction OpCodes.Stloc_1]            <- zipWithOneOffset <| stloc (fun _ _ -> 1)
    opcode2Function.[hashFunction OpCodes.Stloc_2]            <- zipWithOneOffset <| stloc (fun _ _ -> 2)
    opcode2Function.[hashFunction OpCodes.Stloc_3]            <- zipWithOneOffset <| stloc (fun _ _ -> 3)
    opcode2Function.[hashFunction OpCodes.Stloc_S]            <- zipWithOneOffset <| stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Stloc_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Starg]              <- zipWithOneOffset <| starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Starg.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Starg_S]            <- zipWithOneOffset <| starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Starg_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldc_I4]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractInt32 ilBytes (offset + OpCodes.Ldc_I4.Size)) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_0]           <- zipWithOneOffset <| ldc (fun _ _ -> 0) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_1]           <- zipWithOneOffset <| ldc (fun _ _ -> 1) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_2]           <- zipWithOneOffset <| ldc (fun _ _ -> 2) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_3]           <- zipWithOneOffset <| ldc (fun _ _ -> 3) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_4]           <- zipWithOneOffset <| ldc (fun _ _ -> 4) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_5]           <- zipWithOneOffset <| ldc (fun _ _ -> 5) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_6]           <- zipWithOneOffset <| ldc (fun _ _ -> 6) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_7]           <- zipWithOneOffset <| ldc (fun _ _ -> 7) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_8]           <- zipWithOneOffset <| ldc (fun _ _ -> 8) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_M1]          <- zipWithOneOffset <| ldc (fun _ _ -> -1) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_S]           <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractInt8 ilBytes (offset + OpCodes.Ldc_I4_S.Size)) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I8]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractInt64 ilBytes (offset + OpCodes.Ldc_I8.Size)) typedefof<int64>
    opcode2Function.[hashFunction OpCodes.Ldc_R4]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractFloat32 ilBytes (offset + OpCodes.Ldc_R4.Size)) typedefof<float32>
    opcode2Function.[hashFunction OpCodes.Ldc_R8]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractFloat64 ilBytes (offset + OpCodes.Ldc_R8.Size)) typedefof<double>
    opcode2Function.[hashFunction OpCodes.Ldarg]              <- zipWithOneOffset <| ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarg.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldarg_0]            <- zipWithOneOffset <| ldarg (fun _ _ -> 0)
    opcode2Function.[hashFunction OpCodes.Ldarg_1]            <- zipWithOneOffset <| ldarg (fun _ _ -> 1)
    opcode2Function.[hashFunction OpCodes.Ldarg_2]            <- zipWithOneOffset <| ldarg (fun _ _ -> 2)
    opcode2Function.[hashFunction OpCodes.Ldarg_3]            <- zipWithOneOffset <| ldarg (fun _ _ -> 3)
    opcode2Function.[hashFunction OpCodes.Ldarg_S]            <- zipWithOneOffset <| ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarg_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Nop]                <- zipWithOneOffset <| fun _ _ st -> [st]
    opcode2Function.[hashFunction OpCodes.Ldloc]              <- zipWithOneOffset <| ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloc.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldloc_0]            <- zipWithOneOffset <| ldloc (fun _ _ -> 0)
    opcode2Function.[hashFunction OpCodes.Ldloc_1]            <- zipWithOneOffset <| ldloc (fun _ _ -> 1)
    opcode2Function.[hashFunction OpCodes.Ldloc_2]            <- zipWithOneOffset <| ldloc (fun _ _ -> 2)
    opcode2Function.[hashFunction OpCodes.Ldloc_3]            <- zipWithOneOffset <| ldloc (fun _ _ -> 3)
    opcode2Function.[hashFunction OpCodes.Ldloc_S]            <- zipWithOneOffset <| ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloc_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldloca]             <- zipWithOneOffset <| ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloca.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldloca_S]           <- zipWithOneOffset <| ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloca_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ret]                <- ret
    opcode2Function.[hashFunction OpCodes.Dup]                <- zipWithOneOffset <| fun _ _ -> dup

    // branching
    opcode2Function.[hashFunction OpCodes.Brfalse]            <- brfalse
    opcode2Function.[hashFunction OpCodes.Brfalse_S]          <- brfalse
    opcode2Function.[hashFunction OpCodes.Brtrue]             <- brtrue
    opcode2Function.[hashFunction OpCodes.Brtrue_S]           <- brtrue
    opcode2Function.[hashFunction OpCodes.Beq]                <- applyAndBranch "Beq" ceq brtrue
    opcode2Function.[hashFunction OpCodes.Beq_S]              <- applyAndBranch "Beq_S" ceq brtrue
    opcode2Function.[hashFunction OpCodes.Bge]                <- applyAndBranch "Bge" bgeHelper brfalse
    opcode2Function.[hashFunction OpCodes.Bge_S]              <- applyAndBranch "Bge_S" bgeHelper brfalse

    opcode2Function.[hashFunction OpCodes.Bgt]                <- applyAndBranch "Bgt" cgt brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_S]              <- applyAndBranch "Bgt_S" cgt brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_Un]             <- applyAndBranch "Bgt_Un" cgtun brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_Un_S]           <- applyAndBranch "Bgt_Un_S" cgtun brtrue
    opcode2Function.[hashFunction OpCodes.Ble]                <- applyAndBranch "Ble" cgt brfalse
    opcode2Function.[hashFunction OpCodes.Ble_S]              <- applyAndBranch "Ble_S" cgt brfalse
    opcode2Function.[hashFunction OpCodes.Ble_Un]             <- applyAndBranch "Ble_Un" cgtun brfalse
    opcode2Function.[hashFunction OpCodes.Ble_Un_S]           <- applyAndBranch "Ble_Un_S" cgtun brfalse
    opcode2Function.[hashFunction OpCodes.Blt]                <- applyAndBranch "Blt" clt brtrue
    opcode2Function.[hashFunction OpCodes.Blt_S]              <- applyAndBranch "Blt_S" clt brtrue
    opcode2Function.[hashFunction OpCodes.Blt_Un]             <- applyAndBranch "Blt_Un" cltun brtrue
    opcode2Function.[hashFunction OpCodes.Blt_Un_S]           <- applyAndBranch "Blt_Un_S" cltun brtrue
    opcode2Function.[hashFunction OpCodes.Bne_Un]             <- applyAndBranch "Bne_Un" ceq brfalse
    opcode2Function.[hashFunction OpCodes.Bne_Un_S]           <- applyAndBranch "Bne_Un_S" ceq brfalse
    opcode2Function.[hashFunction OpCodes.Bge_Un]             <- applyAndBranch "Bge_Un" cltun brfalse
    opcode2Function.[hashFunction OpCodes.Bge_Un_S]           <- applyAndBranch "Bge_Un_S" cltun brfalse

    opcode2Function.[hashFunction OpCodes.Ldstr]              <- zipWithOneOffset <| ldstr
    opcode2Function.[hashFunction OpCodes.Ldnull]             <- zipWithOneOffset <| fun _ _ -> ldnull
    opcode2Function.[hashFunction OpCodes.Conv_I1]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int8Type
    opcode2Function.[hashFunction OpCodes.Conv_I2]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int16Type
    opcode2Function.[hashFunction OpCodes.Conv_I4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_I8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int64Type
    opcode2Function.[hashFunction OpCodes.Conv_R4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float32Type
    opcode2Function.[hashFunction OpCodes.Conv_R8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64Type
    opcode2Function.[hashFunction OpCodes.Conv_U1]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint8Type
    opcode2Function.[hashFunction OpCodes.Conv_U2]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint16Type
    opcode2Function.[hashFunction OpCodes.Conv_U4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint32Type
    opcode2Function.[hashFunction OpCodes.Conv_U8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint64Type
    opcode2Function.[hashFunction OpCodes.Conv_I]             <- zipWithOneOffset <| fun _ _ -> convi
    opcode2Function.[hashFunction OpCodes.Conv_U]             <- zipWithOneOffset <| fun _ _ -> convu
    opcode2Function.[hashFunction OpCodes.Conv_R_Un]          <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64Type
    opcode2Function.[hashFunction OpCodes.Switch]             <- switch
    opcode2Function.[hashFunction OpCodes.Ldtoken]            <- zipWithOneOffset <| ldtoken
    opcode2Function.[hashFunction OpCodes.Ldftn]              <- zipWithOneOffset <| ldftn
    opcode2Function.[hashFunction OpCodes.Pop]                <- zipWithOneOffset <| fun _ _ st -> pop st |> snd |> List.singleton
    opcode2Function.[hashFunction OpCodes.Initobj]            <- zipWithOneOffset <| initobj
    opcode2Function.[hashFunction OpCodes.Ldarga]             <- zipWithOneOffset <| ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarga.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldarga_S]           <- zipWithOneOffset <| ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarga_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldind_I4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.int32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I1]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.int8Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I2]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.int16Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I8]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.int64Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U1]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.uint8Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U2]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.uint16Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.uint32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_R4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.float32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_R8]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked TypeUtils.float64Type)
    opcode2Function.[hashFunction OpCodes.Ldind_Ref]          <- zipWithOneOffset <| fun _ _ -> ldind always
    opcode2Function.[hashFunction OpCodes.Ldind_I]            <- zipWithOneOffset <| fun _ _ -> ldind MakeIntPtr
    opcode2Function.[hashFunction OpCodes.Isinst]             <- zipWithOneOffset isinst
    opcode2Function.[hashFunction OpCodes.Stobj]              <- zipWithOneOffset <| stobj
    opcode2Function.[hashFunction OpCodes.Ldobj]              <- zipWithOneOffset <| ldobj
    opcode2Function.[hashFunction OpCodes.Stind_I1]           <- zipWithOneOffset <| fun _ _ -> stind (castUnchecked TypeUtils.int8Type)
    opcode2Function.[hashFunction OpCodes.Stind_I2]           <- zipWithOneOffset <| fun _ _ -> stind (castUnchecked TypeUtils.int16Type)
    opcode2Function.[hashFunction OpCodes.Stind_I4]           <- zipWithOneOffset <| fun _ _ -> stind (castUnchecked TypeUtils.int32Type)
    opcode2Function.[hashFunction OpCodes.Stind_I8]           <- zipWithOneOffset <| fun _ _ -> stind (castUnchecked TypeUtils.int64Type)
    opcode2Function.[hashFunction OpCodes.Stind_R4]           <- zipWithOneOffset <| fun _ _ -> stind (castUnchecked TypeUtils.float32Type)
    opcode2Function.[hashFunction OpCodes.Stind_R8]           <- zipWithOneOffset <| fun _ _ -> stind (castUnchecked TypeUtils.float64Type)
    opcode2Function.[hashFunction OpCodes.Stind_Ref]          <- zipWithOneOffset <| fun _ _ -> stind always
    opcode2Function.[hashFunction OpCodes.Stind_I]            <- zipWithOneOffset <| fun _ _ -> stind MakeIntPtr
    opcode2Function.[hashFunction OpCodes.Sizeof]             <- zipWithOneOffset <| sizeofInstruction
    opcode2Function.[hashFunction OpCodes.Leave]              <- leave
    opcode2Function.[hashFunction OpCodes.Leave_S]            <- leave
    opcode2Function.[hashFunction OpCodes.Endfinally]         <- zipWithOneOffset <| endfinally
    opcode2Function.[hashFunction OpCodes.Rethrow]            <- zipWithOneOffset <| rethrow
    opcode2Function.[hashFunction OpCodes.Endfilter]          <- zipWithOneOffset <| endfilter
    opcode2Function.[hashFunction OpCodes.Localloc]           <- zipWithOneOffset <| localloc
    // TODO: notImplemented instructions
    opcode2Function.[hashFunction OpCodes.Stelem_I]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ldelem_I]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Arglist]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Jmp]                <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Break]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Calli]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ckfinite]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Constrained]        <- zipWithOneOffset <| constrained
    opcode2Function.[hashFunction OpCodes.Cpblk]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Cpobj]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Mkrefany]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix1]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix2]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix3]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix4]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix5]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix6]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix7]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefixref]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Readonly]           <- zipWithOneOffset <| (fun _ _ s -> List.singleton s)
    opcode2Function.[hashFunction OpCodes.Refanytype]         <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Refanyval]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Tailcall]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Unaligned]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Volatile]           <- zipWithOneOffset <| (fun _ _ s -> List.singleton s)
    opcode2Function.[hashFunction OpCodes.Initblk]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
