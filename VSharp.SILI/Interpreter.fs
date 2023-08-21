namespace VSharp.Interpreter.IL

open System
open System.Diagnostics
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections
open CilStateOperations
open VSharp
open VSharp.CSharpUtils
open VSharp.Core
open VSharp.Interpreter.IL
open ipOperations
open MethodBody

type cfg = CfgInfo

module internal TypeUtils =
    open Types

    let float64Type = typedefof<double>
    let float32Type = typedefof<float32>
    let int8Type = typedefof<int8>
    let int16Type = typedefof<int16>
    let int32Type = typedefof<int32>
    let int64Type = typedefof<int64>
    let uint8Type = typedefof<uint8>
    let uint16Type = typedefof<uint16>
    let uint32Type = typedefof<uint32>
    let uint64Type = typedefof<uint64>
    let charType = typedefof<char>
    let intPtr = typedefof<IntPtr>
    let uintPtr = typedefof<UIntPtr>

    let isIntegralTerm term = Terms.TypeOf term |> TypeUtils.isIntegral
    let isLong term = TypeOf term |> TypeUtils.isLongTypes
    let isBool term = Terms.TypeOf term |> IsBool

    module Char =
        let Zero() = MakeNumber Unchecked.defaultof<char>
    module Int8 =
        let Zero() = MakeNumber 0y
    module UInt8 =
        let Zero() = MakeNumber 0uy
    module Int16 =
        let Zero() = MakeNumber 0s
    module UInt16 =
        let Zero() = MakeNumber 0us
    module Int32 =
        let Zero() = MakeNumber 0
        let One() = MakeNumber 1
        let MinusOne() = MakeNumber -1
        let MinValue() = MakeNumber System.Int32.MinValue
        let MaxValue() = MakeNumber System.Int32.MaxValue
    module UInt32 =
        let Zero() = MakeNumber 0u
        let MaxValue() = MakeNumber UInt32.MaxValue
    module Int64 =
        let Zero() = MakeNumber 0L
        let MinusOne() = MakeNumber -1L
        let MinValue() = MakeNumber System.Int64.MinValue
        let MaxValue() = MakeNumber System.Int64.MaxValue
    module UInt64 =
        let Zero() = MakeNumber 0UL
        let MaxValue() = MakeNumber UInt64.MaxValue

module internal InstructionsSet =

    let mutable reportError : cilState -> string -> unit = fun _ _ -> ()
    let reportUnspecifiedError state = reportError state "Unspecified"

    let idTransformation term k = k term

    // --------------------------------------- Metadata Interaction ----------------------------------------

    let resolveFieldFromMetadata (m : Method) offset = m.ResolveFieldFromMetadata offset
    let resolveTypeFromMetadata (m : Method) offset = m.ResolveTypeFromMetadata offset
    let resolveMethodFromMetadata (m : Method) offset = m.ResolveMethodFromMetadata offset
    let resolveTokenFromMetadata (m : Method) offset = m.ResolveTokenFromMetadata offset

    let (|EndFinally|_|) = function
        | Instruction(offset, m) when parseInstruction m offset = OpCodes.Endfinally -> Some()
        | _ -> None

    let rec (|InstructionEndingIp|_|) = function
        | Instruction(offset, m)
        | InFilterHandler(offset, m, _, _) -> Some (offset, m)
        | Leave(ip, _, _, _) -> (|InstructionEndingIp|_|) ip
        | _ -> None

    // ------------------------------- Environment interaction -------------------------------

    let rec internalCall (methodInfo : MethodInfo) (argsAndThis : term list) cilState k =
        let s = cilState.state
        let parameters : obj [] =
            match methodInfo.GetParameters().Length with
            | 2 -> [| s; argsAndThis |]
            | _ -> internalfail "Only internal calls with signature 'state * term list -> term | (term * state) list' are supported"
        let result =
            try
                methodInfo.Invoke(null, parameters)
            with
            | :? TargetInvocationException as targetException ->
                Logger.trace $"InternalCall got TargetInvocationException {targetException.Message}"
                let actualException = targetException.GetBaseException()
                Logger.trace $"TargetInvocationException.GetBaseException {actualException.Message}"
                raise actualException
            | e ->
                Logger.trace $"InternalCall got exception {e.Message}"
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
        | _ -> internalfail "Internal call should return 'term' or tuple 'term * state'!"

    // ------------------------------- CIL instructions -------------------------------

    let referenceLocalVariable index (method : Method) =
        let lvi = method.LocalVariables.[index]
        let stackKey = LocalVariableKey(lvi, method)
        Ref (PrimitiveStackLocation stackKey)

    let getArgTerm index (method : Method) =
        let pi = method.Parameters.[index]
        PrimitiveStackLocation (ParameterKey pi) |> Ref

    let castUnchecked typ term : term = Types.Cast term typ
    let ldc numberCreator t (m : Method) shiftedOffset (cilState : cilState) =
        let num = numberCreator m.ILBytes shiftedOffset
        push (Concrete num t) cilState

    let ldloc numberCreator (m : Method) shiftedOffset (cilState : cilState) =
        let index = numberCreator m.ILBytes shiftedOffset
        let reference = referenceLocalVariable index m
        let term = Memory.Read cilState.state reference
        push term cilState
    let ldarg numberCreator (m : Method) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator m.ILBytes shiftedOffset
        let arg =
            let state = cilState.state
            let this = if m.HasThis then Some <| Memory.ReadThis state m else None
            match this, m.HasThis with
            | None, _
            | Some _, false ->
                let term = getArgTerm argumentIndex m
                Memory.Read state term
            | Some this, _ when argumentIndex = 0 -> this
            | Some _, true ->
                let term = getArgTerm (argumentIndex - 1) m
                Memory.Read state term
        push arg cilState
    let ldarga numberCreator (m : Method) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator m.ILBytes shiftedOffset
        let address =
            let this = if m.IsStatic then None else Some <| Memory.ReadThis cilState.state m
            match this with
            | None -> getArgTerm argumentIndex m
            | Some _ when argumentIndex = 0 -> internalfail "can't load address of ``this''"
            | Some _ -> getArgTerm (argumentIndex - 1) m
        push address cilState

    let stloc numberCreator (m : Method) shiftedOffset (cilState : cilState) =
        let variableIndex = numberCreator m.ILBytes shiftedOffset
        let right = pop cilState
        let location = referenceLocalVariable variableIndex m
        let typ = TypeOfLocation location
        let value = Types.Cast right typ
        ConfigureErrorReporter (changeState cilState >> reportError)
        let states = Memory.Write cilState.state location value
        states |> List.map (changeState cilState)
    let private simplifyConditionResult state res k =
        if Contradicts state !!res then k (True())
        elif Contradicts state res then k (False())
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

    let ret (m : Method) (cilState : cilState) =
        let resultTyp = m.ReturnType
        if resultTyp <> typeof<Void> then
            let res = pop cilState
            let castedResult = Types.Cast res resultTyp
            push castedResult cilState
        match cilState.ipStack with
        | _ :: ips -> cilState.ipStack <- (Exit m) :: ips
        | [] -> __unreachable__()

    let transform2BooleanTerm pc (term : term) =
        let check term =
            match TypeOf term with
            | _ when IsRefOrPtr term -> !!(IsNullReference term)
            | Types.Bool -> term
            | t when t = TypeUtils.charType -> term !== TypeUtils.Char.Zero()
            | t when t = TypeUtils.int8Type -> term !== TypeUtils.Int8.Zero()
            | t when t = TypeUtils.uint8Type -> term !== TypeUtils.UInt8.Zero()
            | t when t = TypeUtils.int16Type -> term !== TypeUtils.Int16.Zero()
            | t when t = TypeUtils.uint16Type -> term !== TypeUtils.UInt16.Zero()
            | t when t = TypeUtils.int32Type -> term !== TypeUtils.Int32.Zero()
            | t when t = TypeUtils.uint32Type -> term !== TypeUtils.UInt32.Zero()
            | t when t = TypeUtils.int64Type -> term !== TypeUtils.Int64.Zero()
            | t when t = TypeUtils.uint64Type -> term !== TypeUtils.UInt64.Zero()
            | t when t = TypeUtils.intPtr -> term !== MakeNumber IntPtr.Zero
            | t when t = TypeUtils.uintPtr -> term !== MakeNumber UIntPtr.Zero
            | t when t.IsEnum -> term !== MakeNumber (Activator.CreateInstance t)
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
    let starg numCreator (m : Method) offset (cilState : cilState) =
        let argumentIndex = numCreator m.ILBytes offset
        let argTerm =
           let this = if m.IsStatic then None else Some <| Memory.ReadThis cilState.state m
           match this with
           | None -> getArgTerm argumentIndex m
           | Some this when argumentIndex = 0 -> this
           | Some _ -> getArgTerm (argumentIndex - 1) m
        let value = pop cilState
        let states = Memory.Write cilState.state argTerm value
        states |> List.map (changeState cilState)
    let brcommon condTransform (m : Method) (offset : offset) (cilState : cilState) =
        let cond = pop cilState
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
    let applyAndBranch additionalFunction brtrueFunction (m : Method) offset (cilState : cilState) =
        additionalFunction cilState
        brtrueFunction m offset cilState
    let boolToInt b =
        BranchExpressions (fun k -> k b) (fun k -> k <| TypeUtils.Int32.One()) (fun k -> k <| TypeUtils.Int32.Zero()) id
    let bitwiseOrBoolOperation bitwiseOp boolOp (cilState : cilState) =
        let arg2, arg1 = peek2 cilState
        let typ1, typ2 = TypeOf arg1, TypeOf arg2
        match typ1, typ2 with
        | Types.Bool, Types.Bool ->
            binaryOperationWithBoolResult boolOp idTransformation idTransformation cilState
        | _ when TypeUtils.isIntegral typ1 && TypeUtils.isIntegral typ2 ->
            standardPerformBinaryOperation bitwiseOp cilState
        // Bitwise operations for pointers are pointless (unless result will be checked on null), so returning pointer
        | _ when TypeUtils.isPointer typ1 ->
            pop cilState |> ignore
        | _ when TypeUtils.isPointer typ2 ->
            pop2 cilState |> ignore
            push arg2 cilState
        | Types.Bool, typ2 when TypeUtils.isIntegral typ2 ->
            let newArg1 = boolToInt arg1
            performCILBinaryOperation bitwiseOp (fun _ k -> k newArg1) idTransformation idTransformation cilState
        | typ1, Types.Bool when TypeUtils.isIntegral typ1 ->
            let newArg2 = boolToInt arg2
            performCILBinaryOperation bitwiseOp idTransformation (fun _ k -> k newArg2) idTransformation cilState
        | typ1, typ2 -> internalfail $"unhandled case for bitwise operation {bitwiseOp} and types: {typ1} {typ2}"
    let bitwiseOrBoolNot (cilState : cilState) =
        let arg = peek cilState
        let op =
            match TypeOf arg with
            | Types.Bool -> OperationType.LogicalNot
            | _ -> OperationType.BitwiseNot
        performCILUnaryOperation op cilState
    let retrieveActualParameters (method : Method) (cilState : cilState) =
        let paramsNumber = method.Parameters.Length
        let parameters, evaluationStack = EvaluationStack.PopMany paramsNumber cilState.state.evaluationStack
        let castParameter parameter (parInfo : ParameterInfo) =
            if method.IsDelegateConstructor && TypeUtils.isPointer parInfo.ParameterType then parameter
            else Types.Cast parameter parInfo.ParameterType
        setEvaluationStack evaluationStack cilState
        Seq.map2 castParameter (List.rev parameters) method.Parameters |> List.ofSeq

    let makeUnsignedInteger term k =
        match TypeOf term with
        | Types.Bool -> k <| Types.Cast term TypeUtils.uint32Type
        | t when t = typeof<double> || t = typeof<float> -> k term
        | t when TypeUtils.isIntegral t -> k <| Types.Cast term (TypeUtils.signedToUnsigned t) // no specs found about overflows
        | _ -> k term
    let performUnsignedIntegerOperation op (cilState : cilState) =
        let arg2, arg1 = peek2 cilState
        if TypeUtils.isIntegralTerm arg1 && TypeUtils.isIntegralTerm arg2 then
            performCILBinaryOperation op makeUnsignedInteger makeUnsignedInteger idTransformation cilState
        else internalfailf "arguments for %O are not Integers!" op
    let ldstr (m : Method) offset (cilState : cilState) =
        let stringToken = NumberCreator.extractInt32 m.ILBytes (offset + Offset.from OpCodes.Ldstr.Size)
        let string = m.Module.ResolveString stringToken
        let reference = Memory.AllocateString string cilState.state
        push reference cilState
    let allocateValueTypeInHeap v (cilState : cilState) =
        let address = Memory.BoxValueType cilState.state v
        push address cilState
    let ldnull (cilState : cilState) = push (NullRef typeof<obj>) cilState
    let convu (cilState : cilState) =
        let ptr = pop cilState |> MakeUIntPtr
        push ptr cilState
    let convi (cilState : cilState) =
        let ptr = pop cilState |> MakeIntPtr
        push ptr cilState
    let castTopOfOperationalStack targetType (cilState : cilState) =
        let t = pop cilState
        let termForStack = Types.Cast t targetType
        push termForStack cilState
    let ldloca numberCreator (m : Method) shiftedOffset (cilState : cilState) =
        let index = numberCreator m.ILBytes shiftedOffset
        let term = referenceLocalVariable index m
        push term cilState
    let switch (m : Method) offset (cilState : cilState) =
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
    let ldtoken (m : Method) offset (cilState : cilState) =
        let memberInfo = resolveTokenFromMetadata m (offset + Offset.from OpCodes.Ldtoken.Size)
        let state = cilState.state
        let res =
            match memberInfo with
            | :? FieldInfo as fi -> Memory.ObjectToTerm state fi.FieldHandle typeof<RuntimeFieldHandle>
            | :? Type as t -> Memory.ObjectToTerm state t.TypeHandle typeof<RuntimeTypeHandle>
            | :? MethodInfo as mi -> Memory.ObjectToTerm state mi.MethodHandle typeof<RuntimeMethodHandle>
            | _ -> internalfailf "Could not resolve token"
        push res cilState
    let ldftn (m : Method) offset (cilState : cilState) =
        let methodInfo = resolveMethodFromMetadata m (offset + Offset.from OpCodes.Ldftn.Size)
        let methodPtr = Terms.Concrete methodInfo (methodInfo.GetType())
        push methodPtr cilState
    let initobj (m : Method) offset (cilState : cilState) =
        let targetAddress = pop cilState
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Initobj.Size)
        let states = Memory.Write cilState.state targetAddress (Memory.DefaultOf typ)
        states |> List.map (changeState cilState)

    let clt = binaryOperationWithBoolResult OperationType.Less idTransformation idTransformation
    let cgt = binaryOperationWithBoolResult OperationType.Greater idTransformation idTransformation
    let cltun = binaryOperationWithBoolResult OperationType.Less_Un makeUnsignedInteger makeUnsignedInteger
    let bgeHelper (cilState : cilState) =
        let arg1, arg2 = peek2 cilState
        let typ1, typ2 = Terms.TypeOf arg1, Terms.TypeOf arg2
        if Types.isIntegral typ1 && Types.isIntegral typ2 then clt cilState
        elif Types.IsReal typ1 && Types.IsReal typ2 then cltun cilState
        else __notImplemented__()
    let isinst (m : Method) offset (cilState : cilState) =
        let object = pop cilState
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Isinst.Size)
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference object, state))
            (fun cilState k -> push (NullRef typeof<obj>) cilState; k [cilState])
            (fun cilState k ->
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.IsCast cilState.state object typ, state))
                    (fun cilState k -> push object cilState; k [cilState])
                    (fun cilState k -> push (NullRef typeof<obj>) cilState; k [cilState])
                    k)
            id
    let cgtun (cilState : cilState) =
        let arg2, arg1 = peek2 cilState
        if IsReference arg2 && IsReference arg1 then
            binaryOperationWithBoolResult OperationType.NotEqual idTransformation idTransformation cilState
        else binaryOperationWithBoolResult OperationType.Greater_Un makeUnsignedInteger makeUnsignedInteger cilState
    let ldobj (m : Method) offset (cilState : cilState) =
        let address = pop cilState
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Ldobj.Size)
        let value = Memory.Read cilState.state address
        let typedValue = Types.Cast value typ
        push typedValue cilState
    let stobj reportError (m : Method) offset (cilState : cilState) =
        let src, dest = pop2 cilState
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Stobj.Size)
        let value = Types.Cast src typ
        ConfigureErrorReporter (changeState cilState >> reportError)
        let states = Memory.Write cilState.state dest value
        states |> List.map (changeState cilState)
    let sizeofInstruction (m : Method) offset (cilState : cilState) =
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Sizeof.Size)
        let size = Types.SizeOf typ
        push (MakeNumber size) cilState
    let leave (m : Method) offset (cilState : cilState) =
        let dst = unconditionalBranchTarget m offset
        let ehcs =
            m.ExceptionHandlers
            |> Seq.filter isFinallyClause
            |> Seq.filter (shouldExecuteFinallyClause offset dst)
            |> Seq.sortWith (fun ehc1 ehc2 -> ehc1.handlerOffset - ehc2.handlerOffset |> int)
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
    let br (m : Method) offset (cilState : cilState) =
        let newIp = instruction m (unconditionalBranchTarget m offset)
        setCurrentIp newIp cilState

    // '.constrained' is prefix, which is used before 'callvirt' instruction
    let constrained (m : Method) offset (cilState : cilState) =
        match findNextInstructionOffsetAndEdges OpCodes.Constrained m.ILBytes offset with
        | FallThrough offset ->
            let method = resolveMethodFromMetadata m (offset + Offset.from OpCodes.Callvirt.Size)
            // Method can not be constructor, because it's called via 'callvirt'
            assert(method :? MethodInfo)
            let method = method :?> MethodInfo
            let n = method.GetParameters().Length
            let args, evaluationStack = EvaluationStack.PopMany n cilState.state.evaluationStack
            setEvaluationStack evaluationStack cilState
            let thisForCallVirt = pop cilState
            let thisType = TypeOfLocation thisForCallVirt
            let isValueType = Types.IsValueType thisType
            match thisForCallVirt.term with
            | Ref _ when isValueType && Reflection.typeImplementsMethod thisType method ->
                push thisForCallVirt cilState
                pushMany args cilState
            | Ref _ when isValueType ->
                let thisStruct = Memory.Read cilState.state thisForCallVirt
                let heapRef = Memory.BoxValueType cilState.state thisStruct
                push heapRef cilState
                pushMany args cilState
            | Ref _ ->
                let this = Memory.Read cilState.state thisForCallVirt
                push this cilState
                pushMany args cilState
            | _ -> internalfail $"Calling 'callvirt' with '.constrained': unexpected 'this' {thisForCallVirt}"
        | _ -> __unreachable__()
    let localloc (cilState : cilState) =
        // [NOTE] localloc usually is used for Span
        // So, pushing nullptr, because array will be allocated in Span constructor
        pop cilState |> ignore
        push (MakeNullPtr typeof<Void>) cilState

    let private fallThroughImpl stackSizeBefore newIp cilState =
        // if not constructing runtime exception
        if not <| isUnhandledError cilState && Memory.CallStackSize cilState.state = stackSizeBefore then
            setCurrentIp newIp cilState

    let fallThrough (m : Method) offset cilState op =
        assert(not <| isUnhandledError cilState)
        let stackSizeBefore = Memory.CallStackSize cilState.state
        let newIp = instruction m (fallThroughTarget m offset)
        op m offset cilState
        fallThroughImpl stackSizeBefore newIp cilState
        [cilState]

    let forkThrough (m : Method) offset cilState op =
        assert(not <| isUnhandledError cilState)
        let stackSizeBefore = Memory.CallStackSize cilState.state
        let newIp = instruction m (fallThroughTarget m offset)
        let cilStates = op m offset cilState
        List.iter (fallThroughImpl stackSizeBefore newIp) cilStates
        cilStates

    let readParameter (parameter : ParameterInfo) cilState =
        if parameter.ParameterType.IsByRef then
            let key = ParameterKey parameter
            let stackRef = Memory.ReadLocalVariable cilState.state key
            Memory.Read cilState.state stackRef
        else
            Memory.ReadArgument cilState.state parameter

open InstructionsSet

type UnknownMethodException(message : string, methodInfo : Method, interpreterStackTrace : string) =
    inherit Exception(message)
    member x.Method with get() = methodInfo
    member x.InterpreterStackTrace with get() = interpreterStackTrace

type internal ILInterpreter() as this =

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
            "System.Void System.Array.Fill(T[], T)", this.FillArray
            "System.Char System.String.get_Chars(this, System.Int32)", this.GetChars
            "System.Void System.Threading.Monitor.ReliableEnter(System.Object, System.Boolean&)", this.MonitorReliableEnter
            "System.Void System.Threading.Monitor.Enter(System.Object)", this.MonitorEnter
            "System.Void System.Diagnostics.Debug.Assert(System.Boolean)", this.DebugAssert
            "System.UInt32 System.Collections.HashHelpers.FastMod(System.UInt32, System.UInt32, System.UInt64)", this.FastMod
            "System.Void System.Runtime.CompilerServices.RuntimeHelpers._RunClassConstructor(System.RuntimeType)", this.RunStaticCtor
            "System.Void System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(System.Runtime.CompilerServices.QCallTypeHandle)", this.RunStaticCtor
        ]
    // NOTE: adding implementation names into Loader
    do Loader.CilStateImplementations <- cilStateImplementations.Keys

    member x.ConfigureErrorReporter reporter =
        reportError <- reporter

    member private x.Raise createException (cilState : cilState) k =
        createException cilState
        k [cilState]

    member private x.AccessMultidimensionalArray accessor (cilState : cilState) lengths indices (k : cilState list -> 'a) =
        let checkArrayBounds upperBounds indices =
            let checkOneBound acc (upperBound, index) =
                let lowerBound = Concrete 0 Types.TLength
                let notTooSmall = Arithmetics.(>>=) index lowerBound
                let notTooLarge = Arithmetics.(<<) index upperBound
                acc &&& notTooSmall &&& notTooLarge
            assert(List.length upperBounds = List.length indices)
            let upperBoundsAndIndices = List.zip upperBounds indices
            List.fold checkOneBound (True()) upperBoundsAndIndices
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (checkArrayBounds lengths indices, state))
            accessor
            (x.Raise x.IndexOutOfRangeException)
            k

    member private x.AccessArray accessor (cilState : cilState) length index k =
        x.AccessMultidimensionalArray accessor cilState [length] [index] k

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
        let dest, destPos, src = args[0], args[1], args[2]
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
        let array, index, length = args[0], args[1], args[2]
        let (>>) = API.Arithmetics.(>>)
        let (<<) = API.Arithmetics.(<<)
        let clearCase (cilState : cilState) k =
            Memory.ClearArray cilState.state array index length
            k [cilState]
        let nonNullCase (cilState : cilState) k =
            let zero = MakeNumber 0
            let lb = Memory.ArrayLowerBoundByDimension cilState.state array zero
            let numOfAllElements = Memory.CountOfArrayElements cilState.state array
            let check = (index << lb) ||| ((Arithmetics.Add index length) >> numOfAllElements) ||| (length << zero)
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
        let srcType = MostConcreteTypeOfRef state src
        let dstType = MostConcreteTypeOfRef state dst
        let (>>) = API.Arithmetics.(>>)
        let (>>=) = API.Arithmetics.(>>=)
        let (<<) = API.Arithmetics.(<<)
        let add = Arithmetics.Add
        let zero = TypeUtils.Int32.Zero()
        let srcLB = Memory.ArrayLowerBoundByDimension state src zero
        let dstLB = Memory.ArrayLowerBoundByDimension state dst zero
        let srcNumOfAllElements = Memory.CountOfArrayElements state src
        let dstNumOfAllElements = Memory.CountOfArrayElements state dst
        let defaultCase (cilState : cilState) k =
            Memory.CopyArray cilState.state src srcIndex srcType dst dstIndex dstType length
            k [cilState]
        let lengthCheck (cilState : cilState) =
            let endSrcIndex = add srcIndex length
            let srcNumOfAllElements = srcNumOfAllElements
            let endDstIndex = add dstIndex length
            let dstNumOfAllElements = dstNumOfAllElements
            let check =
                (endSrcIndex >> srcNumOfAllElements) ||| (endSrcIndex << srcLB)
                ||| (endDstIndex >> dstNumOfAllElements) ||| (endDstIndex << dstLB)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (check, state))
                (x.Raise x.ArgumentException)
                defaultCase
        let indicesCheck (cilState : cilState) =
            // TODO: extended form needs
            let primitiveLengthCheck = (length << zero) ||| (if TypeUtils.isLong length then length >> TypeUtils.Int32.MaxValue() else False())
            let srcIndexCheck = (srcIndex << srcLB) ||| (if TypeUtils.isLong srcIndex then srcIndex >>= srcNumOfAllElements else False())
            let dstIndexCheck = (dstIndex << dstLB) ||| (if TypeUtils.isLong dstIndex then dstIndex >>= dstNumOfAllElements else False())

            StatedConditionalExecutionCIL cilState
                (fun state k -> k (primitiveLengthCheck ||| srcIndexCheck ||| dstIndexCheck, state))
                (x.Raise x.ArgumentOutOfRangeException)
                lengthCheck
        let assignableCheck (cilState : cilState) =
            let srcElemType = Types.ElementType srcType
            let dstElemType = Types.ElementType dstType
            let condition =
                if Types.IsValueType srcElemType then True()
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
        let src, srcIndex, dst, dstIndex, length = args[0], args[1], args[2], args[3], args[4]
        x.CommonCopyArray cilState src srcIndex dst dstIndex length

    member private x.CopyArrayShortForm (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let src, dst, length = args[0], args[1], args[2]
        let state = cilState.state
        let zero = TypeUtils.Int32.Zero()
        let srcLB = Memory.ArrayLowerBoundByDimension state src zero
        let dstLB = Memory.ArrayLowerBoundByDimension state src zero
        x.CommonCopyArray cilState src srcLB dst dstLB length

    member private x.FillArray (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let array, value = args[1], args[2]
        let fill cilState k =
            Memory.FillArray cilState.state array value
            k [cilState]
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference array, state))
            (x.Raise x.ArgumentNullException)
            fill
            id

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
        | None -> internalfailf $"String.GetChars: unexpected this {this}"

    member private x.MonitorReliableEnter (cilState : cilState) this (args : term list) =
        assert(List.length args = 2 && Option.isNone this)
        let obj, resultRef = args[0], args[1]
        let success cilState k =
            Memory.Write cilState.state resultRef (True()) |> List.map (changeState cilState) |> k
        BranchOnNullCIL cilState obj
            (x.Raise x.ArgumentNullException)
            success
            id

    member private x.MonitorEnter (cilState : cilState) this (args : term list) =
        assert(List.length args = 1 && Option.isNone this)
        let obj = List.head args
        BranchOnNullCIL cilState obj
            (x.Raise x.ArgumentNullException)
            (fun cilState k -> List.singleton cilState |> k)
            id

    member private x.DebugAssert (cilState : cilState) this (args : term list) =
        assert(List.length args = 1 && Option.isNone this)
        let condition = List.head args
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (condition, state))
            (fun cilState k -> (); k [cilState])
            (fun cilState k ->
                reportError cilState "Debug.Assert failed"
                k [])
            id

    member private x.FastMod (cilState : cilState) this (args : term list) =
        assert(List.length args = 3 && Option.isNone this)
        let left, right = args[0], args[1]
        let validCase cilState k =
            let leftType = TypeOf left
            let rightType = TypeOf right
            let result =
                if TypeUtils.isUnsigned leftType || TypeUtils.isUnsigned rightType then
                    Arithmetics.RemUn left right
                else Arithmetics.Rem left right
            push result cilState
            k [cilState]
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (right === MakeNumber 0, state))
            (x.Raise x.DivideByZeroException)
            validCase
            id

    member private x.RunStaticCtor (cilState : cilState) this (args : term list) =
        assert(List.length args = 1 && Option.isNone this)
        // TODO: initialize statics of argument
        List.singleton cilState

    member private x.TrustedIntrinsics =
        let intPtr = Reflection.getAllMethods typeof<IntPtr> |> Array.map Reflection.getFullMethodName
        let volatile = Reflection.getAllMethods typeof<System.Threading.Volatile> |> Array.map Reflection.getFullMethodName
        let defaultComparer = [|"System.Collections.Generic.Comparer`1[T] System.Collections.Generic.Comparer`1[T].get_Default()"|]
        Array.concat [intPtr; volatile; defaultComparer]

    member private x.IsNotImplementedIntrinsic (method : Method) fullMethodName = // try to set to false
        false
        //let isIntrinsic =
        //    let intrinsicAttr = "System.Runtime.CompilerServices.IntrinsicAttribute"
        //    method.CustomAttributes |> Seq.exists (fun m -> m.AttributeType.ToString() = intrinsicAttr)
        //isIntrinsic && (Array.contains fullMethodName x.TrustedIntrinsics |> not)

    member private x.ShouldMock (method : Method) fullMethodName =
        Loader.isShimmed fullMethodName
        || method.IsExternalMethod && not method.IsQCall

    member private x.InstantiateThisIfNeed state thisOption (method : Method) =
        match thisOption with
        | Some this ->
            let thisType = TypeOfLocation this
            if Types.IsValueType thisType && (method :> IMethod).IsConstructor then
                let newThis = Memory.DefaultOf thisType
                let states = Memory.Write state this newThis
                assert(List.length states = 1 && LanguagePrimitives.PhysicalEquality state (List.head states))
        | None -> ()

    member private x.IsArrayGetOrSet (method : Method) =
        let name = method.Name
        (name = "Set" || name = "Get") && typeof<System.Array>.IsAssignableFrom(method.DeclaringType)

    static member InitFunctionFrameCIL (cilState : cilState) (method : Method) this paramValues =
        Memory.InitFunctionFrame cilState.state method this (paramValues |> Option.bind (List.map Some >> Some))
        pushToIp (instruction method 0<offsets>) cilState

    static member CheckDisallowNullAttribute (method : Method) (argumentsOpt : term list option) (cilState : cilState) shouldReportError k =
        if not <| method.CheckAttributes then
            [cilState] |> k
        else
            let assumptions =
                match argumentsOpt with
                | Some arguments ->
                    let parameters = method.Parameters
                    let getDisallowNullAssumption index argument =
                        if Attribute.IsDefined(parameters[index], typeof<CodeAnalysis.DisallowNullAttribute>)
                            then Some <| !!(IsNullReference argument)
                            else None
                    arguments |> Seq.mapi getDisallowNullAssumption |> Seq.choose id
                | None ->
                    let getDisallowNullAssumption (parameter : ParameterInfo) =
                        if Attribute.IsDefined(parameter, typeof<CodeAnalysis.DisallowNullAttribute>)
                            then Some <| !!(IsNullReference (readParameter parameter cilState))
                            else None
                    method.Parameters |> Seq.map getDisallowNullAssumption |> Seq.choose id

            if Seq.isEmpty assumptions then
                [cilState] |> k
            else
                let assumptions = conjunction assumptions
                let message = "DisallowNull attribute violation"
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (assumptions, state))
                    (fun cilState k -> k [cilState])
                    (fun cilState k ->
                        if shouldReportError then reportError cilState message
                        k [])
                    k

    static member CheckNotNullAttribute (method : Method) (cilState : cilState) shouldReportError k =
        if not <| method.CheckAttributes then
            [cilState] |> k
        else
            let retValueAssumption =
                match method.ReturnParameter with
                | Some returnParameter when Attribute.IsDefined(returnParameter, typeof<CodeAnalysis.NotNullAttribute>) ->
                    let res = pop cilState
                    push res cilState
                    Some <| !!(IsNullReference res)
                | _ -> None

            let getNotNullAssumption (parameter : ParameterInfo) =
                if Attribute.IsDefined(parameter, typeof<CodeAnalysis.NotNullAttribute>)
                    then Some <| !!(IsNullReference (readParameter parameter cilState))
                    else None
            let assumptions = method.Parameters |> Seq.map getNotNullAssumption |> Seq.cons retValueAssumption |> Seq.choose id

            if Seq.isEmpty assumptions then
                [cilState] |> k
            else
                let assumptions = conjunction assumptions
                let message = "NotNull attribute violation"
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (assumptions, state))
                    (fun cilState k -> k [cilState])
                    (fun cilState k ->
                        if shouldReportError then reportError cilState message
                        k [])
                    k

    member private x.InitStaticFieldWithDefaultValue state (f : FieldInfo) =
        assert f.IsStatic
        let fieldType = f.FieldType
        let declaringType = f.DeclaringType
        let value =
            if f.IsLiteral then
                match f.GetValue(null) with // argument means class with field f, so we have null, because f is a static field
                | null -> NullRef fieldType
                | :? string as str -> Memory.AllocateString str state
                | v when f.FieldType.IsPrimitive || f.FieldType.IsEnum -> Concrete v fieldType
                | _ -> __unreachable__()
            elif TypeUtils.isImplementationDetails declaringType then
                Memory.AllocateArrayFromFieldInfo state f
            else Memory.DefaultOf fieldType
        let fieldId = Reflection.wrapField f
        Memory.WriteStaticField state declaringType fieldId value

    member private x.InvokeArrayGetOrSet (cilState : cilState) (method : Method) thisOption args =
        let name = method.Name
        match thisOption with
        | Some arrayRef when name = "Get" ->
            let typ = Some method.ReturnType
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
            let typeInitialized = Memory.IsTypeInitialized cilState.state t
            match typeInitialized with
            | True -> whenInitializedCont cilState
            | _ ->
                let staticConstructor = t.GetConstructors(Reflection.staticBindingFlags) |> Array.tryHead
                Seq.iter (x.InitStaticFieldWithDefaultValue cilState.state) fields
                Memory.InitializeStaticMembers cilState.state t
                match staticConstructor with
                | Some cctor ->
                    let cctor = Application.getMethod cctor
                    x.CommonCall cctor [] None cilState id
                | None -> whenInitializedCont cilState
                // TODO: make assumption ``Memory.withPathCondition state (!!typeInitialized)''

    member private x.ConcreteInvokeCatch (e : Exception) cilState isRuntime =
        let state = cilState.state
        let error = Memory.AllocateConcreteObject state e (e.GetType())
        x.CommonThrow cilState error isRuntime

    member private x.TryConcreteInvoke (method : Method) fullMethodName (args : term list) thisOption (cilState : cilState) =
        let state = cilState.state
        if method.IsConcretelyInvokable && Loader.isInvokeInternalCall fullMethodName then
            // Before term args, type args are located
            let termArgs = List.skip (List.length args - method.Parameters.Length) args
            let objArgs = List.choose (TryTermToObj state) termArgs
            let hasThis = Option.isSome thisOption
            let thisObj = Option.bind (TryTermToObj state) thisOption
            match thisObj with
            | _ when List.length objArgs <> List.length termArgs -> false
            | None when hasThis -> false
            | _ ->
                try
                    let result = method.Invoke thisObj (List.toArray objArgs)
                    let resultType = TypeUtils.getTypeOfConcrete result
                    let returnType = method.ReturnType
                    match resultType with
                    | _ when resultType <> null && resultType.IsValueType && returnType.IsInterface ->
                        // When return type is interface, but real type is struct,
                        // it should be boxed, so allocating it in heap
                        let resultTerm = Memory.AllocateConcreteObject cilState.state result resultType
                        push resultTerm cilState
                    | _ when returnType <> typeof<Void> ->
                        // Case when method returns something
                        let typ = TypeUtils.mostConcreteType resultType method.ReturnType
                        let resultTerm = Memory.ObjectToTerm cilState.state result typ
                        push resultTerm cilState
                    | _ -> ()
                with :? TargetInvocationException as e ->
                    let isRuntime = Loader.isRuntimeExceptionsImplementation fullMethodName
                    x.ConcreteInvokeCatch e.InnerException cilState isRuntime
                true
        else false

    member private x.InlineOrCall (method : Method) args thisOption (cilState : cilState) k =
        let fullyGenericMethod, genericArgs, _ = method.Generalize()
        let fullMethodName = fullyGenericMethod.FullName
        let wrapType arg = Concrete arg typeof<Type>
        // TODO: do not wrap types, pass them through state.typeVariables!
        let typeArgs = genericArgs |> Seq.map wrapType |> List.ofSeq
        let typeAndMethodArgs = typeArgs @ args
        x.InstantiateThisIfNeed cilState.state thisOption method

        let fallThroughCall (cilState : cilState) =
            if isUnhandledError cilState |> not then
                ILInterpreter.FallThroughCall cilState
            cilState

        if x.TryConcreteInvoke method fullMethodName typeAndMethodArgs thisOption cilState then
            fallThroughCall cilState |> List.singleton |> k
        elif Map.containsKey fullMethodName cilStateImplementations then
            cilStateImplementations[fullMethodName] cilState thisOption typeAndMethodArgs |> List.map fallThroughCall |> k
        elif Map.containsKey fullMethodName Loader.FSharpImplementations then
            let thisAndArguments = optCons typeAndMethodArgs thisOption
            internalCall Loader.FSharpImplementations[fullMethodName] thisAndArguments cilState (List.map fallThroughCall >> k)
        elif Map.containsKey fullMethodName Loader.CSharpImplementations then
            assert method.HasBody
            ILInterpreter.InitFunctionFrameCIL cilState method thisOption (Some args)
            [cilState] |> k
        // TODO: add Address function for array and return Ptr #do
        elif x.IsArrayGetOrSet method then
            x.InvokeArrayGetOrSet cilState method thisOption typeAndMethodArgs |> List.map fallThroughCall |> k
        elif ExternMocker.ExtMocksSupported && x.ShouldMock method fullMethodName then
            let mockMethod = ExternMockAndCall cilState.state method None []
            match mockMethod with
            | Some symVal ->
                push symVal cilState
            | None -> ()
            fallThroughCall cilState |> List.singleton |> k
        elif method.IsExternalMethod then
            let stackTrace = Memory.StackTraceString cilState.state.stack
            let message = $"Not supported extern method: {fullMethodName}"
            UnknownMethodException(message, method, stackTrace) |> raise
        elif method.IsInternalCall then
            assert(not <| method.IsImplementedInternalCall)
            let stackTrace = Memory.StackTraceString cilState.state.stack
            let message = $"Not supported internal call: {fullMethodName}"
            UnknownMethodException(message, method, stackTrace) |> raise
        elif x.IsNotImplementedIntrinsic method fullMethodName then
            let stackTrace = Memory.StackTraceString cilState.state.stack
            let message = $"Not supported intrinsic method: {fullMethodName}"
            UnknownMethodException(message, method, stackTrace) |> raise
        elif method.HasBody then
            ILInterpreter.InitFunctionFrameCIL cilState method thisOption (Some args)
            [cilState] |> k
        else internalfailf $"Non-extern method {method.FullName} without body!"

    member private x.CommonCall (method : Method) args thisOption (cilState : cilState) k =
        let inlineOrCall state k =
            x.InlineOrCall method args thisOption state k
        ILInterpreter.CheckDisallowNullAttribute method (Some args) cilState true (fun states ->
        Cps.List.mapk inlineOrCall states (List.concat >> k))

    member x.ResolveVirtualMethod targetType (ancestorMethod : Method) =
        let genericCalledMethod = ancestorMethod.GetGenericMethodDefinition()
        let genericMethodInfo =
            Reflection.resolveOverridingMethod targetType genericCalledMethod
        if genericMethodInfo.IsGenericMethodDefinition then
            genericMethodInfo.MakeGenericMethod(ancestorMethod.GetGenericArguments())
        else genericMethodInfo
        |> Application.getMethod

    member x.CallAbstract targetType (ancestorMethod : Method) (this : term) (arguments : term list) cilState k =
        let thisType = MostConcreteTypeOfRef cilState.state this
        let candidateTypes = ResolveCallVirt cilState.state this thisType ancestorMethod |> List.ofSeq
        let getMethods t (concreteMethods, mockTypes) =
            match t with
            | ConcreteType t ->
                let overriden = x.ResolveVirtualMethod t ancestorMethod
                if overriden.InCoverageZone || t.IsAssignableTo targetType && targetType.Assembly = t.Assembly then
                    (t, overriden) :: concreteMethods, mockTypes
                else concreteMethods, mockTypes
            | MockType m -> concreteMethods, (m :: mockTypes)
        let candidateMethods, typeMocks = List.foldBack getMethods candidateTypes (List.empty, List.empty)
        assert(List.length typeMocks = 1)
        let candidateMethods = List.distinctBy snd candidateMethods
        let invokeMock cilState k =
            let overriden =
                if ancestorMethod.DeclaringType.IsInterface then ancestorMethod
                else x.ResolveVirtualMethod targetType ancestorMethod
            let mockMethod = MethodMockAndCall cilState.state overriden (Some this) []
            match mockMethod with
            | Some symVal ->
                push symVal cilState
            | None -> ()
            match tryCurrentLoc cilState with
            | Some loc ->
                // Moving ip to next instruction after mocking method result
                fallThrough loc.method loc.offset cilState (fun _ _ _ -> ()) |> k
            | _ -> __unreachable__()
        let rec dispatch candidates cilState k =
            match candidates with
            | [] -> invokeMock cilState k
            | (t, method : Method)::rest ->
                StatedConditionalExecutionCIL cilState
                    (fun cilState k -> k (API.Types.TypeIsRef cilState t this, cilState))
                    (fun cilState k ->
                        let this = Types.Cast this method.ReflectedType
                        x.CommonCall method arguments (Some this) cilState k)
                    (dispatch rest)
                    k
        dispatch candidateMethods cilState k

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
        let minMaxOf =
            PersistentDict.ofSeq [
                TypeUtils.int8Type,    (SByte.MinValue  :> IConvertible, SByte.MaxValue  :> IConvertible)
                TypeUtils.int16Type,   (Int16.MinValue  :> IConvertible, Int16.MaxValue  :> IConvertible)
                TypeUtils.int32Type,   (Int32.MinValue  :> IConvertible, Int32.MaxValue  :> IConvertible)
                TypeUtils.int64Type,   (Int64.MinValue  :> IConvertible, Int64.MaxValue  :> IConvertible)
                TypeUtils.uint8Type,   (Byte.MinValue   :> IConvertible, Byte.MaxValue   :> IConvertible)
                TypeUtils.uint16Type,  (UInt16.MinValue :> IConvertible, UInt16.MaxValue :> IConvertible)
                TypeUtils.uint32Type,  (UInt32.MinValue :> IConvertible, UInt32.MaxValue :> IConvertible)
                TypeUtils.uint64Type,  (UInt64.MinValue :> IConvertible, UInt64.MaxValue :> IConvertible)
                TypeUtils.float32Type, (Single.MinValue :> IConvertible, Single.MaxValue :> IConvertible)
                TypeUtils.float64Type, (Double.MinValue :> IConvertible, Double.MaxValue :> IConvertible) ]
        let getSegment leftTyp rightTyp =
            let min1, max1 = minMaxOf.[leftTyp]
            let min2, max2 = minMaxOf.[rightTyp]
            let c = System.Globalization.CultureInfo.CurrentCulture
            match min1.ToDouble(c) < min2.ToDouble(c), max1.ToDouble(c) < max2.ToDouble(c) with
            | true, true   -> min2, max1
            | true, false  -> min2, max2
            | false, true  -> min1, max1
            | false, false -> min1, max2
        let canCastWithoutOverflow term targetType =
            let (<<=) = API.Arithmetics.(<<=)
            assert(Terms.TypeOf term |> Types.IsNumeric)
            let t = Terms.TypeOf term
            if isSubset t targetType then True()
            elif t = TypeUtils.int64Type && targetType = TypeUtils.uint64Type then
                let int64Zero = MakeNumber (0 |> int64)
                int64Zero <<= term
            elif t = TypeUtils.uint64Type && targetType = TypeUtils.int64Type then
                let uint64RightBorder = MakeNumber (Int64.MaxValue |> uint64)
                term <<= uint64RightBorder
            else
                let min, max = getSegment t targetType
                let leftBorder  = Concrete min t // must save type info, because min is int64
                let rightBorder = Concrete max t // must save type info, because max is int64
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

    member private x.CommonCastClass (cilState : cilState) (term : term) (typ : Type) k =
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference term ||| Types.IsCast state term typ, state))
            (fun cilState k ->
                push (Types.Cast term typ) cilState
                k [cilState])
            (x.Raise x.InvalidCastException)
            k
    member private x.CastClass (m : Method) offset (cilState : cilState) : cilState list =
        let term = pop cilState
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Castclass.Size)
        x.CommonCastClass cilState term typ id

    member private x.PushNewObjResultOnEvaluationStack (cilState : cilState) reference (calledMethod : MethodBase) =
        let valueOnStack =
            if calledMethod.DeclaringType.IsValueType then
                  Memory.Read cilState.state reference
            else reference
        push valueOnStack cilState

    member x.RetrieveCalledMethodAndArgs (opCode : OpCode) (calledMethod : Method) (cilState : cilState) =
        let args = retrieveActualParameters calledMethod cilState
        let hasThis = calledMethod.HasThis && opCode <> OpCodes.Newobj
        let this = if hasThis then pop cilState |> Some else None
        this, args

    member x.Call (m : Method) offset (cilState : cilState) =
        let calledMethod = resolveMethodFromMetadata m (offset + Offset.from OpCodes.Call.Size) |> Application.getMethod
        let getArgsAndCall cilState =
            let thisOpt, args = x.RetrieveCalledMethodAndArgs OpCodes.Call calledMethod cilState
            match thisOpt with
            | Some this -> x.NpeOrInvokeStatementCIL cilState this (x.CommonCall calledMethod args thisOpt) id
            | None -> x.CommonCall calledMethod args thisOpt cilState id
        x.InitializeStatics cilState calledMethod.DeclaringType getArgsAndCall

    member x.CallVirt (m : Method) offset (cilState : cilState) =
        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? Tuple<MethodInfo, term> as tuple, _) -> snd tuple, (fst tuple :> MethodBase)
            | _ -> __unreachable__()
        let ancestorMethod = resolveMethodFromMetadata m (offset + Offset.from OpCodes.Call.Size) |> Application.getMethod
        let thisOption, args = x.RetrieveCalledMethodAndArgs OpCodes.Callvirt ancestorMethod cilState
        let this =
            match thisOption with
            | Some this -> this
            | None -> internalfailf "None this in callvirt"

        let isDelegate = ancestorMethod.IsDelegate
        let actualThis, ancestorMethod =
            match this with
            | NonNullRef as this when isDelegate ->
                match Memory.ReadDelegate cilState.state this with
                | Some deleg ->
                    let target, mi = retrieveMethodInfo deleg
                    let mi = Application.getMethod mi
                    // [NOTE] target is ref to closure: when we have it, 'this' = target, otherwise 'this' = thisOption
                    match target with
                    | NullRef _ -> this, mi
                    | _ -> target, mi
                | _ -> this, ancestorMethod
            | _ -> this, ancestorMethod

        let callVirtual (cilState : cilState) this k =
            let baseType = MostConcreteTypeOfRef cilState.state this
            // Forcing CallAbstract for delegates to generate mocks
            if baseType.IsAbstract || ancestorMethod.CanBeOverriden baseType && not baseType.IsSealed || isDelegate then
                x.CallAbstract baseType ancestorMethod this args cilState k
            else
                let targetMethod = x.ResolveVirtualMethod baseType ancestorMethod
                if targetMethod.IsAbstract then
                    x.CallAbstract baseType targetMethod this args cilState k
                else
                    let this = Types.Cast this targetMethod.ReflectedType
                    x.CommonCall targetMethod args (Some this) cilState k

        // NOTE: there is no need to initialize statics, because they were initialized before ``newobj'' execution
        let call (cilState : cilState) k =
            if ancestorMethod.IsVirtual && not ancestorMethod.IsFinal then
                GuardedApplyCIL cilState actualThis callVirtual k
            else
                let actualThis = Types.Cast actualThis ancestorMethod.ReflectedType
                x.CommonCall ancestorMethod args (Some actualThis) cilState k

        x.NpeOrInvokeStatementCIL cilState actualThis call id

    member x.ReduceArrayCreation (arrayType : Type) (cilState : cilState) (lengths : term list) k =
        Memory.AllocateDefaultArray cilState.state lengths arrayType |> k

    member x.Ret (m : Method) (cilState : cilState) =
        ret m cilState
        ILInterpreter.CheckNotNullAttribute m cilState true id

    member x.CommonCreateDelegate (ctor : Method) (cilState : cilState) (args : term list) k =
        let target, methodPtr =
            assert(List.length args = 2)
            args.[0], args.[1]
        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? MethodInfo as mi, _) -> mi
            | _ -> __unreachable__()
        let lambda = Concrete (retrieveMethodInfo methodPtr, target) ctor.DeclaringType
        Memory.AllocateDelegate cilState.state lambda |> k

    member x.NewObj (m : Method) offset (cilState : cilState) =
        let calledMethod = resolveMethodFromMetadata m (offset + Offset.from OpCodes.Newobj.Size) |> Application.getMethod
        assert (calledMethod :> IMethod).IsConstructor
        let constructorInfo = calledMethod
        let typ = constructorInfo.DeclaringType
        x.InitializeStatics cilState constructorInfo.DeclaringType (fun cilState ->
        let this, args = x.RetrieveCalledMethodAndArgs OpCodes.Newobj calledMethod cilState
        assert(Option.isNone this)

        let blockCase (cilState : cilState) =
            let ref =
                if Types.IsValueType typ || TypeUtils.isPointer typ then
                    let freshValue = Memory.DefaultOf typ
                    Memory.AllocateTemporaryLocalVariable cilState.state -1 typ freshValue
                else Memory.AllocateDefaultClass cilState.state typ
            push ref cilState
            x.CommonCall constructorInfo args (Some ref) cilState id

        let k reference =
            let newIp = moveInstruction (fallThroughTarget m offset) (currentIp cilState)
            push reference cilState
            setCurrentIp newIp cilState
            [cilState]

        if constructorInfo.IsDelegateConstructor then
            x.CommonCreateDelegate constructorInfo cilState args k
        elif typ.IsArray && not constructorInfo.HasBody then
            try
                // TODO: can lower bounds be specified via newobj?
                x.ReduceArrayCreation typ cilState args k
            with
            | :? OutOfMemoryException -> x.Raise x.OutOfMemoryException cilState id
        else blockCase cilState)

    member x.LdsFld addressNeeded (m : Method) offset (cilState : cilState) =
        let newIp = moveInstruction (fallThroughTarget m offset) (currentIp cilState)
        let fieldInfo = resolveFieldFromMetadata m (offset + Offset.from OpCodes.Ldsfld.Size)
        assert fieldInfo.IsStatic
        let declaringType = fieldInfo.DeclaringType
        x.InitializeStatics cilState declaringType (fun cilState ->
        let fieldId = Reflection.wrapField fieldInfo
        let value =
            if addressNeeded && not (TypeUtils.isImplementationDetails declaringType) then
                StaticField(declaringType, fieldId) |> Ref
            else Memory.ReadStaticField cilState.state declaringType fieldId
        push value cilState
        setCurrentIp newIp cilState
        [cilState])
    member private x.StsFld (m : Method) offset (cilState : cilState) =
        let newIp = moveInstruction (fallThroughTarget m offset) (currentIp cilState) // TODO: remove this copy-paste
        let fieldInfo = resolveFieldFromMetadata m (offset + Offset.from OpCodes.Stsfld.Size)
        assert fieldInfo.IsStatic
        x.InitializeStatics cilState fieldInfo.DeclaringType (fun cilState ->
        let declaringType = fieldInfo.DeclaringType
        let fieldId = Reflection.wrapField fieldInfo
        let value = pop cilState
        let fieldType = fieldInfo.FieldType
        let value = Types.Cast value fieldType
        Memory.WriteStaticField cilState.state declaringType fieldId value
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
            let t = fieldInfo.DeclaringType
            if t = typeof<IntPtr> || t = typeof<UIntPtr> then
                // This case is used for IntPtr structure -- ignoring field and returning IntPtr pointer
                if addressNeeded then createCilState target
                else Memory.Read cilState.state target |> createCilState
            else
                if addressNeeded then Memory.ReferenceField cilState.state target fieldId |> createCilState
                else Memory.ReadField cilState.state target fieldId |> createCilState
        x.NpeOrInvokeStatementCIL cilState target loadWhenTargetIsNotNull id
    member x.LdFld addressNeeded (m : Method) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata m (offset + Offset.from OpCodes.Ldfld.Size)
        x.LdFldWithFieldInfo fieldInfo addressNeeded cilState
    member x.StFld (m : Method) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata m (offset + Offset.from OpCodes.Stfld.Size)
        assert(not fieldInfo.IsStatic)
        let value, targetRef = pop2 cilState
        let storeWhenTargetIsNotNull (cilState : cilState) k =
            let fieldType = fieldInfo.FieldType
            let value = Types.Cast value fieldType
            let reference =
                if TypeUtils.isPointer fieldInfo.DeclaringType then targetRef
                else Reflection.wrapField fieldInfo |> Memory.ReferenceField cilState.state targetRef
            ConfigureErrorReporter (changeState cilState >> reportError)
            Memory.Write cilState.state reference value |> List.map (changeState cilState) |> k
        x.NpeOrInvokeStatementCIL cilState targetRef storeWhenTargetIsNotNull id
    member private x.LdElemCommon (typ : Type option) (cilState : cilState) arrayRef indices =
        let arrayType = MostConcreteTypeOfRef cilState.state arrayRef
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
    member private x.LdElem (m : Method) offset (cilState : cilState) =
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Ldelem.Size)
        x.LdElemTyp typ cilState
    member private x.LdElemRef = x.LdElemWithCast None
    member private x.LdElema (m : Method) offset (cilState : cilState) =
        let typ = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Ldelema.Size)
        let index, arrayRef = pop2 cilState
        let index = Types.Cast index typeof<int>
        let referenceLocation (cilState : cilState) k =
            let value = Memory.ReferenceArrayIndex cilState.state arrayRef [index] (Some typ)
            push value cilState
            k [cilState]
        let checkTypeMismatch (cilState : cilState) (k : cilState list -> 'a) =
            let elementType = MostConcreteTypeOfRef cilState.state arrayRef |> Types.ElementType
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.TypeIsType typ elementType &&& Types.TypeIsType elementType typ, state))
                referenceLocation
                (x.Raise x.ArrayTypeMismatchException)
                k
        let checkIndex (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            x.AccessArray checkTypeMismatch cilState length index k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkIndex id
    member private x.StElemCommon (typ : Type option) (cilState : cilState) arrayRef indices value =
        let arrayType = MostConcreteTypeOfRef cilState.state arrayRef
        let baseType = Types.ElementType arrayType
        let checkedStElem (cilState : cilState) (k : cilState list -> 'a) =
            let typeOfValue = TypeOf value
            let uncheckedStElem (cilState : cilState) (k : cilState list -> 'a) =
                ConfigureErrorReporter (changeState cilState >> reportError)
                Memory.WriteArrayIndex cilState.state arrayRef indices value typ |> List.map (changeState cilState) |> k
            let checkTypeMismatch (cilState : cilState) (k : cilState list -> 'a) =
                let condition =
                    if Types.IsValueType typeOfValue then True()
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
        let typ = resolveTypeFromMetadata cfg (offset + Offset.from OpCodes.Stelem.Size)
        x.StElemWithCast (Some typ) cilState
    member private x.StElemRef = x.StElemWithCast None
    member private x.LdLen (cilState : cilState) =
        let arrayRef = pop cilState
        let ldlen (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            push length cilState
            k [cilState]
        x.NpeOrInvokeStatementCIL cilState arrayRef ldlen id
    member private x.LdVirtFtn (m : Method) offset (cilState : cilState) =
        let ancestorMethodBase = resolveMethodFromMetadata m (offset + Offset.from OpCodes.Ldvirtftn.Size)
        let this = pop cilState
        let ldvirtftn (cilState : cilState) k =
            assert(IsReference this)
            let thisType = MostConcreteTypeOfRef cilState.state this
            let signature = ancestorMethodBase.GetParameters() |> Array.map (fun p -> p.ParameterType)
            let methodInfo = thisType.GetMethod(ancestorMethodBase.Name, ancestorMethodBase.GetGenericArguments().Length, signature)
            let methodInfoType = methodInfo.GetType()
            let methodPtr = Terms.Concrete methodInfo methodInfoType
            push methodPtr cilState
            k [cilState]
        x.NpeOrInvokeStatementCIL cilState this ldvirtftn id

    member private x.CheckInd ptr invoke cilState =
        let isBadPointer state k =
            match ptr with
            | DetachedPtr _ -> k (True(), state)
            | _ -> k (IsNullReference ptr, state)
        StatedConditionalExecutionCIL cilState
            isBadPointer
            // TODO: may be AccessViolation or NullReference, in general it's undefined behaviour
            (x.Raise x.NullReferenceException)
            invoke
            id

    member private x.Ldind t reportError (cilState : cilState) =
        let address = pop cilState
        let load cilState k =
            let castedAddress =
                if TypeOfLocation address = t then address
                else Types.Cast address (t.MakePointerType())
            ConfigureErrorReporter (changeState cilState >> reportError)
            let value = Memory.Read cilState.state castedAddress
            push value cilState
            k (List.singleton cilState)
        x.CheckInd address load cilState

    member private x.Stind valueCast reportError (cilState : cilState) =
        let value, address = pop2 cilState
        let store cilState k =
            let value = valueCast value
            ConfigureErrorReporter (changeState cilState >> reportError)
            let states = Memory.Write cilState.state address value
            states |> List.map (changeState cilState) |> k
        x.CheckInd address store cilState

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
                    push (NullRef t) cilState
                    k [cilState])
                k
        let hasValueFieldInfo = t.GetField("hasValue", Reflection.instanceBindingFlags)
        let hasValueResults =
            push v cilState
            x.LdFldWithFieldInfo hasValueFieldInfo false cilState  |> List.map (fun cilState -> (pop cilState, cilState))
        Cps.List.mapk boxNullable hasValueResults List.concat

    member x.Box (m : Method) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Box.Size)
        if Types.IsValueType t then
            let v = pop cilState
            if Types.IsNullable t then x.BoxNullable t v cilState
            elif t.IsEnum then
                let enum = Types.Cast v t
                allocateValueTypeInHeap enum cilState
                [cilState]
            else
                allocateValueTypeInHeap v cilState
                [cilState]
        else [cilState]
    member private x.UnboxCommon cilState obj t handleRestResults k =
        let nonExceptionCont (cilState : cilState) res k =
            push res cilState
            k [cilState]
        assert(IsReference obj)
        assert(Types.IsValueType t)
        let nullCase (cilState : cilState) k =
            if Types.IsNullable t then
                let nullableTerm = Memory.DefaultOf t
                let address = Memory.BoxValueType cilState.state nullableTerm
                let ref = HeapReferenceToBoxReference address
                let res = handleRestResults cilState ref
                nonExceptionCont cilState res k
            else
                x.Raise x.NullReferenceException cilState k
        let nullableCase (cilState : cilState) =
            let underlyingTypeOfNullableT = Nullable.GetUnderlyingType t
            ConfigureErrorReporter (changeState cilState >> reportError)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.RefIsType state obj underlyingTypeOfNullableT, state))
                (fun cilState k ->
                    let value = HeapReferenceToBoxReference obj |> Memory.Read cilState.state
                    let nullableTerm = Memory.DefaultOf t
                    let valueField, hasValueField = Reflection.fieldsOfNullable t
                    let nullableTerm = Memory.WriteStructField nullableTerm valueField value
                    let nullableTerm = Memory.WriteStructField nullableTerm hasValueField (MakeBool true)
                    let address = Memory.BoxValueType cilState.state nullableTerm
                    let ref = HeapReferenceToBoxReference address
                    let res = handleRestResults cilState ref
                    nonExceptionCont cilState res k)
                (x.Raise x.InvalidCastException)
        let nonNullCase (cilState : cilState) =
            if Types.IsNullable t then
                nullableCase cilState
            else
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.IsCast state obj t, state))
                    (fun cilState k ->
                        let ref = Types.Cast obj t |> HeapReferenceToBoxReference
                        let res = handleRestResults cilState ref
                        push res cilState
                        k [cilState])
                    (x.Raise x.InvalidCastException)
        BranchOnNullCIL cilState obj
            nullCase
            nonNullCase
            k

    member private x.CommonThrow cilState error isRuntime =
        let codeLocations = List.map (Option.get << ip2codeLocation) cilState.ipStack
        let stackTrace = List.map toString codeLocations |> join ","
        setCurrentIp (SearchingForHandler(codeLocations, List.empty)) cilState
        setException (Unhandled(error, isRuntime, stackTrace)) cilState

    member private x.Throw (cilState : cilState) =
        let error = peek cilState
        let isRuntime = Loader.isRuntimeExceptionsImplementation (currentMethod cilState).FullName
        BranchOnNullCIL cilState error
            (x.Raise x.NullReferenceException)
            (fun cilState k ->
                x.CommonThrow cilState error isRuntime
                clearEvaluationStackLastFrame cilState
                k [cilState])
            id
    member private x.Unbox (m : Method) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Unbox.Size)
        let obj = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then
            let iie = createInsufficientInformation "Unboxing generic parameter"
            cilState.iie <- Some iie
            [cilState]
        else
            x.UnboxCommon cilState obj t (fun _ x -> x) id
    member private x.UnboxAny (m : Method) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Unbox_Any.Size)
        let valueType = typeof<ValueType>
        let obj = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then
            let iie = createInsufficientInformation "Can't introduce generic type X for equation: T = Nullable<X>"
            cilState.iie <- Some iie
            [cilState]
        else
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.TypeIsType t valueType, state))
                (fun cilState k ->
                    let handleRestResults cilState address = Memory.Read cilState.state address
                    x.UnboxCommon cilState obj t handleRestResults k)
                (fun state k -> x.CommonCastClass state obj t k)
                id

    member private this.CommonDivRem performAction (cilState : cilState) =
        let integerCase (cilState : cilState) x y minusOne minValue =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k ((x === minValue) &&& (y === minusOne), state))
                        (this.Raise this.OverflowException)
                        (fun cilState k ->
                            push (performAction x y) cilState
                            k [cilState]))
                id
        let y, x = pop2 cilState
        match y, x with
        | FloatT, FloatT ->
            push (performAction x y) cilState
            [cilState]
        | Int64T, _
        | UInt64T, _
        | _, Int64T
        | _, UInt64T -> integerCase cilState x y (TypeUtils.Int64.MinusOne()) (TypeUtils.Int64.MinValue())
        | _ -> integerCase cilState x y (TypeUtils.Int32.MinusOne()) (TypeUtils.Int32.MinValue())
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
        | _ when TypeUtils.isIntegralTerm x && TypeUtils.isIntegralTerm y ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState k ->
                    push (performAction x y) cilState
                    k [cilState])
                id
        | FloatT, _
        | _, FloatT when isRem -> internalfailf "Rem.Un is unspecified for Floats"
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
        | Int64T, _
        | _, Int64T
        | UInt64T, _
        | _, UInt64T ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            let max = TypeUtils.UInt64.MaxValue()
            let zero = TypeUtils.UInt64.Zero()
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ when TypeUtils.isIntegralTerm x && TypeUtils.isIntegralTerm y ->
            let x, y = makeUnsignedInteger x id, makeUnsignedInteger y id
            let max = TypeUtils.UInt32.MaxValue()
            let zero = TypeUtils.UInt32.Zero()
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> internalfailf "incompatible operands for UnsignedCheckOverflow"
    member private this.SignedCheckOverflow checkOverflow (cilState : cilState) =
        let y, x = pop2 cilState
        match y, x with
        | Int64T, _
        | _, Int64T ->
            let min = TypeUtils.Int64.MinValue()
            let max = TypeUtils.Int64.MaxValue()
            let zero = TypeUtils.Int64.Zero()
            let minusOne = TypeUtils.Int64.MinusOne()
            checkOverflow min max zero minusOne x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | UInt64T, _
        | _, UInt64T -> __unreachable__() // instead of add_ovf should be called add_ovf_un
        | FloatT, _
        | _, FloatT -> __unreachable__() // only integers
        | _ ->
            let min = TypeUtils.Int32.MinValue()
            let max = TypeUtils.Int32.MaxValue()
            let zero = TypeUtils.Int32.Zero()
            let minusOne = TypeUtils.Int32.MinusOne()
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

    member private x.Newarr (m : Method) offset (cilState : cilState) =
        let (>>=) = API.Arithmetics.(>>=)
        let elemType = resolveTypeFromMetadata m (offset + Offset.from OpCodes.Newarr.Size)
        let numElements = pop cilState
        let allocate cilState k =
            try
                let ref = Memory.AllocateVectorArray cilState.state numElements elemType
                push ref cilState
                k [cilState]
            with
            | :? OutOfMemoryException -> x.Raise x.OutOfMemoryException cilState k
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (numElements >>= TypeUtils.Int32.Zero(), state))
            allocate
            (this.Raise this.OverflowException)
            id

    member x.CreateException (exceptionType : Type) arguments cilState =
        assert (not <| exceptionType.IsValueType)
        Logger.printLog Logger.Info $"{exceptionType}!\nStack trace:\n{Memory.StackTrace cilState.state.stack}"
        clearEvaluationStackLastFrame cilState
        let constructors = exceptionType.GetConstructors()
        let argumentsLength = List.length arguments
        let argumentsTypes =
            List.map TypeOf arguments
        let suitable (ci : ConstructorInfo) =
            let parameters = ci.GetParameters()
            parameters.Length = argumentsLength
            && parameters |> Seq.forall2 (fun p1 p2 -> p2.ParameterType.IsAssignableFrom p1) argumentsTypes
        let ctors = constructors |> Array.filter suitable
        assert(Array.length ctors = 1)
        let ctor = ctors[0]
        let fullConstructorName = Reflection.getFullMethodName ctor
        assert (Loader.hasRuntimeExceptionsImplementation fullConstructorName)
        let proxyCtor = Loader.getRuntimeExceptionsImplementation fullConstructorName |> Application.getMethod
        ILInterpreter.InitFunctionFrameCIL cilState proxyCtor None (Some arguments)
        let success = x.TryConcreteInvoke proxyCtor proxyCtor.FullName arguments None cilState
        assert success

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

    member x.ExecuteAllInstructionsForCFGEdges (m : Method) (cilState : cilState) : cilState list * cilState list * cilState list =
        let ip = currentIp cilState
        assert(ip.CanBeExpanded())
        let startingOffset = ip.Offset()
        let cfg = m.CFG
        let endOffset =
            let lastOffset = Seq.last cfg.SortedBasicBlocks
            let rec binarySearch l r =
                if l + 1 = r then l
                else
                    let mid = (l + r) / 2
                    if cfg.SortedBasicBlocks.[mid].StartOffset <= startingOffset then binarySearch mid r
                    else binarySearch l mid
            let index = binarySearch 0 (Seq.length cfg.SortedBasicBlocks)
            if cfg.SortedBasicBlocks.[index] = lastOffset then Offset.from cfg.IlBytes.Length
            else cfg.SortedBasicBlocks.[index + 1].StartOffset

        let isIpOfCurrentBasicBlock (ip : ip) =
            match ip with
            | Instruction(i, m') when m' = m -> startingOffset <= i && i < endOffset
            | _ -> false
        x.ExecuteAllInstructionsWhile isIpOfCurrentBasicBlock cilState

    member x.ExecuteOneInstruction (cilState : cilState) : cilState list * cilState list * cilState list =
        x.ExecuteAllInstructionsWhile (always false) cilState

    // returns finishedStates, incompleteStates, erroredStates
    member x.ExecuteAllInstructionsWhile (condition : ip -> bool) (cilState : cilState) =
        let rec executeAllInstructions (finishedStates, incompleteStates, errors) cilState k =
            let ip = currentIp cilState
            try
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

    member private x.IncrementLevelIfNeeded (m : Method) (offset : offset) (cilState : cilState) =
        if offset = 0<offsets> || m.CFG.IsLoopEntry offset then
            incrementLevel cilState {offset = offset; method = m}

    member private x.DecrementMethodLevel (cilState : cilState) method =
        let key = {offset = 0<offsets>; method = method}
        decrementLevel cilState key

    member private x.FindNeededEHCBlock offset moreSuitable (ehcs : ExceptionHandlingClause seq) =
        let findBlock acc (x : ExceptionHandlingClause) =
            // NOTE: need to execute the most wide finally block
            match acc with
            | Some block when moreSuitable x block -> Some x
            | Some _ -> acc
            | None ->
                // NOTE: check that this block protects current ip
                if x.tryOffset <= offset && x.tryOffset + x.tryLength > offset then Some x else None
        Seq.fold findBlock None ehcs

    static member private FallThroughCall (cilState : cilState) =
        match cilState.ipStack with
        | InstructionEndingIp(offset, caller) as ip :: _ ->
            // TODO: assert (isCallIp ip)
            let newIp = moveInstruction (fallThroughTarget caller offset) ip
            setCurrentIp newIp cilState
            let callOpCode, calledMethod = caller.ParseCallSite offset
            if callOpCode = OpCodes.Newobj && (calledMethod.DeclaringType.IsValueType || TypeUtils.isPointer calledMethod.DeclaringType) then
                pushNewObjForValueTypes cilState
        | _ -> __unreachable__()

    member x.MakeStep (cilState : cilState) =
        cilState.stepsNumber <- cilState.stepsNumber + 1u
        let exit m =
            x.DecrementMethodLevel cilState m
            Logger.info $"Done with method {m}"
            match cilState.ipStack with
            // NOTE: the whole method is executed
//            | [ Exit _ ] when startsFromMethodBeginning cilState ->
//                setCurrentTime [] cilState
//                popFrameOf cilState
            // NOTE: some part of method is executed
            | [ Exit _ ] -> ()
            | Exit m :: ips' when m.IsStaticConstructor ->
                popFrameOf cilState
                setIpStack ips' cilState
            | Exit _ :: _ ->
                popFrameOf cilState
                ILInterpreter.FallThroughCall cilState
            | _ -> __unreachable__()
        let rec makeStep' ip k =
            match ip with
            | Instruction(offset, m) ->
                if offset = 0<offsets> then Logger.info $"Starting to explore method {m}"
                x.ExecuteInstruction m offset cilState |> k
            | Exit m ->
                exit m
                k [cilState]
            | Leave(EndFinally, [],  dst, m) ->
                setCurrentIp (instruction m dst) cilState
                clearEvaluationStackLastFrame cilState
                k [cilState]
            | Leave(EndFinally, ehc :: ehcs,  dst, m) ->
                assert(isFinallyClause ehc || isFaultClause ehc)
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
                // TODO: need SecondBypass if handler was not found
                setCurrentIp (SearchingForHandler([], [])) cilState
                k [cilState]
            | SearchingForHandler(location :: otherLocations, framesToPop) ->
                let method = location.method
                let ehcs = method.ExceptionHandlers
                let filter = ehcs |> Seq.filter isFilterClause // TODO: use
                let exceptionType = MostConcreteTypeOfRef cilState.state (cilState.state.exceptionsRegister.GetError())
                let isSuitable ehc =
                    match ehc.ehcType with
                    | Catch t -> TypeUtils.isSubtypeOrEqual exceptionType t
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
                clearEvaluationStackLastFrame cilState
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
                let ehcs = location.method.ExceptionHandlers
                let finallyBlocks = ehcs |> Seq.filter (fun ehc -> isFinallyClause ehc || isFaultClause ehc)
                let isWider (x : ExceptionHandlingClause) (y : ExceptionHandlingClause) =
                    x.handlerOffset < y.handlerOffset && x.handlerOffset + x.handlerLength > y.handlerOffset + y.handlerLength
                let neededBlock = x.FindNeededEHCBlock location.offset isWider finallyBlocks
                let finallyHandlerIp = neededBlock |> Option.map (fun b -> Instruction(b.handlerOffset, location.method))
                let ip = SecondBypass(finallyHandlerIp, otherLocations, codeLocation)
                clearEvaluationStackLastFrame cilState
                popFrameOf cilState
                clearEvaluationStackLastFrame cilState
                setCurrentIp ip cilState
                k [cilState]
            | _ -> __notImplemented__()
        makeStep' (currentIp cilState) id

    member private x.ExecuteInstruction m offset (cilState : cilState) =
//        Logger.trace "ExecuteInstruction:\n%s" (dump cilState)
        let opCode = parseInstruction m offset
//        let newIps = moveIp cilState |> List.map (fun cilState -> cilState.ipStack)

        let opcodeValue = LanguagePrimitives.EnumOfValue opCode.Value
        let newSts =
            match opcodeValue with
            | OpCodeValues.Br
            | OpCodeValues.Br_S -> br m offset cilState; [cilState]
            | OpCodeValues.Add ->
                (fun _ _ -> standardPerformBinaryOperation OperationType.Add) |> fallThrough m offset cilState // TODO: check float overflow [spec]
            | OpCodeValues.Mul -> (fun _ _ -> standardPerformBinaryOperation OperationType.Multiply) |> fallThrough m offset cilState
            | OpCodeValues.Sub -> (fun _ _ -> standardPerformBinaryOperation OperationType.Subtract) |> fallThrough m offset cilState
            | OpCodeValues.Shl -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftLeft) |> fallThrough m offset cilState
            | OpCodeValues.Shr -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight) |> fallThrough m offset cilState
            | OpCodeValues.Shr_Un -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight_Un) |> fallThrough m offset cilState
            | OpCodeValues.Ceq -> (fun _ _ -> ceq) |> fallThrough m offset cilState
            | OpCodeValues.Cgt -> (fun _ _ -> cgt) |> fallThrough m offset cilState
            | OpCodeValues.Cgt_Un -> (fun _ _ -> cgtun) |> fallThrough m offset cilState
            | OpCodeValues.Clt -> (fun _ _ -> clt) |> fallThrough m offset cilState
            | OpCodeValues.Clt_Un -> (fun _ _ -> cltun) |> fallThrough m offset cilState
            | OpCodeValues.And -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseAnd OperationType.LogicalAnd) |> fallThrough m offset cilState
            | OpCodeValues.Or -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseOr OperationType.LogicalOr) |> fallThrough m offset cilState
            | OpCodeValues.Xor -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseXor OperationType.LogicalXor) |> fallThrough m offset cilState
            | OpCodeValues.Neg -> (fun _ _ -> performCILUnaryOperation OperationType.UnaryMinus) |> fallThrough m offset cilState
            | OpCodeValues.Not -> (fun _ _ -> bitwiseOrBoolNot) |> fallThrough m offset cilState
            | OpCodeValues.Stloc -> stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + Offset.from OpCodes.Stloc.Size) |> int) |> forkThrough m offset cilState
            | OpCodeValues.Stloc_0 -> stloc (fun _ _ -> 0) |> forkThrough m offset cilState
            | OpCodeValues.Stloc_1 -> stloc (fun _ _ -> 1) |> forkThrough m offset cilState
            | OpCodeValues.Stloc_2 -> stloc (fun _ _ -> 2) |> forkThrough m offset cilState
            | OpCodeValues.Stloc_3 -> stloc (fun _ _ -> 3) |> forkThrough m offset cilState
            | OpCodeValues.Stloc_S -> stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + Offset.from OpCodes.Stloc_S.Size) |> int) |> forkThrough m offset cilState
            | OpCodeValues.Starg -> starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + Offset.from OpCodes.Starg.Size) |> int) |> forkThrough m offset cilState
            | OpCodeValues.Starg_S -> starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + Offset.from OpCodes.Starg_S.Size) |> int) |> forkThrough m offset cilState
            | OpCodeValues.Ldc_I4 -> ldc (fun ilBytes offset -> NumberCreator.extractInt32 ilBytes (offset + Offset.from OpCodes.Ldc_I4.Size)) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_0 -> ldc (fun _ _ -> 0) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_1 -> ldc (fun _ _ -> 1) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_2 -> ldc (fun _ _ -> 2) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_3 -> ldc (fun _ _ -> 3) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_4 -> ldc (fun _ _ -> 4) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_5 -> ldc (fun _ _ -> 5) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_6 -> ldc (fun _ _ -> 6) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_7 -> ldc (fun _ _ -> 7) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_8 -> ldc (fun _ _ -> 8) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_M1 -> ldc (fun _ _ -> -1) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I4_S -> ldc (fun ilBytes offset -> NumberCreator.extractInt8 ilBytes (offset + Offset.from OpCodes.Ldc_I4_S.Size)) typedefof<int32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_I8 -> ldc (fun ilBytes offset -> NumberCreator.extractInt64 ilBytes (offset + Offset.from OpCodes.Ldc_I8.Size)) typedefof<int64> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_R4 -> ldc (fun ilBytes offset -> NumberCreator.extractFloat32 ilBytes (offset + Offset.from OpCodes.Ldc_R4.Size)) typedefof<float32> |> fallThrough m offset cilState
            | OpCodeValues.Ldc_R8 -> ldc (fun ilBytes offset -> NumberCreator.extractFloat64 ilBytes (offset + Offset.from OpCodes.Ldc_R8.Size)) typedefof<double> |> fallThrough m offset cilState
            | OpCodeValues.Ldarg -> ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + Offset.from OpCodes.Ldarg.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Ldarg_0 -> ldarg (fun _ _ -> 0) |> fallThrough m offset cilState
            | OpCodeValues.Ldarg_1 -> ldarg (fun _ _ -> 1) |> fallThrough m offset cilState
            | OpCodeValues.Ldarg_2 -> ldarg (fun _ _ -> 2) |> fallThrough m offset cilState
            | OpCodeValues.Ldarg_3 -> ldarg (fun _ _ -> 3) |> fallThrough m offset cilState
            | OpCodeValues.Ldarg_S -> ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + Offset.from OpCodes.Ldarg_S.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Nop -> fallThrough m offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Ldloc -> ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + Offset.from OpCodes.Ldloc.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Ldloc_0 -> ldloc (fun _ _ -> 0) |> fallThrough m offset cilState
            | OpCodeValues.Ldloc_1 -> ldloc (fun _ _ -> 1) |> fallThrough m offset cilState
            | OpCodeValues.Ldloc_2 -> ldloc (fun _ _ -> 2) |> fallThrough m offset cilState
            | OpCodeValues.Ldloc_3 -> ldloc (fun _ _ -> 3) |> fallThrough m offset cilState
            | OpCodeValues.Ldloc_S -> ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + Offset.from OpCodes.Ldloc_S.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Ldloca -> ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + Offset.from OpCodes.Ldloca.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Ldloca_S -> ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + Offset.from OpCodes.Ldloca_S.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Ret -> this.Ret m cilState
            | OpCodeValues.Dup -> (fun _ _ -> dup) |> fallThrough m offset cilState

            // branching
            | OpCodeValues.Brfalse -> brfalse m offset cilState
            | OpCodeValues.Brfalse_S -> brfalse m offset cilState
            | OpCodeValues.Brtrue -> brtrue m offset cilState
            | OpCodeValues.Brtrue_S -> brtrue m offset cilState
            | OpCodeValues.Beq -> applyAndBranch ceq brtrue m offset cilState
            | OpCodeValues.Beq_S -> applyAndBranch ceq brtrue m offset cilState
            | OpCodeValues.Bge -> applyAndBranch bgeHelper brfalse m offset cilState
            | OpCodeValues.Bge_S -> applyAndBranch bgeHelper brfalse m offset cilState

            | OpCodeValues.Bgt -> applyAndBranch cgt brtrue m offset cilState
            | OpCodeValues.Bgt_S -> applyAndBranch cgt brtrue m offset cilState
            | OpCodeValues.Bgt_Un -> applyAndBranch cgtun brtrue m offset cilState
            | OpCodeValues.Bgt_Un_S -> applyAndBranch cgtun brtrue m offset cilState
            | OpCodeValues.Ble -> applyAndBranch cgt brfalse m offset cilState
            | OpCodeValues.Ble_S -> applyAndBranch cgt brfalse m offset cilState
            | OpCodeValues.Ble_Un -> applyAndBranch cgtun brfalse m offset cilState
            | OpCodeValues.Ble_Un_S -> applyAndBranch cgtun brfalse m offset cilState
            | OpCodeValues.Blt -> applyAndBranch clt brtrue m offset cilState
            | OpCodeValues.Blt_S -> applyAndBranch clt brtrue m offset cilState
            | OpCodeValues.Blt_Un -> applyAndBranch cltun brtrue m offset cilState
            | OpCodeValues.Blt_Un_S -> applyAndBranch cltun brtrue m offset cilState
            | OpCodeValues.Bne_Un -> applyAndBranch ceq brfalse m offset cilState
            | OpCodeValues.Bne_Un_S -> applyAndBranch ceq brfalse m offset cilState
            | OpCodeValues.Bge_Un -> applyAndBranch cltun brfalse m offset cilState
            | OpCodeValues.Bge_Un_S -> applyAndBranch cltun brfalse m offset cilState

            | OpCodeValues.Ldstr -> ldstr |> fallThrough m offset cilState
            | OpCodeValues.Ldnull -> (fun _ _ -> ldnull) |> fallThrough m offset cilState
            | OpCodeValues.Conv_I1 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int8Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_I2 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int16Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_I4 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int32Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_I8 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.int64Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_R4 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.float32Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_R8 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.float64Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_U1 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint8Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_U2 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint16Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_U4 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint32Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_U8 -> (fun _ _ -> castTopOfOperationalStack TypeUtils.uint64Type) |> fallThrough m offset cilState
            | OpCodeValues.Conv_I -> (fun _ _ -> convi) |> fallThrough m offset cilState
            | OpCodeValues.Conv_U -> (fun _ _ -> convu) |> fallThrough m offset cilState
            | OpCodeValues.Conv_R_Un -> (fun _ _ -> castTopOfOperationalStack TypeUtils.float64Type) |> fallThrough m offset cilState
            | OpCodeValues.Switch -> switch m offset cilState
            | OpCodeValues.Ldtoken -> ldtoken |> fallThrough m offset cilState
            | OpCodeValues.Ldftn -> ldftn |> fallThrough m offset cilState
            | OpCodeValues.Pop -> (fun _ _ -> pop >> ignore) |> fallThrough m offset cilState
            | OpCodeValues.Initobj -> initobj |> forkThrough m offset cilState
            | OpCodeValues.Ldarga -> ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + Offset.from OpCodes.Ldarga.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Ldarga_S -> ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + Offset.from OpCodes.Ldarga_S.Size) |> int) |> fallThrough m offset cilState
            | OpCodeValues.Ldind_I4 -> (fun _ _ -> x.Ldind TypeUtils.int32Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_I1 -> (fun _ _ -> x.Ldind TypeUtils.int8Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_I2 -> (fun _ _ -> x.Ldind TypeUtils.int16Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_I8 -> (fun _ _ -> x.Ldind TypeUtils.int64Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_U1 -> (fun _ _ -> x.Ldind TypeUtils.uint8Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_U2 -> (fun _ _ -> x.Ldind TypeUtils.uint16Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_U4 -> (fun _ _ -> x.Ldind TypeUtils.uint32Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_R4 -> (fun _ _ -> x.Ldind TypeUtils.float32Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_R8 -> (fun _ _ -> x.Ldind TypeUtils.float64Type reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldind_Ref -> (fun _ _ -> x.Ldind TypeUtils.intPtr reportError) |> forkThrough m offset cilState
            // TODO: need to cast to nativeint? #do
            | OpCodeValues.Ldind_I -> (fun _ _ -> x.Ldind TypeUtils.intPtr reportError) |> forkThrough m offset cilState
            | OpCodeValues.Isinst -> isinst |> forkThrough m offset cilState
            | OpCodeValues.Stobj -> (stobj reportError) |> forkThrough m offset cilState
            | OpCodeValues.Ldobj -> ldobj |> fallThrough m offset cilState
            | OpCodeValues.Stind_I1 -> (fun _ _ -> x.Stind (castUnchecked TypeUtils.int8Type) reportError) |> forkThrough m offset cilState
            | OpCodeValues.Stind_I2 -> (fun _ _ -> x.Stind (castUnchecked TypeUtils.int16Type) reportError) |> forkThrough m offset cilState
            | OpCodeValues.Stind_I4 -> (fun _ _ -> x.Stind (castUnchecked TypeUtils.int32Type) reportError) |> forkThrough m offset cilState
            | OpCodeValues.Stind_I8 -> (fun _ _ -> x.Stind (castUnchecked TypeUtils.int64Type) reportError) |> forkThrough m offset cilState
            | OpCodeValues.Stind_R4 -> (fun _ _ -> x.Stind (castUnchecked TypeUtils.float32Type) reportError) |> forkThrough m offset cilState
            | OpCodeValues.Stind_R8 -> (fun _ _ -> x.Stind (castUnchecked TypeUtils.float64Type) reportError) |> forkThrough m offset cilState
            | OpCodeValues.Stind_Ref -> (fun _ _ -> x.Stind id reportError) |> forkThrough m offset cilState
            | OpCodeValues.Stind_I -> (fun _ _ -> x.Stind MakeIntPtr reportError) |> forkThrough m offset cilState
            | OpCodeValues.Sizeof -> sizeofInstruction |> fallThrough m offset cilState
            | OpCodeValues.Leave
            | OpCodeValues.Leave_S -> leave m offset cilState; [cilState]
            | OpCodeValues.Endfinally -> (fun _ _ -> endfinally) |> fallThrough m offset cilState
            | OpCodeValues.Rethrow -> (fun _ _ -> rethrow) |> fallThrough m offset cilState
            | OpCodeValues.Endfilter -> (fun _ _ -> endfilter) |> fallThrough m offset cilState
            | OpCodeValues.Localloc -> (fun _ _ -> localloc) |> fallThrough m offset cilState
            // TODO: notImplemented instructions
            | OpCodeValues.Stelem_I -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Ldelem_I -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Arglist -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Jmp -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Break -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Calli -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Ckfinite -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Constrained_ -> constrained |> fallThrough m offset cilState
            | OpCodeValues.Cpblk -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Cpobj -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Mkrefany -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefix1 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefix2 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefix3 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefix4 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefix5 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefix6 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefix7 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Prefixref -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Readonly_ -> fallThrough m offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Refanytype -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Refanyval -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Tail_ -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Unaligned_ -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState
            | OpCodeValues.Volatile_ -> fallThrough m offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Initblk -> (fun _ _ _ -> __notImplemented__()) |> fallThrough m offset cilState

            | OpCodeValues.Call -> this.Call m offset cilState
            | OpCodeValues.Callvirt -> this.CallVirt m offset cilState
            | OpCodeValues.Newobj -> this.NewObj m offset cilState
            | OpCodeValues.Ldsfld -> this.LdsFld false m offset cilState
            | OpCodeValues.Ldsflda -> this.LdsFld true m offset cilState
            | OpCodeValues.Stsfld -> this.StsFld m offset cilState
            | OpCodeValues.Ldfld -> this.LdFld false |> forkThrough m offset cilState
            | OpCodeValues.Ldflda -> this.LdFld true |> forkThrough m offset cilState
            | OpCodeValues.Stfld -> this.StFld |> forkThrough m offset cilState
            | OpCodeValues.Ldelem -> this.LdElem |> forkThrough m offset cilState
            | OpCodeValues.Ldelema -> this.LdElema |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_I1 -> (fun _ _ -> this.LdElemTyp TypeUtils.int8Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_I2 -> (fun _ _ -> this.LdElemTyp TypeUtils.int16Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_I4 -> (fun _ _ -> this.LdElemTyp TypeUtils.int32Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_I8 -> (fun _ _ -> this.LdElemTyp TypeUtils.int64Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_R4 -> (fun _ _ -> this.LdElemTyp TypeUtils.float32Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_R8 -> (fun _ _ -> this.LdElemTyp TypeUtils.float64Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_U1 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint8Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_U2 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint16Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_U4 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint32Type) |> forkThrough m offset cilState
            | OpCodeValues.Ldelem_Ref -> (fun _ _ -> this.LdElemRef) |> forkThrough m offset cilState
            | OpCodeValues.Stelem -> (fun _ _ -> this.StElem m offset) |> forkThrough m offset cilState
            | OpCodeValues.Stelem_I1 -> (fun _ _ -> this.StElemTyp TypeUtils.int8Type) |> forkThrough m offset cilState
            | OpCodeValues.Stelem_I2 -> (fun _ _ -> this.StElemTyp TypeUtils.int16Type) |> forkThrough m offset cilState
            | OpCodeValues.Stelem_I4 -> (fun _ _ -> this.StElemTyp TypeUtils.int32Type) |> forkThrough m offset cilState
            | OpCodeValues.Stelem_I8 -> (fun _ _ -> this.StElemTyp TypeUtils.int64Type) |> forkThrough m offset cilState
            | OpCodeValues.Stelem_R4 -> (fun _ _ -> this.StElemTyp TypeUtils.float32Type) |> forkThrough m offset cilState
            | OpCodeValues.Stelem_R8 -> (fun _ _ -> this.StElemTyp TypeUtils.float64Type) |> forkThrough m offset cilState
            | OpCodeValues.Stelem_Ref -> (fun _ _ -> this.StElemRef) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I1 -> (fun _ _ -> this.ConvOvf TypeUtils.int8Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I2 -> (fun _ _ -> this.ConvOvf TypeUtils.int16Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I4 -> (fun _ _ -> this.ConvOvf TypeUtils.int32Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I8 -> (fun _ _ -> this.ConvOvf TypeUtils.int64Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I -> (fun _ _ -> convi) |> fallThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U1 -> (fun _ _ -> this.ConvOvf TypeUtils.uint8Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U2 -> (fun _ _ -> this.ConvOvf TypeUtils.uint16Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U4 -> (fun _ _ -> this.ConvOvf TypeUtils.uint32Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U8 -> (fun _ _ -> this.ConvOvf TypeUtils.uint64Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U -> (fun _ _ -> convu) |> fallThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I1_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int8Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I2_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int16Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I4_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int32Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I8_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.int64Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_I_Un -> (fun _ _ -> convi) |> fallThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U1_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint8Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U2_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint16Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U4_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint32Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U8_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.uint64Type) |> forkThrough m offset cilState
            | OpCodeValues.Conv_Ovf_U_Un -> (fun _ _ -> convu) |> fallThrough m offset cilState
            | OpCodeValues.Castclass -> this.CastClass |> forkThrough m offset cilState
            | OpCodeValues.Ldlen -> (fun _ _ -> this.LdLen) |> forkThrough m offset cilState
            | OpCodeValues.Ldvirtftn -> this.LdVirtFtn |> forkThrough m offset cilState
            | OpCodeValues.Box -> this.Box |> forkThrough m offset cilState
            | OpCodeValues.Unbox -> this.Unbox |> forkThrough m offset cilState
            | OpCodeValues.Unbox_Any -> this.UnboxAny |> forkThrough m offset cilState
            | OpCodeValues.Add_Ovf_Un -> (fun _ _ -> this.Add_ovf_un) |> forkThrough m offset cilState
            | OpCodeValues.Sub_Ovf_Un -> (fun _ _ -> this.Sub_ovf_un) |> forkThrough m offset cilState
            | OpCodeValues.Mul_Ovf_Un -> (fun _ _ -> this.Mul_ovf_un) |> forkThrough m offset cilState
            | OpCodeValues.Add_Ovf -> (fun _ _ -> this.Add_ovf) |> forkThrough m offset cilState
            | OpCodeValues.Sub_Ovf -> (fun _ _ -> this.Sub_ovf) |> forkThrough m offset cilState
            | OpCodeValues.Mul_Ovf -> (fun _ _ -> this.Mul_ovf) |> forkThrough m offset cilState
            | OpCodeValues.Div -> (fun _ _ -> this.Div) |> forkThrough m offset cilState
            | OpCodeValues.Div_Un -> (fun _ _ -> this.DivUn) |> forkThrough m offset cilState
            | OpCodeValues.Rem -> (fun _ _ -> this.Rem) |> forkThrough m offset cilState
            | OpCodeValues.Rem_Un -> (fun _ _ -> this.RemUn) |> forkThrough m offset cilState
            | OpCodeValues.Newarr -> this.Newarr |> forkThrough m offset cilState
            | OpCodeValues.Throw -> this.Throw cilState
            | _ -> __unreachable__()

        let renewInstructionsInfo cilState =
            if not <| isUnhandledError cilState then
                x.IncrementLevelIfNeeded m offset cilState
        newSts |> List.iter renewInstructionsInfo
        newSts
