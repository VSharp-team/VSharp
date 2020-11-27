namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Reflection.Emit

open VSharp
open VSharp.Core
open VSharp.Reflection
open CFG

type public ILMethodMetadata =
    { methodBase : MethodBase }
    override x.ToString () = x.methodBase.Name
    interface IMethodIdentifier with
        member x.IsStatic = x.methodBase.IsStatic
        member x.IsConstructor = x.methodBase.IsConstructor
        member x.Method = x.methodBase
        member x.DeclaringType = x.methodBase.DeclaringType
        member x.DeclaringAssembly = x.methodBase.DeclaringType.Assembly
        member x.ReturnType =
            match x.methodBase with
            | :? MethodInfo as mi -> mi.ReturnType
            | :? ConstructorInfo -> typeof<Void>
            | _ -> __notImplemented__()
        member x.Location = null // TODO: think about renaming ICodeLocation to smth more useful

module internal TypeUtils =
    open Types

    // TODO: get all this functions from Core #mbdo
    let float64TermType = Numeric typedefof<double>
    let float32TermType = Numeric typedefof<float32>
    let int8Type        = Numeric typedefof<int8>
    let int16Type       = Numeric typedefof<int16>
    let int32Type       = Numeric typedefof<int32>
    let int64Type       = Numeric typedefof<int64>
    let uint8Type       = Numeric typedefof<uint8>
    let uint16Type      = Numeric typedefof<uint16>
    let uint32Type      = Numeric typedefof<uint32>
    let uint64Type      = Numeric typedefof<uint64>
    let charType        = Numeric typedefof<char>

    let signed2unsignedOrId = function
        | Bool -> uint32Type
        | Numeric (Id typ) when typ = typedefof<int32> || typ = typedefof<uint32> -> uint32Type
        | Numeric (Id typ) when typ = typedefof<int8>  || typ = typedefof<uint8>  -> uint8Type
        | Numeric (Id typ) when typ = typedefof<int16> || typ = typedefof<uint16> -> uint16Type
        | Numeric (Id typ) when typ = typedefof<int64> || typ = typedefof<uint64> -> uint64Type
        | Numeric (Id typ) when typ = typedefof<double> -> float64TermType
        | _ -> __unreachable__()
    let unsigned2signedOrId = function
        | Bool -> int32Type
        | Numeric (Id typ) when typ = typedefof<int32> || typ = typedefof<uint32> -> int32Type
        | Numeric (Id typ) when typ = typedefof<int8>  || typ = typedefof<uint8> -> int8Type
        | Numeric (Id typ) when typ = typedefof<int16> || typ = typedefof<uint16> -> int16Type
        | Numeric (Id typ) when typ = typedefof<int64> || typ = typedefof<uint64> -> int64Type
        | Numeric (Id typ) when typ = typedefof<double> -> float64TermType
        | Pointer _ as t -> t
        | _ -> __unreachable__()
    let integers = [charType; int8Type; int16Type; int32Type; int64Type; uint8Type; uint16Type; uint32Type; uint64Type]

    let isIntegerTermType typ = integers |> List.contains typ
    let isFloatTermType typ = typ = float32TermType || typ = float64TermType
    let isInteger = Terms.TypeOf >> isIntegerTermType
    let isBool = Terms.TypeOf >> Types.IsBool
    let (|Int8|_|) t = if Terms.TypeOf t = int8Type then Some(t) else None
    let (|UInt8|_|) t = if Terms.TypeOf t = uint8Type then Some(t) else None
    let (|Int16|_|) t = if Terms.TypeOf t = int16Type then Some(t) else None
    let (|UInt16|_|) t = if Terms.TypeOf t = uint16Type then Some(t) else None
    let (|Int32|_|) t = if Terms.TypeOf t = int32Type then Some(t) else None
    let (|UInt32|_|) t = if Terms.TypeOf t = uint32Type then Some(t) else None
    let (|Int64|_|) t = if Terms.TypeOf t = int64Type then Some(t) else None
    let (|UInt64|_|) t = if Terms.TypeOf t = uint64Type then Some(t) else None
    let (|Bool|_|) t = if isBool t then Some(t) else None
    let (|Float32|_|) t = if Terms.TypeOf t = float32TermType then Some(t) else None
    let (|Float64|_|) t = if Terms.TypeOf t = float64TermType then Some(t) else None
    let (|Float|_|) t = if Terms.TypeOf t |> isFloatTermType then Some(t) else None

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

    let __corruptedStack__() = raise (InvalidProgramException())

    // ------------------------------- Helper functions for cilState -------------------------------


    let idTransformation term k = k term

    let pushResultOnStack (cilState : cilState) (res, state) =
        if res <> Nop then
            {cilState with opStack = res :: cilState.opStack; state = state}
        else {cilState with state = state}
    let mapFunctionResultsk mapResult =
        let mapResults (result, cilState : cilState) =
            let result, state = mapResult (result, cilState.state)
            result, {cilState with state = state}
        Cps.List.map mapResults
    let getCilStateFromResult results : cilState list = List.map snd results
    let mapAndPushFunctionResultsk mapResult =
        let mapAndPushResult (term, cilState : cilState) =
            mapResult (term, cilState.state) |> pushResultOnStack cilState
        let exceptionCheck ((_, cilState : cilState) as result) =
            if cilState.HasException then cilState
            else mapAndPushResult result
        Cps.List.map exceptionCheck
    let pushFunctionResults results = mapAndPushFunctionResultsk id results id
    let pushResultFromStateToCilState (cilState : cilState) (states : state list) : cilState list =
        states |> List.map (fun (state : state) ->
            if state.exceptionsRegister.UnhandledError then {cilState with state = state} // TODO: check whether opStack = [] is needed
            else
                let opStack =
                    match state.returnRegister with
                    | None -> cilState.opStack
                    | Some r -> r :: cilState.opStack
                let state = {state with returnRegister = None}
                {cilState with state = state; opStack = opStack})

    let withResult res' (s : state) = {s with returnRegister = Some res'}
    // --------------------------------------- Primitives ----------------------------------------

    let StatedConditionalExecutionCIL (cilState : cilState) (condition : state -> (term * state -> 'a) -> 'a) (thenBranch : cilState -> ('c list -> 'a) -> 'a) (elseBranch : cilState -> ('c list -> 'a) -> 'a) (k : 'c list -> 'a) =
        StatedConditionalExecution cilState.state condition
            (fun state k -> thenBranch {cilState with state = state} k)
            (fun state k -> elseBranch {cilState with state = state} k)
            (fun x y -> List.append x y |> List.singleton)
            (List.head >> k)
//    let GuardedApply (cilState : cilState) term f k =
    let GuardedApply (cilState : cilState) term (f : cilState -> term -> ('a list -> 'b) -> 'b) (k : 'a list -> 'b) =
        GuardedStatedApplyk
            (fun state term k -> f {cilState with state = state} term k)
            cilState.state term id (List.concat >> k)
    let GuardedApplyForState state term f k = GuardedStatedApplyk f state term id (List.concat >> k)
    let BranchOnNull (state : state) term thenBranch elseBranch k =
        StatedConditionalExecution state
            (fun state k -> k (IsNullReference term, state))
            thenBranch
            elseBranch
            (fun res1 res2 -> [res1; res2])
            (List.concat >> k)
    let resolveFieldFromMetadata (cfg : cfgData) = Instruction.resolveFieldFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTypeFromMetadata (cfg : cfgData) = Instruction.resolveTypeFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTermTypeFromMetadata state (cfg : cfgData) = resolveTypeFromMetadata cfg >> Types.FromDotNetType state
    let resolveMethodFromMetadata (cfg : cfgData) = Instruction.resolveMethodFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTokenFromMetadata (cfg : cfgData) = Instruction.resolveTokenFromMetadata cfg.methodBase cfg.ilBytes

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
        let result = methodInfo.Invoke(null, parameters)
        let appendResultToState (term : term, state : state) =
            match term.term with
            | Nop -> {state with returnRegister = None}
            | _ -> {state with returnRegister = Some term}
        match result with
        | :? (term * state) as r -> r |> appendResultToState |> List.singleton |> k
        | :? ((term * state) list) as r -> r |> List.map appendResultToState |> k
        | _ -> internalfail "internal call should return tuple term * state!"


    // ------------------------------- CIL instructions -------------------------------

    let getVarTerm state index (methodBase : MethodBase) =
        let lvi = methodBase.GetMethodBody().LocalVariables.[index]
        let stackKey = LocalVariableKey(lvi, methodBase)
        let typ = Types.FromDotNetType state lvi.LocalType
        Ref (PrimitiveStackLocation stackKey), state, typ
    let getArgTerm index (methodBase : MethodBase) =
        let pi = methodBase.GetParameters().[index]
        PrimitiveStackLocation (ParameterKey pi) |> Ref

    let castReferenceToPointerIfNeeded term typ state =
        if isReference term && Types.IsPointer typ
        then Types.CastReferenceToPointer state term // TODO: casting to pointer is weird
        else term
    let castUnchecked typ term (state : state) : term =
        let term = castReferenceToPointerIfNeeded term typ state
        Types.Cast term typ
    let popOperationalStack (cilState : cilState) =
        match cilState.opStack with
        | t :: ts -> Some t, {cilState with opStack = ts}
        | [] -> None, cilState
    let ldc numberCreator t (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let num = numberCreator cfg.ilBytes shiftedOffset
        let termType = Types.FromDotNetType cilState.state t
        { cilState with opStack = Concrete num termType :: cilState.opStack } :: []

    let ldloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let reference, state, _ = getVarTerm cilState.state index cfg.methodBase
        let term = Memory.ReadSafe state reference
        pushResultOnStack cilState (term, state) :: []

    let ldarg numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let state = cilState.state
        let arg, state =
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis state cfg.methodBase
            match this, cfg.methodBase.IsStatic with
            | None, _
            | Some _, true ->
                let term = getArgTerm argumentIndex cfg.methodBase
                Memory.ReadSafe state term, state
            | Some this, _ when argumentIndex = 0 -> this, state
            | Some _, false ->
                let term = getArgTerm (argumentIndex - 1) cfg.methodBase
                Memory.ReadSafe state term, state
        pushResultOnStack cilState (arg, state) :: []
    let ldarga numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let state = cilState.state
        let address =
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis state cfg.methodBase
            match this with
            | None -> getArgTerm argumentIndex cfg.methodBase
            | Some _ when argumentIndex = 0 -> internalfail "can't load address of ``this''"
            | Some _ -> getArgTerm (argumentIndex - 1) cfg.methodBase
        pushResultOnStack cilState (address, state) :: []
    let stloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let variableIndex = numberCreator cfg.ilBytes shiftedOffset
        let state = cilState.state
        let left, state, typ = getVarTerm state variableIndex cfg.methodBase
        match cilState.opStack with
         | right :: stack ->
            let value = castUnchecked typ right state
            let states = Memory.WriteSafe state left value
            states |> List.map (fun state -> {cilState with opStack = stack; state = state})
         | _ -> __corruptedStack__()
    let performCILUnaryOperation op isChecked (cilState : cilState) =
        // TODO: why isChecked is unused?
        match cilState.opStack with
        | x :: stack ->
            API.PerformUnaryOperation op (Terms.TypeOf x) x (fun term ->
            { cilState with opStack = term :: stack } :: [])
        | _ -> __corruptedStack__()
    let performCILBinaryOperation op operand1Transform operand2Transform resultTransform (cilState : cilState) =
        match cilState.opStack with
        | arg2 :: arg1 :: stack ->
            operand1Transform arg1 (fun arg1 ->
            operand2Transform arg2 (fun arg2 ->
            API.PerformBinaryOperation op arg1 arg2 (fun interimRes ->
            resultTransform interimRes (fun res -> {cilState with opStack = res :: stack } :: []))))
        | _ -> __corruptedStack__()
    let makeSignedInteger term k =
        let typ = Terms.TypeOf term
        let signedTyp = TypeUtils.unsigned2signedOrId typ
        if TypeUtils.isIntegerTermType typ && typ <> signedTyp then
            // TODO: check whether term is not pointer
            k <| Types.Cast term signedTyp // no specs found about overflows
        else k term
    let standardPerformBinaryOperation op =
        performCILBinaryOperation op makeSignedInteger makeSignedInteger idTransformation
    let shiftOperation op (cilState : cilState) =
        let reinterpret termType term k = k <| Types.Cast term termType
        match cilState.opStack with
        | _ :: value :: _ ->
            let op1trans, resTrans =
                match value with
                | TypeUtils.Bool _
                | TypeUtils.Int8 _
                | TypeUtils.UInt8 _
                | TypeUtils.Int16 _
                | TypeUtils.UInt16 _
                | TypeUtils.Int32 _ -> reinterpret TypeUtils.int32Type, reinterpret TypeUtils.int32Type
                | TypeUtils.UInt32 _ -> idTransformation, reinterpret TypeUtils.uint32Type
                | TypeUtils.UInt64 _ -> idTransformation, reinterpret TypeUtils.uint64Type
                | TypeUtils.Int64 _ -> idTransformation, reinterpret TypeUtils.int64Type
                | _ -> __notImplemented__()
            performCILBinaryOperation op op1trans idTransformation resTrans cilState
        | _ -> __corruptedStack__()
    let dup (cilState : cilState) =
        match cilState.opStack with
        | t :: _ -> [{ cilState with opStack = t :: cilState.opStack }]
        | _ -> __corruptedStack__()

    let ret (cfg : cfgData) _ (cilState : cilState) =
        let state = {cilState.state with currentTime = []}
        let cilState = {cilState with state = state}
        let resultTyp =
            match cfg.methodBase with
            | :? ConstructorInfo -> Void
            | :? MethodInfo as mi -> mi.ReturnType |> Types.FromDotNetType state
            | _ -> __notImplemented__()
        let term, cilState = popOperationalStack cilState
        let typ =
            match term with
            | Some t -> TypeOf t
            | None -> Void
        match term, resultTyp with
        | None, Void -> cilState :: []
        | Some t, _ when typ = resultTyp -> { cilState with state = withResult t state } :: [] // TODO: [simplification] remove this heuristics
        | Some t, _ ->
            let t = castUnchecked resultTyp t state
            {cilState with state = withResult t state } :: []
         | _ -> __unreachable__()
    let Transform2BooleanTerm pc (term : term) =
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
                term !== MakeNumber (t.GetEnumValues().GetValue(0))
            | _ when isReference term -> !!(IsNullReference term)//  term !== NullRef
            | _ -> __notImplemented__()
        GuardedApplyExpressionWithPC pc term check

    let ceq (cilState : cilState) =
        match cilState.opStack with
        | y :: x :: _ ->
            let transform =
                if TypeUtils.isBool x || TypeUtils.isBool y
                then fun t k -> k (Transform2BooleanTerm cilState.state.pc t)
                else idTransformation
            performCILBinaryOperation OperationType.Equal transform transform idTransformation cilState
        | _ -> __corruptedStack__()
    let starg numCreator (cfg : cfgData) offset (cilState : cilState) =
        let argumentIndex = numCreator cfg.ilBytes offset
        let argTerm =
           let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.methodBase
           match this with
           | None -> getArgTerm argumentIndex cfg.methodBase
           | Some this when argumentIndex = 0 -> this
           | Some _ -> getArgTerm (argumentIndex - 1) cfg.methodBase
        match cilState.opStack with
        | value :: stack ->
            let states = Memory.WriteSafe cilState.state argTerm value
            states |> List.map (fun state -> { cilState with opStack = stack; state = state})
        | _ -> __corruptedStack__()
    let brcommon condTransform offsets (cilState : cilState) =
        match cilState.opStack with
        | cond :: stack ->
           let offsetThen, offsetElse =
               match offsets with
               | [offsetThen; offsetElse] -> offsetThen, offsetElse
               | _ -> __unreachable__()
           let cilState = {cilState with opStack = stack}
           StatedConditionalExecutionCIL cilState
               (fun state k -> k (condTransform <| Transform2BooleanTerm state.pc cond, state))
               (fun cilState k -> k [offsetThen, cilState])
               (fun cilState k -> k [offsetElse, cilState])
               id
        | _ -> __corruptedStack__()
    let brfalse = brcommon id
    let brtrue = brcommon (!!)
    let applyAndBranch errorStr additionalFunction brtrueFunction (cfg : cfgData) offset newOffsets (cilState : cilState) =
        match additionalFunction cfg offset [Instruction offset] cilState with
        | [_, st] -> brtrueFunction newOffsets st
        | _ -> internalfail errorStr
    let compare op operand1Transformation operand2Transformation (cilState : cilState) =
        match cilState.opStack with
        | _ :: arg1 :: _ ->
            let typ = TypeOf arg1
            if typ = TypeUtils.float64TermType then
                // TODO: handle NaN
                performCILBinaryOperation op idTransformation idTransformation idTransformation cilState
            else
                performCILBinaryOperation op operand1Transformation operand2Transformation idTransformation cilState
        | _ -> __corruptedStack__()
    let boolToInt b =
        BranchExpressions (fun k -> k b) (fun k -> k TypeUtils.Int32.One) (fun k -> k TypeUtils.Int32.Zero) id
    let bitwiseOperation op (cilState : cilState) =
        match cilState.opStack with
        | arg2 :: arg1 :: _ ->
            let typ1, typ2 = TypeOf arg1, TypeOf arg2
            match typ1, typ2 with
            | Bool, Bool ->
                standardPerformBinaryOperation op cilState
            | _ when TypeUtils.isIntegerTermType typ1 && TypeUtils.isIntegerTermType typ2 ->
                standardPerformBinaryOperation op cilState
            | Bool, typ2 when TypeUtils.isIntegerTermType typ2 ->
                let newArg1 = boolToInt arg1
                performCILBinaryOperation op (fun _ k -> k newArg1) idTransformation idTransformation cilState
            | typ1, Bool when TypeUtils.isIntegerTermType typ1 ->
                let newArg2 = boolToInt arg2
                performCILBinaryOperation op idTransformation (fun _ k -> k newArg2) idTransformation cilState
            | typ1, typ2 -> internalfailf "unhandled case for Bitwise operation %O and types: %O %O" op typ1 typ2
        | _ -> __corruptedStack__()
    let retrieveActualParameters (methodBase : MethodBase) (cilState : cilState) =
        let paramsNumber = methodBase.GetParameters().Length
        let parameters, opStack = List.splitAt paramsNumber cilState.opStack
        let castParameter parameter (parInfo : ParameterInfo) =
            if Reflection.IsDelegateConstructor methodBase && parInfo.ParameterType = typeof<System.IntPtr> then parameter
            else
                let typ = parInfo.ParameterType |> Types.FromDotNetType cilState.state
                castUnchecked typ parameter cilState.state
        let parameters = Seq.map2 castParameter (List.rev parameters) (methodBase.GetParameters()) |> List.ofSeq
        parameters, {cilState with opStack = opStack}

    let makeUnsignedInteger term k =
        let typ = Terms.TypeOf term
        let unsignedTyp = TypeUtils.signed2unsignedOrId typ
        if TypeUtils.isIntegerTermType typ && typ <> unsignedTyp then
            // TODO: check whether term is not pointer
            k <| Types.Cast term unsignedTyp // no specs found about overflows
        else k term
    let performUnsignedIntegerOperation op (cilState : cilState) =
        match cilState.opStack with
        | arg2 :: arg1 :: _ when TypeUtils.isInteger arg1 && TypeUtils.isInteger arg2 ->
            performCILBinaryOperation op makeUnsignedInteger makeUnsignedInteger idTransformation cilState
        | _ :: _ :: _ -> internalfailf "arguments for %O are not Integers!" op
        | _ -> __corruptedStack__()
    let ldstr (cfg : cfgData) offset (cilState : cilState) =
        let stringToken = NumberCreator.extractInt32 cfg.ilBytes (offset + OpCodes.Ldstr.Size)
        let string = cfg.methodBase.Module.ResolveString stringToken
        let state = cilState.state
        let referenceAndState = Memory.AllocateString string state
        pushResultOnStack cilState referenceAndState :: []
    let allocateValueTypeInHeap v (cilState : cilState) =
        let address, state = Memory.BoxValueType cilState.state v
        {cilState with opStack = address :: cilState.opStack; state = state} :: []
    let ldnull (cilState : cilState) =
        pushResultOnStack cilState (NullRef, cilState.state) :: []

    let convu (cilState : cilState) = cilState :: []
    let convi (cilState : cilState) = cilState :: []
    let castTopOfOperationalStackUnchecked targetType typeForStack (cilState : cilState) =
        match cilState.opStack with
        | t :: stack ->
            let term = castUnchecked targetType t cilState.state
            let termForStack =castUnchecked typeForStack term cilState.state
            {cilState with opStack = termForStack::stack} :: []
        | _ -> __corruptedStack__()
    let ldloca numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let term, state, _ = getVarTerm cilState.state index cfg.methodBase
        pushResultOnStack cilState (term, state) :: []
    let switch newOffsets (cilState : cilState) =
        match cilState.opStack with
        | value :: stack ->
            let cilState = {cilState with opStack = stack}
            let checkOneCase (guard, newOffset) cilState kRestCases =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (guard, state))
                    (fun cilState k -> k [newOffset, cilState])
                    (fun _ k -> kRestCases cilState k) // ignore pc because we always know that cases do not overlap
            let fallThroughOffset, newOffsets = List.head newOffsets, List.tail newOffsets
            let casesAndOffsets = List.mapi (fun i offset -> value === MakeNumber i, offset) newOffsets
            let fallThroughGuard = // TODO: [cast int :> uint] `value` should be compared as uint
                let cond1 = Arithmetics.(>>=) value (List.length newOffsets |> MakeNumber)
                let cond2 = Arithmetics.(<<) value TypeUtils.Int32.Zero // TODO: so no need to check this
                cond1 ||| cond2
            Cps.List.foldrk checkOneCase cilState ((fallThroughGuard, fallThroughOffset)::casesAndOffsets) (fun _ k -> k []) id
        | _ -> __corruptedStack__()
    let ldtoken (cfg : cfgData) offset (cilState : cilState) =
        let memberInfo = resolveTokenFromMetadata cfg (offset + OpCodes.Ldtoken.Size)
        let state = cilState.state
        let res =
            match memberInfo with
            | :? FieldInfo as fi -> Terms.Concrete fi.FieldHandle (Types.FromDotNetType state typeof<RuntimeFieldHandle>)
            | :? Type as t -> Terms.Concrete t.TypeHandle (Types.FromDotNetType state typeof<RuntimeTypeHandle>)
            | :? MethodInfo as mi -> Terms.Concrete mi.MethodHandle (Types.FromDotNetType state typeof<RuntimeMethodHandle>)
            | _ -> internalfailf "Could not resolve token"
        pushResultOnStack cilState (res, state) :: []
    let ldftn (cfg : cfgData) offset (cilState : cilState) =
        let methodInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Ldftn.Size)
        let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType cilState.state (methodInfo.GetType()))
        pushResultOnStack cilState (methodPtr, cilState.state) :: []
    let initobj (cfg : cfgData) offset (cilState : cilState) =
        match cilState.opStack with
        | targetAddress :: stack ->
            let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Initobj.Size)
            let states = Memory.WriteSafe cilState.state targetAddress (Memory.DefaultOf typ)
            states |> List.map (fun state -> {cilState with state = state; opStack = stack})
        | _ -> __corruptedStack__()
    let ldind valueCast (cilState : cilState) =
        match cilState.opStack with
        | address :: stack ->
            let value = Memory.ReadSafe cilState.state address
            let value = valueCast value cilState.state
            {cilState with opStack = value::stack} :: []
        | _ -> __corruptedStack__()
    let ldindref = ldind always
    let clt = compare OperationType.Less idTransformation idTransformation
    let cltun = compare OperationType.Less makeUnsignedInteger makeUnsignedInteger
    let bgeHelper (cilState : cilState) =
        match cilState.opStack with
        | arg1 :: arg2 :: _ ->
            let typ1, typ2 = Terms.TypeOf arg1, Terms.TypeOf arg2
            if Types.IsInteger typ1 && Types.IsInteger typ2 then clt cilState
            elif Types.IsReal typ1 && Types.IsReal typ2 then cltun cilState
            else __notImplemented__()
        | _ -> __corruptedStack__()
    let isinst (cfg : cfgData) offset (cilState : cilState) =
        match cilState.opStack with
        | object :: stack ->
            let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Isinst.Size)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (IsNullReference object, state))
                (fun cilState k -> k [{cilState with opStack = NullRef :: stack}])
                (fun cilState k ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k (Types.IsCast typ object, state))
                        (fun cilState k -> k [cilState])
                        (fun cilState k -> k [{cilState with opStack = NullRef :: stack}])
                        k)
                id
        | _ -> __corruptedStack__()
    let cgtun (cilState : cilState) =
        match cilState.opStack with
        | arg2 :: arg1 :: _ when isReference arg2 && isReference arg1 ->
            compare OperationType.NotEqual idTransformation idTransformation cilState
        | _ -> compare OperationType.Greater makeUnsignedInteger makeUnsignedInteger cilState
    let ldobj (cfg : cfgData) offset (cilState : cilState) =
        match cilState.opStack with
        | address :: stack ->
            let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Ldobj.Size)
            let value = Memory.ReadSafe cilState.state address
            let typedValue = castUnchecked typ value cilState.state
            {cilState with opStack = typedValue::stack} :: []
        | _ -> __corruptedStack__()
    let stobj (cfg : cfgData) offset (cilState : cilState) =
        match cilState.opStack with
        | src :: dest :: stack ->
            let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Stobj.Size)
            let value = castUnchecked typ src cilState.state
            let states = Memory.WriteSafe cilState.state dest value
            states |> List.map (fun state -> {cilState with opStack = stack; state = state})
        | _ -> __corruptedStack__()
    let stind typ (cilState : cilState) =
        match cilState.opStack with
        | value :: address :: stack ->
            let value = castUnchecked typ value cilState.state
            let states = Memory.WriteSafe cilState.state address value
            states |> List.map (fun state -> {cilState with opStack = stack; state = state})
        | _ -> __corruptedStack__()
    let sizeofInstruction (cfg : cfgData) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Sizeof.Size)
        let size = Types.SizeOf typ
        { cilState with opStack = MakeNumber size :: cilState.opStack } :: []
    let throw cfg offset (cilState : cilState) =
        match cilState.opStack with
        | error :: _ ->
            { cilState with state = {cilState.state with exceptionsRegister = Unhandled error}; opStack = [] } :: []
        | _ -> __corruptedStack__()
    let leave _ _ (cilState : cilState) = cilState :: []
    let rethrow _ _ (cilState : cilState) =
        let state = cilState.state
        Prelude.releaseAssert(Option.isSome state.exceptionsRegister.ExceptionTerm)
        let state = {state with exceptionsRegister = state.exceptionsRegister.TransformToUnhandled()}
        { cilState with state = state} |> List.singleton
    let endfilter _ _ (cilState : cilState) =
        match cilState.opStack with
        | value :: [] -> {cilState with filterResult = Some value} :: []
        | _ -> __notImplemented__()
    let endfinally _ _ (cilState : cilState) =
        { cilState with opStack = [] } :: []
    let zipWithOneOffset op cfgData offset newOffsets cilState =
        assert (List.length newOffsets = 1)
        let newOffset = List.head newOffsets
        let cilStates = op cfgData offset cilState
        List.map (withFst newOffset) cilStates

    let opcode2Function : (cfgData -> offset -> ip list -> cilState -> (ip * cilState) list) [] = Array.create 300 (fun _ _ _ -> internalfail "Interpreter is not ready")
    opcode2Function.[hashFunction OpCodes.Br]                 <- zipWithOneOffset <| fun _ _ cilState -> cilState :: []
    opcode2Function.[hashFunction OpCodes.Br_S]               <- zipWithOneOffset <| fun _ _ cilState -> cilState :: []
    opcode2Function.[hashFunction OpCodes.Add]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Add
    opcode2Function.[hashFunction OpCodes.Mul]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Multiply
    opcode2Function.[hashFunction OpCodes.Sub]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Subtract
    opcode2Function.[hashFunction OpCodes.Shl]                <- zipWithOneOffset <| fun _ _ -> shiftOperation OperationType.ShiftLeft
    opcode2Function.[hashFunction OpCodes.Shr]                <- zipWithOneOffset <| fun _ _ -> shiftOperation OperationType.ShiftRight
    opcode2Function.[hashFunction OpCodes.Shr_Un]             <- zipWithOneOffset <| fun _ _ -> shiftOperation OperationType.ShiftRight
    opcode2Function.[hashFunction OpCodes.Ceq]                <- zipWithOneOffset <| fun _ _ -> ceq
    opcode2Function.[hashFunction OpCodes.Cgt]                <- zipWithOneOffset <| fun _ _ -> compare OperationType.Greater idTransformation idTransformation
    opcode2Function.[hashFunction OpCodes.Cgt_Un]             <- zipWithOneOffset <| fun _ _ -> cgtun
    opcode2Function.[hashFunction OpCodes.Clt]                <- zipWithOneOffset <| fun _ _ -> clt
    opcode2Function.[hashFunction OpCodes.Clt_Un]             <- zipWithOneOffset <| fun _ _ -> cltun
    opcode2Function.[hashFunction OpCodes.And]                <- zipWithOneOffset <| fun _ _ -> bitwiseOperation OperationType.LogicalAnd
    opcode2Function.[hashFunction OpCodes.Or]                 <- zipWithOneOffset <| fun _ _ -> bitwiseOperation OperationType.LogicalOr
    opcode2Function.[hashFunction OpCodes.Xor]                <- zipWithOneOffset <| fun _ _ -> bitwiseOperation OperationType.LogicalXor
    opcode2Function.[hashFunction OpCodes.Neg]                <- zipWithOneOffset <| fun _ _ -> performCILUnaryOperation OperationType.UnaryMinus false
    opcode2Function.[hashFunction OpCodes.Not]                <- zipWithOneOffset <| fun _ _ -> performCILUnaryOperation OperationType.LogicalNeg false
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
    opcode2Function.[hashFunction OpCodes.Ret]                <- zipWithOneOffset <| ret
    opcode2Function.[hashFunction OpCodes.Dup]                <- zipWithOneOffset <| fun _ _ -> dup

    // branching
    opcode2Function.[hashFunction OpCodes.Brfalse]            <- fun _ _ -> brfalse
    opcode2Function.[hashFunction OpCodes.Brfalse_S]          <- fun _ _ -> brfalse
    opcode2Function.[hashFunction OpCodes.Brtrue]             <- fun _ _ -> brtrue
    opcode2Function.[hashFunction OpCodes.Brtrue_S]           <- fun _ _ -> brtrue
    opcode2Function.[hashFunction OpCodes.Beq]                <- applyAndBranch "Beq" (opcode2Function.[hashFunction OpCodes.Ceq]) brtrue
    opcode2Function.[hashFunction OpCodes.Beq_S]              <- applyAndBranch "Beq_S" (opcode2Function.[hashFunction OpCodes.Ceq]) brtrue
    opcode2Function.[hashFunction OpCodes.Bge]                <- applyAndBranch "Bge" (zipWithOneOffset <| fun _ _ -> bgeHelper) brfalse
    opcode2Function.[hashFunction OpCodes.Bge_S]              <- applyAndBranch "Bge_S" (zipWithOneOffset <| fun _ _ -> bgeHelper) brfalse

    opcode2Function.[hashFunction OpCodes.Bgt]                <- applyAndBranch "Bgt" (opcode2Function.[hashFunction OpCodes.Cgt]) brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_S]              <- applyAndBranch "Bgt_S" (opcode2Function.[hashFunction OpCodes.Cgt]) brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_Un]             <- applyAndBranch "Bgt_Un" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_Un_S]           <- applyAndBranch "Bgt_Un_S" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Ble]                <- applyAndBranch "Ble" (opcode2Function.[hashFunction OpCodes.Cgt]) brfalse
    opcode2Function.[hashFunction OpCodes.Ble_S]              <- applyAndBranch "Ble_S" (opcode2Function.[hashFunction OpCodes.Cgt]) brfalse
    opcode2Function.[hashFunction OpCodes.Ble_Un]             <- applyAndBranch "Ble_Un" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brfalse
    opcode2Function.[hashFunction OpCodes.Ble_Un_S]           <- applyAndBranch "Ble_Un_S" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brfalse
    opcode2Function.[hashFunction OpCodes.Blt]                <- applyAndBranch "Blt" (opcode2Function.[hashFunction OpCodes.Clt]) brtrue
    opcode2Function.[hashFunction OpCodes.Blt_S]              <- applyAndBranch "Blt_S" (opcode2Function.[hashFunction OpCodes.Clt]) brtrue
    opcode2Function.[hashFunction OpCodes.Blt_Un]             <- applyAndBranch "Blt_Un" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Blt_Un_S]           <- applyAndBranch "Blt_Un_S" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Bne_Un]             <- applyAndBranch "Bne_Un" (opcode2Function.[hashFunction OpCodes.Ceq]) brfalse
    opcode2Function.[hashFunction OpCodes.Bne_Un_S]           <- applyAndBranch "Bne_Un_S" (opcode2Function.[hashFunction OpCodes.Ceq]) brfalse
    opcode2Function.[hashFunction OpCodes.Bge_Un]             <- applyAndBranch "Bge_Un" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brfalse
    opcode2Function.[hashFunction OpCodes.Bge_Un_S]           <- applyAndBranch "Bge_Un_S" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brfalse

    opcode2Function.[hashFunction OpCodes.Ldstr]              <- zipWithOneOffset <| ldstr
    opcode2Function.[hashFunction OpCodes.Ldnull]             <- zipWithOneOffset <| fun _ _ -> ldnull
    opcode2Function.[hashFunction OpCodes.Conv_I1]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int8Type TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_I2]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int16Type TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_I4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int32Type TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_I8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int64Type TypeUtils.int64Type
    opcode2Function.[hashFunction OpCodes.Conv_R4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float32TermType TypeUtils.float32TermType
    opcode2Function.[hashFunction OpCodes.Conv_R8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64TermType TypeUtils.float64TermType
    opcode2Function.[hashFunction OpCodes.Conv_U1]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint8Type TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_U2]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint16Type TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_U4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint32Type TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_U8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint64Type TypeUtils.int64Type
    opcode2Function.[hashFunction OpCodes.Conv_I]             <- zipWithOneOffset <| fun _ _ -> convi //castTopOfOperationalStackUnchecked TypeUtils.nativeintType TypeUtils.nativeintType
    opcode2Function.[hashFunction OpCodes.Conv_U]             <- zipWithOneOffset <| fun _ _ -> convu //castTopOfOperationalStackUnchecked TypeUtils.unativeintType TypeUtils.nativeintType
    opcode2Function.[hashFunction OpCodes.Conv_R_Un]          <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64TermType TypeUtils.float64TermType
    opcode2Function.[hashFunction OpCodes.Switch]             <- fun _ _ -> switch
    opcode2Function.[hashFunction OpCodes.Ldtoken]            <- zipWithOneOffset <| ldtoken
    opcode2Function.[hashFunction OpCodes.Ldftn]              <- zipWithOneOffset <| ldftn
    opcode2Function.[hashFunction OpCodes.Pop]                <- zipWithOneOffset <| fun _ _ st -> popOperationalStack st |> snd |> List.singleton
    opcode2Function.[hashFunction OpCodes.Initobj]            <- zipWithOneOffset <| initobj
    opcode2Function.[hashFunction OpCodes.Ldarga]             <- zipWithOneOffset <| ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarga.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldarga_S]           <- zipWithOneOffset <| ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarga_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldind_I4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I1]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int8Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I2]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int16Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I8]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int64Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U1]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.uint8Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U2]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.uint16Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.uint32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_R4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.float32TermType)
    opcode2Function.[hashFunction OpCodes.Ldind_R8]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.float64TermType)
    opcode2Function.[hashFunction OpCodes.Ldind_Ref]          <- zipWithOneOffset <| fun _ _ -> ldindref
    opcode2Function.[hashFunction OpCodes.Ldind_I]            <- zipWithOneOffset <| fun _ _ -> ldind always
    opcode2Function.[hashFunction OpCodes.Isinst]             <- zipWithOneOffset isinst
    opcode2Function.[hashFunction OpCodes.Stobj]              <- zipWithOneOffset <| stobj
    opcode2Function.[hashFunction OpCodes.Ldobj]              <- zipWithOneOffset <| ldobj
    opcode2Function.[hashFunction OpCodes.Stind_I]            <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Stind_I4] opcode2Function.[hashFunction OpCodes.Stind_I8]
    opcode2Function.[hashFunction OpCodes.Stind_I1]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int8Type
    opcode2Function.[hashFunction OpCodes.Stind_I2]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int16Type
    opcode2Function.[hashFunction OpCodes.Stind_I4]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Stind_I8]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int64Type
    opcode2Function.[hashFunction OpCodes.Stind_R4]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.float32TermType
    opcode2Function.[hashFunction OpCodes.Stind_R8]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.float64TermType
    opcode2Function.[hashFunction OpCodes.Stind_Ref]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Sizeof]             <- zipWithOneOffset <| sizeofInstruction
    opcode2Function.[hashFunction OpCodes.Throw]              <- zipWithOneOffset <| throw
    opcode2Function.[hashFunction OpCodes.Leave]              <- zipWithOneOffset <| leave
    opcode2Function.[hashFunction OpCodes.Leave_S]            <- zipWithOneOffset <| leave
    opcode2Function.[hashFunction OpCodes.Endfinally]         <- zipWithOneOffset <| endfinally
    opcode2Function.[hashFunction OpCodes.Rethrow]            <- zipWithOneOffset <| rethrow
    opcode2Function.[hashFunction OpCodes.Endfilter]          <- zipWithOneOffset <| endfilter
    // TODO: notImplemented instructions

    opcode2Function.[hashFunction OpCodes.Stelem_I]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Arglist]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Jmp]                <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Break]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Calli]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ckfinite]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Constrained]        <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Cpblk]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Cpobj]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Localloc]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ldelema]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ldelem_I]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Mkrefany]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix1]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix2]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix3]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix4]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix5]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix6]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix7]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefixref]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Readonly]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Refanytype]         <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Refanyval]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Tailcall]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Unaligned]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Volatile]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Initblk]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
