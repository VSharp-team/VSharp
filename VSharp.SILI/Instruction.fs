namespace VSharp.Interpreter.IL

open VSharp
open System.Reflection
open System.Reflection.Emit

exception IncorrectCIL of string

//type offset = VSharp.Core.offset
type offset = int
type term = VSharp.Core.term
type state = VSharp.Core.state

type operationalStack = term list

type ip =
    | Instruction of offset
    | Exit
    | FindingHandler of offset // offset -- source of exception
    with
    member x.CanBeExpanded () =
        match x with
        | Instruction _ -> true
        | _ -> false
    member x.Offset () =
        match x with
        | Instruction i -> i
        | _              -> internalfail "Could not get vertex from destination"
type ipTransition =
    | FallThrough of offset
    | Return
    | UnconditionalBranch of offset
    | ConditionalBranch of offset list
    | ExceptionMechanism

type cilState =
    { ip : ip
      isCompleted : bool
      state : state
      leaveInstructionExecuted : bool
      filterResult : term option
    }
    interface VSharp.Core.IInterpreterState<cilState> with
        member x.InternalState = x.state
        member x.SetState st = {x with state = st}
        member x.SetResultTerm resTerm = {x with state = {x.state with returnRegister = resTerm}}
        member x.ResultTerm = x.state.returnRegister
    member x.CanBeExpanded () = x.ip.CanBeExpanded()
//    member x.IsFinished = x.isFinished x.ip
    member x.HasException = Option.isSome x.state.exceptionsRegister.ExceptionTerm

//    static member Empty =
//        {
//            ip = Exit
//            isFinished = fun ip -> ip = Exit
//            recursiveVertices = []
//            state = VSharp.Core.API.Memory.EmptyState
//            leaveInstructionExecuted = false
//            filterResult = None
//        }
    static member MakeEmpty curV state =
        { ip = curV
          isCompleted = false
          state = state
          leaveInstructionExecuted = false
          filterResult = None
        }
module internal NumberCreator =
    let public extractInt32 (ilBytes : byte []) pos =
        System.BitConverter.ToInt32(ilBytes, pos)
    let public extractUnsignedInt32 (ilBytes : byte []) pos =
        System.BitConverter.ToUInt32(ilBytes, pos)
    let public extractUnsignedInt16 (ilBytes : byte []) pos =
        System.BitConverter.ToUInt16(ilBytes, pos)
    let public extractInt64 (ilBytes : byte []) pos =
        System.BitConverter.ToInt64(ilBytes, pos)
    let public extractInt8 (ilBytes : byte []) pos =
        ilBytes.[pos] |> sbyte |> int
    let public extractUnsignedInt8 (ilBytes : byte []) pos =
        ilBytes.[pos]
    let public extractFloat64 (ilBytes : byte []) pos =
        System.BitConverter.ToDouble(ilBytes, pos)
    let public extractFloat32 (ilBytes : byte []) pos =
        System.BitConverter.ToSingle(ilBytes, pos)

module internal Instruction =

    let private extractToken = NumberCreator.extractInt32

    let resolveFieldFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveField methodBase
    let resolveTypeFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveType methodBase
    let resolveMethodFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveMethod methodBase
    let resolveTokenFromMetadata methodBase ilBytes = extractToken ilBytes >> Reflection.resolveToken methodBase

    let private isSingleByteOpCodeValue = (<) 0
    let private isSingleByteOpCode = (<>) 0xFEuy

    let private equalSizeOpCodesCount = 0x100
    let private singleByteOpCodes = Array.create equalSizeOpCodesCount OpCodes.Nop;
    let private twoBytesOpCodes = Array.create equalSizeOpCodesCount OpCodes.Nop;

    let private fillOpCodes =
        let resolve (field : FieldInfo) =
            match field.GetValue() with
            | :? OpCode as opCode -> let value = int opCode.Value
                                     if isSingleByteOpCodeValue value then singleByteOpCodes.[value] <- opCode
                                     else twoBytesOpCodes.[value &&& 0xFF] <- opCode
            | _ -> ()

        typeof<OpCodes>.GetRuntimeFields() |> Seq.iter resolve

    let private operandType2operandSize = [| 4; 4; 4; 8; 4; 0; -1; 8; 4; 4; 4; 4; 4; 4; 2; 1; 1; 4; 1|]

    let private jumpTargetsForNext (opCode : OpCode) _ pos =
        let nextInstruction = pos + opCode.Size + operandType2operandSize.[int opCode.OperandType]
        FallThrough nextInstruction

    let private jumpTargetsForBranch (opCode : OpCode) ilBytes pos =
        let offset =
            match opCode.OperandType with
            | OperandType.InlineBrTarget -> NumberCreator.extractInt32 ilBytes (pos + opCode.Size)
            | _ -> NumberCreator.extractInt8 ilBytes (pos + opCode.Size)

        let nextInstruction = pos + opCode.Size + operandType2operandSize.[int opCode.OperandType]
        if offset = 0 && opCode <> OpCodes.Leave && opCode <> OpCodes.Leave_S
        then FallThrough nextInstruction
        else UnconditionalBranch <| offset + nextInstruction

    let private inlineBrTarget extract (opCode : OpCode) ilBytes pos =
        let offset = extract ilBytes (pos + opCode.Size)
        let nextInstruction = pos + opCode.Size + operandType2operandSize.[int opCode.OperandType]
        ConditionalBranch [nextInstruction; nextInstruction + offset]

    let private inlineSwitch (opCode : OpCode) ilBytes pos =
        let n = NumberCreator.extractUnsignedInt32 ilBytes (pos + opCode.Size) |> int
        let nextInstruction = pos + opCode.Size + 4 * n + 4
        let nextOffsets =
            List.init n (fun x -> nextInstruction + NumberCreator.extractInt32 ilBytes (pos + opCode.Size + 4 * (x + 1)))
        ConditionalBranch <| nextInstruction :: nextOffsets

    let private jumpTargetsForReturn _ _ _ = Return
    let private jumpTargetsForThrow _ _ _ = ExceptionMechanism

    let findNextInstructionOffsetAndEdges (opCode : OpCode) =
        match opCode.FlowControl with
        | FlowControl.Next
        | FlowControl.Call
        | FlowControl.Break
        | FlowControl.Meta -> jumpTargetsForNext
        | FlowControl.Branch -> jumpTargetsForBranch
        | FlowControl.Cond_Branch ->
            match opCode.OperandType with
            | OperandType.InlineBrTarget -> inlineBrTarget NumberCreator.extractInt32
            | OperandType.ShortInlineBrTarget -> inlineBrTarget NumberCreator.extractInt8
            | OperandType.InlineSwitch -> inlineSwitch
            | _ -> __notImplemented__()
        | FlowControl.Return -> jumpTargetsForReturn
        | FlowControl.Throw -> jumpTargetsForThrow
        | _ -> __notImplemented__()
        <| opCode

    let isLeaveOpCode (opCode : OpCode) = opCode = OpCodes.Leave || opCode = OpCodes.Leave_S

    let private isCallOpCode (opCode : OpCode) =
        opCode = OpCodes.Call
        || opCode = OpCodes.Calli
        || opCode = OpCodes.Callvirt
        || opCode = OpCodes.Tailcall
    let private isNewObjOpCode (opCode : OpCode) =
        opCode = OpCodes.Newobj
    let isDemandingCallOpCode (opCode : OpCode) =
        isCallOpCode opCode || isNewObjOpCode opCode
    let isFinallyClause (ehc : ExceptionHandlingClause) =
        ehc.Flags = ExceptionHandlingClauseOptions.Finally
    let shouldExecuteFinallyClause (src : ip) (dst : ip) (ehc : ExceptionHandlingClause) =
        let srcOffset, dstOffset = src.Offset(), dst.Offset()
        let isInside offset = ehc.TryOffset <= offset && offset < ehc.TryOffset + ehc.TryLength
        isInside srcOffset && not <| isInside dstOffset

    let (|Call|_|) (opCode : OpCode) = if opCode = OpCodes.Call then Some () else None
    let (|CallVirt|_|) (opCode : OpCode) = if opCode = OpCodes.Callvirt then Some () else None
    let (|Calli|_|) (opCode : OpCode) = if opCode = OpCodes.Calli then Some () else None
    let (|TailCall|_|) (opCode : OpCode) = if opCode = OpCodes.Tailcall then Some () else None
    let (|NewObj|_|) (opCode : OpCode) = if opCode = OpCodes.Newobj then Some () else None

    let parseInstruction (ilBytes : byte []) pos =
        let b1 = ilBytes.[pos]
        if isSingleByteOpCode b1 then singleByteOpCodes.[int b1]
        elif pos + 1 >= ilBytes.Length then raise (IncorrectCIL("Prefix instruction FE without suffix!"))
        else twoBytesOpCodes.[int ilBytes.[pos + 1]]



    let countOperationalStackBalance (opCode : OpCode) (calledMethod : MethodBase option) oldBalance =
        let countMethodCallArgumentsNumber opCode (methodBase : MethodBase) =
            let thisAddition = if methodBase.IsStatic || opCode = OpCodes.Newobj then 0 else 1
            thisAddition + methodBase.GetParameters().Length
        let hasResultOnOperationalStackAfterCall : MethodBase * OpCode -> bool = function
            | (:? ConstructorInfo, NewObj)     -> true
            | (:? ConstructorInfo, _)          -> false
            | (:? MethodInfo as methodInfo, _) -> methodInfo.ReturnType <> typedefof<System.Void>
            | _ -> __unreachable__()

        let popCount = function
            | StackBehaviour.Pop0 -> 0
            | StackBehaviour.Popi
            | StackBehaviour.Popref
            | StackBehaviour.Pop1 -> -1
            | StackBehaviour.Popi_pop1
            | StackBehaviour.Popi_popi
            | StackBehaviour.Popi_popi8
            | StackBehaviour.Popi_popr4
            | StackBehaviour.Popi_popr8
            | StackBehaviour.Popref_pop1
            | StackBehaviour.Popref_popi
            | StackBehaviour.Pop1_pop1 -> -2
            | StackBehaviour.Popref_popi_popi
            | StackBehaviour.Popref_popi_popi8
            | StackBehaviour.Popref_popi_popr4
            | StackBehaviour.Popref_popi_popr8
            | StackBehaviour.Popref_popi_popref
            | StackBehaviour.Popref_popi_pop1
            | StackBehaviour.Popi_popi_popi -> -3
            | StackBehaviour.Varpop -> -1 * countMethodCallArgumentsNumber opCode (Option.get calledMethod)
            | _ -> __unreachable__()
        let pushCount = function
            | StackBehaviour.Push0 -> 0
            | StackBehaviour.Pushi
            | StackBehaviour.Pushi8
            | StackBehaviour.Pushr4
            | StackBehaviour.Pushr8
            | StackBehaviour.Pushref
            | StackBehaviour.Push1 -> 1
            | StackBehaviour.Push1_push1 -> 2
            | StackBehaviour.Varpush when hasResultOnOperationalStackAfterCall (Option.get calledMethod, opCode) -> 1
            | StackBehaviour.Varpush -> 0
            | _ -> __unreachable__()
        if opCode = OpCodes.Leave || opCode = OpCodes.Leave_S then 0
        else oldBalance + popCount (opCode.StackBehaviourPop) + pushCount (opCode.StackBehaviourPush)
