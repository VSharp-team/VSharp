namespace VSharp.Interpreter.IL

open System
open System.Text
open VSharp
open System.Reflection
open System.Reflection.Emit

exception IncorrectCIL of string

type offset = int
type term = VSharp.Core.term
type state = VSharp.Core.state

type operationalStack = term list

type ip = VSharp.Core.ip
type level = VSharp.Core.level

type ipTransition =
    | FallThrough of offset
    | Return
    | UnconditionalBranch of offset
    | ConditionalBranch of offset list
    | ExceptionMechanism

type cilState =
    { ip : ip
      state : state
      filterResult : term option
      iie : InsufficientInformationException option
      level : level
    }
    member x.CanBeExpanded () = x.ip.CanBeExpanded()
    member x.HasException = Option.isSome x.state.exceptionsRegister.ExceptionTerm
    static member Make curV state =
        { ip = curV
          state = state
          filterResult = None
          iie = None
          level = PersistentDict.empty
        }

module internal CilStateOperations =
    let compose (cilState1 : cilState) (cilState2 : cilState) k =
        let level =
            PersistentDict.fold (fun (acc : level) k v ->
                let oldValue = if PersistentDict.contains k acc then PersistentDict.find acc k else 0u
                PersistentDict.add k (v + oldValue) acc
            ) cilState1.level cilState2.level

        let states = VSharp.Core.API.Memory.ComposeStates cilState1.state cilState2.state id
        k <| List.map (fun state -> {cilState2 with state = state; level = level}) states

    let incrementLevel (cilState : cilState) k =
        let lvl = cilState.level
        let newValue = if PersistentDict.contains k lvl then PersistentDict.find lvl k + 1u else 1u
        {cilState with level = PersistentDict.add k newValue lvl}

    let stateOf (cilState : cilState) = cilState.state

    let private dumpSection section (sb : StringBuilder) =
        sprintf "--------------- %s: ---------------" section |> sb.AppendLine

    let private dumpSectionValue section value (sb : StringBuilder) =
        sb |> dumpSection section |> (fun sb -> sb.AppendLine value)

    let private dumpDict section sort keyToString valueToString (sb : StringBuilder) d =
        if PersistentDict.isEmpty d then sb
        else
            let sb = dumpSection section sb
            PersistentDict.dump d sort keyToString valueToString |> sb.AppendLine

    let ipAndMethodBase2String (ip : ip, m : MethodBase) =
        sprintf "Method: %O, ip = %O" m ip

    // TODO: print filterResult and IIE ?
    let dump (cilState : cilState) : string =
        let sb = dumpSectionValue "IP" (sprintf "%O" cilState.ip) (StringBuilder())
        let sb = dumpDict "Level" id ipAndMethodBase2String id sb cilState.level

        let stateDump = VSharp.Core.API.Memory.Dump cilState.state
        let sb = dumpSectionValue "State" stateDump sb

        if sb.Length = 0 then "<EmptyCilState>" else sb.ToString()

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
