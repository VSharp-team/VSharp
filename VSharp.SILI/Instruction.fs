namespace VSharp.Interpreter.IL

open VSharp
open System.Reflection
open System.Reflection.Emit

exception IncorrectCIL of string

type term = VSharp.Core.term
type state = VSharp.Core.state

type operationalStack = term list

type destination =
    | Return
    | Intermediate of int
    member x.Vertex () =
        match x with
        | Intermediate i -> i
        | _              -> internalfail "Could not get vertex from destination"
    member x.HasVertex () =
        match x with
        | Intermediate _ -> true
        | _              -> false
type cilState =
    { currentVertex : destination
      targetVertex : destination
      recursiveVertices : int list
      opStack : operationalStack
      functionResult : term option
      exceptionFlag : term option
      state : state
      this : term option
    }
    interface VSharp.Core.IInterpreterState<cilState> with
        member x.InternalState = x.state
        member x.SetState st = {x with state = st}
        member x.SetResultTerm resTerm = {x with functionResult = resTerm}
        member x.ResultTerm = x.functionResult
    member x.CanBeExpanded () = x.currentVertex.HasVertex()
    member x.IsFinished = x.currentVertex = x.targetVertex
    member x.HasException = Option.isSome x.exceptionFlag

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

   let private isSingleByteOpcodeValue = (<) 0
   let private isSingleByteOpcode = (<>) 0xFEuy

   let private equalSizeOpcodesCount = 0x100
   let private singleByteOpcodes = Array.create equalSizeOpcodesCount OpCodes.Nop;
   let private twoBytesOpcodes = Array.create equalSizeOpcodesCount OpCodes.Nop;

   let private fillOpcodes =
       let resolve (field : FieldInfo) =
            match field.GetValue() with
            | :? OpCode as opcode -> let value = int opcode.Value
                                     if isSingleByteOpcodeValue value then singleByteOpcodes.[value] <- opcode
                                     else twoBytesOpcodes.[value &&& 0xFF] <- opcode
            | _ -> ()

       typeof<OpCodes>.GetRuntimeFields() |> Seq.iter resolve

   let private operandType2operandSize = [| 4; 4; 4; 8; 4; 0; -1; 8; 4; 4; 4; 4; 4; 4; 2; 1; 1; 4; 1|]

   let private jumpTargetsForNext (opcode : OpCode) _ pos =
       let nextInstruction = pos + opcode.Size + operandType2operandSize.[int opcode.OperandType]
       Choice1Of2 nextInstruction

   let private jumpTargetsForBranch (opcode : OpCode) ilBytes pos =
       let offset =
           match opcode.OperandType with
           | OperandType.InlineBrTarget -> NumberCreator.extractInt32 ilBytes (pos + opcode.Size)
           | _ -> NumberCreator.extractInt8 ilBytes (pos + opcode.Size)

       let nextInstruction = pos + opcode.Size + operandType2operandSize.[int opcode.OperandType]
       if offset = 0 && opcode <> OpCodes.Leave && opcode <> OpCodes.Leave_S
       then Choice1Of2 nextInstruction
       else Choice2Of2 [offset + nextInstruction]

   let private inlineBrTarget extract (opcode : OpCode) ilBytes pos =
       let offset = extract ilBytes (pos + opcode.Size)
       let nextInstruction = pos + opcode.Size + operandType2operandSize.[int opcode.OperandType]
       Choice2Of2 [nextInstruction; nextInstruction + offset]

   let private inlineSwitch (opcode : OpCode) ilBytes pos =
       let n = NumberCreator.extractUnsignedInt32 ilBytes (pos + opcode.Size) |> int
       let nextInstruction = pos + opcode.Size + 4 * n + 4
       let nextOffsets =
           List.init n (fun x -> nextInstruction + NumberCreator.extractInt32 ilBytes (pos + opcode.Size + 4 * (x + 1)))
       Choice2Of2 <| nextInstruction :: nextOffsets

   let private jumpTargetsForReturn _ _ _ = Choice2Of2 []

   let findNextInstructionOffsetAndEdges (opcode : OpCode) =
       match opcode.FlowControl with
       | FlowControl.Next
       | FlowControl.Call
       | FlowControl.Break
       | FlowControl.Meta -> jumpTargetsForNext
       | FlowControl.Branch -> jumpTargetsForBranch
       | FlowControl.Cond_Branch ->
           match opcode.OperandType with
           | OperandType.InlineBrTarget -> inlineBrTarget NumberCreator.extractInt32
           | OperandType.ShortInlineBrTarget -> inlineBrTarget NumberCreator.extractInt8
           | OperandType.InlineSwitch -> inlineSwitch
           | _ -> __notImplemented__()
       | FlowControl.Return
       | FlowControl.Throw -> jumpTargetsForReturn
       | _ -> __notImplemented__()
       <| opcode

   let parseInstruction (ilBytes : byte []) pos =
       let b1 = ilBytes.[pos]
       if isSingleByteOpcode b1 then singleByteOpcodes.[int b1]
       elif pos + 1 >= ilBytes.Length then raise (IncorrectCIL("Prefix instruction FE without suffix!"))
       else twoBytesOpcodes.[int ilBytes.[pos + 1]]
