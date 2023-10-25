namespace VSharp.System

open VSharp.Core.API
open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

module internal Object =

    let internal MemberwiseClone (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        let object = args[0]
        let state = cilState.state
        let t = MostConcreteTypeOfRef state object
        if TypeUtils.isArrayType t then
            if not t.IsSZArray then
                internalfail $"MemberwiseClone: non-vector arrays are not supported {t}"
            let newObject = Memory.AllocateDefaultClass state t
            let zero = MakeNumber 0
            let len = Memory.ArrayLengthByDimension state object zero
            Memory.CopyArray state object zero t newObject zero t len
            push newObject cilState
            List.singleton cilState
        elif t.IsValueType then
            let v = read cilState object
            let ref = Memory.BoxValueType cilState.state v
            push ref cilState
            List.singleton cilState
        else
            let t =
                if t.IsAbstract then
                    let address =
                        match object.term with
                        | HeapRef(address, _) -> address
                        | _ -> internalfail $"MemberwiseClone: unexpected object ref {object}"
                    match state.typeStorage[address] with
                    | Some candidates -> Seq.head (Seq.head candidates.ConcreteTypes).Types
                    | _ -> t
                else t
            let newObject = Memory.AllocateDefaultClass state t
            let fields = Reflection.fieldsOf false t
            let copyField cilStates (field, _) =
                let copyForState cilState =
                    let v = readField cilState object field
                    writeClassField cilState newObject field v
                List.collect copyForState cilStates
            let cilStates = Array.fold copyField (List.singleton cilState) fields
            for cilState in cilStates do
                push newObject cilState
            cilStates
