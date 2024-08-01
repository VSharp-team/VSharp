namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState
open VSharp.TypeUtils
open Arithmetics

module internal Dictionary =
    let GetCount (state : state) (args : term list) =
        assert(List.length args = 3)
        let this = args[0]
        Memory.GetDictionaryCount state this

    let IsContainsKey (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
         assert(List.length args = 4)
         let this, key = args[0], args[3]
         let keyType = TypeOf key
         let contains (cilState : cilState) k =
            cilState.Push <| Memory.ContainsKey cilState.state this key
            k [cilState]
         match keyType with
         | ReferenceType ->
             cilState.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference key, state))
                (interpreter.Raise interpreter.ArgumentNullException)
                contains
                id
         | _ -> contains cilState id

    let GetItem (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 4)
        let dictionary, key = args[0], args[3]
        let keyType = TypeOf key
        let get (cilState : cilState) k =
            let state = cilState.state
            let count = Memory.GetDictionaryCount state dictionary
            let assume = count >> MakeNumber 0
            AssumeStatedConditionalExecution state assume
            let value = Memory.ReadDictionaryKey state dictionary key
            cilState.Push value
            k [cilState]
        let elseBranch (cilState : cilState) k =
            k <| cilState.StatedConditionalExecutionCIL
                (fun state k -> k (Memory.ContainsKey cilState.state dictionary key, state))
                get
                (interpreter.Raise interpreter.ArgumentException)
                id
        match keyType with
        | ReferenceType ->
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference key, state))
                (interpreter.Raise interpreter.ArgumentNullException)
                elseBranch
                id
        | _ -> elseBranch cilState id

    let SetItem (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 5)
        let dictionary, key, value = args[0], args[3], args[4]
        let keyType = TypeOf key
        let set (cilState : cilState) k =
            let state = cilState.state
            Memory.WriteDictionaryKey cilState.state dictionary key value
            let count = Memory.GetDictionaryCount state dictionary
            let assume = count >> MakeNumber 0
            AssumeStatedConditionalExecution state assume
            k [cilState]
        match keyType with
        | ReferenceType ->
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference key, state))
                (interpreter.Raise interpreter.ArgumentNullException)
                set
                id
        | _ -> set cilState id

    let AddElement (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 5)
        let dictionary, key, value = args[0], args[3], args[4]
        let keyType = TypeOf key
        let add (cilState : cilState) k =
            let state = cilState.state
            Memory.WriteDictionaryKey state dictionary key value
            let count = Memory.GetDictionaryCount state dictionary
            let assume = count >> MakeNumber 0
            AssumeStatedConditionalExecution state assume
            k [cilState]
        let elseBranch (cilState : cilState) k =
            k <| cilState.StatedConditionalExecutionCIL
                (fun state k -> k (Memory.ContainsKey cilState.state dictionary key, state))
                (interpreter.Raise interpreter.ArgumentException)
                add
                id
        match keyType with
        | ReferenceType ->
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (IsNullReference key, state))
                (interpreter.Raise interpreter.ArgumentNullException)
                elseBranch
                id
        | _ -> elseBranch cilState id
