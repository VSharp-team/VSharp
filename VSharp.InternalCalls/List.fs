namespace VSharp.System

open System
open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState
open VSharp.TypeUtils
open Arithmetics

module internal List =
    let inRange index count =
        let left = (MakeNumber 0) <<= index
        let right = index << count
        left &&& right

    let GetCount (state : state) (args : term list) =
        assert(args.Length = 2)
        let this = args[0]
        Memory.GetListCount state this

    let AddItem (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 3)
        let this, item = args[0], args[2]
        let state = cilState.state
        let count = Memory.GetListCount state this
        Memory.WriteListKey state this count item
        let newCount = Memory.GetListCount state this
        let assume = (MakeNumber 0) << newCount
        AssumeStatedConditionalExecution state assume
        [cilState]

    let SetItem (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 4)
        let this, index, item = args[0], args[2], args[3]
        let count = Memory.GetListCount cilState.state this
        let notEmpty = count !== (MakeNumber 0)
        let set (cilState : cilState) k =
            let assume = (MakeNumber 0) << count
            AssumeStatedConditionalExecution cilState.state assume
            Memory.WriteListKey cilState.state this index item
            k [cilState]
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k ((&&&) notEmpty <| inRange index count, state))
            set
            (interpreter.Raise interpreter.ArgumentOutOfRangeException)
            id

    let ReadByIndex (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 3)
        let this, index = args[0], args[2]
        let count = Memory.GetListCount cilState.state this
        let read (cilState : cilState) k =
            let assume = (MakeNumber 0) << count
            AssumeStatedConditionalExecution cilState.state assume
            cilState.Push <| Memory.ReadListIndex cilState.state this index
            k [cilState]
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (inRange index count, state))
            read
            (interpreter.Raise interpreter.ArgumentOutOfRangeException)
            id

    let ReadFirst (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 2)
        let this = args[0]
        let index = MakeNumber 0
        let count = Memory.GetListCount cilState.state this
        let notEmpty = count !== (MakeNumber 0)
        let read (cilState : cilState) k =
            let assume = (MakeNumber 0) << count
            AssumeStatedConditionalExecution cilState.state assume
            cilState.Push <| Memory.ReadListIndex cilState.state this index
            k [cilState]
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (notEmpty, state))
            read
            (interpreter.Raise interpreter.InvalidOperationException)
            id

    let ReadLast (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 2)
        let this = args[0]
        let count = Memory.GetListCount cilState.state this
        let index = Sub count <| MakeNumber 1
        let notEmpty = count !== (MakeNumber 0)
        let read (cilState : cilState) k =
            let assume = (MakeNumber 0) << count
            AssumeStatedConditionalExecution cilState.state assume
            cilState.Push <| Memory.ReadListIndex cilState.state this index
            k [cilState]
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (notEmpty, state))
            read
            (interpreter.Raise interpreter.InvalidOperationException)
            id

    let RemoveAt (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 3)
        let this, index = args[0], args[2]
        let count = Memory.GetListCount cilState.state this
        let remove (cilState : cilState) k =
            let assume = (MakeNumber 0) << count
            AssumeStatedConditionalExecution cilState.state assume
            Memory.RemoveListAtIndex cilState.state this index
            k [cilState]
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (inRange index count, state))
            remove
            (interpreter.Raise interpreter.ArgumentOutOfRangeException)
            id

    let Insert (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 4)
        let this, index, item = args[0], args[2], args[3]
        let count = Memory.GetListCount cilState.state this
        let condition = (inRange index count) ||| (index === count)
        let insert (cilState : cilState) k =
            Memory.InsertListIndex cilState.state this index item
            let count = Memory.GetListCount cilState.state this
            let assume = (MakeNumber 0) << count
            AssumeStatedConditionalExecution cilState.state assume
            k [cilState]
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (condition, state))
            insert
            (interpreter.Raise interpreter.ArgumentOutOfRangeException)
            id


    let CopyToRange (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 6)
        let this, index, array, arrayIndex, count = args[0], args[2], args[3], args[4], args[5]
        let listCount = Memory.GetListCount cilState.state this
        let arrayCount = SystemArray.get_Length cilState.state [array]
        let copy (cilState : cilState) k =
            Memory.ListCopyToRange cilState.state this index array arrayIndex count
            k [cilState]
        let inRangeBranch (cilState : cilState) k =
            let condition = (Sub listCount index) >> (Sub arrayCount arrayIndex)
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (condition, state))
                (interpreter.Raise interpreter.ArgumentException)
                copy
                k
        let notNullBranch (cilState : cilState) k =
            let condition =
                let isNegative term = (MakeNumber 0) >> term
                (isNegative index) ||| (isNegative arrayIndex) ||| (isNegative count)
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (condition, state))
                (interpreter.Raise interpreter.ArgumentOutOfRangeException)
                inRangeBranch
                k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference array, state))
            (interpreter.Raise interpreter.NullReferenceException)
            notNullBranch
            id

    let CopyToFull (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 4)
        let this, array, arrayIndex = args[0], args[2], args[3]
        let index = MakeNumber 0
        let count = Memory.GetListCount cilState.state this
        CopyToRange interpreter cilState [this; args[1]; index; array; arrayIndex; count]

    let CopyToSimple (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(args.Length = 3)
        let this, array = args[0], args[2]
        let index = MakeNumber 0
        let count = Memory.GetListCount cilState.state this
        CopyToRange interpreter cilState [this; args[1]; index; array; index; count]
