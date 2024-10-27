namespace VSharp.System

open FSharpx.Collections
open global.System

open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState
open VSharp.Core.API.Arithmetics

// ------------------------------- System.String -------------------------------

module internal String =

    let CtorOfCharArray state args =
        assert(List.length args = 2)
        let this, arrayRef = List.item 0 args, List.item 1 args
        let states = Memory.StringCtorOfCharArray state arrayRef this
        List.map (fun state -> (Nop(), state)) states

    let CtorFromReplicatedChar state args =
        assert(List.length args = 3)
        let this, char, length = args[0], args[1], args[2]
        Memory.StringFromReplicatedChar state this char length
        Nop()

    let CtorFromSpan (_ : IInterpreter) (cilState : cilState) (args : term list) : cilState list =
        assert(List.length args = 2)
        let this, span = args[0], args[1]
        let ref = ReadOnlySpan.GetContentsRef cilState span
        let len = ReadOnlySpan.CommonGetLength cilState span
        let state = cilState.state
        let t = MostConcreteTypeOfRef state ref
        assert(TypeUtils.isArrayType t || t = typeof<string> || t = typeof<char>)
        let states = Memory.StringCtorOfCharArrayAndLen state ref this len
        List.map cilState.ChangeState states

    let CtorFromPtr (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 4)
        let this = args[0]
        let ptr = args[1]
        let startIndex = args[2]
        let length = args[3]
        let zero = MakeNumber 0
        let rangeCheck() =
            (length >>= zero)
            &&& (startIndex >>= zero)
        let copy (cilState : cilState) k =
            cilState.WriteClassField this Reflection.stringLengthField length
            let bytesCount = Mul length (MakeNumber sizeof<char>)
            Buffer.CommonMemmove cilState this None ptr (Some startIndex) bytesCount |> k
        let checkPtr (cilState : cilState) k =
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (!!(IsBadRef ptr), state))
                copy
                (i.Raise i.ArgumentOutOfRangeException)
                k
        let emptyStringCase (cilState : cilState) k =
            cilState.WriteClassField this Reflection.stringLengthField zero
            List.singleton cilState |> k
        let checkLength (cilState : cilState) k =
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (length === zero, state))
                emptyStringCase
                checkPtr
                k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (rangeCheck(), state))
            checkLength
            (i.Raise i.ArgumentOutOfRangeException)
            id

    let GetLength (state : state) (args : term list) =
        assert(List.length args = 1)
        Memory.StringLength state (List.head args)

    let ToUpperInvariant (state : state) (args : term list) =
        assert(List.length args = 1)
        let this = List.head args
        match this.term with
        | HeapRef({term = ConcreteHeapAddress _}, _) ->
            let readOneChar i =
                let index = Concrete i Types.IndexType
                match Memory.ReadStringChar state this index with
                | {term = Concrete(obj, _)} -> obj :?> char
                | _ -> __insufficientInformation__ "ToUpperInvariant works only for fully concrete strings right now"
            let length = Memory.StringLength state this
            match length.term with
            | Concrete(obj, _) ->
                let len = obj :?> int
                let indices = List.init len id
                let string = List.map readOneChar indices |> String.Concat
                Memory.AllocateString (string.ToUpperInvariant()) state
            | _ -> __insufficientInformation__ "ToUpperInvariant works only for concrete length strings right now"
        | _ -> __insufficientInformation__ "ToUpperInvariant works only for concrete address strings right now"

    let CreateFromChar (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let char = List.head args
        Memory.CreateStringFromChar state char

    let CharToUpper (_ : state) (args : term list) =
        assert(List.length args = 1)
        let char = List.head args
        match char.term with
        | Concrete(symbol, typ) when typ = typeof<char> ->
            let char = symbol :?> char
            Char.ToUpper(char) |> MakeNumber
        | _ -> __insufficientInformation__ "Char.ToUpper works only for concrete chars right now"

    let Equals (state : state) (args : term list) =
        assert(List.length args = 2)
        let str1, str2 = args[0], args[1]
        let length1 = Memory.StringLength state str1
        let length2 = Memory.StringLength state str2
        match length1.term, length2.term with
        | Concrete(obj1, _), Concrete(obj2, _) ->
            let checkOneChar acc i =
                let index = MakeNumber i
                let char1 = Memory.ReadStringChar state str1 index
                let char2 = Memory.ReadStringChar state str2 index
                acc &&& (char1 === char2)
            let len1 = obj1 :?> int
            let len2 = obj2 :?> int
            if len1 = len2 then
                let indices = List.init len1 id
                List.fold checkOneChar (True()) indices
            else MakeBool false
        | _ -> __insufficientInformation__ "String.Equals works only for concrete length strings right now"

    let FastAllocateString (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let length = List.head args
        Memory.AllocateEmptyString state length

    let FillStringChecked (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 3)
        let state = cilState.state
        let dest, destPos, src = args[0], args[1], args[2]
        let srcPos = MakeNumber 0
        let srcLength = Memory.StringLength state src
        let destLength = Memory.StringLength state dest
        let (<<=) = Arithmetics.(<<=)
        let check = srcLength <<= (Sub destLength destPos)
        let copy (cilState : cilState) k =
            Memory.CopyStringArray cilState.state src srcPos dest destPos srcLength
            k [cilState]
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (check, state))
            copy
            (interpreter.Raise interpreter.IndexOutOfRangeException)
            id

    let GetChars (interpreter : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let this = args[0]
        let index = args[1]
        let length = Memory.StringLength cilState.state this
        let getChar (cilState : cilState) k =
            let char = Memory.ReadStringChar cilState.state this index
            cilState.Push char
            List.singleton cilState |> k
        interpreter.AccessArray getChar cilState length index id

    let AllCharsInUInt32AreAscii (_ : state) (_ : term list) =
        // TODO: try to not internal call it
        MakeBool true
