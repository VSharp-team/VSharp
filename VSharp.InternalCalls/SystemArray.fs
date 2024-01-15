namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    let GetRank (state : state) args =
        assert(List.length args = 1)
        List.head args |> Memory.ArrayRank state

    let get_Rank state args =
        GetRank state args

    let commonGetLength state arrayRef =
        let getLengthFromRank arrayRef =
            let rank = Terms.MostConcreteTypeOfRef state arrayRef |> Types.RankOf
            assert(rank >= 1)
            let lengths = List.init rank (MakeNumber >> Memory.ArrayLengthByDimension state arrayRef)
            match lengths with
            | [l] -> l
            | l::ls -> List.fold Arithmetics.Mul l ls
            | _ -> __unreachable__()
        GuardedApplyExpression arrayRef getLengthFromRank

    let get_Length state args =
        assert(List.length args = 1)
        commonGetLength state args[0]

    let get_NativeLength state args =
        assert(List.length args = 1)
        let length = commonGetLength state args[0]
        Types.Cast length typeof<UIntPtr>

    let ContainsChar (state : state) args =
        assert(List.length args = 3)
        let this, char = args[0], args[2]
        match this.term with
        | HeapRef({term = ConcreteHeapAddress _}, _) ->
            let checkOneElement acc i =
                let index = Concrete i Types.IndexType
                let elem = Memory.ReadArrayIndex state this [index] None
                acc ||| (elem === char)
            let length = Memory.ArrayLengthByDimension state this (MakeNumber 0)
            match length.term with
            | Concrete(obj, _) ->
                let length = obj :?> int
                let indices = List.init length id
                List.fold checkOneElement (False()) indices
            | _ -> __unreachable__()
        | _ -> __insufficientInformation__ "Contains works only for concrete address arrays"

    let GetCount (state : state) (args : term list) =
        assert(List.length args = 2)
        let this = List.head args
        get_Length state [this]

    let GetItem (state : state) (args : term list) =
        assert(List.length args = 3)
        let this, index = args[0], args[2]
        Memory.ReadArrayIndex state this [index] None

    let GetArrayLength (interpreter : IInterpreter) cilState args =
        assert(List.length args = 2)
        let array = args[0]
        let dim = args[1]
        let arrayLengthByDimension arrayRef index (cilState : cilState) k =
            cilState.Push (Memory.ArrayLengthByDimension cilState.state arrayRef index)
            List.singleton cilState |> k
        interpreter.AccessArrayDimension arrayLengthByDimension cilState array dim

    let GetArrayLowerBound (interpreter : IInterpreter) cilState args =
        assert(List.length args = 2)
        let array = args[0]
        let dim = args[1]
        let arrayLowerBoundByDimension arrayRef index (cilState : cilState) k =
            cilState.Push (Memory.ArrayLowerBoundByDimension cilState.state arrayRef index)
            List.singleton cilState |> k
        interpreter.AccessArrayDimension arrayLowerBoundByDimension cilState array dim

    let CommonInitializeArray (interpreter : IInterpreter) cilState args =
        assert(List.length args = 2)
        let array = args[0]
        let handleTerm = args[1]
        let initArray (cilState : cilState) k =
            Memory.InitializeArray cilState.state array handleTerm
            List.singleton cilState |> k
        interpreter.NpeOrInvoke cilState array (fun cilState k ->
        interpreter.NpeOrInvoke cilState handleTerm initArray k) id

    let ClearWithIndexLength (interpreter : IInterpreter) (cilState : cilState) args =
        assert(List.length args = 3)
        let array, index, length = args[0], args[1], args[2]
        let (>>) = API.Arithmetics.(>>)
        let (<<) = API.Arithmetics.(<<)
        let clearCase (cilState : cilState) k =
            Memory.ClearArray cilState.state array index length
            List.singleton cilState |> k
        let nonNullCase (cilState : cilState) k =
            let zero = MakeNumber 0
            let lb = Memory.ArrayLowerBoundByDimension cilState.state array zero
            let numOfAllElements = Memory.CountOfArrayElements cilState.state array
            let check =
                (index << lb)
                ||| ((Arithmetics.Add index length) >> numOfAllElements)
                ||| (length << zero)
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (check, state))
                (interpreter.Raise interpreter.IndexOutOfRangeException)
                clearCase
                k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference array, state))
            (interpreter.Raise interpreter.ArgumentNullException)
            nonNullCase
            id

    let ClearWhole (interpreter : IInterpreter) (cilState : cilState) args =
        assert(List.length args = 1)
        let array = args[0]
        let clearCase (cilState : cilState) k =
            let zero = MakeNumber 0
            let index = Memory.ArrayLowerBoundByDimension cilState.state array zero
            let numOfAllElements = Memory.CountOfArrayElements cilState.state array
            Memory.ClearArray cilState.state array index numOfAllElements
            List.singleton cilState |> k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference array, state))
            (interpreter.Raise interpreter.ArgumentNullException)
            clearCase
            id

    let CommonCopyArray (i : IInterpreter) (cilState : cilState) src srcIndex dst dstIndex length =
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
            List.singleton cilState |> k
        let lengthCheck (cilState : cilState) =
            let endSrcIndex = add srcIndex length
            let srcNumOfAllElements = srcNumOfAllElements
            let endDstIndex = add dstIndex length
            let dstNumOfAllElements = dstNumOfAllElements
            let check =
                (endSrcIndex >> srcNumOfAllElements) ||| (endSrcIndex << srcLB)
                ||| (endDstIndex >> dstNumOfAllElements) ||| (endDstIndex << dstLB)
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (check, state))
                (i.Raise i.ArgumentException)
                defaultCase
        let indicesCheck (cilState : cilState) =
            // TODO: extended form needs
            let primitiveLengthCheck = (length << zero) ||| (if TypeUtils.isLong length then length >> TypeUtils.Int32.MaxValue() else False())
            let srcIndexCheck = (srcIndex << srcLB) ||| (if TypeUtils.isLong srcIndex then srcIndex >>= srcNumOfAllElements else False())
            let dstIndexCheck = (dstIndex << dstLB) ||| (if TypeUtils.isLong dstIndex then dstIndex >>= dstNumOfAllElements else False())

            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (primitiveLengthCheck ||| srcIndexCheck ||| dstIndexCheck, state))
                (i.Raise i.ArgumentOutOfRangeException)
                lengthCheck
        let assignableCheck (cilState : cilState) =
            let srcElemType = Types.ElementType srcType
            let dstElemType = Types.ElementType dstType
            let condition =
                if Types.IsValueType srcElemType then True()
                else Types.TypeIsType srcElemType dstElemType
            cilState.StatedConditionalExecutionCIL
                (fun state k -> k (condition, state))
                indicesCheck
                (i.Raise i.InvalidCastException)
        let rankCheck (cilState : cilState) =
            if Types.RankOf srcType = Types.RankOf dstType then assignableCheck cilState
            else i.Raise i.RankException cilState
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference src ||| IsNullReference dst, state))
            (i.Raise i.ArgumentNullException)
            rankCheck
            id

    let CopyArrayExtendedForm1 (interpreter : IInterpreter) cilState args =
        assert(List.length args = 6)
        let src, srcIndex, dst, dstIndex, length = args[0], args[1], args[2], args[3], args[4]
        CommonCopyArray interpreter cilState src srcIndex dst dstIndex length

    let CopyArrayExtendedForm2 (interpreter : IInterpreter) cilState args =
        assert(List.length args = 5)
        let src, srcIndex, dst, dstIndex, length = args[0], args[1], args[2], args[3], args[4]
        CommonCopyArray interpreter cilState src srcIndex dst dstIndex length

    let CopyArrayShortForm (interpreter : IInterpreter) (cilState : cilState) args =
        assert(List.length args = 3)
        let src, dst, length = args[0], args[1], args[2]
        let state = cilState.state
        let zero = TypeUtils.Int32.Zero()
        let srcLB = Memory.ArrayLowerBoundByDimension state src zero
        let dstLB = Memory.ArrayLowerBoundByDimension state dst zero
        CommonCopyArray interpreter cilState src srcLB dst dstLB length

    let FillArray (interpreter : IInterpreter) (cilState : cilState) args =
        assert(List.length args = 3)
        let array, value = args[1], args[2]
        let fill (cilState : cilState) k =
            Memory.FillArray cilState.state array value
            List.singleton cilState |> k
        cilState.StatedConditionalExecutionCIL
            (fun state k -> k (IsNullReference array, state))
            (interpreter.Raise interpreter.ArgumentNullException)
            fill
            id

    let AllocateUninitializedArray (state : state) args =
        assert(List.length args = 3)
        let typ, length = args[0], args[1]
        let elemType = Helpers.unwrapType typ
        Memory.AllocateVectorArray state length elemType
