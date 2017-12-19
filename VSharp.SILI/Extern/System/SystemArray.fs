namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Array -------------------------------

module internal SystemArray =

    let private indexOutOfRangeException state = CreateInstance typeof<IndexOutOfRangeException> [] state

    let checkBounds state lower upper x fits k =
        simplifyGreaterOrEqual x lower (fun notTooSmall ->
        simplifyLess x upper (fun notTooLarge ->
        simplifyAnd notTooSmall notTooLarge (fun inBounds ->
        Interpreter.reduceConditionalStatements state
            (fun state k -> k (inBounds, state))
            fits
            (fun state k ->
                let term, state = indexOutOfRangeException state
                k (Throw term, state))
            k)))

    let GetLength state args =
        let this, dimension = List.item 0 args, List.item 1 args
        let array, state = Memory.deref state this
        let getLength state dimension term =
            match term.term with
            | Error _ -> (term, state)
            | Array(d, _, _, _, _, _, _) ->
                let lowerBound = Concrete 0 Arrays.lengthTermType
                checkBounds state lowerBound d dimension
                    (fun state k ->
                        let lengthRef = Memory.referenceArrayLength this dimension
                        let result, state = Memory.deref state lengthRef
                        k (Return result, state))
                    (fun (term, state) -> ControlFlow.resultToTerm term, state)
            | term -> internalfailf "expected array, but %O got!" term
        let result, state =
            match array.term with
            | Union gvs -> Merging.guardedStateMap (fun state term -> getLength state dimension term) gvs state
            | _ -> getLength state dimension array
        in (ControlFlow.throwOrReturn result, state)

    let GetRank state args =
        let array, state = Memory.deref state (List.head args)
        let rec getRank term =
            match term.term with
            | Error _ -> term
            | Array(d, _, _, _, _, _, _) -> d
            | Union gvs -> Merging.guardedMap getRank gvs
            | term -> internalfailf "expected array, but %O got!" term
        in (Return (getRank array), state)

    let get_Rank state args =
        GetRank state args

    let get_Length state args =
        let array, state = Memory.deref state (List.head args)
        (Return (VSharp.Arrays.length array), state)

    let GetLowerBound state args =
        let this, dimension = List.item 0 args, List.item 1 args
        let array, state = Memory.deref state this
        let getLowerBound state dimension term =
            match term.term with
            | Error _ -> (term, state)
            | Array(d, _, _, _, _, _, _) ->
                let lowerBound = Concrete 0 Arrays.lengthTermType
                checkBounds state lowerBound d dimension
                    (fun state k ->
                        let boundRef = Memory.referenceArrayLowerBound this dimension
                        let result, state = Memory.deref state boundRef
                        k (Return result, state))
                    (fun (term, state) -> ControlFlow.resultToTerm term, state)
            | term -> internalfailf "expected array, but %O got!" term
        let result, state =
            match array.term with
            | Union gvs -> Merging.guardedStateMap (fun state term -> getLowerBound state dimension term) gvs state
            | _ -> getLowerBound state dimension array
        in (ControlFlow.throwOrReturn result, state)
