namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Array -------------------------------

module SystemArray =

    let private indexOutOfRangeException state = CreateInstance typeof<IndexOutOfRangeException> [] state

    let checkBounds state lower upper x fits k =
        simplifyGreaterOrEqual x lower (fun notTooSmall ->
        simplifyLess x upper (fun notTooLarge ->
        simplifyAnd notTooSmall notTooLarge (fun inBounds ->
        Interpreter.reduceConditionalStatements state
            (fun state k -> k (inBounds, state))
            fits
            (fun state k ->
                let term, state = indexOutOfRangeException state in
                k (Throw term, state))
            k)))

    let GetLength state args =
        let this, dimension = List.item 0 args, List.item 1 args in
        let array, state = Memory.deref state this in
        let rec getLength state dimension term =
            match term.term with
            | Error _ -> (term, state)
            | Array(_, _, _, lengths, _) ->
                match dimension.term with
                | Error _ -> (dimension, state)
                | Concrete(obj, _) ->
                    let d = obj :?> int in
                    if d < 0 || d >= lengths.Length then
                        let term, state = indexOutOfRangeException state in
                        (Error term, state)
                    else (lengths.[d], state)
                | _ ->
                    let lowerBound = Concrete 0 VSharp.Array.lengthTermType in
                    let upperBound = Concrete lengths.Length VSharp.Array.lengthTermType in
                    checkBounds state lowerBound upperBound dimension
                        (fun state k ->
                            let guards =
                                List.init lengths.Length
                                          (fun i -> simplifyEqual dimension (Concrete i VSharp.Array.lengthTermType) id)
                            in
                            let result = List.zip guards (List.ofArray lengths) |> Merging.merge in
                            k (Return result, state))
                        (fun (term, state) -> ControlFlow.resultToTerm term, state)
            | term -> internalfailf "expected array, but %O got!" term
        in
        let result, state =
            match array.term with
            | Union gvs -> Merging.guardedStateMap (fun state term -> getLength state dimension term) gvs state
            | _ -> getLength state dimension array
        in (ControlFlow.throwOrReturn result, state)

    let GetRank state args =
        let array, state = Memory.deref state (List.head args) in
        let rec getRank term =
            match term.term with
            | Error _ -> term
            | Array(_, _, _, _, ArrayType(_, rank)) -> Concrete rank (Numeric typedefof<int>)
            | Union gvs -> Merging.guardedMap getRank gvs
            | term -> internalfailf "expected array, but %O got!" term
        in (Return (getRank array), state)

    let get_Rank state args =
        GetRank state args

    let get_Length state args =
        let array, state = Memory.deref state (List.head args) in
        (Return (VSharp.Array.length array), state)

    let GetLowerBound state args =
        let this, dimension = List.item 0 args, List.item 1 args in
        let array, state = Memory.deref state this in
        let rec getLowerBound state dimension term =
            match term.term with
            | Error _ -> (term, state)
            | Array(lowerBounds, _, _, _, _) ->
                match dimension.term with
                | Error _ -> (dimension, state)
                | Concrete(obj, _) ->
                    let d = obj :?> int in
                    if d < 0 || d >= lowerBounds.Length then
                        let term, state = indexOutOfRangeException state in
                        (Error term, state)
                    else (lowerBounds.[d], state)
                | _ ->
                    let lowerBound = Concrete 0 VSharp.Array.lengthTermType in
                    let upperBound = Concrete lowerBounds.Length VSharp.Array.lengthTermType in
                    checkBounds state lowerBound upperBound dimension
                        (fun state k ->
                            let guards =
                                List.init lowerBounds.Length
                                          (fun i -> simplifyEqual dimension (Concrete i VSharp.Array.lengthTermType) id)
                            in
                            let result = List.zip guards (List.ofArray lowerBounds) |> Merging.merge in
                            k (Return result, state))
                        (fun (term, state) -> ControlFlow.resultToTerm term, state)
            | term -> internalfail (sprintf "expected array, but %O got!" term)
        in
        let result, state =
            match array.term with
            | Union gvs -> Merging.guardedStateMap (fun state term -> getLowerBound state dimension term) gvs state
            | _ -> getLowerBound state dimension array
        in (ControlFlow.throwOrReturn result, state)
