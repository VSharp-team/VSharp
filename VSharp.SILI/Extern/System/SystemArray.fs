namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Array -------------------------------

module SystemArray =

    let private indexOutOfRangeException state = State.activator.CreateInstance typeof<IndexOutOfRangeException> [] state

    let checkBounds state lower upper x fits k =
        Arithmetics.simplifyGreaterOrEqual x lower (fun notTooSmall ->
        Arithmetics.simplifyLess x upper (fun notTooLarge ->
        Propositional.simplifyAnd notTooSmall notTooLarge (fun inBounds ->
        Interpreter.reduceConditionalExecution state
            (fun state k -> k (inBounds, state))
            fits
            (fun state k ->
                let term, state = indexOutOfRangeException state in
                k (Throw term, state))
            k)))

    let GetLength state args =
        let this, dimension = List.item 0 args, List.item 1 args in
        let array, state = Memory.deref state this in
        let rec getLength state dimension = function
            | Error _ as e -> (e, state)
            | Array(_, _, _, lengths, _) ->
                match dimension with
                | Error _ as e -> (e, state)
                | Concrete(obj, _) ->
                    let d = obj :?> int in
                    if d < 0 || d >= lengths.Length then
                        let term, state = indexOutOfRangeException state in
                        (Error term, state)
                    else (lengths.[d], state)
                | _ ->
                    let lowerBound = Concrete(0, Array.lengthTermType) in
                    let upperBound = Concrete(lengths.Length, Array.lengthTermType) in
                    checkBounds state lowerBound upperBound dimension
                        (fun state k ->
                            let guards =
                                List.init lengths.Length
                                          (fun i -> simplifyEqual dimension (Concrete(i, Array.lengthTermType)) id)
                            in
                            let result = List.zip guards (List.ofArray lengths) |> Merging.merge in
                            k (Return result, state))
                        (fun (term, state) -> ControlFlow.resultToTerm term, state)
            | term -> internalfailf "expected array, but %O got!" term
        in
        let result, state =
            match array with
            | Union gvs -> Merging.guardedStateMap (getLength state dimension) gvs state
            | _ -> getLength state dimension array
        in (ControlFlow.throwOrReturn result, state)

    let GetRank state args =
        let array, state = Memory.deref state (List.head args) in
        let rec getRank = function
            | Error _ as e -> e
            | Array(_, _, _, _, ArrayType(_, rank)) -> Concrete(rank, Numeric typedefof<int>)
            | Union gvs -> Merging.guardedMap getRank gvs
            | term -> internalfail "expected array, but %O got!" term
        in (Return (getRank array), state)

    let get_Length state args =
        let array, state = Memory.deref state (List.head args) in
        (Return (Array.length array), state)

    let GetLowerBound state args =
        let this, dimension = List.item 0 args, List.item 1 args in
        let array, state = Memory.deref state this in
        let rec getLowerBound state dimension = function
            | Error _ as e -> (e, state)
            | Array(lowerBounds, _, _, _, _) ->
                match dimension with
                | Error _ as e -> (e, state)
                | Concrete(obj, _) ->
                    let d = obj :?> int in
                    if d < 0 || d >= lowerBounds.Length then 
                        let term, state = indexOutOfRangeException state in
                        (Error term, state)
                    else (lowerBounds.[d], state)
                | _ ->
                    let lowerBound = Concrete(0, Array.lengthTermType) in
                    let upperBound = Concrete(lowerBounds.Length, Array.lengthTermType) in
                    checkBounds state lowerBound upperBound dimension
                        (fun state k ->
                            let guards =
                                List.init lowerBounds.Length
                                          (fun i -> simplifyEqual dimension (Concrete(i, Array.lengthTermType)) id)
                            in
                            let result = List.zip guards (List.ofArray lowerBounds) |> Merging.merge in
                            k (Return result, state))
                        (fun (term, state) -> ControlFlow.resultToTerm term, state)
            | term -> internalfail "expected array, but %O got!" term
        in
        let result, state =
            match array with
            | Union gvs -> Merging.guardedStateMap (getLowerBound state dimension) gvs state
            | _ -> getLowerBound state dimension array
        in (ControlFlow.throwOrReturn result, state)
