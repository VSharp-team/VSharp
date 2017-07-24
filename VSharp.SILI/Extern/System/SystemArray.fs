namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Array -------------------------------

module SystemArray =

    let checkBounds state lower upper x fits k =
        Arithmetics.simplifyGreaterOrEqual x lower (fun notTooSmall ->
        Arithmetics.simplifyLess x upper (fun notTooLarge ->
        Propositional.simplifyAnd notTooSmall notTooLarge (fun inBounds ->
        Interpreter.reduceConditionalExecution state
            (fun state k -> k (inBounds, state))
            fits
            (fun state k -> k (ControlFlow.throw(new IndexOutOfRangeException()), state))
            k)))

    let GetLength state args =
        let this, dimension = List.item 0 args, List.item 1 args in
        let array = Memory.deref state this in
        let rec getLength dimension = function
            | Error _ as e -> e
            | Array(_, _, _, lengths, _) ->
                match dimension with
                | Error _ as e -> e
                | Concrete(obj, _) ->
                    let d = obj :?> int in
                    if d < 0 || d >= lengths.Length then Terms.MakeError(new IndexOutOfRangeException())
                    else lengths.[d]
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
                        (fst >> ControlFlow.resultToTerm)
            | term -> internalfail (sprintf "expected array, but %s got!" (toString term))
        in
        let result =
            match array with
            | Union gvs -> Merging.guardedMap (getLength dimension) gvs
            | _ -> getLength dimension array
        in (ControlFlow.throwOrReturn result, state)

    let GetRank state args =
        let array = Memory.deref state (List.head args) in
        let rec getRank = function
            | Error _ as e -> e
            | Array(_, _, _, _, ArrayType(_, rank)) -> Concrete(rank, Numeric typedefof<int>)
            | Union gvs -> Merging.guardedMap getRank gvs
            | term -> internalfail (sprintf "expected array, but %s got!" (toString term))
        in (Return (getRank array), state)

    let get_Length state args =
        let array = Memory.deref state (List.head args) in
        (Return (Array.length array), state)

    let GetLowerBound state args =
        let this, dimension = List.item 0 args, List.item 1 args in
        let array = Memory.deref state this in
        let rec getLowerBound dimension = function
            | Error _ as e -> e
            | Array(lowerBounds, _, _, _, _) ->
                match dimension with
                | Error _ as e -> e
                | Concrete(obj, _) ->
                    let d = obj :?> int in
                    if d < 0 || d >= lowerBounds.Length then Terms.MakeError(new IndexOutOfRangeException())
                    else lowerBounds.[d]
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
                        (fst >> ControlFlow.resultToTerm)
            | term -> internalfail (sprintf "expected array, but %O got!" term)
        in
        let result =
            match array with
            | Union gvs -> Merging.guardedMap (getLowerBound dimension) gvs
            | _ -> getLowerBound dimension array
        in (ControlFlow.throwOrReturn result, state)