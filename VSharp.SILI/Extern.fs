namespace VSharp

open System

// ------------------------------- mscorelib.System -------------------------------

module private System =

    module Array =

        let checkBounds state lower upper x fits k =
            Arithmetics.simplifyGreaterOrEqual x lower (fun notTooSmall ->
            Arithmetics.simplifyLess x upper (fun notTooLarge ->
            Propositional.simplifyAnd notTooSmall notTooLarge (fun inBounds ->
            Interpreter.reduceConditionalExecution state
                (fun state k -> k (inBounds, state))
                fits
                (fun state k -> k (Throw(Terms.MakeConcrete(new IndexOutOfRangeException()) typedefof<IndexOutOfRangeException>), state))
                k)))

        [<Implements("System.Int32 System.Array.GetLength(this, System.Int32)")>]
        let GetLength (state : State.state) (args : Term list) (k : StatementResult * State.state -> 'a) =
            let this, dimension = args.[0], args.[1] in
            let array = Memory.deref state this in
            let rec getLength dimension = function
                | Array(_, _, _, lengths, _) ->
                    match dimension with
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
                            (fun (result, _) -> ControlFlow.resultToTerm result)

                | term -> internalfail (sprintf "expected array, but %s got!" (toString term))
            in
            let result =
                match array with
                | Terms.GuardedValues(gs, vs) ->
                    vs |> List.map (getLength dimension) |> List.zip gs |> Merging.merge
                | _ -> getLength dimension array
            in k (ControlFlow.throwOrReturn result, state)

        [<Implements("System.Int32 System.Array.GetRank(this)")>]
        let GetRank (state : State.state) (args : Term list) (k : StatementResult * State.state -> 'a) =
            let array = Memory.deref state args.[0] in
            let rec getRank = function
                | Array(_, _, _, _, ArrayType(_, rank)) -> Concrete(rank, Numeric typedefof<int>)
                | Terms.GuardedValues(gs, vs) -> vs |> List.map getRank |> List.zip gs |> Merging.merge
                | term -> internalfail (sprintf "expected array, but %s got!" (toString term))
            in k (Return (getRank array), state)
