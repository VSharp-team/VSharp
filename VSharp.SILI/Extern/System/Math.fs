namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

module private MathImpl =

    let (===) t1 t2 = simplifyEqual t1 t2 id
    let (%%%) t1 t2 = simplifyRemainder false State.empty (t1 |> TypeOf |> Types.ToDotNetType) t1 t2 fst

    let impl<'a when 'a : comparison> (concrete: ('a -> 'a)) standFunc (state : State.state) args =
        let arg = List.item 0 args in
        let rec impl term =
            match term.term with
            | Error _-> term
            | Concrete(obj, _) ->
                let a = obj :?> 'a in
                MakeNumber (concrete a)
            | Constant(_, _, t)
            | Expression(_, _, t) ->
                Expression (Application(StandardFunctionIdentifier standFunc)) [term] t
            | Union gvs -> Merging.guardedMap impl gvs
            | term -> internalfail (sprintf "expected number, but %O got!" term) in
        (Return (impl arg), state)

    let pow<'a when 'a : comparison> convert isNaN isPosInf isNegInf concrete standFunc (state : State.state) args =
        let zero = convert 0.0 in
        let one = convert 1.0 in
        let minusInf = convert -infinity in
        let zeroTerm = MakeNumber zero in
        let oneTerm = MakeNumber one in
        let infTerm = convert infinity |> MakeNumber in
        let minusOneTerm = convert -1.0 |> MakeNumber in
        let b, p = List.item 0 args, List.item 1 args in
        let rec power p term =
            match term.term with
            | Error _ -> term
            | Concrete(bObj, _) ->
                let bConc = term in
                let b = bObj :?> 'a in
                let rec pow term =
                    match term.term with
                    | Error _ -> term
                    | Concrete(pObj, _) ->
                        let p = pObj :?> 'a in
                        MakeNumber (concrete(b, p))
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        let p = term in
                        // base is concrete, exponent is symbolic
                        match b with
                        | _ when b = zero ->
                            let pIsLessZero = simplifyLess p zeroTerm id in
                            let pIsZero = simplifyEqual p zeroTerm id in
                                Union([(pIsZero, oneTerm); (pIsLessZero, infTerm);
                                       (!!pIsLessZero, zeroTerm)])
                        | _ when b = one -> oneTerm
                        | _ when isNaN b -> bConc
                        | _ when isPosInf b ->
                            let pIsZero = simplifyEqual p zeroTerm id in
                            let pIsLessZero = simplifyLess p zeroTerm id in
                            Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm);
                                  (!!pIsZero &&& !!pIsLessZero, infTerm)])
                        | _ when isNegInf b ->
                            let pIsZero = simplifyEqual p zeroTerm id in
                            let pIsLessZero = simplifyLess p zeroTerm id in
                            if Types.IsInteger t then
                                let pIsGreaterZeroAndEven = (p %%% (Concrete 2 t)) === MakeNumber 0 in
                                    Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm); (pIsGreaterZeroAndEven, infTerm);
                                           (!!pIsZero &&& !!pIsLessZero &&& !!pIsGreaterZeroAndEven, MakeNumber minusInf)])
                            else Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm);
                                        (!!pIsZero &&& !!pIsLessZero, infTerm)])
                        | _ -> Expression (Application(StandardFunctionIdentifier(standFunc))) [bConc; p] t
                    | Union gvs -> Merging.guardedMap pow gvs
                    | term -> internalfail (sprintf "expected number for power, but %O got!" term)
                in
                pow p
            | Constant(_, _, t) | Expression(_, _, t) ->
                let b = term in
                let rec pow term =
                    match term.term with
                    | Error _ -> term
                    | Concrete(pObj, _) ->
                        let pConc = term in
                        // base is symbolic, exponent is concrete
                        match pObj :?> 'a with
                        | p when p = zero -> oneTerm
                        | p when p = one -> b
                        | p when isNaN p -> pConc
                        | p when isPosInf p ->
                            let bIsOne = b === oneTerm in
                            let bIsMinusOne = b === minusOneTerm in
                            let bIsBetweenMinOneOne = simplifyLess minusOneTerm b id
                                                        &&& simplifyGreater oneTerm b id in
                            Union([(bIsOne, oneTerm); (bIsMinusOne, MakeNumber nan);
                                   (bIsBetweenMinOneOne, zeroTerm);
                                   (!!bIsOne &&& !!bIsMinusOne &&& !!bIsBetweenMinOneOne, infTerm)])
                        | p when isNegInf p ->
                            let bIsOne = b === oneTerm in
                            let bIsMinusOne = b === minusOneTerm in
                            let bIsBetweenMinOneOne = simplifyLess minusOneTerm b id
                                                        &&& simplifyGreater oneTerm b id
                            Union([(bIsOne, oneTerm); (bIsMinusOne, MakeNumber nan);
                                   (bIsBetweenMinOneOne, infTerm);
                                   (!!bIsOne &&& !!bIsMinusOne &&& !!bIsBetweenMinOneOne, zeroTerm)])
                        | _ -> Expression (Application(StandardFunctionIdentifier(Operations.Power))) [b; pConc] t
                    | Constant(_, _, t) | Expression(_, _, t) ->
                        Expression (Application(StandardFunctionIdentifier(Operations.Power))) [b; term] t
                    | Union gvs -> Merging.guardedMap pow gvs
                    | term -> internalfail (sprintf "expected number for power, but %O got!" term)
                in
                pow p
            | Union gvs -> Merging.guardedMap (power p) gvs
            | term -> internalfail (sprintf "expected number for base, but %O got!" term) in
        (Return (power p b), state)

    let atan2<'a when 'a : comparison> convert isNan isInf concrete standFunc (state : State.state) args =
        let y, x = List.item 0 args, List.item 1 args in
        let inf, Nan = convert infinity, convert nan
        let rec atanY x term =
            match term.term with
            | Error _ -> term
            | Concrete(yObj, _) ->
                let yConc = term in
                let y = yObj :?> 'a in
                let rec atanX term =
                    match term.term with
                    | Error _ -> term
                    | Concrete(xObj, _) -> MakeNumber(concrete (y, xObj :?> 'a))
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        let x = term in
                        // x is symbolic, y is concrete
                        let exp = Expression (Application(StandardFunctionIdentifier standFunc)) [yConc; x] t
                        match y with
                        | y when isNan y -> yConc
                        | y when isInf y ->
                              let xIsInf = x === MakeNumber inf in
                              Union([(xIsInf, MakeNumber Nan); (!!xIsInf, exp)])
                        | _ -> exp
                    | Union gvs -> Merging.guardedMap atanX gvs
                    | term -> internalfail (sprintf "expected number for x, but %O got!" term) in
                    atanX x
            | Constant(_, _, t)
            | Expression(_, _, t) ->
                let y = term in
                let rec atanX term =
                    match term.term with
                    | Error _ -> term
                    | Concrete(xObj, _) ->
                        let xConc = term in
                        // x is concrete, y is symbolic
                        let exp = Expression (Application(StandardFunctionIdentifier standFunc)) [y; xConc] t in
                        match xObj :?> 'a with
                        | x when isNan x -> xConc
                        | x when isInf x ->
                            let yIsInf = y === MakeNumber inf in
                            Union([(yIsInf, MakeNumber Nan); (!!yIsInf, exp)])
                        | _ -> exp
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        Expression (Application(StandardFunctionIdentifier standFunc)) [y; term] t
                    | Union gvs -> Merging.guardedMap atanX gvs
                    | term -> internalfail (sprintf "expected number for x, but %O got!" term) in
                atanX x
            | Union gvs -> Merging.guardedMap (atanY x) gvs
            | term -> internalfail (sprintf "expected number for y, but %O got!" term) in
        (Return (atanY x y), state)

module internal Math =
    let Acos state args = MathImpl.impl<double> Math.Acos Operations.Arccosine state args
    let Asin state args = MathImpl.impl<double> Math.Asin Operations.Arcsine state args
    let Atan state args = MathImpl.impl<double> Math.Atan Operations.Arctangent state args
    let Atan2 state args = MathImpl.atan2<double> double Double.IsNaN Double.IsInfinity Math.Atan2 Operations.Arctangent2 state args
    let Ceiling state args = MathImpl.impl<double> Math.Ceiling Operations.Ceiling state args
    let Cos state args = MathImpl.impl<double> Math.Cos Operations.Cosine state args
    let Cosh state args = MathImpl.impl<double> Math.Cosh Operations.HyperbolicCosine state args
    let Floor state args = MathImpl.impl<double> Math.Floor Operations.Floor state args
    let Sin state args = MathImpl.impl<double> Math.Sin Operations.Sine state args
    let Tan state args = MathImpl.impl<double> Math.Tan Operations.Tangent state args
    let Sinh state args = MathImpl.impl<double> Math.Sinh Operations.HyperbolicSine state args
    let Tanh state args = MathImpl.impl<double> Math.Tanh Operations.HyperbolicTangent state args
    let Round state args = MathImpl.impl<double> Math.Round Operations.Round state args
    let Sqrt state args = MathImpl.impl<double> Math.Sqrt Operations.SquareRoot state args
    let Log state args = MathImpl.impl<double> Math.Log Operations.Logarithm state args
    let Log10 state args = MathImpl.impl<double> Math.Log10 Operations.Logarithm10 state args
    let Exp state args = MathImpl.impl<double> Math.Exp Operations.Exponent state args
    let Pow state args = MathImpl.pow<double> double Double.IsNaN Double.IsPositiveInfinity Double.IsNegativeInfinity Math.Pow Operations.Power state args
    let Abs state args = MathImpl.impl<double> Math.Abs Operations.Absolute state args
    let AbsS state args = MathImpl.impl<single> Math.Abs Operations.AbsoluteS state args
