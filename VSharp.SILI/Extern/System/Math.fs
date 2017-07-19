namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

module private MathImpl =

    let impl<'a when 'a : comparison> concrete standFunc (state : State.state) args =
        let arg = List.item 1 args in
        let rec impl = function
            | Error _ as e -> e
            | Concrete(obj, _) ->
                let a = obj :?> 'a in
                Terms.MakeNumber(concrete a)
            | Constant(_, _, t)
            | Expression(_, _, t) as c ->
                Expression(Application(StandardFunctionIdentifier standFunc), [c], t)
            | Union gvs -> Merging.guardedMap impl gvs
            | term -> internalfail (sprintf "expected number, but %A got!" term) in
        (Return (impl arg), state)

    let pow<'a when 'a : comparison> convert isNaN isPosInf isNegInf concrete standFunc (state : State.state) args =
        let zero = convert 0.0 in
        let one = convert 1.0 in
        let Nan = convert nan in
        let inf = convert infinity in
        let b, p = List.item 1 args, List.item 2 args in
        let rec power p = function
            | Error _ as e -> e
            | Concrete(bObj, _) as bConc ->
                let b = bObj :?> 'a in
                let rec pow = function
                    | Error _ as e -> e
                    | Concrete(pObj, _) -> let p = pObj :?> 'a
                                           Terms.MakeNumber(concrete(b, p))
                    | Constant(_, _, t)
                    | Expression(_, _, t) as p ->
                        match b with
                        | b when isPosInf b || isNegInf b ->
                            let pIsZero = p === Terms.MakeNumber zero in
                            let pIsNegInf = p === Terms.MakeNumber (convert -infinity)
                            Union([(pIsZero, Terms.MakeNumber one); (pIsNegInf, Terms.MakeNumber zero);
                                  (!!pIsZero &&& !!pIsNegInf, Terms.MakeNumber inf)])
                        | b when isNaN b -> Terms.MakeNumber Nan
                        | b when b = zero -> Terms.MakeNumber zero
                        | b when b = one -> Terms.MakeNumber one
                        | _ -> Expression(Application(StandardFunctionIdentifier(standFunc)), [bConc; p], t)
                    | Union gvs -> Merging.guardedMap pow gvs
                    | term -> internalfail (sprintf "expected number for power, but %A got!" term)
                in
                pow p
            | Constant(_, _, t) | Expression(_, _, t) as b ->
                let rec pow = function
                    | Error _ as e -> e
                    | Concrete(pObj, _) as pConc ->
                        match pObj :?> 'a with
                        | p when isPosInf p ->
                            let bIsZero = b === Terms.MakeNumber zero in
                            Union([(bIsZero, Terms.MakeNumber zero); (!!bIsZero, pConc)])
                        | p when isNegInf p ->
                            let bIsZero = b === Terms.MakeNumber zero in
                            Union([(bIsZero, Terms.MakeNumber inf);
                                  (!!bIsZero, Terms.MakeNumber zero)])
                        | p when isNaN p -> Concrete(convert nan, Numeric typedefof<'a>)
                        | p when p = zero -> Concrete(convert 1.0, Numeric typedefof<'a>)
                        | p when p = one -> b
                        | _ -> Expression(Application(StandardFunctionIdentifier
                                            (Operations.Power)), [b; pConc], t)
                    | Constant(_, _, t) | Expression(_, _, t) as p ->
                        Expression(Application(StandardFunctionIdentifier(Operations.Power)),
                            [b; p], t)
                    | Union gvs -> Merging.guardedMap pow gvs
                    | term -> internalfail (sprintf "expected number for power, but %A got!" term)
                in
                pow p
            | Union gvs -> Merging.guardedMap (power p) gvs
            | term -> internalfail (sprintf "expected number for base, but %A got!" term) in
        (Return (power p b), state)
        
module Math =
    let Sqrt state args = MathImpl.impl<double> Math.Sqrt Operations.SquareRoot state args
    let Log state args = MathImpl.impl<double> Math.Log Operations.Logarithm state args
    let Log10 state args = MathImpl.impl<double> Math.Log10 Operations.Logarithm10 state args
    let Exp state args = MathImpl.impl<double> Math.Exp Operations.Exponent state args
    let Pow state args = MathImpl.pow<double> double Double.IsNaN Double.IsPositiveInfinity Double.IsNegativeInfinity Math.Pow Operations.Power state args


