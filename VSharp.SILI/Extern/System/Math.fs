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
                Concrete(concrete a, Numeric typedefof<double>)
            | Constant(_, _, t) | Expression(_, _, t) as c ->
                let l = [c] in
                Expression(Application(StandardFunctionIdentifier standFunc),
                    l, t)
            | Union gvs -> Merging.guardedMap impl gvs
            | term -> internalfail (sprintf "expected number, but %A got!" term) in
        (Return (impl arg), state)

    let pow<'a when 'a : comparison> zero one concrete standFunc (state : State.state) args =
        let b, p = List.item 1 args, List.item 2 args in
        let rec power p = function
            | Error _ as e -> e
            | Concrete(bObj, _) as bConc ->
                let b = bObj :?> 'a in
                let rec pow = function
                    | Error _ as e -> e
                    | Concrete(pObj, _) -> let p = pObj :?> 'a
                                           Concrete(concrete(b, p), Numeric typedefof<double>)
                    | Constant(_, _, t) | Expression(_, _, t) as p ->
                        if b = one then Concrete(one, Numeric typedefof<double>)
                        else let l = [bConc; p]
                             Expression(Application(StandardFunctionIdentifier(standFunc)), l, t)
                    | Union gvs -> Merging.guardedMap pow gvs
                    | term -> internalfail (sprintf "expected number for power, but %A got!" term)
                in
                pow p
            | Constant(_, _, t) | Expression(_, _, t) as b ->
                let rec pow = function
                    | Error _ as e -> e
                    | Concrete(pObj, _) as pConc -> let p = pObj :?> 'a
                                                    if p = zero then Concrete(one, Numeric typedefof<double>)
                                                    elif p = one then b
                                                    else let l = [b; pConc]
                                                         Expression(Application(StandardFunctionIdentifier
                                                                     (Operations.Power)), l, t)
                    | Constant(_, _, t) | Expression(_, _, t) as p ->
                        let l = [b; p]
                        Expression(Application(StandardFunctionIdentifier(Operations.Power)),
                            l, t)
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

    let Pow state args = MathImpl.pow<double> 0.0 1.0 Math.Pow Operations.Power state args
