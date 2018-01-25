namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorelib.System.Math -------------------------------

module private MathImpl =

    open Arithmetics

    let impl<'a when 'a : comparison> (concrete : 'a -> 'a) standFunc (state : state) args =
        assert(List.length args = 1)
        let arg = List.head args
        let result = GuardedApplyExpression arg (fun term ->
            match term.term with
            | Concrete(obj, _) ->
                let a = obj :?> 'a
                MakeNumber (concrete a)
            | Constant(_, _, t)
            | Expression(_, _, t) ->
                Expression (Application(StandardFunctionIdentifier standFunc)) [term] t
            | term -> internalfailf "expected number, but %O got!" term)
        Return result, state

    let pow<'a when 'a : comparison> convert isNaN isPosInf isNegInf concrete (state : state) args =
        let mkPowExpr args typ = Expression (Application(StandardFunctionIdentifier StandardFunction.Power)) args typ
        let zero = convert 0.0
        let one = convert 1.0
        let minusInf = convert -infinity
        let zeroTerm = MakeNumber zero
        let oneTerm = MakeNumber one
        let infTerm = convert infinity |> MakeNumber
        let minusOneTerm = convert -1.0 |> MakeNumber
        let b, p = List.item 0 args, List.item 1 args
        let result = GuardedApplyExpression b (fun term ->
            match term.term with
            | Concrete(bObj, _) ->
                let bConc = term
                let b = bObj :?> 'a
                GuardedApplyExpression p (fun term ->
                    match term.term with
                    | Concrete(pObj, _) ->
                        let p = pObj :?> 'a
                        MakeNumber (concrete(b, p))
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        let p = term
                        // base is concrete, exponent is symbolic
                        match b with
                        | _ when b = zero ->
                            let pIsLessZero = p << zeroTerm
                            let pIsZero = p === zeroTerm
                            Union([(pIsZero, oneTerm); (pIsLessZero, infTerm);
                                   (!!pIsLessZero, zeroTerm)])
                        | _ when b = one -> oneTerm
                        | _ when isNaN b -> bConc
                        | _ when isPosInf b ->
                            let pIsZero = p === zeroTerm
                            let pIsLessZero = p << zeroTerm
                            Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm);
                                  (!!pIsZero &&& !!pIsLessZero, infTerm)])
                        | _ when isNegInf b ->
                            let pIsZero = p === zeroTerm
                            let pIsLessZero = p << zeroTerm
                            if Types.IsInteger t then
                                let pIsGreaterZeroAndEven = (p %%% (Concrete 2 t)) === MakeNumber 0
                                Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm); (pIsGreaterZeroAndEven, infTerm);
                                       (!!pIsZero &&& !!pIsLessZero &&& !!pIsGreaterZeroAndEven, MakeNumber minusInf)])
                            else Union([(pIsZero, oneTerm); (pIsLessZero, zeroTerm);
                                        (!!pIsZero &&& !!pIsLessZero, infTerm)])
                        | _ -> mkPowExpr [bConc; p] t
                    | term -> internalfailf "expected number for power, but %O got!" term)
            | Constant(_, _, t) | Expression(_, _, t) ->
                let b = term
                GuardedApplyExpression p (fun term ->
                    match term.term with
                    | Concrete(pObj, _) ->
                        let pConc = term
                        // base is symbolic, exponent is concrete
                        match pObj :?> 'a with
                        | p when p = zero -> oneTerm
                        | p when p = one -> b
                        | p when isNaN p -> pConc
                        | p when isPosInf p ->
                            let bIsOne = b === oneTerm
                            let bIsMinusOne = b === minusOneTerm
                            let bIsBetweenMinOneOne = (minusOneTerm << b) &&& (b << oneTerm)
                            Union([(bIsOne, oneTerm); (bIsMinusOne, MakeNumber nan);
                                   (bIsBetweenMinOneOne, zeroTerm);
                                   (!!bIsOne &&& !!bIsMinusOne &&& !!bIsBetweenMinOneOne, infTerm)])
                        | p when isNegInf p ->
                            let bIsOne = b === oneTerm
                            let bIsMinusOne = b === minusOneTerm
                            let bIsBetweenMinOneOne = (minusOneTerm << b) &&& (b << oneTerm)
                            Union([(bIsOne, oneTerm); (bIsMinusOne, MakeNumber nan);
                                   (bIsBetweenMinOneOne, infTerm);
                                   (!!bIsOne &&& !!bIsMinusOne &&& !!bIsBetweenMinOneOne, zeroTerm)])
                        | _ -> mkPowExpr [b; pConc] t
                    | Constant(_, _, t) | Expression(_, _, t) ->
                        mkPowExpr [b; term] t
                    | term -> internalfailf "expected number for power, but %O got!" term)
            | term -> internalfailf "expected number for base, but %O got!" term)
        Return result, state

    let atan2<'a when 'a : comparison> convert isNan isInf concrete (state : state) args =
        let atanOp = Application(StandardFunctionIdentifier StandardFunction.Arctangent2)
        let y, x = List.item 0 args, List.item 1 args
        let inf, Nan = convert infinity, convert nan
        let result = GuardedApplyExpression y (fun term ->
            match term.term with
            | Concrete(yObj, _) ->
                let yConc = term
                let y = yObj :?> 'a
                GuardedApplyExpression x (fun term ->
                    match term.term with
                    | Concrete(xObj, _) -> MakeNumber(concrete (y, xObj :?> 'a))
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        let x = term
                        // x is symbolic, y is concrete
                        let exp = Expression atanOp [yConc; x] t
                        match y with
                        | y when isNan y -> yConc
                        | y when isInf y ->
                              let xIsInf = x === MakeNumber inf
                              Union([(xIsInf, MakeNumber Nan); (!!xIsInf, exp)])
                        | _ -> exp
                    | term -> internalfailf "expected number for x, but %O got!" term)
            | Constant(_, _, t)
            | Expression(_, _, t) ->
                let y = term
                GuardedApplyExpression x (fun term ->
                    match term.term with
                    | Concrete(xObj, _) ->
                        let xConc = term
                        // x is concrete, y is symbolic
                        let exp = Expression atanOp [y; xConc] t
                        match xObj :?> 'a with
                        | x when isNan x -> xConc
                        | x when isInf x ->
                            let yIsInf = y === MakeNumber inf
                            Union([(yIsInf, MakeNumber Nan); (!!yIsInf, exp)])
                        | _ -> exp
                    | Constant(_, _, t)
                    | Expression(_, _, t) ->
                        Expression atanOp [y; term] t
                    | term -> internalfailf "expected number for x, but %O got!" term)
            | term -> internalfailf "expected number for y, but %O got!" term)
        Return result, state

module internal Math =
    let Acos state args = MathImpl.impl<double> Math.Acos StandardFunction.Arccosine state args
    let Asin state args = MathImpl.impl<double> Math.Asin StandardFunction.Arcsine state args
    let Atan state args = MathImpl.impl<double> Math.Atan StandardFunction.Arctangent state args
    let Atan2 state args = MathImpl.atan2<double> double Double.IsNaN Double.IsInfinity Math.Atan2 state args
    let Ceiling state args = MathImpl.impl<double> Math.Ceiling StandardFunction.Ceiling state args
    let Cos state args = MathImpl.impl<double> Math.Cos StandardFunction.Cosine state args
    let Cosh state args = MathImpl.impl<double> Math.Cosh StandardFunction.HyperbolicCosine state args
    let Floor state args = MathImpl.impl<double> Math.Floor StandardFunction.Floor state args
    let Sin state args = MathImpl.impl<double> Math.Sin StandardFunction.Sine state args
    let Tan state args = MathImpl.impl<double> Math.Tan StandardFunction.Tangent state args
    let Sinh state args = MathImpl.impl<double> Math.Sinh StandardFunction.HyperbolicSine state args
    let Tanh state args = MathImpl.impl<double> Math.Tanh StandardFunction.HyperbolicTangent state args
    let Round state args = MathImpl.impl<double> Math.Round StandardFunction.Round state args
    let Sqrt state args = MathImpl.impl<double> Math.Sqrt StandardFunction.SquareRoot state args
    let Log state args = MathImpl.impl<double> Math.Log StandardFunction.Logarithm state args
    let Log10 state args = MathImpl.impl<double> Math.Log10 StandardFunction.Logarithm10 state args
    let Exp state args = MathImpl.impl<double> Math.Exp StandardFunction.Exponent state args
    let Pow state args = MathImpl.pow<double> double Double.IsNaN Double.IsPositiveInfinity Double.IsNegativeInfinity Math.Pow state args
    let Abs state args = MathImpl.impl<double> Math.Abs StandardFunction.Absolute state args
    let AbsS state args = MathImpl.impl<single> Math.Abs StandardFunction.AbsoluteS state args
