namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

module private MathImpl =

    let log<'a when 'a : comparison> zero concrete (state : State.state) args =
        let arg = List.item 1 args in
        let rec log = function
            | Error _ as e -> e
            | Concrete(obj, _) ->
                let a = obj :?> 'a in
                if a < zero then Nop
                else Concrete(concrete a, Numeric typedefof<double>)
            | Constant(_, _, t) | Expression(_, _, t) as c ->
                let l = [c] in
                Expression(Application(StandardFunctionIdentifier(Operations.Logarithm)),
                    l, t)
            | Union gvs -> Merging.guardedMap log gvs
            | term -> internalfail (sprintf "expected number, but %s got!" (toString term)) in 
        (Return (log arg), state)
        
module Math =

    let Log state args = MathImpl.log<double> 0.0 Math.Log state args

