namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.Math -------------------------------

module Math =

    let log<'a when 'a : comparison> zero concrete state args =
        let arg = Memory.deref state (List.head args) in
        let rec log = function
            | Error _ as e -> e
            | Concrete(obj, t) ->
                match t with
                | Numeric t ->                    
                    let a = obj :?> 'a in
                    if a <= zero then Nop
                    else Concrete(concrete a, Numeric t)
                | _ as t-> internalfail (sprintf "expected numeric type, but %s got" (toString t))
            | Constant(str, source, t) -> Constant("log(" + str + ")", source, t)
            | Union gvs -> Merging.guardedMap log gvs
            | term -> internalfail (sprintf "expected number, but %s got!" (toString term)) in 
        (Return (log arg), state)
        
    let Log = log<double> 0.0 Math.Log