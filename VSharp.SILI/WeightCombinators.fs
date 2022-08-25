namespace VSharp.Interpreter.IL

open System

module internal WeightCombinators =
    
    let private logarithmicScale weight =
        if weight = 0u then 0u
        elif weight = 1u then 1u
        else double weight |> Math.Log2 |> Math.Ceiling |> uint
        
    let withInverseLogFallback bound (one : uint option) (another : uint option) =
        match one, another with
        | (None | Some 0u), Some anotherWeight ->
            let log = logarithmicScale anotherWeight
            Some(uint (max 0 (int bound - int log)))
        | Some oneWeight, _ -> Some(bound + oneWeight)
        | None, None -> None
