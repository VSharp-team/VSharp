namespace VSharp.Interpreter.IL


module internal WeightCombinators =
    
    let withInverseLinearFallback bound (one : uint option) (another : uint option) =
        match one, another with
        | (None | Some 0u), Some anotherWeight ->
            Some(uint (max 0 (int bound - int anotherWeight)))
        | Some oneWeight, _ -> Some(bound + oneWeight)
        | None, None -> None
