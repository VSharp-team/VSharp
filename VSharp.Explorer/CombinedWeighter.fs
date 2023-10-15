namespace VSharp.Explorer

open System

/// <summary>
/// Combines two weighters using the given combinator function.
/// </summary>
type internal CombinedWeighter(one : IWeighter, another : IWeighter, maxPriority : uint, combinator : uint option -> uint option -> uint option) =

    interface IWeighter with
        override x.Weight(state) =
            let oneWeight = one.Weight state
            let anotherWeight = another.Weight state
            combinator oneWeight anotherWeight

type internal AugmentedWeighter(baseWeighter : IWeighter, mapping : uint option -> uint option) =

        interface IWeighter with
            override x.Weight(state) = baseWeighter.Weight state |> mapping

module internal WeightOperations =

    let withInverseLinearFallback bound (one : uint option) (another : uint option) =
        match one, another with
        | (None | Some 0u), Some anotherWeight ->
            let fallbackWeight = max 0 (int bound - int anotherWeight)
            Some(uint fallbackWeight)
        | Some oneWeight, _ -> Some(bound + oneWeight)
        | None, None -> None

    let inverseLogarithmic (maxValuePower : uint) (weight : uint option) =
        match weight with
        | None -> None
        | Some weight ->
            let log =
                match weight with
                | 0u -> 0.0
                | _ -> Math.Log2 (float weight)
            let newWeight = Math.Pow(2.0, float maxValuePower - 5.0) * (32.0 - log)
            newWeight |> Math.Floor |> uint |> Some
