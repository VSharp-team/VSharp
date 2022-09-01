namespace VSharp.Interpreter.IL

open VSharp.Interpreter.IL

/// <summary>
/// Combines two weighters using the given combinator function.
/// </summary>
type internal CombinedWeighter(one : IWeighter, another : IWeighter, maxPriority : uint, combinator : uint option -> uint option -> uint option) =
    
    interface IWeighter with
        override x.Weight(state) =
            let oneWeight = one.Weight state
            let anotherWeight = another.Weight state
            combinator oneWeight anotherWeight
        override x.Next() = maxPriority
