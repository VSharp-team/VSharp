namespace VSharp.Analyzer

open System
open FSharpx.Collections
open VSharp
open VSharp.Core

[<StructuralEquality;NoComparison>]
type private primeSource =
//    { address : ISymbolicConstantSource; name : string; typ : termType }
    { primedConstant : term } with
    interface INonComposableSymbolicConstantSource with
        override x.SubTerms = [x.primedConstant] :> term seq

type VariablesSubst = System.Collections.Generic.Dictionary<term, term>

module Variables =

    let private primeConst (effects : state) = function
        | {term = Constant(name, LazyInstantiation(loc, None, _), typ)} as c when Memory.HasEffectOnAddress effects loc ->
            let source : primeSource = {primedConstant = c}
            Constant (name.v + "!prime") source typ
        | t -> t

    let private unprimeConst = function
        | {term = Constant(_, (:? primeSource as src), _)} -> src.primedConstant
        | t -> t

    let prime (effects : state) (f : formula) =
        Substitute (primeConst effects) f

    let unprime (f : formula) =
        Substitute unprimeConst f

    let rename (f : formula) (subst : VariablesSubst) =
        let doSubst t =
            let res = ref Nop
            if subst.TryGetValue(t, res) then !res else t
        Substitute doSubst f

    let state2Formula (effect : state) : formula =
        __notImplemented__()
