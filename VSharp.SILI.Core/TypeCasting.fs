namespace VSharp.Core

open VSharp
open VSharp.Core.Types.Constructor

module internal TypeCasting =

    let primitiveCast mtd isChecked hierarchyCast targetType state term k =
        // TODO: get rid of hierarchy cast parameter!
        match term.term with
        | Error _ -> k (term, state)
        | Nop -> internalfailf "casting void to %O!" targetType
        | _ when Terms.isNull term -> k (Terms.makeNullRef targetType mtd, state)
        | Concrete(value, _) ->
            if Terms.isFunction term && Types.isFunction targetType
            then k (Concrete term.metadata value targetType, state)
            else k (CastConcrete value (Types.toDotNetType targetType) term.metadata, state)
        | Constant(_, _, t)
        | Expression(_, _, t) -> k (makeCast t targetType term isChecked mtd, state)
        | StackRef _ ->
            // printfn "Warning: casting stack reference %O to %O!" term targetType
            hierarchyCast targetType state term k
        | HeapRef _
        | Struct _ -> hierarchyCast targetType state term k
        | _ -> __notImplemented__()

    let private doCast mtd term targetType isChecked =
        let changeLast = // For References
            List.changeLast (fun (addr, _) -> (addr, targetType))

        let isUpCast l r =
            match l, r with
            | ComplexType(t1, _, _), ComplexType(t2, _, _) -> t1.Is t2
            | _ -> false

        let castPointer term typ = // For Pointers
            match targetType with
            | Pointer typ' when Types.sizeOf typ = Types.sizeOf typ' || typ = termType.Void || typ' = termType.Void ->
                castReferenceToPointer mtd typ' term
            | _ -> makeCast (termType.Pointer typ) targetType term isChecked mtd // TODO: [columpio] [Reinterpretation]

        match term.term with
        | PointerTo typ -> castPointer term typ
        | ReferenceTo typ when isUpCast typ targetType -> term
        | HeapRef (addrs, t, _) -> HeapRef mtd (addrs |> NonEmptyList.toList |> changeLast |> NonEmptyList.ofList) t
        | StackRef (key, path, _) -> StackRef mtd key (changeLast path)
        | StaticRef (key, path, _) -> StaticRef mtd key (changeLast path)
        | _ -> __unreachable__()

    let rec canCast mtd state targetType term =
        let derefForCast = Memory.derefWith (fun m s _ -> Concrete m null Null, s)
        match term.term with
        | PointerTo typ -> Common.is mtd (termType.Pointer typ) targetType, state
        | HeapRef _
        | StackRef _
        | StaticRef _ ->
            let contents, state = derefForCast mtd state term
            canCast mtd state targetType contents
        | Union gvs -> Merging.guardedStateMap (fun state term -> canCast mtd state targetType term) gvs state
        | _ -> Common.is mtd (typeOf term) targetType, state

    let cast mtd state argument targetType isChecked primitiveCast fail k =
        let isCasted state term = canCast mtd state targetType term
        let hierarchyCast targetType state term k =
            Common.reduceConditionalExecution state
                (fun state k -> k (isCasted state term))
                (fun state k -> k (doCast mtd term targetType isChecked |> Return mtd, state))
                (fun state k -> k (fail state term targetType))
                ControlFlow.mergeResults ControlFlow.merge2Results ControlFlow.throwOrIgnore
                (fun (statementResult, state) -> k (ControlFlow.resultToTerm statementResult, state))
        Merging.statedMapk (primitiveCast hierarchyCast targetType) state argument k

    let castReferenceToPointer mtd state reference k =
        let derefForCast = Memory.derefWith (fun m s _ -> makeNullRef Null m, s)
        let term, state = derefForCast mtd state reference
        k (castReferenceToPointer mtd (typeOf term) reference, state)
