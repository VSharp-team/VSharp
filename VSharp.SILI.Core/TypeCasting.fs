namespace VSharp.Core

open Common
open VSharp

module internal TypeCasting =

    let rec primitiveCast mtd isChecked hierarchyCast targetType state term k =
        match term.term with
        | Nop -> internalfailf "casting void to %O!" targetType
        | Concrete(value, _) ->
            if Terms.isFunction term && Types.isFunction targetType
            then k (Concrete term.metadata value targetType, state)
            else k (CastConcrete value (Types.toDotNetType targetType) term.metadata, state)
        | Constant(_, _, t)
        | Expression(_, _, t) -> k (makeCast t targetType term isChecked mtd, state)
        | Ref(NullAddress, path) ->
            assert(List.isEmpty path)
            k (Terms.makeNullRef mtd, state)
        | Ref(TopLevelHeap _, _)
        | Ptr(TopLevelHeap _, _, _, _) ->
            Common.statedConditionalExecution state
                (fun state k -> k (Pointers.isNull mtd term, state))
                (fun state k -> k (Terms.makeNullRef mtd, state))
                (fun state k -> hierarchyCast targetType state term k)
                k
        | Ref _
        | Ptr _
        | Struct _ -> hierarchyCast targetType state term k
        | _ -> __notImplemented__()

    let private doCast mtd term targetType isChecked =
        let castPointer term typ = // For Pointers
            match targetType with
            | Pointer typ' -> castReferenceToPointer mtd typ' term
            | _ -> makeCast (Pointer typ) targetType term isChecked mtd

        match term.term with
        | Ptr(_, _, typ, _) -> castPointer term typ
        | Ref(TopLevelHeap(addr, baseType, _), []) -> HeapRef term.metadata addr baseType targetType []
        | _ -> __unreachable__()

    let canCast mtd state targetType term =
        let castCheck term =
            match term.term with
            | Ptr(_, _, typ, _) -> typeIsType mtd (Pointer typ) targetType
            | Ref _ ->
                Common.statelessConditionalExecution state.pc
                    (fun k -> Pointers.isNull mtd term |> k)
                    (fun k -> makeFalse mtd |> k)
                    (fun k -> refIsType mtd term targetType |> k)
                    id
            | _ -> typeIsType mtd (typeOf term) targetType
        Merging.guardedErroredApply castCheck term

    let cast mtd state argument targetType isChecked primitiveCast fail k =
        let hierarchyCast targetType state term k =
            Common.commonStatedConditionalExecution state
                (fun state k -> k (canCast mtd state targetType term, state))
                (fun state k -> k (doCast mtd term targetType isChecked |> Return mtd, state))
                (fun state k -> k (fail state term targetType))
                ControlFlow.mergeResults ControlFlow.merge2Results ControlFlow.throwOrIgnore
                (fun (statementResult, state) -> k (ControlFlow.resultToTerm statementResult, state))
        Merging.guardedErroredStatedApplyk (primitiveCast hierarchyCast targetType) state argument k

    let castReferenceToPointer mtd state reference =
        let getType ref =
            match ref.term with
            | Ref(TopLevelStack key, []) -> State.typeOfStackLocation state key
            | _ -> baseTypeOfRef ref
        let typ = commonTypeOf getType reference
        Terms.castReferenceToPointer mtd typ reference
