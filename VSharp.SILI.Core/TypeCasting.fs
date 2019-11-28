namespace VSharp.Core

open Common
open VSharp

module internal TypeCasting =

    let private doCast mtd term targetType isChecked =
        let castPointer term typ = // For Pointers
            match targetType with
            | Pointer typ' -> castReferenceToPointer mtd typ' term
            | _ -> makeCast (Pointer typ) targetType term isChecked mtd

        match term.term with
        | Ptr(_, _, typ, _) -> castPointer term typ
        | Ref(TopLevelHeap(addr, baseType, _), []) -> HeapRef term.metadata addr baseType targetType []
        | Struct _ -> term
        | _ -> __unreachable__()

    let canCast mtd term targetType =
        let castCheck term =
            match term.term with
            | Ptr(_, _, typ, _) -> typeIsType mtd (Pointer typ) targetType
            | Ref _ -> refIsType mtd term targetType
            | _ -> typeIsType mtd (typeOf term) targetType
        Merging.guardedErroredApply castCheck term

    let isCast mtd state term targetType =
        Common.statelessConditionalExecutionWithMerge state.pc
            (fun k -> Pointers.isNull mtd term |> k)
            (fun k -> makeFalse mtd |> k)
            (fun k -> canCast mtd term targetType |> k)

    let cast mtd isChecked state term targetType fail k =
        let hierarchyCast state term =
            Common.statedConditionalExecutionWithMergek state
                (fun state k -> k (canCast mtd term targetType, state))
                (fun state k -> k (doCast mtd term targetType isChecked, state))
                (fun state k -> k (fail state term targetType))
        let primitiveCast state term k =
            match term.term with
            | _ when typeOf term = targetType -> k (term, state)
            | Nop -> internalfailf "casting void to %O!" targetType
            | Concrete(value, _) -> k (CastConcrete isChecked value (Types.toDotNetType targetType) term.metadata, state)
            | Constant(_, _, t)
            | Expression(_, _, t) -> k (makeCast t targetType term isChecked mtd, state)
            | Ref _ ->
                statedConditionalExecutionWithMergek state
                    (fun state k -> k (Pointers.isNull mtd term, state))
                    (fun state k -> k (makeNullRef mtd, state))
                    (fun state k -> hierarchyCast state term k)
                    k
            | Ptr _
            | Array _
            | Struct _ -> hierarchyCast state term k
            | _ -> __unreachable__()
        Merging.guardedErroredStatedApplyk primitiveCast state term k

    let castReferenceToPointer mtd state reference =
        let getType ref =
            match ref.term with
            | Ref(TopLevelStack key, []) -> State.typeOfStackLocation state key
            | _ -> baseTypeOfRef ref
        let doCast reference =
            let typ = commonTypeOf getType reference
            Terms.castReferenceToPointer mtd typ reference
        Merging.guardedErroredApply doCast reference
