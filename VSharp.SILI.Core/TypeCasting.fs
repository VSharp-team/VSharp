namespace VSharp.Core

open Common
open VSharp

module internal TypeCasting =

    let private doCast mtd term targetType =
        let castPointer term typ = // For Pointers
            match targetType with
            | Pointer typ' -> castReferenceToPointer mtd typ' term
            | _ -> makeCast (Pointer typ) targetType term mtd

        match term.term with
        | Ptr(_, _, typ, _) -> castPointer term typ
        | Ref(RefTopLevelHeap(addr, baseType, _), []) -> HeapRef term.metadata addr baseType targetType []
        | Struct _ -> term
        | _ -> __unreachable__()

    let canCast mtd term targetType =
        let castCheck term =
            match term.term with
            | Concrete(value, _) -> makeBool mtd <| CanCastConcrete value targetType
            | Ptr(_, _, typ, _) -> typeIsType mtd (Pointer typ) targetType
            | Ref _ -> refIsType mtd term targetType
            | _ -> typeIsType mtd (typeOf term) targetType
        Merging.guardedApply castCheck term

    let isCast mtd state term targetType =
        Common.statelessConditionalExecutionWithMerge state.pc
            (fun k -> Pointers.isNull mtd term |> k)
            (fun k -> makeFalse mtd |> k)
            (fun k -> canCast mtd term targetType |> k)

    let cast mtd pc term targetType k =
        let primitiveCast term k =
            match term.term with
            | _ when typeOf term = targetType -> k term
            | Nop -> internalfailf "casting void to %O!" targetType
            | Concrete(value, _) -> k <| CastConcrete value (Types.toDotNetType targetType) term.metadata
            | Constant(_, _, t)
            | Expression(_, _, t) -> k <| makeCast t targetType term mtd
            | Ref _ ->
                statelessConditionalExecutionWithMergek pc
                    (fun k -> k <| Pointers.isNull mtd term)
                    (fun k -> k <| makeNullRef mtd)
                    (fun k -> k <| doCast mtd term targetType)
                    k
            | Ptr _
            | Struct _ -> k <| doCast mtd term targetType
            | _ -> __unreachable__()
        Merging.guardedApplyk primitiveCast term k

    let castReferenceToPointer mtd state reference =
        let getType ref =
            match ref.term with
            | Ref(RefTopLevelStack key, []) -> State.typeOfStackLocation state key
            | _ -> baseTypeOfRef ref
        let doCast reference =
            let typ = commonTypeOf getType reference
            Terms.castReferenceToPointer mtd typ reference
        Merging.guardedApply doCast reference
