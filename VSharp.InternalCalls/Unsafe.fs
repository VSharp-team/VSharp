namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.Unsafe --------------------------------

module Unsafe =

    let private getTypeFromTerm typ =
        match typ.term with
        | Concrete(:? Type as t, _) -> t
        | _ -> __unreachable__()

    let internal AsPointer (_ : state) (args : term list) : term =
        assert(List.length args = 2)
//        Types.Cast (List.item 1 args) (Pointer Void)
        List.item 1 args

    let internal ObjectAsT (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let typ, ref = args[0], args[1]
        let typ = getTypeFromTerm typ
        Types.Cast ref typ

    let internal AsRef (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        args.[1]

    let internal TFromAsTTo (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let toType = getTypeFromTerm args[1]
        let pointerType = toType.MakePointerType()
        let ref = args[2]
        assert(IsReference ref || IsPtr ref)
        Types.Cast ref pointerType

    let internal NullRef (_ : state) (args : term list) : term =
        match args with
        | [{term = Concrete(:? Type as t, _)}] -> NullRef t
        | _ -> __unreachable__()

    let internal IsNullRef (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let ref = args[1]
        IsNullReference ref

    let internal AddByteOffset (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let ref, offset = args[1], args[2]
        PerformBinaryOperation OperationType.Add ref offset id

    let private CommonAdd typ ref offset =
        let size = getTypeFromTerm typ |> Types.SizeOf |> MakeNumber
        let byteOffset = Arithmetics.Mul offset size
        PerformBinaryOperation OperationType.Add ref byteOffset id

    let internal AddIntPtr (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let typ, ref, offset = args[0], args[1], args[2]
        CommonAdd typ ref offset

    let internal AddInt (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let typ, ref, offset = args[0], args[1], args[2]
        CommonAdd typ ref offset

    let internal ReadUnaligned (state : state) (args : term list) : term =
        assert(List.length args = 2)
        let typ, ref = args[0], args[1]
        let typ = getTypeFromTerm typ
        let castedPtr = Types.Cast ref (typ.MakePointerType())
        Memory.Read state castedPtr

    let WriteUnaligned (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 3)
        let typ, ref, value = args[0], args[1], args[2]
        let typ = getTypeFromTerm typ
        let castedPtr = Types.Cast ref (typ.MakePointerType())
        let states = Memory.Write state castedPtr value
        List.map (withFst <| Nop()) states

    let internal SizeOf (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let typ = getTypeFromTerm args[0]
        Types.SizeOf typ |> MakeNumber

    let internal AreSame (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let ptr1, ptr2 = args[1], args[2]
        ptr1 === ptr2

    let internal GetRawData (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let ref = args[0]
        match ref.term with
        | HeapRef(address, _) ->
            let t = MostConcreteTypeOfRef state ref
            Ptr (HeapLocation(address, t)) typeof<byte> (MakeNumber 0)
        | Ref(BoxedLocation(address, t)) ->
            Ptr (HeapLocation(address, t)) typeof<byte> (MakeNumber 0)
        | _ -> internalfail $"GetRawData: unexpected ref {ref}"

    let internal SkipInit (_ : state) (_ : term list) : term =
        Nop()
