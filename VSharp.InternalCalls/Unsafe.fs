namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

// ------------------------------ System.Unsafe --------------------------------

module internal Unsafe =

    let AsPointer (_ : state) (args : term list) : term =
        assert(List.length args = 2)
//        Types.Cast (List.item 1 args) (Pointer Void)
        List.item 1 args

    let ObjectAsT (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let typ, ref = args[0], args[1]
        let typ = Helpers.unwrapType typ
        Types.Cast ref typ

    let AsRef (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let t = Helpers.unwrapType args[0]
        let ptr = args[1]
        Types.Cast ptr (t.MakePointerType())

    let PointerAsRef (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        args[1]

    let TFromAsTTo (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let toType = Helpers.unwrapType args[1]
        let pointerType = toType.MakePointerType()
        let ref = args[2]
        assert(IsReference ref || IsPtr ref)
        Types.Cast ref pointerType

    let NullRef (_ : state) (args : term list) : term =
        match args with
        | [{term = Concrete(:? Type as t, _)}] -> NullRef t
        | _ -> __unreachable__()

    let IsNullRef (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let ref = args[1]
        IsNullReference ref

    let AddByteOffset (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let ref, offset = args[1], args[2]
        PerformBinaryOperation OperationType.Add ref offset id

    let ByteOffset (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let origin, target = args[0], args[1]
        let offset = PerformBinaryOperation OperationType.Subtract target origin id
        Types.Cast offset typeof<IntPtr>

    let private CommonAdd typ ref offset =
        let size = Helpers.unwrapType typ |> Types.SizeOf |> MakeNumber
        let byteOffset = Arithmetics.Mul offset size
        PerformBinaryOperation OperationType.Add ref byteOffset id

    let AddIntPtr (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let typ, ref, offset = args[0], args[1], args[2]
        CommonAdd typ ref offset

    let AddInt (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let typ, ref, offset = args[0], args[1], args[2]
        CommonAdd typ ref offset

    let ReadUnaligned (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let typ, ref = args[0], args[1]
        let typ = Helpers.unwrapType typ
        let castedPtr = Types.Cast ref (typ.MakePointerType())
        let readByPtr (cilState : cilState) k =
            let value = cilState.Read castedPtr
            cilState.Push value
            List.singleton cilState |> k
        i.NpeOrInvoke cilState castedPtr readByPtr id

    let WriteUnalignedGeneric (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 3)
        let typ, ref, value = args[0], args[1], args[2]
        let typ = Helpers.unwrapType typ
        let castedPtr = Types.Cast ref (typ.MakePointerType())
        let writeByPtr (cilState : cilState) k =
            cilState.Write castedPtr value
            List.singleton cilState |> k
        i.NpeOrInvoke cilState castedPtr writeByPtr id

    let WriteUnaligned (i : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 2)
        let ref, value = args[0], args[1]
        let writeByPtr (cilState : cilState) k =
            cilState.Write ref value
            List.singleton cilState |> k
        i.NpeOrInvoke cilState ref writeByPtr id

    let SizeOf (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let typ = Helpers.unwrapType args[0]
        Types.SizeOf typ |> MakeNumber

    let AreSame (_ : state) (args : term list) : term =
        assert(List.length args = 3)
        let ptr1, ptr2 = args[1], args[2]
        ptr1 === ptr2

    let GetRawData (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let ref = args[0]
        match ref.term with
        | HeapRef(address, _) ->
            let t = MostConcreteTypeOfRef state ref
            Ptr (HeapLocation(address, t)) typeof<byte> (MakeNumber 0)
        | Ref(BoxedLocation(address, t)) ->
            Ptr (HeapLocation(address, t)) typeof<byte> (MakeNumber 0)
        | Ptr(pointerBase, _, offset) ->
            Ptr pointerBase typeof<byte> offset
        | _ -> internalfail $"GetRawData: unexpected ref {ref}"

    let SkipInit (_ : state) (_ : term list) : term =
        Nop()
