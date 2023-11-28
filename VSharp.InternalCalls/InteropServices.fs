namespace VSharp.System

open System
open System.Runtime.InteropServices
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilStateOperations

module internal InteropServices =

    let private marshalType = typeof<Marshal>

    let private lastPInvokeErrorFieldId =
        {
            declaringType = marshalType
            name = "__System.Runtime.InteropServices.Marshal.LastPInvokeError__"
            typ = typeof<int>
        }

    let private lastSystemErrorFieldId =
        {
            declaringType = marshalType
            name = "__System.Runtime.InteropServices.Marshal.LastSystemError__"
            typ = typeof<int>
        }

    let GetArrayDataReference (state : state) (args : term list) =
        assert(List.length args = 2)
        let array = args[1]
        Memory.ReferenceArrayIndex state array [MakeNumber 0] None

    let Free (_ : state) (args : term list) =
        assert(List.length args = 1)
        // TODO: add checks
        Nop()

    let AlignedAlloc (state : state) (args : term list) =
        assert(List.length args = 2)
        let count = Types.Cast args[0] typeof<int>
        let ref = Memory.AllocateVectorArray state count typeof<byte>
        match ref.term with
        | HeapRef(address, sightType) ->
            let pointerBase = HeapLocation(address, sightType)
            let t = typeof<byte>.MakePointerType()
            let offset = MakeNumber 0
            Ptr pointerBase t offset
        | _ -> internalfail $"AlignedAlloc: unexpected array reference {ref}"

    let private isHandleField fieldId =
        fieldId.name = "_handle"

    let private gcHandleType = typeof<System.Runtime.InteropServices.GCHandle>

    let private gcHandleField =
        lazy(
            let fields = Reflection.fieldsOf false gcHandleType
            Array.find (fst >> isHandleField) fields |> fst
        )

    let private CommonAlloc obj =
        let gcHandle = Memory.DefaultOf gcHandleType
        let handleField = gcHandleField.Value
        Memory.WriteStructField gcHandle handleField obj

    let GCHandleAllocWithType (_ : state) (args : term list) =
        assert(List.length args = 2)
        let obj = args[0]
        CommonAlloc obj

    let GCHandleIsPinned (_ : state) (args : term list) =
        assert(List.length args = 1)
        True()

    let GCHandleGetHandleValue (_ : state) (args : term list) =
        assert(List.length args = 1)
        let ptr = args[0]
        ptr

    let GCHandleAlloc (_ : state) (args : term list) =
        assert(List.length args = 1)
        let obj = args[0]
        CommonAlloc obj

    let GCHandleInternalGet (_ : IInterpreter) cilState args =
        assert(List.length args = 1)
        let ptr = args[0]
        let obj = read cilState ptr
        push obj cilState
        List.singleton cilState

    let GCHandleFree (_ : state) (args : term list) =
        assert(List.length args = 1)
        Nop()

    let AddrOfPinnedObject (state : state) (args : term list) =
        assert(List.length args = 1)
        let this = args[0]
        let gcHandle = Memory.Read state this
        let handleField = gcHandleField.Value
        Memory.ReadField state gcHandle handleField

    let TypeHandleGetGCHandle (state : state) (args : term list) =
        assert(List.length args = 2)
        let this = args[0]
        match Memory.Read state this |> TryTermToObj state with
        | Some obj ->
            assert(obj :? RuntimeTypeHandle)
            let rth = obj :?> RuntimeTypeHandle
            let t = global.System.Type.GetTypeFromHandle rth
            Memory.ObjectToTerm state t typeof<Type>
        | None -> internalfail "TypeHandleGetGCHandle: symbolic runtime type handle"

    let SetLastPInvokeError (state : state) (args : term list) =
        assert(List.length args = 1)
        let error = args[0]
        Memory.WriteStaticField state marshalType lastPInvokeErrorFieldId error
        Nop()

    let GetLastPInvokeError (state : state) (args : term list) =
        assert(List.isEmpty args)
        Memory.ReadStaticField state marshalType lastPInvokeErrorFieldId

    let SetLastSystemError (state : state) (args : term list) =
        assert(List.length args = 1)
        let error = args[0]
        Memory.WriteStaticField state marshalType lastSystemErrorFieldId error
        Nop()

    let GetLastSystemError (state : state) (args : term list) =
        assert(List.isEmpty args)
        Memory.ReadStaticField state marshalType lastSystemErrorFieldId
