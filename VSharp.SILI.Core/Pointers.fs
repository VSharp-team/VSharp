namespace VSharp.Core

open VSharp
open VSharp.TypeUtils
open VSharp.Core.Common

module internal Pointers =

// ------------------------ Basic pointer operations ------------------------

    let private sizeOfUnderlyingPointerType = term >> function // for `T* ptr` returns `sizeof(T)`
        | Ptr(_, Void, _) -> makeNumber 1
        | Ptr(_, typ, _) -> makeNumber (internalSizeOf typ)
        | t -> internalfailf $"Taking sizeof underlying type of not pointer type: {t}"

    // NOTE: returns 'ptr', shifted by 'shift' bytes
    let private shift ptr shift =
        match ptr.term with
        | Ptr(address, typ, shift') ->
            assert(typeOf shift' = typeof<int>)
            let shift = primitiveCast shift typeof<int>
            Ptr address typ (add shift' shift)
        | _ -> __unreachable__()

    let private getFieldOffset fieldId =
        Reflection.getFieldIdOffset fieldId |> makeNumber

    let rec addressToBaseAndOffset (address : address) =
        match address with
        | ClassField(heapAddress, field) ->
            HeapLocation(heapAddress, field.declaringType), getFieldOffset field
        | StructField(address, field) ->
            let baseAddress, structOffset = addressToBaseAndOffset address
            baseAddress, getFieldOffset field |> add structOffset
        | StaticField(symbolicType, field) ->
            // TODO: use specific 'getFieldOffset'
            StaticLocation symbolicType, getFieldOffset field
        // NOTE: only vector case
        | ArrayIndex(heapAddress, [index], ({elemType = elementType; isVector = true} as arrayType)) ->
            let sizeOfElement = internalSizeOf elementType |> makeNumber
            let typ = arrayTypeToSymbolicType arrayType
            HeapLocation(heapAddress, typ), mul index sizeOfElement
        // TODO: Address function should use Ptr instead of Ref
        | ArrayIndex _ -> internalfail "ref should not be used for multidimensional array index!"
        | BoxedLocation(heapAddress, typ) ->
            let baseAddress = HeapLocation(heapAddress, typ)
            baseAddress, makeNumber 0
        | StackBufferIndex _ -> internalfail "addressToBaseAndOffset: StackBufferIndex case is not implemented"
        | PrimitiveStackLocation loc ->
            StackLocation loc, makeNumber 0
        | _ -> __unreachable__()

// -------------------------- Comparison operations --------------------------

    let private isZero x = simplifyEqual x (zeroAddress()) id

    let isZeroAddress x = fastNumericCompare x (zeroAddress())

    let rec private compareAddress = function
        | PrimitiveStackLocation k1, PrimitiveStackLocation k2 -> makeBool (k1 = k2)
        | ClassField(addr1, f1), ClassField(addr2, f2) -> if f1 = f2 then simplifyEqual addr1 addr2 id else False()
        | StructField(addr1, f1), StructField(addr2, f2) -> if f1 = f2 then compareAddress(addr1, addr2) else False()
        | ArrayIndex(addr1, idcs1, t1), ArrayIndex(addr2, idcs2, t2) ->
            if t1 = t2 && idcs1.Length = idcs2.Length then simplifyEqual addr1 addr2 id
            else False()
        | StackBufferIndex(k1, i1), StackBufferIndex(k2, i2) -> makeBool (k1 = k2) &&& simplifyEqual i1 i2 id
        | BoxedLocation(addr1, _), BoxedLocation(addr2, _) -> makeBool (addr1 = addr2)
        | ArrayLength _, ArrayLength _
        | ArrayLowerBound _, ArrayLowerBound _
        | StaticField _, StaticField _ -> __unreachable__()
        | _ -> False()

    let rec private comparePointerBase left right =
        match left, right with
        | StackLocation loc1, StackLocation loc2 -> makeBool (loc1 = loc2)
        | HeapLocation(loc1, _), HeapLocation(loc2, _) -> simplifyEqual loc1 loc2 id
        | StaticLocation _, StaticLocation _ -> __unreachable__()
        | _ -> False()

    let simplifyReferenceEqualityExt x y k =
        match x.term, y.term with
        | _ when x = y -> k (True())
        | Ref address1, Ref address2 -> compareAddress (address1, address2) |> k
        | Ptr(address1, _, shift1), Ptr(address2, _, shift2) ->
            let addressEq = comparePointerBase address1 address2
            addressEq &&& simplifyEqual shift1 shift2 id |> k
        | Concrete(number, _), other
        | other, Concrete(number, _) ->
            let i = convert number typeof<int>
            match other with
            | DetachedPtr shift -> simplifyEqual shift (makeNumber i) k
            | Ptr(HeapLocation(address, _), _, shift) ->
                simplifyEqual address (zeroAddress()) id &&& simplifyEqual shift (makeNumber i) id |> k
            | HeapRef(address, _) when i = 0 -> simplifyEqual address (zeroAddress()) k
            | _ -> False() |> k
        | Ref _, Ptr _
        | Ptr _, Ref _ -> internalfail "comparison between ref and ptr is not implemented"
        | HeapRef(address1, _), HeapRef(address2, _) -> simplifyEqual address1 address2 k
        | Ptr(HeapLocation(addr, _), _, shift), HeapRef(term, _)
        | HeapRef(term, _), Ptr(HeapLocation(addr, _), _, shift) ->
            simplifyEqual addr term id &&& simplifyEqual shift (makeNumber 0) id |> k
        | _, DetachedPtr shift when typeOf x |> isNative ->
            simplifyEqual x shift k
        | DetachedPtr shift, _ when typeOf y |> isNative ->
            simplifyEqual shift y k
        | Ref(ArrayIndex(addr2, _, _)), HeapRef(addr1, _)
        | HeapRef(addr1, _), Ref(ArrayIndex(addr2, _, _)) when addr1 = zeroAddress() && addr2 = zeroAddress() ->
            True() |> k
        | _ -> False() |> k

    let rec simplifyReferenceEqualityk x y k =
        simplifyGenericBinary "reference comparison" x y k
            (fun _ _ _ -> __unreachable__())
            simplifyReferenceEqualityExt
            simplifyReferenceEqualityk

    let isNull heapReference =
        simplifyReferenceEqualityk heapReference (nullRef (typeOf heapReference)) id

    let rec isBadRef ref =
        match ref.term with
        | Ptr(HeapLocation(address, _), _, _) ->
            simplifyEqual address (zeroAddress()) id
        | Ptr _ -> False()
        | _ when isReference ref -> isNull ref
        | _ when typeOf ref |> isNative -> True()
        | Union gvs -> Merging.guardedMap isBadRef gvs
        | _ -> False()

// -------------------------- Address arithmetic --------------------------

    // NOTE: IL contains number (already in bytes) and pointer, that need to be shifted
    let private addNumberToPtr ptr bytesToShift k =
        let bytesToShift = primitiveCast bytesToShift typeof<int>
        match ptr.term with
        | _ when bytesToShift = makeNumber 0 -> k ptr
        | Ptr _ -> shift ptr bytesToShift |> k
        | Ref address ->
            let typ = address.TypeOfLocation
            let baseAddress, offset = addressToBaseAndOffset address
            let ptr = Ptr baseAddress typ offset
            shift ptr bytesToShift |> k
        | HeapRef(address, t) ->
            assert t.IsArray
            let ptrType = t.GetElementType()
            Ptr (HeapLocation(address, t)) ptrType bytesToShift |> k
        | _ -> internalfailf $"address arithmetic: expected pointer, but got {ptr}"

    // NOTE: IL contains number (already in bytes) and pointer, that need to be shifted
    let private multiplyPtrByNumber ptr number k =
        match ptr.term with
        | DetachedPtr offset ->
            mul offset number |> k
        | Ptr(pointerBase, _, offset) ->
            let baseNumber = pointerBase.GetHashCode() |> makeNumber
            let ptrNumber = add baseNumber offset
            mul ptrNumber number |> k
        | _ -> internalfail $"multiplyPtrByNumber: unexpected pointer {ptr}"

    let private simplifyOperands x y =
        let xType = typeOf x
        let yType = typeOf y
        match x.term, y.term with
        | _ when isNumeric yType -> x, y
        | _ when isNumeric xType -> y, x
        | _, DetachedPtr offset ->
            assert(isRefOrPtr x)
            x, offset
        | DetachedPtr offset, _ ->
            assert(isRefOrPtr y)
            y, offset
        | _ -> internalfail $"pointer arithmetic: unexpected operands {x} {y}"

    let rec private simplifyPointerAddition x y k =
        let ptr, number = simplifyOperands x y
        simplifyGenericBinary "add shift to pointer" ptr number k
            (fun _ _ _ -> __unreachable__())
            addNumberToPtr
            simplifyPointerAddition

    let rec private simplifyPointerMultiply x y k = // y must be normalized by Arithmetics!
        let ptr, number = simplifyOperands x y
        simplifyGenericBinary "multiply pointer by number" ptr number k
            (fun _ _ _ -> __unreachable__())
            multiplyPtrByNumber
            simplifyPointerMultiply

    let rec private simplifyPointerDivide op x y k =
        let x, y =
            match x.term, y.term with
            | DetachedPtr offset1, DetachedPtr offset2 -> offset1, offset2
            | DetachedPtr offset, _ -> offset, y
            | _, DetachedPtr offset -> x, offset
            | _ -> __insufficientInformation__ $"simplifyPointerDivide: {x} {y}"
        simplifyBinaryOperation op x y k

    let private toPointerIfNeeded x =
        match x.term with
        | HeapRef(address, t) -> Ptr (HeapLocation(address, t)) t (makeNumber 0)
        | Ref address ->
            let pointerBase, offset = addressToBaseAndOffset address
            Ptr pointerBase address.TypeOfLocation offset
        | Ptr _ -> x
        | _ when typeOf x |> isNative ->
            let offset = primitiveCast x typeof<int>
            makeDetachedPtr offset typeof<byte>
        | _ -> internalfail $"toPointerIfNeeded: unexpected reference {x}"

    let private pointerDifference x y k =
        let x = toPointerIfNeeded x
        let y = toPointerIfNeeded y
        match x.term, y.term with
        | Ptr(base1, _, offset1), Ptr(base2, _, offset2) when base1 = base2 ->
            sub offset1 offset2 |> k
        | Ptr(HeapLocation({term = ConcreteHeapAddress _}, _), _, _), Ptr(HeapLocation({term = ConcreteHeapAddress _}, _), _, _) ->
            undefinedBehaviour "trying to get pointer difference between different pointer bases"
        | _ -> __insufficientInformation__ "need more information about pointer address for pointer difference"

    let rec private commonPointerSubtraction x y k =
        simplifyGenericBinary "pointer1 - pointer2" x y k
            (fun _ _ _ -> __unreachable__())
            pointerDifference
            commonPointerSubtraction

    let private simplifyPointerSubtraction x y k =
        if Terms.isNumeric y then simplifyPointerAddition x (neg y) k
        elif Terms.isNumeric x then simplifyPointerAddition (neg x) y k
        else commonPointerSubtraction x y k

    let private pointerIdOfBaseAndOffset pointerBase offset =
        match pointerBase with
        | HeapLocation({term = ConcreteHeapAddress a}, _) ->
            let pointerBase = convert (VectorTime.hash a) typeof<int> |> makeNumber
            add pointerBase offset
        | _ -> __insufficientInformation__ $"pointerId: unable to get pointer ID of pointerBase {pointerBase}"

    let private pointerId ptr =
        match ptr.term with
        | DetachedPtr offset -> offset
        | _ when typeOf ptr |> isNative ->
            primitiveCast ptr typeof<int>
        | Ptr(pointerBase, _, o) ->
            pointerIdOfBaseAndOffset pointerBase o
        | Ref address ->
            let pointerBase, offset = addressToBaseAndOffset address
            pointerIdOfBaseAndOffset pointerBase offset
        | HeapRef({term = ConcreteHeapAddress a}, _) ->
            convert (VectorTime.hash a) typeof<int> |> makeNumber
        | _ -> __insufficientInformation__ $"pointerId: unable to get pointer ID of symbolic pointer {ptr}"

    let private simplifyPointerComparison op x y k =
        match x.term, y.term with
        | Ptr(pointerBase1, _, offset1), Ptr(pointerBase2, _, offset2) when pointerBase1 = pointerBase2 ->
            simplifyBinaryOperation op offset1 offset2 k
        | _ ->
            let number1 = pointerId x
            let number2 = pointerId y
            simplifyBinaryOperation op number1 number2 k

    let simplifyBinaryOperation op x y k =
        match op with
        | OperationType.Subtract ->
            simplifyPointerSubtraction x y k
        | OperationType.Add ->
            simplifyPointerAddition x y k
        | OperationType.Multiply ->
            simplifyPointerMultiply x y k
        | OperationType.Divide
        | OperationType.Divide_Un -> simplifyPointerDivide op x y k
        | OperationType.Equal -> simplifyReferenceEqualityk x y k
        | OperationType.NotEqual ->
            simplifyReferenceEqualityk x y (fun e ->
            simplifyNegation e k)
        | OperationType.Less
        | OperationType.Less_Un
        | OperationType.LessOrEqual
        | OperationType.LessOrEqual_Un
        | OperationType.Greater
        | OperationType.Greater_Un
        | OperationType.GreaterOrEqual
        | OperationType.GreaterOrEqual_Un ->
            simplifyPointerComparison op x y k
        | _ -> internalfailf $"{op} is not a binary arithmetical operator"

    let isPointerOperation op left right =
        match op with
        | OperationType.Subtract
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.Add
        | OperationType.Multiply
        | OperationType.Divide
        | OperationType.Divide_Un
        | OperationType.Less
        | OperationType.Less_Un
        | OperationType.LessOrEqual
        | OperationType.LessOrEqual_Un
        | OperationType.Greater
        | OperationType.Greater_Un
        | OperationType.GreaterOrEqual
        | OperationType.GreaterOrEqual_Un -> isRefOrPtr left || isRefOrPtr right
        | _ -> false
