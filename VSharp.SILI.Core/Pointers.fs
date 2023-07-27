namespace VSharp.Core

open VSharp
open VSharp.TypeUtils
open VSharp.Core.Common

module internal Pointers =

// ------------------------ Basic pointer operations ------------------------

    let private sizeOfUnderlyingPointerType = term >> function // for `T* ptr` returns `sizeof(T)`
        | Ptr(_, Void, _) -> makeNumber 1
        | Ptr(_, typ, _) -> makeNumber (internalSizeOf typ)
        | t -> internalfailf "Taking sizeof underlying type of not pointer type: %O" t

    // NOTE: returns 'ptr', shifted by 'shift' bytes
    let private shift ptr shift =
        match ptr.term with
        | Ptr(address, typ, shift') -> Ptr address typ (add shift' shift)
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
            StaticLocation symbolicType, getFieldOffset field
        // NOTE: only vector case
        | ArrayIndex(heapAddress, [index], (elementType, _, true as arrayType)) ->
            let sizeOfElement = internalSizeOf elementType |> makeNumber
            let typ = arrayTypeToSymbolicType arrayType
            HeapLocation(heapAddress, typ), mul index sizeOfElement
        // TODO: Address function should use Ptr instead of Ref
        | ArrayIndex _ -> internalfail "ref should not be used for multidimensional array index!"
        | BoxedLocation(concreteHeapAddress, typ) ->
            let baseAddress = HeapLocation(ConcreteHeapAddress concreteHeapAddress, typ)
            baseAddress, makeNumber 0
        | StackBufferIndex _ -> internalfail "addressToBaseAndOffset: StackBufferIndex case is not implemented"
        | PrimitiveStackLocation loc ->
            StackLocation loc, makeNumber 0
        | _ -> __unreachable__()

// -------------------------- Comparison operations --------------------------

    let private isZero x = simplifyEqual x zeroAddress id

    let isZeroAddress x = fastNumericCompare x zeroAddress

    let rec private compareAddress = function
        | PrimitiveStackLocation k1, PrimitiveStackLocation k2 -> makeBool (k1 = k2)
        | ClassField(addr1, f1), ClassField(addr2, f2) -> if f1 = f2 then simplifyEqual addr1 addr2 id else False
        | StructField(addr1, f1), StructField(addr2, f2) -> if f1 = f2 then compareAddress(addr1, addr2) else False
        | ArrayIndex(addr1, idcs1, t1), ArrayIndex(addr2, idcs2, t2) ->
            if t1 = t2 && idcs1.Length = idcs2.Length then simplifyEqual addr1 addr2 id
            else False
        | StackBufferIndex(k1, i1), StackBufferIndex(k2, i2) -> makeBool (k1 = k2) &&& simplifyEqual i1 i2 id
        | BoxedLocation(addr1, _), BoxedLocation(addr2, _) -> makeBool (addr1 = addr2)
        | ArrayLength _, ArrayLength _
        | ArrayLowerBound _, ArrayLowerBound _
        | StaticField _, StaticField _ -> __unreachable__()
        | _ -> False

    let rec private comparePointerBase left right =
        match left, right with
        | StackLocation loc1, StackLocation loc2 -> makeBool (loc1 = loc2)
        | HeapLocation(loc1, _), HeapLocation(loc2, _) -> simplifyEqual loc1 loc2 id
        | StaticLocation _, StaticLocation _ -> __unreachable__()
        | _ -> False

    let rec simplifyReferenceEqualityk x y k =
        simplifyGenericBinary "reference comparison" x y k
            (fun _ _ _ -> __unreachable__())
            (fun x y k ->
                match x.term, y.term with
                | _ when x = y -> k True
                | Ref address1, Ref address2 -> compareAddress (address1, address2) |> k
                | Ptr(address1, _, shift1), Ptr(address2, _, shift2) ->
                    let addressEq = comparePointerBase address1 address2
                    addressEq &&& simplifyEqual shift1 shift2 id |> k
                | Concrete(:? int as i, _), Ptr(HeapLocation(address, _), _, shift)
                | Ptr(HeapLocation(address, _), _, shift), Concrete(:? int as i, _) when i = 0 ->
                    simplifyEqual address zeroAddress id &&& simplifyEqual shift (makeNumber 0) id |> k
                | Concrete(:? int as i, _), HeapRef(address, _)
                | HeapRef(address, _), Concrete(:? int as i, _) when i = 0 -> simplifyEqual address zeroAddress k
                | Ref _, Ptr _
                | Ptr _, Ref _ -> internalfail "comparison between ref and ptr is not implemented"
                | HeapRef(address1, _), HeapRef(address2, _) -> simplifyEqual address1 address2 k
                | Ptr(HeapLocation(addr, _), _, shift), HeapRef(term, _)
                | HeapRef(term, _), Ptr(HeapLocation(addr, _), _, shift) ->
                    simplifyEqual addr term id &&& simplifyEqual shift (makeNumber 0) id |> k
                | _ -> False |> k)
            (fun x y k -> simplifyReferenceEqualityk x y k)

    let isNull heapReference =
        simplifyReferenceEqualityk heapReference (nullRef (typeOf heapReference)) id

// -------------------------- Address arithmetic --------------------------

    // NOTE: IL contains number (already in bytes) and pointer, that need to be shifted
    let private addNumberToPtr ptr bytesToShift k =
        match ptr.term with
        | _ when bytesToShift = makeNumber 0 -> k ptr
        | Ptr _ -> shift ptr bytesToShift |> k
        | Ref address ->
            let typ = typeOfAddress address
            let baseAddress, offset = addressToBaseAndOffset address
            let ptr = Ptr baseAddress typ offset
            shift ptr bytesToShift |> k
        | _ -> internalfailf "address arithmetic: expected pointer, but got %O" ptr

    // NOTE: IL contains number (already in bytes) and pointer, that need to be shifted
    let private multiplyPtrByNumber ptr number k =
        match ptr.term with
        | DetachedPtr offset ->
            mul offset number |> k
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

    let private pointerDifference x y k =
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
        if Terms.isNumeric y
        then simplifyPointerAddition x (neg y) k
        else commonPointerSubtraction x y k

    let simplifyBinaryOperation op x y k =
        match op with
        | OperationType.Subtract ->
            simplifyPointerSubtraction x y k
        | OperationType.Add ->
            simplifyPointerAddition x y k
        | OperationType.Multiply ->
            simplifyPointerMultiply x y k
        | OperationType.Equal -> simplifyReferenceEqualityk x y k
        | OperationType.NotEqual ->
            simplifyReferenceEqualityk x y (fun e ->
            simplifyNegation e k)
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let isPointerOperation op t1 t2 =
        let isRefOrPtr = function
            | ReferenceType _ -> true
            | t -> t.IsByRef || isPointer t
        match op with
        | OperationType.Equal
        | OperationType.NotEqual -> isRefOrPtr t1 || isRefOrPtr t2
        | OperationType.Subtract -> isRefOrPtr t1 && (isRefOrPtr t2 || isNumeric t2)
        | OperationType.Add -> isRefOrPtr t1 || isRefOrPtr t2
        | OperationType.Multiply -> isRefOrPtr t1 || isRefOrPtr t2
        | _ -> false
