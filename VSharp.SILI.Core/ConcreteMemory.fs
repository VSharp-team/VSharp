namespace VSharp.Core

open VSharp

module internal PhysToVirt =

    let find (state : state) obj = PersistentDict.find state.physToVirt {object = obj}

module internal ConcreteMemory =

// ----------------------------- Primitives -----------------------------

    let getObject (physicalAddress : physicalAddress) = physicalAddress.object

    let contains (cm : concreteMemory) address =
        cm.ContainsKey address

    let tryFind (cm : concreteMemory) address =
        let result = ref {object = null}
        if cm.TryGetValue(address, result) then
            getObject !result |> Some
        else None

// ----------------------------- Allocation -----------------------------

    let allocate (state : state) address (obj : obj) =
        let cm = state.concreteMemory
        assert(cm.ContainsKey address |> not)
        let physicalAddress = {object = obj}
        cm.Add(address, physicalAddress)
        state.physToVirt <- PersistentDict.add physicalAddress address state.physToVirt

// ------------------------------- Reading -------------------------------

    let readObject (cm : concreteMemory) address =
        assert(cm.ContainsKey address)
        cm.[address] |> getObject

    let readClassField (cm : concreteMemory) address (field : fieldId) =
        let object = readObject cm address
        let fieldInfo = Reflection.getFieldInfo field
        fieldInfo.GetValue(object)

    let readArrayIndex (cm : concreteMemory) address (indices : int list) =
        match readObject cm address with
        | :? System.Array as array -> array.GetValue(Array.ofList indices)
        | :? System.String as string when List.length indices = 1 -> string.[List.head indices] :> obj
        | obj -> internalfailf "reading array index from concrete memory: expected to read array, but got %O" obj

    let readArrayLowerBound (cm : concreteMemory) address dimension =
        match readObject cm address with
        | :? System.Array as array -> array.GetLowerBound(dimension)
        | :? System.String when dimension = 0 -> 0
        | obj -> internalfailf "reading array lower bound from concrete memory: expected to read array, but got %O" obj

    let readArrayLength (cm : concreteMemory) address dimension =
        match readObject cm address with
        | :? System.Array as array -> array.GetLength(dimension)
        | :? System.String as string when dimension = 0 -> string.Length
        | obj -> internalfailf "reading array length from concrete memory: expected to read array, but got %O" obj

// ------------------------------- Writing -------------------------------

    let private writeObject (state : state) address obj =
        let cm = state.concreteMemory
        assert(cm.ContainsKey address)
        let physicalAddress = {object = obj}
        state.concreteMemory.[address] <- physicalAddress

    let writeClassField (state : state) address (field : fieldId) value =
        let object = readObject state.concreteMemory address
        let fieldInfo = Reflection.getFieldInfo field
        fieldInfo.SetValue(object, value)
        writeObject state address object

    let writeArrayIndex (state : state) address (indices : int list) value =
        match readObject state.concreteMemory address with
        | :? System.Array as array ->
            array.SetValue(value, Array.ofList indices)
            writeObject state address array
        | obj -> internalfailf "writing array index to concrete memory: expected to read array, but got %O" obj

    let initializeArray (state : state) address (rfh : System.RuntimeFieldHandle) =
        match readObject state.concreteMemory address with
        | :? System.Array as array ->
            System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(array, rfh)
            writeObject state address array
        | obj -> internalfailf "initializing array in concrete memory: expected to read array, but got %O" obj

    let copyCharArrayToString (state : state) arrayAddress stringAddress =
        let array = readObject state.concreteMemory arrayAddress :?> char array
        let string = new string(array) :> obj
        writeObject state stringAddress string
        state.physToVirt <- PersistentDict.add {object = string} stringAddress state.physToVirt

// ------------------------------- Remove -------------------------------

    let remove (state : state) address =
        let cm = state.concreteMemory
        let object = readObject cm address
        let removed = state.concreteMemory.Remove address
        assert removed
        state.physToVirt <- PersistentDict.remove {object = object} state.physToVirt
