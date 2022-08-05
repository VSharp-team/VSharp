namespace VSharp.Core

open VSharp

module internal PhysToVirt =

    let find (state : state) obj = PersistentDict.find state.physToVirt {object = obj}

module internal ConcreteMemory =

// ----------------------------- Primitives -----------------------------

    let deepCopy (state : state) =
        let cm = state.concreteMemory
        let cm' = System.Collections.Generic.Dictionary<concreteHeapAddress, physicalAddress>()
        let cmSeq = Seq.map (|KeyValue|) cm
        let updateOne acc (k, v : physicalAddress) =
            let v' = {object = v.object} // TODO: deep copy object
            cm'.Add(k, v')
            PersistentDict.add v' k acc
        let physToVirt = Seq.fold updateOne PersistentDict.empty cmSeq
        state.physToVirt <- physToVirt
        { state with state.concreteMemory = cm' }

    let getObject (physicalAddress : physicalAddress) = physicalAddress.object

    let contains (cm : concreteMemory) address =
        cm.ContainsKey address

    let tryFind (cm : concreteMemory) address =
        let result = ref {object = null}
        if cm.TryGetValue(address, result) then
            getObject result.Value |> Some
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

    let private getArrayIndicesWithValues (array : System.Array) =
        let ubs = List.init array.Rank array.GetUpperBound
        let lbs = List.init array.Rank array.GetLowerBound
        let indices = List.map2 (fun lb ub -> [lb .. ub]) lbs ubs |> List.cartesian
        indices |> Seq.map (fun index -> index, array.GetValue(Array.ofList index))

    let getAllArrayData (cm : concreteMemory) address =
        match readObject cm address with
        | :? System.Array as array -> getArrayIndicesWithValues array
        | :? System.String as string -> string.ToCharArray() |> getArrayIndicesWithValues
        | obj -> internalfailf "reading array data concrete memory: expected to read array, but got %O" obj

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
        // TODO: strings must be immutable! This is used by copying, so copy string another way #hack
        | :? System.String as string when List.length indices = 1 ->
            let charArray = string.ToCharArray()
            charArray.SetValue(value, List.head indices)
            let newString = System.String(charArray)
            writeObject state address newString
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
