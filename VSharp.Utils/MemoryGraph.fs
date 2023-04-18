namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open System.Xml.Serialization
open Microsoft.FSharp.Collections

open VSharp

[<CLIMutable>]
[<Serializable>]
type typeRepr = {
    assemblyName : string
    moduleFullyQualifiedName : string
    name : string
    genericArgs : typeRepr array
}
with
    static member Encode(t : Type) =
        if t = null then
            {
                assemblyName = null
                moduleFullyQualifiedName = null
                name = null
                genericArgs = null
            }
        else
            let name, arguments =
                if t.IsGenericType then
                    if not t.IsConstructedGenericType then
                        internalfail "Encoding not constructed generic types not supported"

                    let arguments =
                        t.GetGenericArguments()
                        |> Seq.map typeRepr.Encode
                        |> Seq.toArray
                    let name = t.GetGenericTypeDefinition().FullName
                    name, arguments
                else
                    t.FullName, null

            {
                assemblyName = t.Module.Assembly.FullName
                moduleFullyQualifiedName = t.Module.FullyQualifiedName
                name = name
                genericArgs = arguments
            }

    member x.Decode() =
        let rec decodeTypeRec (t : typeRepr) =
            let mdle = Reflection.resolveModule t.assemblyName t.moduleFullyQualifiedName
            let typ = mdle.GetType t.name
            Debug.Assert(typ <> null)

            if typ.IsGenericType then
                Debug.Assert(t.genericArgs <> null && typ.GetGenericArguments().Length = t.genericArgs.Length)

                let args = t.genericArgs |> Seq.map decodeTypeRec |> Seq.toArray
                typ.MakeGenericType args
            else
                typ
        if x.assemblyName = null then
            null
        else
            let decodedType = decodeTypeRec x
            AssemblyManager.NormalizeType decodedType

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<typeRepr>)>]
type methodRepr = {
    declaringType : typeRepr
    token : int
}
with
    static member Encode(m : MethodBase) : methodRepr =
        {
            declaringType = typeRepr.Encode m.DeclaringType
            token = m.MetadataToken
        }

    member x.Decode() =
        let declaringType = x.declaringType.Decode()
        declaringType.GetMethods() |> Seq.find (fun m -> m.MetadataToken = x.token)

[<CLIMutable>]
[<Serializable>]
type referenceRepr = {
    index : int
}

[<CLIMutable>]
[<Serializable>]
type enumRepr = {
    typ : int
    underlyingValue : obj
}

[<CLIMutable>]
[<Serializable>]
type pointerRepr = {
    index : int
    shift : int
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
type structureRepr = {
    typ : int
    fields : obj array
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
// If indices = null, then values is just the whole content of an array.
// Otherwise, indices.Length = values.Length is guaranteed, and the array can be decoded by filling
//    the whole array with defaultValue and then synchronously writing values into indices
type arrayRepr = {
    typ : int
    defaultValue : obj
    indices : int array array
    values : obj array
    lengths : int array
    lowerBounds : int array
}

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<methodRepr>)>]
type typeMockRepr = {
    name : string
    baseClass : typeRepr
    interfaces : typeRepr array
    baseMethods : methodRepr array
    methodImplementations : obj array array
}
with
    static member NullRepr =
        {
            name = null
            baseClass = typeRepr.Encode null
            interfaces = [||]
            baseMethods = [||]
            methodImplementations = [||]
        }

    static member Encode (t : Mocking.Type) (encode : obj -> obj) =
        {
            name = t.Id
            baseClass = typeRepr.Encode t.BaseClass
            interfaces =
                t.Interfaces |> Seq.map typeRepr.Encode |> Array.ofSeq
            baseMethods =
                t.MethodMocks |> Seq.map (fun m -> methodRepr.Encode m.BaseMethod) |> Array.ofSeq
            methodImplementations =
                t.MethodMocks |> Seq.map (fun m -> m.ReturnValues |> Array.map encode) |> Array.ofSeq
        }

    member x.Decode() =
        let baseClass = x.baseClass.Decode()
        let interfaces = x.interfaces |> Array.map (fun i -> i.Decode())
        let baseMethods = x.baseMethods |> Array.map (fun m -> m.Decode())
        Mocking.Type.Deserialize x.name baseClass interfaces baseMethods x.methodImplementations

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<typeMockRepr>)>]
type memoryRepr = {
    objects : obj array
    types : typeRepr array
}

type public CompactArrayRepr = {
    array : Array
    defaultValue : obj
    indices : int array array
    values : obj array
}

type MockStorage() =
    let mocker = Mocking.Mocker()
    let mockedTypes = List<Mocking.Type>()

    member x.Deserialize(typeMocks : typeMockRepr array) =
        typeMocks
        |> Array.map (fun r -> r.Decode())
        |> mockedTypes.AddRange

    member x.RegisterMockedType (typ : Mocking.Type) =
        match mockedTypes |> Seq.tryFindIndex ((=) typ) with
        | Some idx -> -idx - 1
        | None ->
            mockedTypes.Add(typ)
            -mockedTypes.Count

    member x.Item(index : int) : Mocking.Type * Type =
        let mockedType = mockedTypes[-index - 1]
        mockedType, mocker.BuildDynamicType mockedType

    member x.TypeMocks
        with get() = mockedTypes

type MemoryGraph(repr : memoryRepr, mockStorage : MockStorage, createCompactRepr : bool) =

    let sourceTypes = List<Type>(repr.types |> Array.map (fun t -> t.Decode()))
    let compactRepresentations = Dictionary<obj, CompactArrayRepr>()

    let createMockObject decode index =
        let mockType, t = mockStorage[index]
        mockType.EnsureInitialized decode t
        let baseClass = mockType.BaseClass
        if TypeUtils.isDelegate baseClass then Mocking.Mocker.CreateDelegate baseClass t
        else Reflection.createObject t

    let rec allocateDefault (obj : obj) =
        match obj with
        | null
        | :? referenceRepr -> null
        | :? structureRepr as repr when repr.typ >= 0 ->
            // Case for structs or classes of .NET type
            let t = sourceTypes[repr.typ]
            if t.IsByRefLike then
                internalfailf "Generating test: unable to create byref-like object (type = %O)" t
            if t.ContainsGenericParameters then
                internalfailf "Generating test: unable to create object with generic type parameters (type = %O)" t
            else System.Runtime.Serialization.FormatterServices.GetUninitializedObject(t)
        | :? structureRepr as repr ->
            // Case for mocked structs or classes
            createMockObject allocateDefault repr.typ
        | :? arrayRepr as repr ->
            let t = sourceTypes[repr.typ]
            let elementType = t.GetElementType()
            if repr.lowerBounds = null then Array.CreateInstance(elementType, repr.lengths) :> obj
            else Array.CreateInstance(elementType, repr.lengths, repr.lowerBounds) :> obj
        | _ -> obj

    let sourceObjects = List<obj>(repr.objects |> Array.map allocateDefault)
    let objReprs = List<obj>(repr.objects)

    let rec decodeValue (obj : obj) =
        match obj with
        | :? referenceRepr as repr ->
            sourceObjects[repr.index]
        | :? pointerRepr -> __notImplemented__()
        | :? structureRepr as repr when repr.typ >= 0 ->
            // Case for structs or classes of .NET type
            let t = sourceTypes[repr.typ]
            if not t.IsValueType then
                internalfailf "Expected value type inside object, but got representation of %s!" t.FullName
            let obj = allocateDefault repr
            decodeStructure repr obj
            obj
        | :? structureRepr as repr ->
            // Case for mocked structs or classes
            let obj = createMockObject decodeValue repr.typ
            decodeMockedStructure repr obj
            obj
        | :? arrayRepr -> internalfail "Unexpected array representation inside object!"
        | :? enumRepr as repr ->
            let t = sourceTypes[repr.typ]
            Enum.ToObject(t, repr.underlyingValue)
        | _ -> obj

    and decodeFields (fieldsRepr : obj array) obj t : unit =
        let fields = Reflection.fieldsOf false t
        assert(Array.length fields = Array.length fieldsRepr)
        let decodeField (_, field : FieldInfo) repr =
            let value = decodeValue repr
            field.SetValue(obj, value)
        Array.iter2 decodeField fields fieldsRepr

    and decodeStructure (repr : structureRepr) obj : unit =
        let t = obj.GetType()
        decodeFields repr.fields obj t

    and decodeMockedStructure (repr : structureRepr) obj : unit =
        let fieldsRepr = repr.fields
        if Array.isEmpty fieldsRepr |> not then
            let t = obj.GetType().BaseType
            decodeFields repr.fields obj t

    and decodeArray (repr : arrayRepr) (obj : obj) : unit =
        assert(repr.lowerBounds = null || repr.lengths.Length = repr.lowerBounds.Length)
        let arr = obj :?> Array
        match repr with
        | _ when repr.indices = null ->
            assert(arr.Length = repr.values.Length)
            match repr.lengths, repr.lowerBounds with
            | [|len|], null ->
                let arr = obj :?> Array
                assert(arr.Length = len)
                repr.values |> Array.iteri (fun i r -> arr.SetValue(decodeValue r, i))
            | lens, lbs ->
                repr.values |> Array.iteri (fun i r ->
                    let value = decodeValue r
                    let indices = Array.delinearizeArrayIndex i lens lbs
                    arr.SetValue(value, indices))
        | _ ->
            let defaultValue = decodeValue repr.defaultValue
            let values = Array.map decodeValue repr.values
            Array.fill arr defaultValue
            Array.iter2 (fun (i : int[]) v -> arr.SetValue(v, i)) repr.indices values
            if createCompactRepr then
                let compactRepr = {array = arr; defaultValue = defaultValue; indices = repr.indices; values = values}
                compactRepresentations.Add(arr, compactRepr)

    and decodeObject (repr : obj) (obj : obj) =
        match repr with
        | :? structureRepr as repr when repr.typ >= 0 ->
            // Case for structs or classes of .NET type
            decodeStructure repr obj
        | :? structureRepr as repr ->
            // Case for mocked structs or classes
            let mockType, t = mockStorage[repr.typ]
            let mockInstanceType =
                match obj with
                | :? Delegate as d -> d.Method.DeclaringType
                | _ -> obj.GetType()
            assert(t = mockInstanceType)
            mockType.Update decodeValue mockInstanceType
            decodeMockedStructure repr obj
        | :? arrayRepr as repr ->
            decodeArray repr obj
        | _ -> ()

    let () = Seq.iter2 decodeObject objReprs sourceObjects

    member x.DecodeValue (obj : obj) = decodeValue obj

    member x.CompactRepresentations() = compactRepresentations

    member private x.IsSerializable (t : Type) =
        // TODO: find out which types can be serialized by XMLSerializer
        (t.IsPrimitive && not t.IsEnum) || t = typeof<string> || (t.IsArray && (x.IsSerializable <| t.GetElementType()))

    member private x.CreateArray (arr : Array) =
        let lowerBounds =
            if arr.Rank = 1 && arr.GetLowerBound 0 = 0 then null
            else Array.init arr.Rank arr.GetLowerBound
        let repr : arrayRepr =
            {
                typ = x.RegisterType (arr.GetType())
                defaultValue = null
                indices = null
                values = Array.empty
                lengths = Array.init arr.Rank arr.GetLength
                lowerBounds = lowerBounds
            }
        repr :> obj

    member private x.InitArray (arr : Array) index =
        let contents =
            seq {
                for elem in arr do
                    yield x.Encode elem
            } |> Array.ofSeq
        let encoded = objReprs[index]
        assert(encoded :? arrayRepr)
        let encodedArray = encoded :?> arrayRepr
        objReprs[index] <- {encodedArray with values = contents}

    member private x.RegisterType (typ : Type) =
        match sourceTypes |> Seq.tryFindIndex ((=) typ) with
        | Some idx -> idx
        | None ->
            sourceTypes.Add(typ)
            sourceTypes.Count - 1

    member private x.CreateStructure (obj : obj) =
        let t = obj.GetType()
        let repr : structureRepr = {typ = x.RegisterType t; fields = Array.empty}
        repr :> obj

    member private x.InitStructure (repr : structureRepr) (obj : obj) =
        let fields =
            obj.GetType()
            |> Reflection.fieldsOf false
            |> Seq.map (fun (_, field) -> field.GetValue(obj) |> x.Encode)
            |> Array.ofSeq
        { repr with fields = fields }

    member private x.InitClass (obj : obj) index =
        let encoded = objReprs[index]
        assert(encoded :? structureRepr)
        let encodedStructure = encoded :?> structureRepr
        let initialized = x.InitStructure encodedStructure obj
        objReprs[index] <- initialized

    member private x.EncodeStructure (obj : obj) =
        let repr = x.CreateStructure obj :?> structureRepr
        x.InitStructure repr obj

    member x.RepresentEnum (obj : obj) =
        let t = obj.GetType()
        let repr : enumRepr = {typ = x.RegisterType t; underlyingValue = Convert.ChangeType(obj, Enum.GetUnderlyingType t)}
        repr :> obj

    member private x.Bind (obj : obj) (repr : obj) =
        sourceObjects.Add obj
        objReprs.Add repr
        assert(sourceObjects.Count = objReprs.Count)
        sourceObjects.Count - 1

    member x.Encode (obj : obj) : obj =
        match obj with
        | null -> null
        | :? referenceRepr -> obj
        | :? structureRepr -> obj
        | :? arrayRepr -> obj
        | :? pointerRepr -> obj
        | :? enumRepr -> obj
        | _ ->
            let t = obj.GetType()
            if x.IsSerializable t then obj
            else
                match t with
                | _ when t.IsValueType ->
                    if t.IsEnum then x.RepresentEnum obj
                    else x.EncodeStructure obj
                | _ ->
                    let idx =
                        match Seq.tryFindIndex (fun obj' -> Object.ReferenceEquals(obj, obj')) sourceObjects with
                        | Some idx -> idx
                        | None ->
                            match obj with
                            | :? Array as arr ->
                                let idx = x.CreateArray arr |> x.Bind obj
                                x.InitArray arr idx
                                idx
                            | _ ->
                                let idx = x.CreateStructure obj |> x.Bind obj
                                x.InitClass obj idx
                                idx
                    let reference : referenceRepr = {index = idx}
                    reference :> obj

    member x.RepresentStruct (typ : Type) (fields : obj array) =
        let repr : structureRepr = {typ = x.RegisterType typ; fields = fields}
        repr :> obj

    member x.RepresentMockedStruct (typ : Mocking.Type) (fields : obj array) =
        let repr : structureRepr = {typ = mockStorage.RegisterMockedType typ; fields = fields}
        repr :> obj

    member x.ReserveRepresentation() = x.Bind null null

    member x.AddClass (typ : Type) (fields : obj array) index =
        let repr : structureRepr = {typ = x.RegisterType typ; fields = fields}
        objReprs.[index] <- repr
        { index = index }

    member x.AddMockedClass (typ : Mocking.Type) (fields : obj array) index =
        let repr : structureRepr = {typ = mockStorage.RegisterMockedType typ; fields = fields}
        objReprs.[index] <- repr
        { index = index }

    member x.AddArray (typ : Type) (contents : obj array) (lengths : int array) (lowerBounds : int array) index =
        let repr : arrayRepr = {typ = x.RegisterType typ; defaultValue = null; indices = null; values = contents; lengths = lengths; lowerBounds = lowerBounds }
        objReprs.[index] <- repr
        { index = index }

    member x.AddCompactArrayRepresentation (typ : Type) (defaultValue : obj) (indices : int array array) (values : obj array) (lengths : int array) (lowerBounds : int array) index =
        let repr : arrayRepr = {typ = x.RegisterType typ; defaultValue = defaultValue; indices = indices; values = values; lengths = lengths; lowerBounds = lowerBounds }
        objReprs.[index] <- repr
        { index = index }

    member x.Serialize (target : memoryRepr) =
        let t = typeof<memoryRepr>
        let p = t.GetProperty("objects")
        p.SetValue(target, objReprs.ToArray())
        let p = t.GetProperty("types")
        p.SetValue(target, sourceTypes |> Seq.map typeRepr.Encode |> Array.ofSeq)
