namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.CodeAnalysis
open System.Xml.Serialization
open VSharp

[<CLIMutable>]
[<Serializable>]
type typeRepr = {
    assemblyName : string
    moduleFullyQualifiedName : string
    name : string
    genericArgs : typeRepr array
}

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

module Serialization =

    let rec encodeType ([<MaybeNull>] t : Type) : typeRepr =
        if t = null then {assemblyName = null; moduleFullyQualifiedName = null; name = null; genericArgs = null}
        else
            let name, arguments =
                if t.IsGenericType then
                    if not t.IsConstructedGenericType then
                        internalfail "Encoding not constructed generic types not supported"

                    let arguments = t.GetGenericArguments() |> Seq.map encodeType |> Seq.toArray
                    t.GetGenericTypeDefinition().FullName, arguments
                else
                    t.FullName, null

            {assemblyName = t.Module.Assembly.FullName; moduleFullyQualifiedName = t.Module.FullyQualifiedName; name = name; genericArgs = arguments}

    [<MaybeNull>]
    let decodeType (t : typeRepr) =
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
        if t.assemblyName = null then
            null
        else
            let decodedType = decodeTypeRec t
            AssemblyManager.NormalizeType decodedType

type ITypeMockSerializer =
    abstract IsMockObject : obj -> bool
    abstract IsMockRepresentation : obj -> bool
    abstract Serialize : obj -> obj
    abstract Deserialize : (obj -> obj) -> obj -> obj
    abstract UpdateMock : (obj -> obj) -> obj -> obj -> unit

and MemoryGraph(repr : memoryRepr, mocker : ITypeMockSerializer, createCompactRepr : bool) =

    let sourceTypes = List<Type>(repr.types |> Array.map Serialization.decodeType)
    let compactRepresentations = Dictionary<obj, CompactArrayRepr>()

    let rec allocateDefault (obj : obj) =
        match obj with
        | null
        | :? referenceRepr -> null
        | :? structureRepr as repr ->
            let t = sourceTypes.[repr.typ]
            if t.IsByRefLike then
                internalfailf "Generating test: unable to create byref-like object (type = %O)" t
            if t.ContainsGenericParameters then
                internalfailf "Generating test: unable to create object with generic type parameters (type = %O)" t
            else System.Runtime.Serialization.FormatterServices.GetUninitializedObject(t)
        | :? arrayRepr as repr ->
            let t = sourceTypes.[repr.typ]
            let elementType = t.GetElementType()
            if repr.lowerBounds = null then Array.CreateInstance(elementType, repr.lengths) :> obj
            else Array.CreateInstance(elementType, repr.lengths, repr.lowerBounds) :> obj
        | _ when mocker.IsMockRepresentation obj ->
            mocker.Deserialize allocateDefault obj
        | _ -> obj

    let sourceObjects = List<obj>(repr.objects |> Array.map allocateDefault)
    let objReprs = List<obj>(repr.objects)

    let rec decodeValue (obj : obj) =
        match obj with
        | :? referenceRepr as repr ->
            sourceObjects.[repr.index]
        | :? pointerRepr -> __notImplemented__()
        | :? structureRepr as repr ->
            let t = sourceTypes.[repr.typ]
            if not t.IsValueType then
                internalfailf "Expected value type inside object, but got representation of %s!" t.FullName
            let obj = allocateDefault repr
            decodeStructure repr obj
            obj
        | :? arrayRepr -> internalfail "Unexpected array representation inside object!"
        | :? enumRepr as repr ->
            let t = sourceTypes.[repr.typ]
            Enum.ToObject(t, repr.underlyingValue)
        | _ when mocker.IsMockRepresentation obj -> mocker.Deserialize decodeValue obj
        | _ -> obj

    and decodeStructure (repr : structureRepr) obj : unit =
        let t = obj.GetType()
        Reflection.fieldsOf false t |> Array.iteri (fun i (_, field) ->
            let value = decodeValue repr.fields.[i]
            field.SetValue(obj, value))

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

    and decodeObject (repr : obj) obj =
        match repr with
        | :? structureRepr as repr ->
            decodeStructure repr obj
        | :? arrayRepr as repr ->
            decodeArray repr obj
        | _ when mocker.IsMockRepresentation repr ->
            mocker.UpdateMock decodeValue repr obj
        | _ -> ()

    let () = Seq.iter2 decodeObject objReprs sourceObjects

    member x.DecodeValue (obj : obj) = decodeValue obj

    member x.CompactRepresentations() = compactRepresentations

    member private x.IsSerializable (t : Type) =
        // TODO: find out which types can be serialized by XMLSerializer
        (t.IsPrimitive && not t.IsEnum) || t = typeof<string> || (t.IsArray && (x.IsSerializable <| t.GetElementType()))

    member private x.EncodeArray (arr : Array) =
        // TODO: handle recursive arrays! #do
        let contents =
            seq {
                for elem in arr do
                    yield x.Encode elem
            } |> Array.ofSeq
        let lowerBounds =
            if arr.Rank = 1 && arr.GetLowerBound 0 = 0 then null
            else Array.init arr.Rank arr.GetLowerBound
        let repr : arrayRepr = {typ = x.RegisterType (arr.GetType())
                                defaultValue = null
                                indices = null
                                values = contents
                                lengths = Array.init arr.Rank arr.GetLength
                                lowerBounds = lowerBounds }
        repr :> obj

    member private x.RegisterType (typ : Type) =
        match sourceTypes |> Seq.tryFindIndex ((=) typ) with
        | Some idx -> idx
        | None ->
            sourceTypes.Add(typ)
            sourceTypes.Count - 1

    member private x.EncodeStructure (obj : obj) =
        let t = obj.GetType()
        let fields = t |> Reflection.fieldsOf false |> Seq.map (fun (_, field) -> field.GetValue(obj) |> x.Encode) |> Array.ofSeq
        let repr : structureRepr = {typ = x.RegisterType t; fields = fields}
        repr :> obj

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
        | _ when mocker.IsMockObject obj -> mocker.Serialize obj
        | _ ->
            // TODO: delegates?
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
                            let repr =
                                match t with
                                | _ when t.IsArray -> x.EncodeArray (obj :?> Array)
                                | _ -> x.EncodeStructure obj
                            x.Bind obj repr
                    let reference : referenceRepr = {index = idx}
                    reference :> obj

    member x.RepresentStruct (typ : Type) (fields : obj array) =
        let repr : structureRepr = {typ = x.RegisterType typ; fields = fields}
        repr :> obj

    member x.ReserveRepresentation() = x.Bind null null

    member x.AddClass (typ : Type) (fields : obj array) index =
        let repr : structureRepr = {typ = x.RegisterType typ; fields = fields}
        objReprs.[index] <- repr
        { index = index }

    member x.AddMockedClass (mockRepr : obj) index =
        objReprs.[index] <- mockRepr
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
        p.SetValue(target, sourceTypes |> Seq.map Serialization.encodeType |> Array.ofSeq)
