namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics.CodeAnalysis
open System.Xml.Serialization
open VSharp

[<CLIMutable>]
[<Serializable>]
type typeRepr = {
    assemblyName : string
    moduleFullyQualifiedName : string
    fullName : string
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
type arrayRepr = {
    typ : int
    contents : obj array
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

module Serialization =

    let encodeType ([<MaybeNull>] t : Type) : typeRepr =
        if t = null then {assemblyName = null; moduleFullyQualifiedName = null; fullName = null}
        else
            {assemblyName = t.Module.Assembly.FullName; moduleFullyQualifiedName = t.Module.FullyQualifiedName; fullName = t.FullName}

    [<MaybeNull>]
    let decodeType (t : typeRepr) =
        if t.assemblyName = null then null
        else
            let mdle = Reflection.resolveModule t.assemblyName t.moduleFullyQualifiedName
            mdle.GetType(t.fullName)


type ITypeMockSerializer =
    abstract IsMockObject : obj -> bool
    abstract IsMockRepresentation : obj -> bool
    abstract Serialize : obj -> obj
    abstract Deserialize : MemoryGraph -> obj -> obj

and MemoryGraph(repr : memoryRepr, mocker : ITypeMockSerializer) as this =

    let sourceTypes = List<Type>(repr.types |> Array.map Serialization.decodeType)

    let allocatePlaceholder (obj : obj) =
        assert(obj <> null)
        match obj with
        | :? structureRepr as repr ->
            let t = sourceTypes.[repr.typ]
            System.Runtime.Serialization.FormatterServices.GetUninitializedObject(t)
        | :? arrayRepr as repr ->
            let t = sourceTypes.[repr.typ]
            let elementType = t.GetElementType()
            if repr.lowerBounds = null then Array.CreateInstance(elementType, repr.lengths) :> obj
            else Array.CreateInstance(elementType, repr.lengths, repr.lowerBounds) :> obj
        | _ -> obj

    let sourceObjects = List<obj>(repr.objects |> Array.map allocatePlaceholder)
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
            let obj = allocatePlaceholder repr
            decodeStructure repr obj
            obj
        | :? arrayRepr -> internalfail "Unexpected array representation inside object!"
        | :? enumRepr as repr ->
            let t = sourceTypes.[repr.typ]
            Enum.ToObject(t, repr.underlyingValue)
        | _ when mocker.IsMockRepresentation obj -> mocker.Deserialize this obj
        | _ -> obj

    and decodeStructure (repr : structureRepr) obj =
        let t = obj.GetType()
        Reflection.fieldsOf false t |> Array.iteri (fun i (_, field) ->
            let value = decodeValue repr.fields.[i]
            field.SetValue(obj, value))

    and decodeArray (repr : arrayRepr) (obj : obj) =
        match repr.lengths, repr.lowerBounds with
        | [|len|], null ->
            let arr = obj :?> Array
            assert(arr.Length = len)
            repr.contents |> Array.iteri (fun i r -> arr.SetValue(decodeValue r, i))
        | lens, lbs ->
            assert(lens.Length = lbs.Length)
            let arr = obj :?> Array
            repr.contents |> Array.iteri (fun i r ->
                let value = decodeValue r
                let indices = Seq.delinearizeArrayIndex i lens lbs
                arr.SetValue(value, indices))

    and decodeObject (repr : obj) obj =
        match repr with
        | :? structureRepr as repr ->
            decodeStructure repr obj
        | :? arrayRepr as repr ->
            decodeArray repr obj
        | _ -> ()

    let () =
        Seq.iter2 decodeObject objReprs sourceObjects

    member x.DecodeValue (obj : obj) = decodeValue obj

    member private x.IsSerializable (t : Type) =
        // TODO: find out which types can be serialized by XMLSerializer
        (t.IsPrimitive && not t.IsEnum) || t = typeof<string> || (t.IsArray && (x.IsSerializable <| t.GetElementType()))

    member private x.EncodeArray (arr : Array) =
        let contents =
            seq {
                for elem in arr do
                    yield x.Encode elem
            } |> Array.ofSeq
        let lowerBounds =
            if arr.Rank = 1 && arr.GetLowerBound 0 = 0 then null
            else Array.init arr.Rank arr.GetLowerBound
        let repr : arrayRepr = {typ = x.RegisterType (arr.GetType())
                                contents = contents
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

    member x.AddArray (typ : Type) (contents : obj array) (lengths : int array) (lowerBounds : int array) index =
        let repr : arrayRepr = {typ = x.RegisterType typ; contents = contents; lengths = lengths; lowerBounds = lowerBounds }
        objReprs.[index] <- repr
        { index = index }

    member x.Serialize (target : memoryRepr) =
        let t = typeof<memoryRepr>
        let p = t.GetProperty("objects")
        p.SetValue(target, objReprs.ToArray())
        let p = t.GetProperty("types")
        p.SetValue(target, sourceTypes |> Seq.map Serialization.encodeType |> Array.ofSeq)
