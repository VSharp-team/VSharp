namespace VSharp.Utils

open System
open System.Collections.Generic
open System.Runtime.Serialization
open System.Threading
open VSharp

type Copier () =
    static let nonCopyableTypes = [
        typeof<Type>
        TypeUtils.systemRuntimeType
        typeof<Thread>
        typeof<System.Diagnostics.Tracing.EventSource>
        typeof<System.Reflection.FieldInfo>
    ]

    let cannotBeCopied (typ : Type) =
        List.exists typ.IsAssignableTo nonCopyableTypes

    let copiedObjects = Dictionary<physicalAddress, physicalAddress>()

    let rec deepCopyObject (phys : physicalAddress) =
        let obj = phys.object
        let typ = TypeUtils.getTypeOfConcrete obj
        let shouldNotCopy typ =
            cannotBeCopied typ || TypeUtils.isPrimitive typ
            || typ.IsEnum || typ.IsPointer || typ = typeof<System.Reflection.Pointer>
            || typ = typeof<IntPtr> || typ = typeof<UIntPtr>
        match obj with
        | null -> phys
        | _ when shouldNotCopy typ -> phys
        | _ -> deepCopyComplex phys typ

    and deepCopyComplex (phys : physicalAddress) typ =
        let copied = ref {object = null}
        if copiedObjects.TryGetValue(phys, copied) then copied.Value
        else createCopyComplex phys typ

    and createCopyComplex (phys : physicalAddress) typ =
        let obj = phys.object
        match obj with
        | :? Array as a when typ.GetElementType().IsPrimitive ->
            let phys' = {object = a.Clone()}
            copiedObjects.Add(phys, phys')
            phys'
        | :? Array as a ->
            let rank = a.Rank
            let dims = Array.init rank id
            let lengths = Array.map a.GetLength dims
            let lowerBounds = Array.map a.GetLowerBound dims
            let a' = Array.CreateInstance(typ.GetElementType(), lengths, lowerBounds)
            let phys' = {object = a'}
            copiedObjects.Add(phys, phys')
            let indicesWithValues = Array.getArrayIndicesWithValues a
            for index, v in indicesWithValues do
                let index = List.toArray index
                let v' = deepCopyObject {object = v}
                a'.SetValue(v'.object, index)
            phys'
        | :? String as s ->
            let phys' = {object = String(s)}
            copiedObjects.Add(phys, phys')
            phys'
        | _ when TypeUtils.isDelegate typ ->
            assert(obj :? Delegate)
            let obj = obj :?> Delegate
            let obj' = obj.Clone()
            let phys' = {object = obj'}
            copiedObjects.Add(phys, phys')
            let target' = deepCopyObject {object = obj.Target}
            let targetField = typ.GetField("_target", Reflection.instanceBindingFlags)
            assert(targetField <> null)
            targetField.SetValue(obj', target'.object)
            phys'
        | _ when typ.IsClass || typ.IsValueType ->
            let obj' = FormatterServices.GetUninitializedObject typ
            let phys' = {object = obj'}
            copiedObjects.Add(phys, phys')
            let fields = Reflection.fieldsOf false typ
            for _, field in fields do
                let v' = deepCopyObject {object = field.GetValue obj}
                field.SetValue(obj', v'.object)
            phys'
        | _ -> internalfailf "ConcreteMemory, deepCopyObject: unexpected object %O" obj

    member this.DeepCopy (phys : physicalAddress) = deepCopyObject phys
