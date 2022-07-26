namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Type --------------------------------

module internal Type =

    let systemRuntimeType = typeof<Object>.GetType()

    let fieldWithTypeInfo : fieldId =
        {declaringType = systemRuntimeType; name = "typeInfo"; typ = systemRuntimeType}

    let getActualType state runtimeType =
        let field = Memory.ReadField state runtimeType fieldWithTypeInfo
        match field.term with
        | Concrete(:? Type as t, _) -> t
        | _ -> __unreachable__()

    let private allocateType state (typeToAllocate : Type) =
        // NOTE: allocating empty RuntimeType
        let ref = Memory.AllocateDefaultClass state systemRuntimeType
        let value = Concrete typeToAllocate systemRuntimeType
        // NOTE: add field with information about actual type
        let states = Memory.WriteClassField state ref fieldWithTypeInfo value
        List.map (withFst ref) states

    let GetTypeFromHandle (state : state) (args : term list) : (term * state) list =
        assert (List.length args = 1)
        let handle = List.head args
        match handle.term with
        // NOTE: ldtoken case
        | Concrete(:? RuntimeTypeHandle as handle, _) ->
            let runtimeType = Type.GetTypeFromHandle handle
            allocateType state runtimeType
        // NOTE: created via reflection case
        | _ when IsStruct handle ->
            let typeField = {declaringType = typeof<RuntimeTypeHandle>; name = "m_type"; typ = systemRuntimeType}
            let typeRef = Memory.ReadField state handle typeField
            List.singleton (typeRef, state)
        | _ -> __notImplemented__()
//        TypeOfMethod state (Types.FromDotNetType state t)
        // TODO: restore it after rewriting marshaling/unmarshaling
//        __notImplemented__()

    let GetAssembly (state : state) (args : term list) : (term * state) list =
        assert (List.length args = 1)
        let runtimeType = typeof<Object>.Assembly.GetType()
        allocateType state runtimeType

    let GetType (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 1)
        let ref = List.head args
        let typ = MostConcreteTypeOfHeapRef state ref
        allocateType state typ

    let GetElementType (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        allocateType state (actualType.GetElementType())

    let private equality transform (state : state) (args : term list) =
        assert(List.length args = 2)
        let runtimeType1 = List.head args
        let runtimeType2 = args |> List.tail |> List.head
        let eq =
            match runtimeType1, runtimeType2 with
            | NullRef _, NullRef _ -> True
            | NullRef _, _
            | _, NullRef _ -> False
            | _ ->
                let actualType1 = getActualType state runtimeType1
                let actualType2 = getActualType state runtimeType2
                MakeBool (actualType1 = actualType2)
        transform eq

    let opInequality (state : state) (args : term list) =
        equality (!!) state args

    let opEquality (state : state) (args : term list) =
        equality id state args

    let isGenericTypeDefinition (_ : state) (_ : term list) =
        MakeBool false

    let isInterface (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        MakeBool actualType.IsInterface

    let isGenericVariable (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        MakeBool actualType.IsGenericParameter

    let get_Name (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        Memory.AllocateString actualType.Name state

    let isValueType (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        MakeBool actualType.IsValueType

    let getEnumValues (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        let values : obj seq = Enum.GetValues(actualType) |> System.Linq.Enumerable.OfType<obj>
        let length = Seq.length values |> MakeNumber
        Memory.AllocateConcreteVectorArray state length actualType values

    let getEnumUnderlyingType (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        actualType.GetEnumUnderlyingType() |> allocateType state
