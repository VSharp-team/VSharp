namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Type --------------------------------

module internal Type =

    let fieldWithTypeInfo : fieldId =
        {declaringType = TypeUtils.systemRuntimeType; name = "typeInfo"; typ = TypeUtils.systemRuntimeType}

    let getActualType state runtimeType =
        let field = Memory.ReadField state runtimeType fieldWithTypeInfo
        match field.term with
        | Concrete(:? Type as t, _) -> t
        | _ -> __unreachable__()

    let private allocateType state (typeToAllocate : Type) =
        // NOTE: allocating empty RuntimeType
        let ref = Memory.AllocateConcreteObject state typeToAllocate TypeUtils.systemRuntimeType
//        let value = Concrete typeToAllocate systemRuntimeType
//        // NOTE: add field with information about actual type
//        let states = Memory.WriteClassField state ref fieldWithTypeInfo value
//        List.map (withFst ref) states
        List.singleton (ref, state)

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
            let typeField = {declaringType = typeof<RuntimeTypeHandle>; name = "m_type"; typ = TypeUtils.systemRuntimeType}
            let typeRef = Memory.ReadField state handle typeField
            List.singleton (typeRef, state)
        | _ -> __notImplemented__()
//        TypeOfMethod state (Types.FromDotNetType state t)
        // TODO: restore it after rewriting marshaling/unmarshaling
//        __notImplemented__()

    let GetAssembly (state : state) (args : term list) : (term * state) list =
        assert (List.length args = 1)
        let assemblyType = Reflection.mscorlibAssembly.GetType()
        allocateType state assemblyType

    let GetType (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 1)
        let ref = List.head args
        let typ = MostConcreteTypeOfRef state ref
        allocateType state typ

    let GetElementType (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        allocateType state (actualType.GetElementType())

    let opInequality (_ : state) (args : term list) =
        assert(List.length args = 2)
        let typ1, typ2 = args[0], args[1]
        !!(typ1 === typ2)

    let opEquality (_ : state) (args : term list) =
        assert(List.length args = 2)
        let typ1, typ2 = args[0], args[1]
        typ1 === typ2

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
        EnumUtils.getEnumUnderlyingTypeChecked actualType |> allocateType state
