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
        let symbolicRuntimeType = Types.FromDotNetType systemRuntimeType
        let ref, state = Memory.AllocateDefaultClass state symbolicRuntimeType // TODO: marshalling #hack
        let value = Concrete typeToAllocate symbolicRuntimeType
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
//        TypeOfMethod state (Types.FromDotNetType state t)
        // TODO: restore it after rewriting marshaling/unmarshaling
//        __notImplemented__()

    let GetType (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 1)
        let ref = List.head args
        let typ = MostConcreteTypeOfHeapRef state ref
        allocateType state (Types.ToDotNetType typ)
//        GetTypeMethod state ref
        // TODO: restore it after rewriting marshaling/unmarshaling
//        __notImplemented__()

    let private equality transform (state : state) (args : term list) =
        assert(List.length args = 2)
        let typ1 = List.head args
        let typ2 = args |> List.tail |> List.head
        transform (typ1 === typ2), state

    let opInequality (state : state) (args : term list) =
        equality (!!) state args

    let opEquality (state : state) (args : term list) =
        equality id state args

    let isGenericTypeDefinition (state : state) (_ : term list) =
        MakeBool false, state

    let isInterface (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        MakeBool actualType.IsInterface, state

    let isGenericVariable (state : state) (args : term list) =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let actualType = getActualType state runtimeType
        MakeBool actualType.IsGenericParameter, state

    let get_Name (state : state) (_ : term list) =
        Memory.AllocateString "RuntimeType" state
