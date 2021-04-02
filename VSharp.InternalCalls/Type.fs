namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Type --------------------------------

module internal Type =

    let GetTypeFromHandle (state : state) (args : term list) : term * state =
        assert (List.length args = 1)
        let handle = List.head args
        let t =
            match handle.term with
            |  Concrete(:? System.RuntimeTypeHandle as handle, _) -> Type.GetTypeFromHandle handle
            | _ -> __notImplemented__()
        let t = typeof<Object>.GetType()
        Memory.AllocateDefaultClass state (Types.FromDotNetType t) // TODO: hack #do
//        TypeOfMethod state (Types.FromDotNetType state t)
        // TODO: restore it after rewriting marshaling/unmarshaling
//        __notImplemented__()

    let GetAssembly (state : state) (args : term list) : term * state =
        assert (List.length args = 1)
        let t = typeof<Object>.Assembly.GetType()
        Memory.AllocateDefaultClass state (Types.FromDotNetType t) // TODO: hack #do
//        TypeOfMethod state (Types.FromDotNetType state t)
        // TODO: restore it after rewriting marshaling/unmarshaling
//        __notImplemented__()

    let GetType (state : state) (args : term list) : term * state =
        assert(List.length args = 1)
        let ref = List.head args
//        GetTypeMethod state ref
        // TODO: restore it after rewriting marshaling/unmarshaling
        __notImplemented__()

    let private equality transform (state : state) (args : term list) =
        assert(List.length args = 2)
        let typ1 = List.head args
        let typ2 = args |> List.tail |> List.head
        transform (typ1 === typ2), state

    let op_Inequality (state : state) (args : term list) =
        equality (!!) state args

    let op_Equality (state : state) (args : term list) =
        equality id state args

    let isGenericTypeDefinition (state : state) (_ : term list) =
        MakeBool false, state

    let get_Name (state : state) (_ : term list) =
        Memory.AllocateString "RuntimeType" state
