namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ mscorlib.System.Type --------------------------------

module Type =

    let internal GetTypeFromHandle (state : state) (args : term list) =
        assert (List.length args = 1)
        let handle = List.head args
        let t =
            match handle.term with
            |  Concrete(:? System.RuntimeTypeHandle as handle, _) -> System.Type.GetTypeFromHandle handle
            | _ -> __notImplemented__()
        TypeOfMethod state (Types.FromDotNetType state t)

    let internal GetType (state : state) (args : term list) =
        assert(List.length args = 1)
        let ref = List.head args
        GetTypeMethod state ref

    let private equality transform (state : state) (args : term list) =
        assert(List.length args = 2)
        let typ1 = List.head args
        let typ2 = args |> List.tail |> List.head
        transform (typ1 === typ2), state

    let internal op_Inequality (state : state) (args : term list) =
        equality (!!) state args

    let internal op_Equality (state : state) (args : term list) =
        equality id state args
