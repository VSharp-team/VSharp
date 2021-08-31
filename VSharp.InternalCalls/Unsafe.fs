namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.Unsafe --------------------------------

module Unsafe =

    let internal AsPointer (state : state) (args : term list) : term =
        assert(List.length args = 2)
//        Types.Cast (List.item 1 args) (Pointer Void)
        List.item 1 args

    let internal As (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let typ, ref = args.[0], args.[1]
        let typ =
            match typ.term with
            | Concrete(:? Type as t, _) -> Types.FromDotNetType t
            | _ -> __unreachable__()
        Types.Cast ref typ

    let internal NullRef (state : state) (_ : term list) : term =
        NullRef

    let internal IsNullRef (state : state) (args : term list) : term =
        assert(List.length args = 2)
        let ref = args.[1]
        IsNullReference ref
