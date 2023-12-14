namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

type private Dummy = { x : int }

module internal Helpers =

    let unwrapType term =
        match term.term with
        | Concrete(:? Type as t, _) -> t
        | _ -> internalfail $"InternalCalls: unexpected wrapped type {term}"

module SetUp =

    let ConfigureInternalCalls() =
        Loader.SetInternalCallsAssembly typeof<Dummy>.Assembly
