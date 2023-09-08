namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Helpers =

    let unwrapType term =
        match term.term with
        | Concrete(:? Type as t, _) -> t
        | _ -> internalfail $"InternalCalls: unexpected wrapped type {term}"

module SetUp =

    let ConfigureInternalCalls() =
        Loader.SetInternalCallsAssembly (System.Reflection.Assembly.GetExecutingAssembly())
