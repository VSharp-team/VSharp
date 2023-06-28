namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.IntPtr --------------------------------

module IntPtr =

    let private intPtrCtor (state : state) this term : (term * state) list =
        let ptr = MakeIntPtr term
        Memory.Write state this ptr |> List.map (withFst Nop)

    let internal intPtrCtorFromInt (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, intTerm = List.item 0 args, List.item 1 args
        intPtrCtor state this intTerm

    let internal intPtrCtorFromPtr (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, ptrTerm = List.item 0 args, List.item 1 args
        intPtrCtor state this ptrTerm

    let internal intPtrCtorFromLong (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, intTerm = List.item 0 args, List.item 1 args
        intPtrCtor state this intTerm

    let internal intPtrToPointer (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let this = List.item 0 args
        let ptr = Memory.Read state this
        NativeToPtr ptr

// ------------------------------ System.UIntPtr --------------------------------

module UIntPtr =

    let private uintPtrCtor (state : state) this term : (term * state) list =
        let ptr = MakeUIntPtr term
        Memory.Write state this ptr |> List.map (withFst Nop)

    let internal uintPtrCtorFromUInt (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, intTerm = List.item 0 args, List.item 1 args
        uintPtrCtor state this intTerm

    let internal uintPtrCtorFromPtr (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, ptrTerm = List.item 0 args, List.item 1 args
        uintPtrCtor state this ptrTerm

    let internal uintPtrCtorFromULong (state : state) (args : term list) : (term * state) list =
        assert(List.length args = 2)
        let this, intTerm = List.item 0 args, List.item 1 args
        uintPtrCtor state this intTerm

    let internal uintPtrToPointer (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let this = List.item 0 args
        let ptr = Memory.Read state this
        NativeToPtr ptr
