namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open System.Runtime.InteropServices
open System.Reflection

module Runtime_CompilerServices_RuntimeHelpers =

    // This function checks, whether type can be checked on equality using only it's bits
    // Example: any value type, because it doesn't have metadata
    let IsBitwiseEquatable (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let typ = List.head args
        match typ with
        | {term = Concrete(:? Type as typ, _)} -> MakeBool typ.IsValueType
        | _ -> __unreachable__()

    let IsReferenceOrContainsReferences (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let typ = List.head args
        match typ with
        | {term = Concrete(:? Type as typ, _)} -> MakeBool (Reflection.isReferenceOrContainsReferences typ)
        | _ -> __unreachable__()

    let GetHashCode (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let object = List.head args
        GetHashCode object

    let Equals (state : state) (args : term list) : term =
        assert(List.length args = 2)
        let x, y = args.[0], args.[1]
        x === y
