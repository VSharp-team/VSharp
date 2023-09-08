namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module Runtime_CompilerServices_RuntimeHelpers =

    // This function checks, whether type can be checked on equality using only it's bits
    // Example: any value type, because it doesn't have metadata
    let IsBitwiseEquatable (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let typ = List.head args |> Helpers.unwrapType
        MakeBool typ.IsValueType

    let IsReferenceOrContainsReferences (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let typ = List.head args |> Helpers.unwrapType
        MakeBool (Reflection.isReferenceOrContainsReferences typ)

    let CommonGetHashCode (_ : state) (args : term list) : term =
        assert(List.length args = 1)
        let object = List.head args
        GetHashCode object

    let ValueTypeGetHashCode (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let boxedThis = List.head args
        let structValue = Memory.Read state boxedThis
        GetHashCode structValue

    let EnumGetHashCode (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let boxedThis = List.head args
        let enumValue = Memory.Read state boxedThis
        GetHashCode enumValue

    let Equals (_ : state) (args : term list) : term =
        assert(List.length args = 2)
        let x, y = args.[0], args.[1]
        x === y

    let RunStaticCtor (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        // TODO: initialize statics of argument
        List.singleton cilState
