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
        let x, y = args[0], args[1]
        x === y

    let EnumEquals (_ : state) (args : term list) =
        assert(List.length args = 3)
        let x, y = args[1], args[2]
        x === y

    let RunStaticCtor (_ : IInterpreter) (cilState : cilState) (args : term list) =
        assert(List.length args = 1)
        // TODO: initialize statics of argument
        List.singleton cilState

    let TryEnsureSufficientExecutionStack (_ : state) (args : term list) =
        assert(List.length args = 0)
        // 'True' value leads to more runtime optimizations code exploration
        MakeBool true

    let EnsureSufficientExecutionStack (_ : state) (args : term list) =
        assert(List.length args = 0)
        Nop()

    let ExceptionGetSource (state : state) (args : term list) =
        assert(List.length args = 1)
        let exceptionRef = args[0]
        let t = MostConcreteTypeOfRef state exceptionRef
        Memory.AllocateString (t.ToString()) state

    let BadImageFormatExceptionToString (state : state) (args : term list) =
        assert(List.length args = 1)
        Memory.AllocateString "BadImageFormatException" state

    let BadImageFormatExceptionGetMessage (state : state) (args : term list) =
        assert(List.length args = 1)
        Memory.AllocateString "BadImageFormatException" state
