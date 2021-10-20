namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------ System.SR --------------------------------

module SR =

    let internal get_Arg_OverflowException (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_OverflowException" state

    let internal get_Arg_IndexOutOfRangeException (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_IndexOutOfRangeException" state

    let internal get_Arg_NullReferenceException (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_NullReferenceException" state

    let internal get_Arg_ArrayTypeMismatchException (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_ArrayTypeMismatchException" state

    let internal get_Arg_InvalidHandle (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_InvalidHandle" state

    let internal get_Arg_PlatformNotSupported (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_PlatformNotSupported" state

    let internal get_Arg_NotGenericTypeDefinition (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_NotGenericTypeDefinition" state

    let internal get_Arg_ArgumentException (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_ArgumentException" state

    let internal get_Arg_ArgumentOutOfRangeException (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_ArgumentOutOfRangeException" state

    let internal get_Arg_ArgumentNullException (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "Arg_ArgumentNullException" state

    let internal get_ArgumentNull_Generic (state : state) (args : term list) : term =
        assert(List.length args = 0)
        Memory.AllocateString "ArgumentNull_Generic" state

    let internal getMessageFromNativeResources (state : state) (args : term list) : term =
        assert(List.length args = 1)
        Memory.AllocateString "getMessageFromNativeResources" state
