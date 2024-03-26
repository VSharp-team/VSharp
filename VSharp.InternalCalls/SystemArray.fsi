namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    [<Implements("System.Int32 System.Array.GetRank(this)")>]
    val GetRank : state -> term list -> term

    [<Implements("System.Int32 System.Array.get_Rank(this)")>]
    val get_Rank : state -> term list -> term

    [<Implements("System.Int32 System.Array.get_Length(this)")>]
    val get_Length : state -> term list -> term

    [<Implements("System.UIntPtr System.Array.get_NativeLength(this)")>]
    val get_NativeLength : state -> term list -> term

    [<Implements("System.Boolean System.SZArrayHelper.Contains(this, T)")>]
    val ContainsChar : state -> term list -> term

    [<Implements("System.Int32 System.SZArrayHelper.get_Count(this)")>]
    val GetCount : state -> term list -> term

    [<Implements("T System.SZArrayHelper.get_Item(this, System.Int32)")>]
    val GetItem : state -> term list -> term

    [<Implements("System.Int32 System.Array.GetLength(this, System.Int32)")>]
    val GetArrayLength : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Int32 System.Array.GetLowerBound(this, System.Int32)")>]
    val GetArrayLowerBound : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)")>]
    val CommonInitializeArray : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Array.Clear(System.Array, System.Int32, System.Int32)")>]
    val ClearWithIndexLength : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Array.Clear(System.Array)")>]
    val ClearWhole : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Array.Copy(System.Array, System.Int32, System.Array, System.Int32, System.Int32, System.Boolean)")>]
    val CopyArrayExtendedForm1 : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Array.Copy(System.Array, System.Int32, System.Array, System.Int32, System.Int32)")>]
    val CopyArrayExtendedForm2 : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Array.Copy(System.Array, System.Array, System.Int32)")>]
    val CopyArrayShortForm : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Void System.Array.Fill(T[], T)")>]
    val FillArray : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("T[] System.GC.AllocateUninitializedArray(System.Int32, System.Boolean)")>]
    val AllocateUninitializedArray : state -> term list -> term

    [<Implements("System.Byte& System.Runtime.InteropServices.MemoryMarshal.GetArrayDataReference(System.Array)")>]
    val GetArrayDataReference : IInterpreter -> cilState -> term list -> cilState list
