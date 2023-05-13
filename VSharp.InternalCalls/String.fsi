namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.String -------------------------------

module internal String =

    [<Implements("System.Void System.String..ctor(this, System.Char[])")>]
    val CtorOfCharArray : state -> term list -> (term * state) list

    [<Implements("System.Void System.String..ctor(this, System.Char, System.Int32)")>]
    val CtorFromReplicatedChar : state -> term list -> term

    [<Implements("System.Void System.String..ctor(this, System.ReadOnlySpan`1[System.Char])")>]
    val CtorFromSpan : state -> term list -> term

    [<Implements("System.Int32 System.String.get_Length(this)")>]
    val GetLength : state -> term list -> term

//    [<Implements("System.String System.String.ToUpperInvariant(this)")>]
    // NOTE: this works only for fully concrete strings
    val ToUpperInvariant : state -> term list -> term

    [<Implements("System.String System.String.CreateFromChar(System.Char)")>]
    val CreateFromChar : state -> term list -> term

    [<Implements("System.Char System.Char.ToUpper(System.Char)")>]
    val CharToUpper : state -> term list -> term

    // [<Implements("System.Boolean System.String.EqualsHelper(System.String, System.String)")>]
    val Equals : state -> term list -> term

    [<Implements("System.String System.String.FastAllocateString(System.Int32)")>]
    val FastAllocateString : state -> term list -> term
