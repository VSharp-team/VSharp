namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.String -------------------------------

module internal String =

    [<Implements("System.Void System.String..ctor(this, System.Char[])")>]
    val CtorOfCharArray : state -> term list -> (term * state) list

    [<Implements("System.Int32 System.String.get_Length(this)")>]
    val GetLength : state -> term list -> term * state

    [<Implements("System.Char System.String.get_Chars(this, System.Int32)")>]
    val GetChars : state -> term list -> term * state

    [<Implements("System.String System.String.ToUpperInvariant(this)")>]
    // NOTE: this works only for fully concrete strings
    // TODO: delete this and explore .NET code (unsafe is needed)
    val ToUpperInvariant : state -> term list -> term * state

    [<Implements("System.String System.String.CreateFromChar(System.Char)")>]
    val CreateFromChar : state -> term list -> (term * state) list
