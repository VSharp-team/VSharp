namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Globalization =

    [<Implements("System.Globalization.CultureInfo System.Globalization.CultureInfo.get_CurrentCulture()")>]
    val internal get_CurrentCulture : state -> term list -> term

    [<Implements("System.Globalization.CultureInfo System.Globalization.CultureInfo.get_InvariantCulture()")>]
    val internal get_InvariantCulture : state -> term list -> term

    [<Implements("System.Globalization.CompareInfo System.Globalization.CultureInfo.get_CompareInfo(this)")>]
    val internal get_CompareInfo : state -> term list -> term

    [<Implements("System.Boolean System.Globalization.GlobalizationMode.get_Invariant()")>]
    val internal get_Invariant : state -> term list -> term
