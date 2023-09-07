namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Globalization =

    [<Implements("System.Globalization.CultureInfo System.Globalization.CultureInfo.get_CurrentCulture()")>]
    val get_CurrentCulture : state -> term list -> term

    [<Implements("System.Globalization.CultureInfo System.Globalization.CultureInfo.get_InvariantCulture()")>]
    val get_InvariantCulture : state -> term list -> term

    [<Implements("System.Globalization.CompareInfo System.Globalization.CultureInfo.get_CompareInfo(this)")>]
    val get_CompareInfo : state -> term list -> term

    [<Implements("System.Boolean System.Globalization.GlobalizationMode.get_Invariant()")>]
    val get_Invariant : state -> term list -> term
