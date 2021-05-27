namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Interlocked =

    [<Implements("System.Globalization.CultureInfo System.Threading.Interlocked.CompareExchange(System.Globalization.CultureInfo&, System.Globalization.CultureInfo, System.Globalization.CultureInfo)")>]
    val internal compareExchange : state -> term list -> (term * state) list
