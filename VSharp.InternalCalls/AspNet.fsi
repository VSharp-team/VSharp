namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal AspNet =

    [<Implements("System.Boolean Microsoft.AspNetCore.HostFiltering.HostFilteringMiddleware.CheckHost(this, Microsoft.AspNetCore.Http.HttpContext, System.Collections.Generic.IList`1[Microsoft.Extensions.Primitives.StringSegment])")>]
    val CheckHost : state -> term list -> term
