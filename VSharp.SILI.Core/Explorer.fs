namespace VSharp.Core

open Types

#nowarn "69"

open VSharp
open System.Reflection

type IMethodIdentifier =
    inherit IFunctionIdentifier
    abstract IsStatic : bool
    abstract DeclaringType : System.Type
    abstract DeclaringAssembly : Assembly

