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

type ILCodePortion(vertexNumber : int, funcId : IFunctionIdentifier, state : state) =
    member x.VertexNumber with get() = vertexNumber
    member x.Frames with get() = state.frames
    member x.FuncId with get() = funcId
    override x.Equals(b) =
        match b with
        | :? ILCodePortion as ilcode -> ilcode.VertexNumber = vertexNumber && ilcode.FuncId = funcId
        | _ -> false
    override x.GetHashCode() =
        let codeLoc = x :> ICodeLocation
        codeLoc.Location.GetHashCode()
    override x.ToString() = sprintf "Vertex = %d" vertexNumber
    interface ICodeLocation with
        override x.Location = vertexNumber :> obj
