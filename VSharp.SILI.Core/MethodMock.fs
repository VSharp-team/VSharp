namespace VSharp.Core

open System
open System.Reflection
open FSharpx.Collections
open VSharp
open VSharp.Core

[<StructuralEquality; NoComparison>]
type functionResultConstantSource =
    {
        mock : MethodMock
        constIndex : int
        this : term option
        args : term list
        t : Type
    }
with
    interface INonComposableSymbolicConstantSource with
        override x.TypeOfLocation = x.t
        override x.SubTerms = []
        override x.Time = VectorTime.zero
    override x.ToString() =
        let args = x.args |> List.map toString |> join ", "
        $"{x.mock.Method.Name}({args}):{x.constIndex}"

and MethodMock(method : IMethod, mockingType : MockingType) =
    let mutable constIndex = 0
    let callResults = ResizeArray<term>()
    let outResults = ResizeArray<term list>()
    let methodParams = method.Parameters |> Array.toList
    let hasOutParams = List.exists (fun (p : ParameterInfo) -> p.IsOut) methodParams

    member x.Method : IMethod = method

    member private x.SetIndex idx = constIndex <- idx

    member private x.SetCallResults rets (outs : ResizeArray<term list>) =
        callResults.Clear()
        outResults.Clear()
        callResults.AddRange rets
        outResults.AddRange outs

    member private x.GenSymbolicVal (memory : IMemory) this retType args =
        let src : functionResultConstantSource = {
            mock = x
            constIndex = constIndex
            this = this
            args = args
            t = retType
        }
        constIndex <- constIndex + 1
        memory.MakeSymbolicValue src (toString src) retType

    interface IMethodMock with
        override x.BaseMethod =
            match method.MethodBase with
            | :? MethodInfo as mi -> mi
            | _ -> __notImplemented__()

        override x.MockingType = mockingType

        override x.Call state this args =
            let memory = state.memory
            let genOutParam (values : term list) (p : ParameterInfo) (arg : term) =
                if not <| p.IsOut then values
                else
                    let newVal = x.GenSymbolicVal memory this (typeOfRef arg) []
                    memory.Write Memory.emptyReporter arg newVal |> ignore
                    newVal :: values

            if hasOutParams then
                let outParams = List.fold2 genOutParam List.empty methodParams args
                outResults.Add(outParams)

            if method.ReturnType = typeof<Void> then None
            else
                let result = x.GenSymbolicVal memory this method.ReturnType args
                callResults.Add result
                Some result

        override x.GetImplementationClauses() = callResults.ToArray()

        override x.GetOutClauses() =
            outResults.ToArray() |> Array.map List.toArray

        override x.Copy() =
            let result = MethodMock(method, mockingType)
            result.SetIndex constIndex
            result.SetCallResults callResults outResults
            result

module internal MethodMocking =

    type private EmptyMethodMock() =
        let empty() = internalfail "method mock is empty"
        interface IMethodMock with
            override x.BaseMethod = empty()
            override x.MockingType = empty()
            override x.Call _ _ _ = empty()
            override x.GetImplementationClauses() = empty()
            override x.GetOutClauses() = empty()
            override x.Copy() = empty()

    let private mockMethod state method mockingType =
        let methodMocks = state.methodMocks
        let mock = ref (EmptyMethodMock() :> IMethodMock)
        if methodMocks.TryGetValue(method, mock) then mock.Value
        else
            let mock = MethodMock(method, mockingType)
            methodMocks.Add(method, mock)
            mock

    let mockAndCall state method this args mockingType =
        let mock = mockMethod state method mockingType
        mock.Call state this args
