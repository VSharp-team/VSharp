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
        callIndex : int
        this : term option
        args : term list
    }
with
    interface INonComposableSymbolicConstantSource with
        override x.TypeOfLocation = x.mock.Method.ReturnType
        override x.SubTerms = []
        override x.Time = VectorTime.zero
    override x.ToString() =
        let args = x.args |> List.map toString |> join ", "
        $"{x.mock.Method.Name}({args}):{x.callIndex}"

and MethodMock(method : IMethod, mockingType : MockingType) =
    let mutable callIndex = 0
    let callResults = ResizeArray<term>()

    member x.Method : IMethod = method

    member private x.SetIndex idx = callIndex <- idx

    member private x.SetClauses clauses =
        callResults.Clear()
        callResults.AddRange clauses

    interface IMethodMock with
        override x.BaseMethod =
            match method.MethodBase with
            | :? MethodInfo as mi -> mi
            | _ -> __notImplemented__()

        override x.MockingType = mockingType

        override x.Call this args =
            let returnType = method.ReturnType
            if returnType = typeof<Void> then
                internalfailf "Mocked procedures cannot be called"
            let src : functionResultConstantSource = {
                mock = x
                callIndex = callIndex
                this = this
                args = args
            }
            let result = Memory.makeSymbolicValue src (toString src) returnType
            callIndex <- callIndex + 1
            callResults.Add result
            result

        override x.GetImplementationClauses() = callResults.ToArray()

        override x.Copy() =
            let result = MethodMock(method, mockingType)
            result.SetIndex callIndex
            result.SetClauses callResults
            result

module internal MethodMocking =

    type private EmptyMethodMock() =
        let empty() = internalfail "method mock is empty"
        interface IMethodMock with
            override x.BaseMethod = empty()
            override x.MockingType = empty()
            override x.Call _ _ = empty()
            override x.GetImplementationClauses() = empty()
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
        // mocked procedures' calls are ignored
        if method.ReturnType <> typeof<Void> then
            mock.Call this args |> Some
        else None
