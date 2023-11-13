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

    member private x.SetClauses clauses =
        callResults.Clear()
        callResults.AddRange clauses

    member private x.SetOuts (clauses : ResizeArray<term list>) =
        outResults.Clear()
        outResults.AddRange clauses

    interface IMethodMock with
        override x.BaseMethod =
            match method.MethodBase with
            | :? MethodInfo as mi -> mi
            | _ -> __notImplemented__()

        override x.MockingType = mockingType

        override x.Call state this args =
            let genSymbolycVal retType args =
                let src : functionResultConstantSource = {
                    mock = x
                    constIndex = constIndex
                    this = this
                    args = args
                    t = retType
                }
                Memory.makeSymbolicValue src (toString src) retType

            let genOutParam (s : state * term list) (p : ParameterInfo) (arg : term) =
                if not <| p.IsOut then s
                else
                    let tVal = typeOfRef arg
                    let newVal = genSymbolycVal tVal []
                    constIndex <- constIndex + 1
                    Memory.write Memory.emptyReporter (fst s) arg newVal, newVal :: (snd s)

            let resState =
                if not <| hasOutParams then None
                else
                    let resState, outParams =
                        List.fold2 genOutParam (state, List.empty) methodParams args
                    outResults.Add(outParams)
                    Some resState

            let resTerm =
                if method.ReturnType = typeof<Void> then None
                else
                    let result = genSymbolycVal method.ReturnType args
                    constIndex <- constIndex + 1
                    callResults.Add result
                    Some result

            resState, resTerm

        override x.GetImplementationClauses() = callResults.ToArray()

        override x.GetOutClauses() =
            outResults.ToArray() |> Array.map List.toArray

        override x.Copy() =
            let result = MethodMock(method, mockingType)
            result.SetIndex constIndex
            result.SetClauses callResults
            result.SetOuts outResults
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
