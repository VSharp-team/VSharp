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
    let outResults = ResizeArray<term list>()

    member x.Method : IMethod = method

    member private x.SetIndex idx = callIndex <- idx

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
            let genSymbolycVal retType =
                let src : functionResultConstantSource = {
                    mock = x
                    callIndex = callIndex
                    this = this
                    args = args
                }
                Memory.makeSymbolicValue src (toString src) retType

            let mParams = method.Parameters |> Array.toList
            let resState =
                // if List.exists (fun (p : ParameterInfo) -> p.ParameterType.IsPointer) mParams then
                if List.exists (fun (p : ParameterInfo) -> p.IsOut) mParams then
                    let resState, outParams =
                        let genOutParam (s : state * term list) (p : ParameterInfo) (arg : term) =
                            if p.IsOut then
                            // if p.ParameterType.IsPointer then
                                let tVal = typeOfRef arg
                                // let arr = Array.CreateInstance tVal 1
                                let newVal = genSymbolycVal tVal
                                callIndex <- callIndex + 1
                                Memory.write Memory.emptyReporter (fst s) arg newVal, newVal :: (snd s)
                            else
                                s
                        List.fold2 genOutParam (state, List.empty) mParams args
                    outResults.Add(outParams)
                    Some resState
                else
                    None

            let reads =
                match resState with
                | Some s ->
                    let mapper (p : ParameterInfo) (a : term) =
                        if p.ParameterType.IsPointer then
                            Memory.read Memory.emptyReporter s a
                        else
                            a
                    List.map2 mapper mParams args
                | None -> []

            let resTerm =
                if method.ReturnType <> typeof<Void> then
                    let result = genSymbolycVal method.ReturnType
                    callIndex <- callIndex + 1
                    callResults.Add result
                    Some result
                else
                    None

            resState, resTerm

        override x.GetImplementationClauses() = callResults.ToArray()

        override x.GetOutClauses() =
            outResults.ToArray() |> Array.map List.toArray

        override x.Copy() =
            let result = MethodMock(method, mockingType)
            result.SetIndex callIndex
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
        // mocked procedures' calls are ignored
        mock.Call state this args
