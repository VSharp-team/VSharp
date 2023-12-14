namespace VSharp.Fuzzer

open VSharp
open VSharp.Core

type internal SolvingResult = {
    concreteClassParams: System.Type[]
    mockedClassParams: Mocking.Type option[]
    concreteMethodParams: System.Type[]
    mockedMethodParams: Mocking.Type option[]
}

type internal TypeSolver() =
    let dynamicTypeBuilder = Mocking.Mocker()
    let mockCache = System.Collections.Generic.Dictionary<System.Type list, ITypeMock>()
    let mockTypeCache = System.Collections.Generic.Dictionary<ITypeMock, Mocking.Type>()
    let systemTypeCache = System.Collections.Generic.Dictionary<Mocking.Type, System.Type>()
    let defaultClausesCount = 10

    let mockMethod (freshMock: Mocking.Type) (generate: System.Type -> obj) (method: Mocking.Method) =
        let baseMethod = method.BaseMethod
        let implementation = Array.init defaultClausesCount (fun _ -> generate baseMethod.ReturnType)
        let outParams = baseMethod.GetParameters() |> Array.filter (fun p -> p.IsOut)
        let outImplementations =
            if Array.isEmpty outParams then Array.empty
            else
                let types = outParams |> Array.map (fun p -> p.ParameterType.GetElementType())
                Array.init defaultClausesCount (fun _ -> Array.map generate types)
        freshMock.AddMethod(baseMethod, implementation, outImplementations)

    let encodeMock (mock: ITypeMock) (generate: System.Type -> obj) =
        Logger.traceTypeSolving $"Encode mock {mock.Name}"
        match mockTypeCache.TryGetValue(mock) with
        | true, value ->
            Logger.traceTypeSolving $"{mock.Name} got from cache"
            value
        | _ ->
            Logger.traceTypeSolving $"{mock.Name} new Mocking.Type"
            let freshMock = Mocking.Type(mock.Name)
            mock.SuperTypes |> Seq.iter freshMock.AddSuperType
            freshMock.MethodMocks |> Seq.iter (mockMethod freshMock generate)
            mockTypeCache.Add(mock, freshMock)
            freshMock

    let mockToType (mock: Mocking.Type) =
        Logger.traceTypeSolving $"Build mock {mock.Id}"
        match systemTypeCache.TryGetValue(mock) with
        | true, value ->
            Logger.traceTypeSolving $"{mock.Id} got from cache"
            value
        | _ ->
            Logger.traceTypeSolving $"{mock.Id} new System.Type"
            let dynamicType = dynamicTypeBuilder.BuildDynamicType mock
            systemTypeCache.Add(mock, dynamicType)
            dynamicType

    member this.MockType (t: System.Type) (generate: System.Type -> obj) =
        Logger.traceTypeSolving $"Mock type {t.Name}"
        let mock =
            match mockCache.TryGetValue [t] with
            | true, v ->
                Logger.traceTypeSolving $"{t.Name} got from cache"
                v
            | false, _ ->
                Logger.traceTypeSolving $"{t.Name} new TypeMock"
                let mock = TypeMock([t])
                mockCache.Add([t], mock)
                mock
        let encodedMock = encodeMock mock generate
        let typ = mockToType encodedMock
        mock, typ

    member this.GetMocks () = mockTypeCache

    member this.SolveGenericMethodParameters (method: Method) (generate: System.Type -> obj) =
        // TODO: Receive type parameters substitution from master process
        Logger.traceTypeSolving $"Solve generics for {method.Name}"
        let substituteGenerics classParams methodParams =
            let getConcreteType =
                function
                | ConcreteType t -> t
                | MockType m ->
                    mockCache.Add (m.SuperTypes |> Seq.toList, m)
                    encodeMock m generate |> mockToType

            let methodBase = (method :> IMethod).MethodBase
            let classParams = classParams |> Array.map getConcreteType
            let methodParams = methodParams |> Array.map getConcreteType
            let declaringType = Reflection.concretizeTypeParameters methodBase.DeclaringType classParams
            let method = Reflection.concretizeMethodParameters declaringType methodBase methodParams
            method

        let typeStorage = typeStorage()

        match SolveGenericMethodParameters typeStorage method with
        | Some(classParams, methodParams) ->
            let method = substituteGenerics classParams methodParams
            Logger.traceTypeSolving $"Solved generics for {method.Name}"
            Some (method, typeStorage)
        | _ ->
            Logger.traceTypeSolving $"Failed solve generics for {method.Name}"
            None
