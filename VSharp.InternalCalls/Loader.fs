namespace VSharp

open global.System
open System.Reflection

module Loader =
    let private collectImplementations (ts : Type seq) =
        let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
        ts
        |> Seq.collect (fun t ->
            t.GetMethods(bindingFlags)
            |> Seq.choose (fun m ->
                match m.GetCustomAttributes(typedefof<ImplementsAttribute>) with
                | Seq.Cons(:? ImplementsAttribute as attr, _) -> Some (attr.Name, m)
                | _ -> None))
        |> Map.ofSeq

    let public concreteExternalImplementations =
        seq [
            Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Array")
            Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Monitor")
            Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.RuntimeHelpersUtils")
            Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.CLRConfig")
            Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Interop")
        ]
        |> collectImplementations

    let public internalImplementations =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule
        |> collectImplementations

    let private runtimeExceptionsConstructors =
        Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Exceptions")
        |> Seq.singleton
        |> collectImplementations

    let public hasRuntimeExceptionsImplementation (fullMethodName : string) =
        Map.containsKey fullMethodName runtimeExceptionsConstructors

    let public getRuntimeExceptionsImplementation (fullMethodName : string) =
        runtimeExceptionsConstructors.[fullMethodName]
