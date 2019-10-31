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
        Assembly.Load(new AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Array")
        |> Seq.singleton
        |> collectImplementations

    let public internalImplementations =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule
        |> collectImplementations
