namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open VSharp
open VSharp.Core

type TestGenerator(outputDir : string) =
    let testPrefix = "VSharp.tests."
    let testExtension = ".vst"
    let mutable testNumber = 0u
    let mutable errorNumber = 1u
    let rootDir = Directory.CreateDirectory(outputDir)
    let mutable currentDir = rootDir

    let () =
        let testDirs = HashSet<string>()
        rootDir.EnumerateDirectories(testPrefix + "*") |> Seq.iter (fun dir -> dir.Name |> testDirs.Add |> ignore)
        let uniqueName = Seq.initInfinite id |> Seq.pick (fun i ->
            let name = testPrefix + i.ToString()
            if testDirs.Contains name then None else Some name)
        currentDir <- rootDir.CreateSubdirectory(uniqueName)
        let linkName = $"%s{rootDir.FullName}%c{Path.DirectorySeparatorChar}%s{testPrefix}last"
        FileSystem.createSymlink currentDir.FullName linkName

    let term2obj = function
        | {term = Concrete(v, _)} -> v
        | {term = Nop} -> null
        | _ -> __notImplemented__()

    let generateTest (m : MethodBase) (cilState : cilState) (name : string) =
        let test = Test m
        match cilState.state.model with
        | Some model ->
            model.Iter (fun kvp ->
                let value : obj = term2obj kvp.Value
                match kvp.Key with
                | StackReading key ->
                    match key with
                    | ParameterKey pi -> test.AddArg pi value
                    | ThisKey m ->
                        let pi = m.GetParameters().[0]
                        assert(pi.Name = "this")
                        test.AddArg pi value
                    | _ -> __notImplemented__()
                | _ -> __notImplemented__())
            let retVal = model.Eval cilState.Result
            test.Expected <- term2obj retVal
        | None ->
            m.GetParameters() |> Seq.iter (fun pi ->
                let defaultValue = System.Runtime.Serialization.FormatterServices.GetUninitializedObject pi.ParameterType
                test.AddArg pi defaultValue)
            let emptyModel = model.DefaultComplete
            let retVal = emptyModel.Eval cilState.Result
            test.Expected <- term2obj retVal
        test.Serialize $"%s{currentDir.FullName}%c{Path.DirectorySeparatorChar}%s{name}%s{testExtension}"

    interface IDisposable with
        override x.Dispose() =
            ()

    member x.GenerateTest (m : MethodBase) (cilState : cilState) =
        testNumber <- testNumber + 1u
        generateTest m cilState ("test" + testNumber.ToString())

    member x.GenerateError (m : MethodBase) (cilState : cilState) =
        errorNumber <- errorNumber + 1u
        generateTest m cilState ("error" + errorNumber.ToString())
