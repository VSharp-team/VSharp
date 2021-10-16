namespace VSharp

open System
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text
open System.Xml

type CoverageTool(testDir : string, runnerDir : string) =
    let runDotnet args =
        let output = StringBuilder()
        let error = StringBuilder()
        let info = ProcessStartInfo()
        info.WorkingDirectory <- testDir
        info.FileName <- "dotnet"
        info.Arguments <- args
        info.UseShellExecute <- false
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        let proc = Process.Start info
        proc.OutputDataReceived.Add <| fun args -> output.Append args.Data |> ignore
        proc.ErrorDataReceived.Add <| fun args -> error.Append args.Data |> ignore
        proc.BeginOutputReadLine()
        proc.BeginErrorReadLine()
        proc.WaitForExit()
        proc.ExitCode, output.ToString(), error.ToString()

    let run =
        let _, localTools, _ = runDotnet "tool list"
        let globalArg =
            if localTools.Contains "dotcover" then ""
            else
                let _, globalTools, _ = runDotnet "tool list -g"
                if globalTools.Contains "dotcover" then "-g"
                else raise (InvalidOperationException "JetBrains.DotCover tool not found! Either install it locally by running 'dotnet tool restore' in build directory, or globally by running 'dotnet tool install JetBrains.dotCover.GlobalTool -g'")
        fun (directory : DirectoryInfo) ->
            let filters = ["-:module=Microsoft.*"; "-:module=FSharp.*"; "-:class=VSharp.*"; "-:module=VSharp.Utils"]
            let code, _, error = runDotnet <| sprintf "dotcover --dcFilters=\"%s\" %s%cVSharp.TestRunner.dll %s --dcReportType=DetailedXML %s" (filters |> join ";") runnerDir Path.DirectorySeparatorChar directory.FullName globalArg
            code = 0, error

    let mutable doc : XmlDocument = null

    let rec typeName4Dotcover (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let name = typ.GetGenericTypeDefinition().FullName.Replace("+", ".")
            let index = name.IndexOf("`")
            let trimmedName = if index >= 0 then name.Substring(0, index) else name
            sprintf "%s<%s>" trimmedName (args |> Array.map typeName4Dotcover |> join ",")
        elif typ.IsGenericParameter then typ.Name
        else typ.FullName.Replace("+", ".") // Replace + in inner class names with dots

    let rec declaringTypeName4Dotcover (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let name = typ.GetGenericTypeDefinition().Name.Replace("+", ".")
            let index = name.IndexOf("`")
            let trimmedName = if index >= 0 then name.Substring(0, index) else name
            sprintf "%s<%s>" trimmedName (args |> Array.map typeName4Dotcover |> join ",")
        else typ.Name

    member x.Run(testDir : DirectoryInfo) =
        let success, error = run testDir
        if not success then
            raise <| InvalidOperationException ("Running dotCover failed: " + error)
        doc <- XmlDocument()
        doc.Load(sprintf "%s%cdotCover.Output.xml" testDir.FullName Path.DirectorySeparatorChar)

    member x.GetCoverage (m : MethodInfo) =
        if String.IsNullOrEmpty m.DeclaringType.Namespace then
            __notImplemented__() // TODO: does coverage report in this case omit the namespace?
        // TODO: we might also have inner classes and properties
        let assemblyName = m.Module.Assembly.GetName().Name
        let namespaceName = m.DeclaringType.Namespace
        let typeName = declaringTypeName4Dotcover m.DeclaringType
        let nameOfParameter (p : ParameterInfo) = typeName4Dotcover p.ParameterType
        let parametersTypes = m.GetParameters() |> Seq.map nameOfParameter |> join ","
        let returnType = m.ReturnType |> typeName4Dotcover
        let methodName = sprintf "%s(%s):%s" m.Name parametersTypes returnType
        let xpath = sprintf "/Root/Assembly[@Name='%s']/Namespace[@Name='%s']/Type[@Name='%s']/Method[@Name='%s']/@CoveragePercent" assemblyName namespaceName typeName methodName
        let nodes = doc.DocumentElement.SelectNodes(xpath)
        match nodes.Count with
        | 0 -> raise (InvalidOperationException <| sprintf "Coverage results for %O not found!" m)
        | 1 ->
            let elem = nodes.[0]
            (elem :?> XmlAttribute).Value |> Int32.Parse
        | _ -> raise (InvalidOperationException <| sprintf "Invalid query of coverage results for %O!" m)
