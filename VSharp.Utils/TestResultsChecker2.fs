namespace VSharp

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text
open System.Xml
open System.Xml.Serialization

[<CLIMutable>]
[<Serializable>]
[<XmlType "Statement">]
type coveredStatement = {
    [<XmlAttribute "FileIndex">]
    fileIndex : int
    [<XmlAttribute "Line">]
    line : int
    [<XmlAttribute "Column">]
    column : int
    [<XmlAttribute "EndLine">]
    endLine : int
    [<XmlAttribute "EndColumn">]
    endColumn : int
    [<XmlAttribute "Covered">]
    covered : string
}

type TestResultsChecker2(method : MethodInfo, tempDirectory : DirectoryInfo, runnerDirectory : DirectoryInfo) =
    
    do
        Directory.CreateDirectory(tempDirectory.FullName) |> ignore
    
    let coveredStatements = HashSet<coveredStatement>()
    
    let mutable totalStatements = -1    
    let mutable errorString : string option = None
    
    let runDotnet args =
        let output = StringBuilder()
        let error = StringBuilder()
        let info = ProcessStartInfo()
        info.WorkingDirectory <- tempDirectory.FullName
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

(*    let run =
        match expectedCoverage with
        | Some _ ->
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
        | None ->
            fun (directory : DirectoryInfo) ->*)

                
    let run() =
        let code, _, error = runDotnet <| $" {runnerDirectory.FullName}{Path.DirectorySeparatorChar}VSharp.TestRunner.dll {tempDirectory.FullName}"
        code = 0, error
        
    let runAndCover() =
        let _, localTools, _ = runDotnet "tool list"
        let globalArg =
            if localTools.Contains "dotcover" then ""
            else
                let _, globalTools, _ = runDotnet "tool list -g"
                if globalTools.Contains "dotcover" then "-g"
                else raise (InvalidOperationException "JetBrains.DotCover tool not found! Either install it locally by running 'dotnet tool restore' in build directory, or globally by running 'dotnet tool install JetBrains.dotCover.GlobalTool -g'")
        let filters = ["-:module=Microsoft.*"; "-:module=FSharp.*"; "-:class=VSharp.*"; "-:module=VSharp.Utils"]
        let code, _, error = runDotnet <| sprintf "dotcover --dcFilters=\"%s\" %s%cVSharp.TestRunner.dll %s --dcReportType=DetailedXML %s" (filters |> join ";") runnerDirectory.FullName Path.DirectorySeparatorChar tempDirectory.FullName globalArg
        code = 0, error

    let rec typeName4Dotcover (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let name = typ.GetGenericTypeDefinition().FullName.Replace("+", ".")
            let index = name.IndexOf("`")
            let trimmedName = if index >= 0 then name.Substring(0, index) else name
            sprintf "%s<%s>" trimmedName (args |> Array.map typeName4Dotcover |> join ",")
        elif typ.IsGenericParameter then typ.Name
        else typ.FullName.Replace("+", ".") // Replace + in inner class names with dots

    let splitTypeName (typeName : string) =
        // NOTE: splitting inner classes
        let name = typeName.Split([|'+'|])
        // NOTE: deleting namespace from first class
        name.[0] <- Array.last (name.[0].Split([|'.'|]))
        name

    let rec declaringTypeName4Dotcover (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let names = typ.GetGenericTypeDefinition().FullName |> splitTypeName
            let handleGenericsInName (name : string) =
                let index = name.IndexOf("`")
                let trimmedName = if index >= 0 then name.Substring(0, index) else name
                sprintf "%s<%s>" trimmedName (args |> Array.map typeName4Dotcover |> join ",")
            names.[names.Length - 1] <- handleGenericsInName names.[names.Length - 1]
            names
        else typ.FullName |> splitTypeName        

    let getCoverageFromReport() =
        if String.IsNullOrEmpty method.DeclaringType.Namespace then
            __notImplemented__() // TODO: does coverage report in this case omit the namespace?
        // TODO: we might also have inner classes and properties
        let assemblyName = method.Module.Assembly.GetName().Name
        let namespaceName = method.DeclaringType.Namespace
        let typeNames = declaringTypeName4Dotcover method.DeclaringType
        let typeSection = Array.map (fun t -> $"/Type[@Name='{t}']") typeNames |> join ""
        let nameOfParameter (p : ParameterInfo) = typeName4Dotcover p.ParameterType
        let parametersTypes = method.GetParameters() |> Seq.map nameOfParameter |> join ","
        let returnType = method.ReturnType |> typeName4Dotcover
        let methodName = $"{method.Name}({parametersTypes}):{returnType}"
        let xmlMethodPath = $"/Root/Assembly[@Name='{assemblyName}']/Namespace[@Name='{namespaceName}']{typeSection}/Method[@Name='{methodName}']"
        let doc = XmlDocument()
        let docPath = Path.Combine(tempDirectory.FullName, "dotCover.Output.xml")
        assert(File.Exists(docPath))
        doc.Load(docPath)
        let xmlMethodNodes = doc.DocumentElement.SelectNodes xmlMethodPath
        match xmlMethodNodes.Count with
        | 0 -> invalidOp $"Coverage results for {method} not found!"
        | 1 ->
            let methodNode = xmlMethodNodes.[0]
            totalStatements <- (methodNode.Attributes.GetNamedItem("TotalStatements") :?> XmlAttribute).Value |> Int32.Parse
            
            let xmlSerializer = XmlSerializer(typeof<coveredStatement>)
            
            let deserializeCoveredStatement (xmlNode : string) =
                use stream = new MemoryStream(Encoding.ASCII.GetBytes xmlNode)
                xmlSerializer.Deserialize stream :?> coveredStatement

            methodNode.ChildNodes
                |> Seq.cast<XmlNode>
                |> Seq.map (fun n -> deserializeCoveredStatement n.OuterXml)
                |> Seq.filter (fun s -> s.covered = "True")
                |> Seq.iter (ignore << coveredStatements.Add)
        | _ -> invalidOp $"Invalid query of coverage results for {method}!"
        
    let fail() =
        failwith ("Running test results checker failed: " + errorString.Value)
        
    let tempDirectoryIsEmpty() =
        Directory.EnumerateFiles(tempDirectory.FullName, "*.vst") |> Seq.isEmpty
        
    let getCoverage() =
        Math.Round(double coveredStatements.Count / double totalStatements * 100.0, MidpointRounding.AwayFromZero)
    
    member x.AddTest(vstFile : FileInfo) =
        assert(vstFile.Extension = ".vst")
        let destFilePath = Path.Combine(tempDirectory.FullName, vstFile.Name)
        if not <| File.Exists destFilePath then
            File.Copy(vstFile.FullName, destFilePath)
        
    member x.AddTests(vstFiles : FileInfo seq) =
        vstFiles |> Seq.iter x.AddTest
    
    member x.Run() =
        if errorString.IsSome then fail()
        if tempDirectoryIsEmpty() then
            if totalStatements = -1 then invalidOp "Tests were not added" else ()
        else
            let success, error = run()
            if not success then
                errorString <- Some error
                fail()
    
    member x.RunAndCover() =
        if errorString.IsSome then fail()            
        let coverage = x.Cover()        
        if errorString.IsSome then fail()
        coverage
    
    member x.Cover() =
        if tempDirectoryIsEmpty() then
            if totalStatements = -1 then invalidOp "Tests were not added"
            getCoverage()
        else
            let success, error = runAndCover()
            getCoverageFromReport()
            Directory.GetFiles(tempDirectory.FullName) |> Seq.iter File.Delete
            if not success then
                errorString <- Some error                
            getCoverage()
