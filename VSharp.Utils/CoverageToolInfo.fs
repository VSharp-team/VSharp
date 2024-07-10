namespace VSharp

open System.IO
open System.Runtime.InteropServices    

module CoverageToolInfo =
    
    [<Literal>]
    let DllName = "libvsharpCoverage"
    
    [<Literal>]
    let ProfilerUid = "{2800fea6-9667-4b42-a2b6-45dc98e77e9e}"
    
    [<Literal>]
    let Enable = "1"

    let (|Windows|MacOs|Linux|) _ =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then Windows
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then Linux
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then MacOs
        else __notImplemented__()

    let libExtension =
        match () with
        | Windows -> ".dll"
        | Linux -> ".so"
        | MacOs -> ".dylib"
    
    let fullPath =
        $"{Directory.GetCurrentDirectory()}{Path.DirectorySeparatorChar}{DllName}{libExtension}"
