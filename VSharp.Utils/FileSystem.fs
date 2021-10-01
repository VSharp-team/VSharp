namespace VSharp

open System.Runtime.InteropServices
open System.Diagnostics

module FileSystem =
    let run executable args =
        use cmd = new Process()
        cmd.StartInfo.FileName <- executable
        cmd.StartInfo.Arguments <- args
        cmd.StartInfo.RedirectStandardInput <- false
        cmd.StartInfo.RedirectStandardOutput <- false
        cmd.StartInfo.CreateNoWindow <- true
        cmd.StartInfo.UseShellExecute <- false
        assert(cmd.Start())
        cmd.WaitForExit()

    let private createSymlinkWindows target link =
        __notImplemented__()
    let private createSymlinkLinux target link =
        run "unlink" link
        run "ln" (sprintf "-s \"%s\" \"%s\"" target link)
    let private createSymlinkOSX target link =
        __notImplemented__()

    let createSymlink =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then createSymlinkWindows
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then createSymlinkLinux
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then createSymlinkOSX
        else __notImplemented__()
