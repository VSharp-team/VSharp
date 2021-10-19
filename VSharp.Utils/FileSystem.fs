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
        let started = cmd.Start()
        assert started
        cmd.WaitForExit()

    let private createSymlinkWindows target link =
        run "cmd.exe" (sprintf "/c if exist %s ( rmdir %s )" link link)
        run "cmd.exe" (sprintf "/c mklink /d %s %s" link target)

    let private createSymlinkUnix target link =
        run "unlink" link
        run "ln" (sprintf "-s \"%s\" \"%s\"" target link)

    let createSymlink =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then createSymlinkWindows
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then createSymlinkUnix
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then createSymlinkUnix
        else __notImplemented__()
