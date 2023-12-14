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

    let private createSymlinkWindows (target : string) (link : string) =
        run "cmd.exe" $"/c if exist {link} ( rmdir {link} )"
        run "cmd.exe" $"/c mklink /d {link} {target}"

    let private createSymlinkUnix (target : string) (link : string) =
        if System.IO.Path.Exists link then run "unlink" link
        run "ln" $"-s \"{target}\" \"{link}\""

    let createSymlink =
        if RuntimeInformation.IsOSPlatform(OSPlatform.Windows) then createSymlinkWindows
        elif RuntimeInformation.IsOSPlatform(OSPlatform.Linux) then createSymlinkUnix
        elif RuntimeInformation.IsOSPlatform(OSPlatform.OSX) then createSymlinkUnix
        else __notImplemented__()
