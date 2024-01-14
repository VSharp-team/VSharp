using System;
using System.Collections.Specialized;
using System.Diagnostics;

namespace VSharp.CSharpUtils;

public static class ProcessUtils
{
    public static Process StartWithLogging(this ProcessStartInfo procInfo, Action<string> printInfo, Action<string> printError)
    {
        procInfo.RedirectStandardError = true;
        procInfo.RedirectStandardOutput = true;
        procInfo.UseShellExecute = false;

        var proc = new Process();
        proc.StartInfo = procInfo;

        proc.OutputDataReceived +=
            (_, e) =>
            {
                var data = e.Data;
                if (string.IsNullOrEmpty(data))
                    return;
                printInfo(data);
            };

        proc.ErrorDataReceived +=
            (_, e) =>
            {
                var data = e.Data;
                if (string.IsNullOrEmpty(data))
                    return;
                printError(data);
            };

        proc.Start();
        proc.BeginOutputReadLine();
        proc.BeginErrorReadLine();
        return proc;
    }

    public static bool IsSuccess(this Process proc)
    {
        Debug.Assert(proc.HasExited);
        return proc.ExitCode == 0;
    }
}
