namespace VSharp.CSharpUtils;

using System.Collections.Generic;
using System.Reflection.Emit;
using System;
using System.Runtime.InteropServices;
using MonoMod.RuntimeDetour;
using MonoMod.Utils;
using System.ComponentModel;

public static class ExternMocker
{
    public static bool ExtMocksSupported = !OperatingSystem.IsMacOS() |
                                          RuntimeInformation.OSArchitecture == Architecture.X86 |
                                          RuntimeInformation.OSArchitecture == Architecture.X64;

    public static IntPtr GetExternPtr(string libName, string methodName)
    {
        
        // if (!(PlatformHelper.Is(Platform.Linux) && DynDll.TryOpenLibrary("libc.so.6", out IntPtr libc)) &&
        //     !(PlatformHelper.Is(Platform.MacOS) && DynDll.TryOpenLibrary("/usr/lib/libc.dylib", out libc)) &&
        //     !DynDll.TryOpenLibrary($"libc.{PlatformHelper.LibrarySuffix}", out libc))
                // return;
        DynDll.TryOpenLibrary("libc.so.6", out IntPtr libc);
        return libc.GetFunction("rand");
    }

    public static NativeDetour buildAndApplyDetour(IntPtr from, IntPtr to)
    {
        bool manualApply = PlatformHelper.Is(Platform.MacOS);

        NativeDetour d = new NativeDetour(
            from,
            to,
            new NativeDetourConfig()
            {
                ManualApply = manualApply
            }
        );

        if (manualApply) {
            try {
                d.Apply();
            } catch (Win32Exception) {
                // Modern macOS doesn't give us permission to mess with this anymore.
                try {
                    d.Dispose();
                } catch (Win32Exception) {
                    // Of course it might also throw while trying to undo any made changes.
                }
            }
        }

        // if (!d.IsApplied)
        //     fail    

        return d;
    }
}
