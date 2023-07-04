namespace VSharp.CSharpUtils;

using System;
using System.Reflection;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using MonoMod.RuntimeDetour;
using MonoMod.Utils;
using System.ComponentModel;

public static class ExternMocker
{
    public static bool ExtMocksSupported =
        !OperatingSystem.IsMacOS()
        || RuntimeInformation.OSArchitecture == Architecture.X86
        || RuntimeInformation.OSArchitecture == Architecture.X64;

    private static List<NativeDetour> _detours = new();

    public static IntPtr GetExternPtr(string libName, string methodName)
    {
        var assembly = Assembly.GetCallingAssembly();
        if (!NativeLibrary.TryLoad(libName, assembly, null, out IntPtr libRef))
        {
            throw new Exception("Could not open extern library");
        }

        return libRef.GetFunction(methodName);
    }

    public static void BuildAndApplyDetour(IntPtr from, IntPtr to)
    {
        bool manualApply = PlatformHelper.Is(Platform.MacOS);

        var config = new NativeDetourConfig
        {
            ManualApply = manualApply
        };
        NativeDetour d = new NativeDetour(from, to, config);

        if (manualApply) {
            try {
                d.Apply();
            } catch (Win32Exception) {
                try
                {
                    d.Dispose();
                }
                finally
                {
                    throw new Exception("Could not apply extern mock");
                }
            }
        }

        if (!d.IsApplied)
            throw new Exception("Could not apply extern mock");

        _detours.Add(d);
    }

    public static void UnPatch()
    {
        foreach (var d in _detours)
        {
            d.Undo();
        }
        _detours.Clear();
    }
}
