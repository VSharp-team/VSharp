using System.Linq;
using System.Reflection;

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

    public static IntPtr GetExternPtr(MethodInfo mInfo)
    {
        string libName = "";
        string methodName = "";

        foreach (var attr in mInfo.CustomAttributes)
        {
            if (attr.AttributeType.Name == "DllImportAttribute")
            {
                libName = attr.ConstructorArguments.First().ToString();
                foreach (var arg in attr.NamedArguments)
                {
                    if (arg.MemberName == "EntryPoint")
                        methodName = arg.TypedValue.ToString();

                }
            }
        }

        libName = libName.Replace("\"", "");
        methodName = methodName.Replace("\"", "");

        if (!NativeLibrary.TryLoad(libName, Assembly.GetCallingAssembly(), null, out IntPtr libref))
        {
            throw new Exception("Could not open extern library");
        }

        return libref.GetFunction(methodName);
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

        if (!d.IsApplied)
            throw new Exception("Could not apply detour");

        return d;
    }
}
