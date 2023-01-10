namespace VSharp.CSharpUtils;

using System.Collections.Generic;
using System.Reflection.Emit;
using System;
using HarmonyLib;
using System.Runtime.InteropServices;

public static class ExternMocker
{
    public static bool ExtMocksSupported = !OperatingSystem.IsMacOS() |
                                          RuntimeInformation.OSArchitecture == Architecture.X86 |
                                          RuntimeInformation.OSArchitecture == Architecture.X64;

    static IEnumerable<CodeInstruction> ProcTranspiler(IEnumerable<CodeInstruction> instructions)
    {
        yield return new CodeInstruction(OpCodes.Ret);
    }

    static IEnumerable<CodeInstruction> FuncTranspiler(IEnumerable<CodeInstruction> instructions)
    {
        yield return new CodeInstruction(OpCodes.Newobj,
            typeof(NullReferenceException).GetConstructor(System.Array.Empty<Type>()));
        yield return new CodeInstruction(OpCodes.Throw);
    }
}
