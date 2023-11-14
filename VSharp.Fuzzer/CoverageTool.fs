namespace VSharp.Fuzzer

open System.Reflection
open System.Runtime.InteropServices
open Microsoft.FSharp.NativeInterop
open VSharp

#nowarn "9"

module private ExternalCalls =
    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetEntryMain(byte* assemblyName, int assemblyNameLength, byte* moduleName, int moduleNameLength, int methodToken)

    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void GetHistory(nativeint size, nativeint data)

    [<DllImport("libvsharpCoverage", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)>]
    extern void SetCurrentThreadId(int id)

    let inline castPtr ptr =
        ptr |> NativePtr.toVoidPtr |> NativePtr.ofVoidPtr

type internal CoverageTool() =
    let mutable entryMainWasSet = false

    do
        if Startup.isCoverageToolAttached () |> not then internalfail "Coverage tool wasn't attached"

    member this.GetRawHistory () =
        if not entryMainWasSet then Prelude.internalfail "Try call GetRawHistory, while entryMain wasn't set"
        let sizePtr = NativePtr.stackalloc<uint> 1
        let dataPtrPtr = NativePtr.stackalloc<nativeint> 1

        ExternalCalls.GetHistory(NativePtr.toNativeInt sizePtr, NativePtr.toNativeInt dataPtrPtr)

        let size = NativePtr.read sizePtr |> int
        let dataPtr = NativePtr.read dataPtrPtr

        let data = Array.zeroCreate<byte> size
        Marshal.Copy(dataPtr, data, 0, size)
        data

    member this.SetEntryMain (assembly: Assembly) (moduleName: string) (methodToken: int) =
        entryMainWasSet <- true
        let assemblyNamePtr = fixed assembly.FullName.ToCharArray()
        let moduleNamePtr = fixed moduleName.ToCharArray()
        let assemblyNameLength = assembly.FullName.Length
        let moduleNameLength = moduleName.Length

        ExternalCalls.SetEntryMain(
            ExternalCalls.castPtr assemblyNamePtr,
            assemblyNameLength,
            ExternalCalls.castPtr moduleNamePtr,
            moduleNameLength,
            methodToken
        )

    member this.SetCurrentThreadId id =
        ExternalCalls.SetCurrentThreadId(id)
