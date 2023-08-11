namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal PlatformHelper =

    [<Implements("System.Int32 System.Threading.PlatformHelper.get_ProcessorCount()")>]
    [<Implements("System.Int32 System.Environment.get_ProcessorCount()")>]
    val internal get_ProcessorCount : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Lzcnt.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Lzcnt+X64.get_IsSupported()")>]
    val internal lzcntIsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.ArmBase.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.AdvSimd+Arm64.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.ArmBase+Arm64.get_IsSupported()")>]
    val internal armBaseIsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Avx2.get_IsSupported()")>]
    val internal avx2IsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Sse2.get_IsSupported()")>]
    val internal sse2IsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.X86Base.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.X86Base+X64.get_IsSupported()")>]
    val internal x86BaseIsSupported : state -> term list -> term
