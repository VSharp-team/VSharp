namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal PlatformHelper =

    [<Implements("System.Int32 System.Threading.PlatformHelper.get_ProcessorCount()")>]
    [<Implements("System.Int32 System.Environment.get_ProcessorCount()")>]
    val get_ProcessorCount : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Lzcnt.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Lzcnt+X64.get_IsSupported()")>]
    val lzcntIsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.ArmBase.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.AdvSimd+Arm64.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.ArmBase+Arm64.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.AdvSimd.get_IsSupported()")>]
    val armBaseIsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Avx2.get_IsSupported()")>]
    val avx2IsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Sse2.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Sse2+X64.get_IsSupported()")>]
    val sse2IsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.X86Base.get_IsSupported()")>]
    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.X86Base+X64.get_IsSupported()")>]
    val x86BaseIsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.Vector128.get_IsHardwareAccelerated()")>]
    val vector128IsHardwareAccelerated : state -> term list -> term

    [<Implements("System.Boolean System.Net.Quic.QuicListener.get_IsSupported()")>]
    val quicListenerIsSupported : state -> term list -> term
