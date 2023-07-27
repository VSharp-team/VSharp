namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal PlatformHelper =

    [<Implements("System.Int32 System.Threading.PlatformHelper.get_ProcessorCount()")>]
    val internal get_ProcessorCount : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.X86.Lzcnt.get_IsSupported()")>]
    val internal lzcntIsSupported : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.Intrinsics.Arm.ArmBase.get_IsSupported()")>]
    val internal armIsSupported : state -> term list -> term
