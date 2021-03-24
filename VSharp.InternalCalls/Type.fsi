namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Type =
    [<Implements("System.Type System.Type.GetTypeFromHandle(System.RuntimeTypeHandle)")>]
    val internal GetTypeFromHandle : state -> term list -> term * state

    [<Implements("System.Reflection.RuntimeAssembly System.RuntimeTypeHandle.GetAssembly(System.RuntimeType)")>]
    val internal GetAssembly : state -> term list -> term * state

    [<Implements("System.Type System.Object.GetType(this)")>]
    val internal GetType : state -> term list -> term * state

     [<Implements("System.Boolean System.Type.op_Inequality(System.Type, System.Type)")>]
    val internal op_Inequality : state -> term list -> term * state

     [<Implements("System.Boolean System.Type.op_Equality(System.Type, System.Type)")>]
    val internal op_Equality : state -> term list -> term * state
