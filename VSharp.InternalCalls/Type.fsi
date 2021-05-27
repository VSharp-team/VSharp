namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal Type =

    val fieldWithTypeInfo : fieldId

    val getActualType : state -> term -> Type

    [<Implements("System.Type System.Type.GetTypeFromHandle(System.RuntimeTypeHandle)")>]
    val GetTypeFromHandle : state -> term list -> (term * state) list

    [<Implements("System.Reflection.RuntimeAssembly System.RuntimeTypeHandle.GetAssembly(System.RuntimeType)")>]
    val GetAssembly : state -> term list -> (term * state) list

    [<Implements("System.Type System.Object.GetType(this)")>]
    val GetType : state -> term list -> (term * state) list

    [<Implements("System.Boolean System.Type.op_Inequality(System.Type, System.Type)")>]
    val opInequality : state -> term list -> term * state

    [<Implements("System.Boolean System.Type.op_Equality(System.Type, System.Type)")>]
    val opEquality : state -> term list -> term * state

    [<Implements("System.Boolean System.RuntimeTypeHandle.IsGenericTypeDefinition(System.RuntimeType)")>]
    val isGenericTypeDefinition : state -> term list -> term * state

    [<Implements("System.Boolean System.RuntimeTypeHandle.IsInterface(System.RuntimeType)")>]
    val isInterface : state -> term list -> term * state

    [<Implements("System.Boolean System.RuntimeTypeHandle.IsGenericVariable(System.RuntimeType)")>]
    val isGenericVariable : state -> term list -> term * state

    [<Implements("System.String System.RuntimeType.get_Name(this)")>]
    val get_Name : state -> term list -> term * state
