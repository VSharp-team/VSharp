namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL

module internal Runtime_CompilerServices_RuntimeHelpers =

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsBitwiseEquatable()")>]
    val IsBitwiseEquatable : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsReferenceOrContainsReferences()")>]
    val IsReferenceOrContainsReferences : state -> term list -> term

    [<Implements("System.Int32 System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(System.Object)")>]
    val CommonGetHashCode : state -> term list -> term

    [<Implements("System.Int32 System.ValueType.GetHashCode(this)")>]
    val ValueTypeGetHashCode : state -> term list -> term

    [<Implements("System.Int32 System.Enum.GetHashCode(this)")>]
    val EnumGetHashCode : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.Equals(System.Object, System.Object)")>]
    val Equals : state -> term list -> term

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.EnumEquals(T, T)")>]
    val EnumEquals : state -> term list -> term

    [<Implements("System.Void System.Runtime.CompilerServices.RuntimeHelpers._RunClassConstructor(System.RuntimeType)")>]
    [<Implements("System.Void System.Runtime.CompilerServices.RuntimeHelpers.RunClassConstructor(System.Runtime.CompilerServices.QCallTypeHandle)")>]
    val RunStaticCtor : IInterpreter -> cilState -> term list -> cilState list

    [<Implements("System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.TryEnsureSufficientExecutionStack()")>]
    val TryEnsureSufficientExecutionStack : state -> term list -> term

    [<Implements("System.Void System.Runtime.CompilerServices.RuntimeHelpers.EnsureSufficientExecutionStack()")>]
    val EnsureSufficientExecutionStack : state -> term list -> term

    [<Implements("System.String System.Exception.get_Source(this)")>]
    val ExceptionGetSource : state -> term list -> term

    [<Implements("System.String System.BadImageFormatException.ToString(this)")>]
    val BadImageFormatExceptionToString : state -> term list -> term

    [<Implements("System.String System.BadImageFormatException.get_Message(this)")>]
    val BadImageFormatExceptionGetMessage : state -> term list -> term
