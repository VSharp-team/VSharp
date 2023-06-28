namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal SR =

    [<Implements("System.String System.SR.get_Arg_OverflowException()")>]
    val internal get_Arg_OverflowException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_SystemException()")>]
    val internal get_Arg_SystemException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_IndexOutOfRangeException()")>]
    val internal get_Arg_IndexOutOfRangeException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_NullReferenceException()")>]
    val internal get_Arg_NullReferenceException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArrayTypeMismatchException()")>]
    val internal get_Arg_ArrayTypeMismatchException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_InvalidHandle()")>]
    val internal get_Arg_InvalidHandle : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_InvalidOperationException()")>]
    val internal get_Arg_InvalidOperationException : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentOutOfRange_Index()")>]
    val internal get_ArgumentOutOfRange_Index : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_PlatformNotSupported()")>]
    val internal get_Arg_PlatformNotSupported : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_NotGenericTypeDefinition()")>]
    val internal get_Arg_NotGenericTypeDefinition : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArgumentException()")>]
    val internal get_Arg_ArgumentException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArgumentOutOfRangeException()")>]
    val internal get_Arg_ArgumentOutOfRangeException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArgumentNullException()")>]
    val internal get_Arg_ArgumentNullException : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentNull_Generic()")>]
    val internal get_ArgumentNull_Generic : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_DivideByZero()")>]
    val internal get_Arg_DivideByZero : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArithmeticException()")>]
    val internal get_Arg_ArithmeticException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_KeyNotFoundWithKey()")>]
    val internal get_Arg_KeyNotFoundWithKey : state -> term list -> term

    [<Implements("System.String System.SR.get_InvalidOperation_EmptyStack()")>]
    val internal get_InvalidOperation_EmptyStack : state -> term list -> term

    [<Implements("System.String System.Exception.GetMessageFromNativeResources(System.Exception+ExceptionMessageKind)")>]
    val internal getMessageFromNativeResources : state -> term list -> term

    [<Implements("System.String System.SR.get_ConcurrentDictionary_ConcurrencyLevelMustBePositive()")>]
    val internal concurrencyLevelMustBePositive : state -> term list -> term

    [<Implements("System.String System.SR.get_ConcurrentDictionary_ConcurrencyLevelMustBeNegative()")>]
    val internal concurrencyLevelMustBeNegative : state -> term list -> term
