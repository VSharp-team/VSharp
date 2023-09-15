namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

module internal SR =

    [<Implements("System.String System.SR.get_Arg_OverflowException()")>]
    val get_Arg_OverflowException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_SystemException()")>]
    val get_Arg_SystemException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_IndexOutOfRangeException()")>]
    val get_Arg_IndexOutOfRangeException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_NullReferenceException()")>]
    val get_Arg_NullReferenceException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArrayTypeMismatchException()")>]
    val get_Arg_ArrayTypeMismatchException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_InvalidHandle()")>]
    val get_Arg_InvalidHandle : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_InvalidOperationException()")>]
    val get_Arg_InvalidOperationException : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentOutOfRange_Index()")>]
    val get_ArgumentOutOfRange_Index : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_PlatformNotSupported()")>]
    val get_Arg_PlatformNotSupported : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_NotGenericTypeDefinition()")>]
    val get_Arg_NotGenericTypeDefinition : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArgumentException()")>]
    val get_Arg_ArgumentException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArgumentOutOfRangeException()")>]
    val get_Arg_ArgumentOutOfRangeException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArgumentNullException()")>]
    val get_Arg_ArgumentNullException : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentNull_Generic()")>]
    val get_ArgumentNull_Generic : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_DivideByZero()")>]
    val get_Arg_DivideByZero : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_ArithmeticException()")>]
    val get_Arg_ArithmeticException : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_KeyNotFoundWithKey()")>]
    val get_Arg_KeyNotFoundWithKey : state -> term list -> term

    [<Implements("System.String System.SR.get_InvalidOperation_EmptyStack()")>]
    val get_InvalidOperation_EmptyStack : state -> term list -> term

    [<Implements("System.String System.Exception.GetMessageFromNativeResources(System.Exception+ExceptionMessageKind)")>]
    val getMessageFromNativeResources : state -> term list -> term

    [<Implements("System.String System.SR.get_ConcurrentDictionary_ConcurrencyLevelMustBePositive()")>]
    val concurrencyLevelMustBePositive : state -> term list -> term

    [<Implements("System.String System.SR.get_ConcurrentDictionary_ConcurrencyLevelMustBeNegative()")>]
    val concurrencyLevelMustBeNegative : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentOutOfRange_BadYearMonthDay()")>]
    val get_ArgumentOutOfRange_BadYearMonthDay : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentOutOfRange_Count()")>]
    val get_ArgumentOutOfRange_Count : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentOutOfRange_StartIndex()")>]
    val get_ArgumentOutOfRange_StartIndex : state -> term list -> term

    [<Implements("System.String System.SR.get_ArgumentOutOfRange_SmallCapacity()")>]
    val get_ArgumentOutOfRange_SmallCapacity : state -> term list -> term

    [<Implements("System.String System.SR.get_Argument_HasToBeArrayClass()")>]
    val get_Argument_HasToBeArrayClass : state -> term list -> term

    [<Implements("System.String System.SR.get_ThreadLocal_Disposed()")>]
    val get_ThreadLocal_Disposed : state -> term list -> term

    [<Implements("System.String System.SR.get_Arg_NotImplementedException()")>]
    val get_Arg_NotImplementedException : state -> term list -> term

    [<Implements("System.String System.SR.get_Argument_InvalidTypeWithPointersNotSupported()")>]
    val get_Argument_InvalidTypeWithPointersNotSupported : state -> term list -> term
