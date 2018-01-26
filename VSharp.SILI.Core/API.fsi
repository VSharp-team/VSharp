namespace VSharp.Core

open VSharp

[<AutoOpen>]
module API =
    val Enter : locationBinding -> state -> ('a -> 'b) -> ('a -> 'b)

    val Configure : IActivator -> IInterpreter -> unit
    val Reset : unit -> unit
    val SaveConfiguration : unit -> unit
    val Restore : unit -> unit

    val InterpretEntryPoint : IFunctionIdentifier -> (statementResult * state -> 'a) -> 'a
    val Explore : IFunctionIdentifier -> (statementResult * state -> 'a) -> 'a

    val Call : IFunctionIdentifier -> state -> (state -> (statementResult * state -> 'a) -> 'a) -> (statementResult * state -> 'a) -> 'a
    val InvokeAfter : bool -> (statementResult * state) -> (state -> (statementResult * state -> 'a) -> 'a) -> (statementResult * state -> 'a) -> 'a

    val BranchStatements : state -> (state -> (term * state -> 'a) -> 'b) -> (state -> (statementResult * state -> 'a) -> 'a) -> (state -> (statementResult * state -> 'a) -> 'a) -> (statementResult * state -> 'a) -> 'b
    val BranchExpressions : state -> (state -> (term * state -> 'a) -> 'b) -> (state -> (term * state -> 'a) -> 'a) -> (state -> (term * state -> 'a) -> 'a) -> (term * state -> 'a) -> 'b
    val BranchStatementsOnNull : state -> term -> (state -> (statementResult * state -> 'a) -> 'a) -> (state -> (statementResult * state -> 'a) -> 'a) -> (statementResult * state -> 'a) -> 'a

    val GuardedApplyExpressionK : term -> (term -> (term -> 'a) -> 'a) -> (term -> 'a) -> 'a
    val GuardedApplyExpression : term -> (term -> term) -> term
    val GuardedApplyStatement : state -> term -> (state -> term -> (statementResult * state -> 'a) -> 'a) -> (statementResult * state -> 'a) -> 'a
    val GuardedApplyStatelessStatement : term -> (term -> statementResult) -> statementResult

    val PerformBinaryOperation : OperationType -> bool -> state -> System.Type -> term -> term -> (term * state -> 'a) -> 'a
    val PerformUnaryOperation : OperationType -> bool -> state -> termType -> term -> (term * state -> 'a) -> 'a

    [<AutoOpen>]
    module Terms =
        val Nop : term
        val Error : term -> term
        val Concrete : 'a -> termType -> term
        val Constant : string -> ISymbolicConstantSource -> termType -> term
        val Expression : operation -> term list -> termType -> term
        val Union : (term * term) list -> term

        val True : term
        val False : term

        val MakeNullRef : termType -> term
        val MakeDefault : termType -> term
        val MakeNumber : 'a -> term
        val MakeString : int -> 'a -> term
        val MakeLambda : 'a symbolicLambda -> termType -> term
        val MakeDefaultArray : term list -> termType -> term
        val MakeInitializedArray : int -> termType -> term -> term

        val TypeOf : term -> termType
        val (|Lambda|_|) : termNode -> 'a symbolicLambda option

    module RuntimeExceptions =
        val NullReferenceException : state -> (term -> 'a) -> 'a * state
        val InvalidCastException : state -> (term -> 'a) -> 'a * state
        val TypeInitializerException : string -> term -> state -> (term -> 'a) -> 'a * state
        val IndexOutOfRangeException : state -> (term -> 'a) -> 'a * state

    module Types =
        val FromDotNetType : System.Type -> termType
        val ToDotNetType : termType -> System.Type
        val WrapReferenceType : termType -> termType
        val NewTypeVariable : System.Type -> termType

        val SizeOf : termType -> int

        val TLength : termType
        val IsInteger : termType -> bool

        val CanCast : state -> termType -> term -> term * state
        val Cast : state -> term -> termType -> bool -> (state -> term -> termType -> statementResult * state) -> (term * state -> 'b) -> 'b
        val HierarchyCast : state -> term -> termType -> (state -> term -> termType -> statementResult * state) -> (term * state -> 'b) -> 'b
        val CastConcrete : 'a -> System.Type -> term
        val CastReferenceToPointer : state -> term -> (term * state -> 'a) -> 'a

    [<AutoOpen>]
    module public ControlFlowConstructors =
        val NoComputation : statementResult
        val NoResult : unit -> statementResult
        val Break : unit -> statementResult
        val Continue : unit -> statementResult
        val Return : term -> statementResult
        val Throw : term -> statementResult
        val Guarded : (term * statementResult) list -> statementResult

    module public ControlFlow =
        val ResultToTerm : statementResult -> term
        val ThrowOrReturn : term -> statementResult
        val ThrowOrIgnore : term -> statementResult
        val ConsumeErrorOrReturn : (term -> statementResult) -> term -> statementResult
        val ComposeSequentially : statementResult -> statementResult -> state -> state -> statementResult * state
        val ConsumeBreak : statementResult -> statementResult
        val PickOutExceptions : statementResult -> (term * term) option * (term * statementResult) list

    [<AutoOpen>]
    module public Operators =
        val (!!) : term -> term
        val (&&&) : term -> term -> term
        val (|||) : term -> term -> term
        val (===) : term -> term -> term
        val (!==) : term -> term -> term

    module public Arithmetics =
        val (===) : term -> term -> term
        val (!==) : term -> term -> term
        val (<<) : term -> term -> term
        val (<<=) : term -> term -> term
        val (>>) : term -> term -> term
        val (>>=) : term -> term -> term
        // Lightweight version: divide by zero exceptions are ignored!
        val (%%%) : term -> term -> term

    module public Memory =
        val PopStack : state -> state
        val NewStackFrame : state -> IFunctionIdentifier -> (stackKey * term symbolicValue * termType option) list -> state
        val NewScope : state -> (stackKey * term symbolicValue * termType option) list -> state

        val ReferenceField : state -> bool -> string -> termType -> term -> term * state
        val ReferenceLocalVariable : state -> stackKey -> bool -> term
        val ReferenceStaticField : state -> bool -> string -> termType -> string -> term * state
        val ReferenceArrayIndex : state -> term -> term list -> term * state

        val Dereference : state -> term -> term * state
        val DereferenceLocalVariable : state -> stackKey -> term * state
        val Mutate : state -> term -> term -> term * state

        val AllocateOnStack : state -> stackKey -> term -> state
        val AllocateInHeap : state -> term -> term * state
        val AllocateDefaultStatic : state -> string -> state
        val MakeDefaultStruct : string -> term

        val IsTypeNameInitialized : string -> state -> term
        val Dump : state -> string

        val ArrayLength : term -> term
        val ArrayLengthByDimension : state -> term -> term -> term * state
        val ArrayLowerBoundByDimension : state -> term -> term -> term * state
