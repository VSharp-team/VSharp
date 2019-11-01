namespace VSharp.Core

open VSharp

[<AutoOpen>]
module API =
    val Enter : locationBinding -> state -> ('a -> 'b) -> ('a -> 'b)

    val Configure : IActivator -> unit
    val ConfigureSolver : ISolver -> unit
    val ConfigureSimplifier : IPropositionalSimplifier -> unit
    val Reset : unit -> unit
    val SaveConfiguration : unit -> unit
    val Restore : unit -> unit

    val HigherOrderApply : ICodeLocation -> state -> (term * state -> 'a) -> 'a
    val BranchStatements : state -> (state -> (term * state -> 'a) -> 'b) -> (state -> (term * state -> 'a) -> 'a) -> (state -> (term * state -> 'a) -> 'a) -> (term * state -> 'a) -> 'b
    val BranchStatementsOnNull : state -> term -> (state -> (term * state -> 'a) -> 'a) -> (state -> (term * state -> 'a) -> 'a) -> (term * state -> 'a) -> 'a
    val BranchExpressions : state -> ((term -> 'a) -> 'b) -> ((term -> 'a) -> 'a) -> ((term -> 'a) -> 'a) -> (term -> 'a) -> 'b
    val StatedConditionalExecution : (state -> (state -> (term * state -> 'a) -> 'b) -> (state -> ('item * state -> 'a) -> 'a) -> (state -> ('item * state -> 'a) -> 'a) -> ((term * 'item) list -> 'item) -> (term list -> state list -> state) -> (term -> term -> 'item -> 'item -> 'item) -> (term -> term -> state -> state -> state) -> (term -> 'item) -> ('item * state -> 'a) -> 'b)

    val GuardedApplyExpression : term -> (term -> term) -> term
    val GuardedStatedApplyStatementK : state -> term -> (state -> term -> (term * state -> 'a) -> 'a) -> (term * state -> 'a) -> 'a
    val GuardedErroredStatedApplyk : (state -> term -> ('item * state -> 'a) -> 'a) -> (term -> 'item) -> state -> term -> ((term * 'item) list -> 'item) -> (term list -> state list -> state) -> ('item * state -> 'a) -> 'a

    val PerformBinaryOperation : OperationType -> bool -> state -> term -> term -> (term * state -> 'a) -> 'a
    val PerformUnaryOperation : OperationType -> bool -> state -> termType -> term -> (term * state -> 'a) -> 'a

    [<AutoOpen>]
    module Terms =
        val Nop : term
        val Error : term -> term
        val Concrete : 'a -> termType -> term
        val Constant : string -> ISymbolicConstantSource -> termType -> term
        val Expression : operation -> term list -> termType -> term
        val Struct : string heap -> termType -> term
        val Class : string heap -> term
        val Union : (term * term) list -> term

        val True : term
        val False : term

        val MakeNullRef : unit -> term
        val MakeDefault : termType -> term -> term
        val MakeNumber : 'a -> term

        val TypeOf : term -> termType
        val BaseTypeOfRef : term -> termType
        val SightTypeOfRef : term -> termType

        val isStruct : term -> bool
        val isReference : term -> bool
        val IsNullReference : term -> term

        val (|True|_|) : term -> unit option
        val (|False|_|) : term -> unit option
        val (|LazyInstantiation|_|) : ISymbolicConstantSource -> (term * 'a generalizedHeap option * bool) option
        val (|RecursionOutcome|_|) : ISymbolicConstantSource -> (ICodeLocation * state * term option * bool) option
        val (|Conjunction|_|) : term -> term list option
        val (|Disjunction|_|) : term -> term list option

        val ConstantsOf : term seq -> term System.Collections.Generic.ISet

        val AddConditionToState : state -> term -> state

    module RuntimeExceptions =
        val InvalidCastException : state -> (term -> 'a) -> 'a * state
        val TypeInitializerException : string -> term -> state -> (term -> 'a) -> 'a * state
        val IndexOutOfRangeException : state -> (term -> 'a) -> 'a * state
        val InvalidProgramException : state -> (term -> 'a) -> 'a * state

    module Types =
        val FromDotNetType : state -> System.Type -> termType
        val ToDotNetType : termType -> System.Type

        val SizeOf : termType -> int

        val TLength : termType
        val IsBool : termType -> bool
        val IsInteger : termType -> bool
        val IsReal : termType -> bool
        val IsPointer : termType -> bool
        val IsValueType : termType -> term

        val String : termType
        val (|StringType|_|) : termType -> unit option

        val elementType : termType -> termType

        val TypeIsType : termType -> termType -> term
        val TypeIsNullable : termType -> term
        val TypeIsRef : termType -> term -> term
        val RefIsType : term -> termType -> term
        val RefIsRef : term -> term -> term
        val IsCast : state -> termType -> term -> term
        val Cast : state -> term -> termType -> bool -> (state -> term -> termType -> term * state) -> (term * state -> 'b) -> 'b
        val CastConcrete : bool -> 'a -> System.Type -> term
        val CastReferenceToPointer : state -> term -> term

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
        val EmptyState : state

        val PopStack : state -> state
        val PopTypeVariables : state -> state
        val NewStackFrame : state -> IFunctionIdentifier -> (stackKey * term symbolicValue * termType) list -> state
        val NewScope : state -> (stackKey * term symbolicValue * termType) list -> state
        val NewTypeVariables : state -> (typeId * termType) list -> state

        val ReferenceField : term -> string -> termType -> term
        val ReferenceLocalVariable : stackKey -> term
        val ReferenceStaticField : termType -> string -> termType -> term
        val ReferenceArrayIndex : state -> term -> term list -> term * state

        val Dereference : state -> term -> term * state
        val DereferenceWithoutValidation : state -> term -> term
        val DereferenceLocalVariable : state -> stackKey -> term * state
        val Mutate : state -> term -> term -> term * state
        val ReadBlockField : term -> string -> termType -> term

        val AllocateOnStack : state -> stackKey -> termType -> term -> state
        val AllocateInHeap : state -> termType -> term -> term * state
        val AllocateDefaultStatic : state -> termType -> state
        val MakeDefaultBlock : termType -> fql -> term
        val AllocateDefaultBlock : state -> termType -> term * state
        val AllocateDefaultArray : state -> term list -> termType -> term * state
        val AllocateInitializedArray : state -> term list -> int -> termType -> term -> term * state
        val AllocateString : string -> state -> term * state

        val IsTypeNameInitialized : termType -> state -> term
        val Dump : state -> string

        val ArrayLength : term -> term
        val ArrayRank : term -> term
        val ArrayLengthByDimension : state -> term -> term -> term * state
        val ArrayLowerBoundByDimension : state -> term -> term -> term * state

        val StringLength : state -> term -> term * state
        val StringCtorOfCharArray : state -> term -> term -> term * state

    module Database =
        val QuerySummary : ICodeLocation -> codeLocationSummary
