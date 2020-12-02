namespace VSharp.Core

open VSharp
open System.Reflection

[<AutoOpen>]
module API =
    val ConfigureSimplifier : IPropositionalSimplifier -> unit
    val Reset : unit -> unit
    val SaveConfiguration : unit -> unit
    val Restore : unit -> unit

    val BranchStatements : state -> (state -> (term * state -> 'a) -> 'b) -> (state -> (term * state -> 'a) -> 'a) -> (state -> (term * state -> 'a) -> 'a) -> ((term * state) list -> 'a) -> 'b
    val BranchStatementsOnNull : state -> term -> (state -> (term * state -> 'a) -> 'a) -> (state -> (term * state -> 'a) -> 'a) -> ((term * state) list -> 'a) -> 'a
    val BranchExpressions : ((term -> 'a) -> 'b) -> ((term -> 'a) -> 'a) -> ((term -> 'a) -> 'a) -> (term -> 'a) -> 'b
    val StatedConditionalExecution : (state -> (state -> (term * state -> 'a) -> 'b) -> (state -> ('item -> 'a) -> 'a) -> (state -> ('item -> 'a) -> 'a) -> ('item -> 'item -> 'item list) -> ('item  list -> 'a) -> 'b)
    val StatedConditionalExecutionAppendResults : (state -> (state -> (term * state -> 'a) -> 'b) -> (state -> (state list -> 'a) -> 'a) -> (state -> (state list -> 'a) -> 'a) -> (state list -> 'a) -> 'b)

    val GuardedApplyExpression : term -> (term -> term) -> term
    val GuardedApplyExpressionWithPC : pathCondition -> term -> (term -> term) -> term
    val GuardedStatedApplyStatementK : state -> term -> (state -> term -> (term * state -> 'a) -> 'a) -> ((term * state) list -> 'a) -> 'a
    val GuardedStatedApplyk : (state -> term -> ('item -> 'a) -> 'a) -> state -> term -> ('item list -> 'item list) -> ('item list -> 'a) -> 'a

    val PerformBinaryOperation : OperationType -> term -> term -> (term -> 'a) -> 'a
    val PerformUnaryOperation : OperationType -> symbolicType -> term -> (term -> 'a) -> 'a

    [<AutoOpen>]
    module Terms =
        val Nop : term
        val Concrete : 'a -> symbolicType -> term
        val Constant : string -> ISymbolicConstantSource -> symbolicType -> term
        val Expression : operation -> term list -> symbolicType -> term
        val Struct : pdict<fieldId, term> -> symbolicType -> term
        val Ref : address -> term
        val Ptr : address option -> symbolicType -> term option -> term
        val HeapRef : heapAddress -> symbolicType -> term
        val Union : (term * term) list -> term

        val True : term
        val False : term
        val NullRef : term

        val MakeBool : bool -> term

        val MakeFunctionResultConstant : state -> callSite -> term
        val MakeNumber : 'a -> term

        val TypeOf : term -> symbolicType
        val BaseTypeOfHeapRef : state -> term -> symbolicType

        val isStruct : term -> bool
        val isReference : term -> bool
        val IsNullReference : term -> term

        val (|True|_|) : term -> unit option
        val (|False|_|) : term -> unit option
        val (|Conjunction|_|) : term -> term list option
        val (|Disjunction|_|) : term -> term list option

        val ConstantsOf : term seq -> term System.Collections.Generic.ISet

        val HeapReferenceToBoxReference : term -> term

        val WithPathCondition : state -> term -> state

    module Types =
        val Numeric : System.Type -> symbolicType
        val ObjectType : symbolicType

        val FromDotNetType : state -> System.Type -> symbolicType
        val ToDotNetType : symbolicType -> System.Type

        val SizeOf : symbolicType -> int
        val RankOf : symbolicType -> int

        val TLength : symbolicType
        val IsBool : symbolicType -> bool
        val IsInteger : symbolicType -> bool
        val IsReal : symbolicType -> bool
        val IsPointer : symbolicType -> bool
        val IsValueType : symbolicType -> bool

        val String : symbolicType
        val (|StringType|_|) : symbolicType -> unit option

        val ElementType : symbolicType -> symbolicType

        val TypeIsType : symbolicType -> symbolicType -> term
        val TypeIsNullable : symbolicType -> bool
        val TypeIsRef : symbolicType -> term -> term
        val RefIsType : term -> symbolicType -> term
        val RefIsRef : term -> term -> term
        val IsCast : symbolicType -> term -> term
        val Cast : term -> symbolicType -> term
        val CastConcrete : 'a -> System.Type -> term
        val CastReferenceToPointer : state -> term -> term

    [<AutoOpen>]
    module public Operators =
        val (!!) : term -> term
        val (&&&) : term -> term -> term
        val (|||) : term -> term -> term
        val (===) : term -> term -> term
        val (!==) : term -> term -> term
        val conjunction : term seq -> term
        val disjunction : term seq -> term

    module public Arithmetics =
        val (===) : term -> term -> term
        val (!==) : term -> term -> term
        val (<<) : term -> term -> term
        val (<<=) : term -> term -> term
        val (>>) : term -> term -> term
        val (>>=) : term -> term -> term
        // Lightweight version: divide by zero exceptions are ignored!
        val (%%%) : term -> term -> term
        val Mul : term -> term -> term
        val IsZero : term -> term

    module public Memory =
        val EmptyState : state

        val PopStack : state -> state
        val PopTypeVariables : state -> state
        val NewStackFrame : state -> IFunctionIdentifier -> (stackKey * term symbolicValue * symbolicType) list -> bool -> state
        val NewTypeVariables : state -> (typeId * symbolicType) list -> state

        val ReferenceField : term -> fieldId -> term
        val ReferenceArrayIndex : term -> term list -> term

        val ReadSafe : state -> term -> term
        val ReadLocalVariable : state -> stackKey -> term
        val ReadThis : state -> MethodBase -> term
        val ReadArgument : state -> ParameterInfo -> term
        val ReadField : state -> term -> fieldId -> term
        val ReadArrayIndex : state -> term -> term list -> term
        val ReadStaticField : state -> symbolicType -> fieldId -> term
        val ReadDelegate : state -> term -> term

        val WriteSafe : state -> term -> term -> state list
        val WriteLocalVariable : state -> stackKey -> term -> state
        val WriteStructField : term -> fieldId -> term -> term
        val WriteClassField : state -> term -> fieldId -> term -> state list
        val WriteArrayIndex : state -> term -> term list -> term -> state list
        val WriteStaticField : state -> symbolicType -> fieldId -> term -> state

        val DefaultOf : symbolicType -> term
        val AllocateOnStack : state -> stackKey -> term -> state
        val AllocateTemporaryLocalVariable : state -> System.Type -> term -> term * state
        val MakeSymbolicThis : System.Reflection.MethodBase -> term
        val BoxValueType : state -> term -> term * state
        val AllocateDefaultStatic : state -> symbolicType -> state
        val AllocateDefaultClass : state -> symbolicType -> term * state
        val AllocateDefaultArray : state -> term list -> symbolicType -> term * state
        val AllocateString : string -> state -> term * state
        val AllocateDelegate : state -> term -> term * state
//        val ThrowException : state -> symbolicType -> term * state

        val IsTypeInitialized : state -> symbolicType -> term
        val Dump : state -> string

        val ArrayRank : state -> term -> term
        val ArrayLengthByDimension : state -> term -> term -> term
        val ArrayLowerBoundByDimension : state -> term -> term -> term

        val StringLength : state -> term -> term
        val StringCtorOfCharArray : state -> term -> term -> state list

        // TODO: get rid of all unnecessary stuff below!
        val ComposeStates : state -> state -> (state list -> 'a) -> 'a

        val Merge2States : state -> state -> state

    module Options =
        val HandleNativeInt : 'a -> 'a -> 'a

//    module Marshalling =
//        val Unmarshal : state -> obj -> term * state
//        val CanBeCalledViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> bool
//        val CallViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> (term * state -> 'a) -> 'a
