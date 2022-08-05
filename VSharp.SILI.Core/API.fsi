namespace VSharp.Core

open VSharp
open System
open System.Reflection

[<AutoOpen>]
module API =
    val ConfigureSolver : SolverInteraction.ISolver -> unit
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

    val ReleaseBranches : unit -> unit
    val AquireBranches : unit -> unit

    val PerformBinaryOperation : OperationType -> term -> term -> (term -> 'a) -> 'a
    val PerformUnaryOperation : OperationType -> term -> (term -> 'a) -> 'a

    val SolveTypes : model -> state -> (symbolicType[] * symbolicType[]) option
    val TryGetModel : state -> model option
    val ResolveCallVirt : state -> term -> concreteHeapAddress * symbolicType seq

    val ConfigureErrorReporter : (state -> unit) -> unit
    val ErrorReporter : unit -> (state -> term -> unit)

    [<AutoOpen>]
    module Terms =
        val Nop : term
        val Concrete : 'a -> Type -> term
        val Constant : string -> ISymbolicConstantSource -> Type -> term
        val Expression : operation -> term list -> Type -> term
        val Struct : pdict<fieldId, term> -> Type -> term
        val Ref : address -> term
        val Ptr : pointerBase -> Type -> term -> term
        val Slice : term -> term -> term -> term -> term
        val HeapRef : heapAddress -> Type -> term
        val Union : (term * term) list -> term

        val True : term
        val False : term
        val NullRef : Type -> term
        val MakeNullPtr : Type -> term
        val ConcreteHeapAddress : concreteHeapAddress -> term

        val MakeBool : bool -> term
        val MakeNumber : 'a -> term
        val MakeIntPtr : term -> term
        val AddressToBaseAndOffset : address -> pointerBase * term

        val TypeOf : term -> Type
        val TypeOfLocation : term -> Type
        val MostConcreteTypeOfHeapRef : state -> term -> Type
        val TypeOfAddress : state -> term -> Type

        val GetHashCode : term -> term

        val ReinterpretConcretes : term list -> Type -> obj

        val IsStruct : term -> bool
        val IsReference : term -> bool
        val IsPtr : term -> bool
        val IsConcrete : term -> bool
        val IsNullReference : term -> term

        val (|ConcreteHeapAddress|_|) : termNode -> concreteHeapAddress option

        val (|Combined|_|) : term -> (term list * Type) option

        val (|True|_|) : term -> unit option
        val (|False|_|) : term -> unit option
        val (|Negation|_|) : term -> term option
        val (|Conjunction|_|) : term -> term list option
        val (|Disjunction|_|) : term -> term list option
        val (|NullRef|_|) : term -> Type option
        val (|NonNullRef|_|) : term -> unit option
        val (|NullPtr|_|) : term -> unit option

        val (|StackReading|_|) : ISymbolicConstantSource -> option<stackKey>
        val (|HeapReading|_|) : ISymbolicConstantSource -> option<heapAddressKey * memoryRegion<heapAddressKey, vectorTime intervals>>
        val (|ArrayIndexReading|_|) : ISymbolicConstantSource -> option<bool * heapArrayIndexKey * memoryRegion<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>>>
        val (|VectorIndexReading|_|) : ISymbolicConstantSource -> option<bool * heapVectorIndexKey * memoryRegion<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>>
        val (|StackBufferReading|_|) : ISymbolicConstantSource -> option<stackBufferIndexKey * memoryRegion<stackBufferIndexKey, int points>>
        val (|StaticsReading|_|) : ISymbolicConstantSource -> option<symbolicTypeKey * memoryRegion<symbolicTypeKey, freeRegion<typeWrapper>>>
        val (|StructFieldSource|_|) : ISymbolicConstantSource -> option<ISymbolicConstantSource * fieldId>
        val (|StructFieldChain|_|) : ISymbolicConstantSource -> option<fieldId list * ISymbolicConstantSource>
        val (|HeapAddressSource|_|) : ISymbolicConstantSource -> option<ISymbolicConstantSource>
        val (|TypeInitializedSource|_|) : IStatedSymbolicConstantSource -> option<Type * symbolicTypeSet>
        val (|TypeSubtypeTypeSource|_|) : ISymbolicConstantSource -> option<Type * Type>
        val (|RefSubtypeTypeSource|_|) : ISymbolicConstantSource -> option<heapAddress * Type>
        val (|TypeSubtypeRefSource|_|) : ISymbolicConstantSource -> option<Type * heapAddress>
        val (|RefSubtypeRefSource|_|) : ISymbolicConstantSource -> option<heapAddress * heapAddress>
        val (|MockResultSource|_|) : ISymbolicConstantSource -> option<concreteHeapAddress * MethodMock>

        val GetHeapReadingRegionSort : ISymbolicConstantSource -> regionSort

        val HeapReferenceToBoxReference : term -> term

        val AddConstraint : state -> term -> unit
        val IsFalsePathCondition : state -> bool
        val Contradicts : state -> term -> bool
        val PathConditionToSeq : pathCondition -> term seq
        val EmptyPathCondition : pathCondition

    module Types =

        val SizeOf : Type -> int
        val RankOf : Type -> int

        val IndexType : Type
        val TLength : Type
        val IsBool : Type -> bool
        val IsInteger : Type -> bool
        val IsReal : Type -> bool
        val IsNumeric : Type -> bool
        val IsPointer : Type -> bool
        val IsValueType : Type -> bool
        val IsArrayType : Type -> bool
        val (|Bool|_|) : Type -> unit option
        val (|StringType|_|) : Type -> unit option

        val ElementType : Type -> Type
        val ArrayTypeToSymbolicType : arrayType -> Type

        val TypeIsType : Type -> Type -> term
        val IsNullable : Type -> bool
        val TypeIsRef :  state -> Type -> term -> term
        val RefIsType : state -> term -> Type -> term
        val RefIsAssignableToType : state -> term -> Type -> term
        val RefIsRef : state -> term -> term -> term
        val IsCast : state -> term -> Type -> term
        val Cast : term -> Type -> term

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
        val Sub : term -> term -> term
        val Add : term -> term -> term
        val IsZero : term -> term

        val Acos : term -> term
        val Asin : term -> term
        val Atan : term -> term
        val Atan2 : term -> term -> term
        val Ceiling : term -> term
        val Cos : term -> term
        val Cosh : term -> term
        val Floor : term -> term
        val Sin : term -> term
        val Tan : term -> term
        val Sinh : term -> term
        val Tanh : term -> term
        val Round : term -> term
        val Sqrt : term -> term
        val Log : term -> term
        val Log10 : term -> term
        val Exp : term -> term
        val Pow : term -> term -> term
        val Abs : term -> term
        val AbsS : term -> term

    module public EvaluationStack =
        val Pop : evaluationStack -> term * evaluationStack
        val PopMany : int -> evaluationStack -> term list * evaluationStack
        val Push : term -> evaluationStack -> evaluationStack
        val PushMany : term list -> evaluationStack -> evaluationStack
        val GetItem : int -> evaluationStack -> term
        val FilterActiveFrame : (term -> bool) -> evaluationStack -> evaluationStack
        val Union : evaluationStack -> evaluationStack -> evaluationStack
        val MakeSymbolicActiveFrame : (int -> term -> term) -> evaluationStack -> evaluationStack
        val Length : evaluationStack -> int
        val ToList : evaluationStack -> term list
        val ClearActiveFrame : evaluationStack -> evaluationStack
        val EmptyStack : evaluationStack

    module public Memory =
        val EmptyState : unit -> state
        val EmptyModel : IMethod -> model
        val PopFrame : state -> unit
        val ForcePopFrames : int -> state -> unit
        val PopTypeVariables : state -> unit
        val NewStackFrame : state -> IMethod option -> (stackKey * term option * Type) list -> unit
        val NewTypeVariables : state -> (Type * Type) list -> unit

        val ReferenceArrayIndex : state -> term -> term list -> Type option -> term
        val ReferenceField : state -> term -> fieldId -> term

        val Read : state -> term -> term
        val ReadLocalVariable : state -> stackKey -> term
        val ReadThis : state -> IMethod -> term
        val ReadArgument : state -> ParameterInfo -> term
        val ReadField : state -> term -> fieldId -> term
        val ReadArrayIndex : state -> term -> term list -> Type option -> term
        val ReadStringChar : state -> term -> term -> term
        val ReadStaticField : state -> Type -> fieldId -> term
        val ReadDelegate : state -> term -> term

        val InitializeArray : state -> term -> term -> unit

        val Write : state -> term -> term -> state list
        val WriteLocalVariable : state -> stackKey -> term -> unit
        val WriteStructField : term -> fieldId -> term -> term
        val WriteClassField : state -> term -> fieldId -> term -> state list
        val WriteArrayIndex : state -> term -> term list -> term -> Type option -> state list
        val WriteStaticField : state -> Type -> fieldId -> term -> unit

        val DefaultOf : Type -> term

        val MakeSymbolicThis : IMethod -> term
        val MakeSymbolicValue : ISymbolicConstantSource -> string -> Type -> term

        val CallStackContainsFunction : state -> IMethod -> bool
        val CallStackSize : state -> int
        val GetCurrentExploringFunction : state -> IMethod

        val BoxValueType : state -> term -> term

        val InitializeStaticMembers : state -> Type -> unit

        val AllocateTemporaryLocalVariable : state -> Type -> term -> term
        val AllocateDefaultClass : state -> Type -> term
        val AllocateDefaultArray : state -> term list -> Type -> term
        val AllocateVectorArray : state -> term -> Type -> term
        val AllocateConcreteVectorArray : state -> term -> Type -> 'a seq -> term
        val AllocateString : string -> state -> term
        val AllocateEmptyString : state -> term -> term
        val AllocateDelegate : state -> term -> term
        val CreateStringFromChar : state -> term -> term

        val LinearizeArrayIndex : state -> term -> term list -> arrayType -> term

        val CopyArray : state -> term -> term -> Type -> term -> term -> Type -> term -> unit
        val CopyStringArray : state -> term -> term -> term -> term -> term -> unit

        val ClearArray : state -> term -> term -> term -> unit

        val StringFromReplicatedChar : state -> term -> term -> term -> unit

        val IsTypeInitialized : state -> Type -> term
        val Dump : state -> string
        val StackTrace : callStack -> IMethod list
        val StackTraceString : callStack -> string

        val ArrayRank : state -> term -> term
        val ArrayLengthByDimension : state -> term -> term -> term
        val ArrayLowerBoundByDimension : state -> term -> term -> term

        val CountOfArrayElements : state -> term -> term

        val StringLength : state -> term -> term
        val StringCtorOfCharArray : state -> term -> term -> state list

        // TODO: get rid of all unnecessary stuff below!
        val ComposeStates : state -> state -> state list
        val WLP : state -> pathCondition -> pathCondition

        val Merge2States : state -> state -> state list
        val Merge2Results : term * state -> term * state -> (term * state) list

        val FillRegion : state -> term -> regionSort -> unit

    module Print =
        val Dump : state -> string
        val PrintPC : pathCondition -> string

//    module Marshalling =
//        val Unmarshal : state -> obj -> term * state
//        val CanBeCalledViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> bool
//        val CallViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> (term * state -> 'a) -> 'a
