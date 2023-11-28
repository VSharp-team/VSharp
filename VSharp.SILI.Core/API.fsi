namespace VSharp.Core

open System.Collections.Generic
open VSharp
open System
open System.Reflection

[<AutoOpen>]
module API =
    val ConfigureSolver : SolverInteraction.ISolver -> unit
    val ConfigureSimplifier : IPropositionalSimplifier -> unit
    val CharsArePretty : bool
    val ConfigureChars : bool -> unit

    val Reset : unit -> unit
    val SaveConfiguration : unit -> unit
    val Restore : unit -> unit
    val SetMaxBuferSize : int -> unit

    val BranchStatements : state -> (state -> (term * state -> 'a) -> 'b) -> (state -> (term * state -> 'a) -> 'a) -> (state -> (term * state -> 'a) -> 'a) -> ((term * state) list -> 'a) -> 'b
    val BranchStatementsOnNull : state -> term -> (state -> (term * state -> 'a) -> 'a) -> (state -> (term * state -> 'a) -> 'a) -> ((term * state) list -> 'a) -> 'a
    val BranchExpressions : ((term -> 'a) -> 'b) -> ((term -> 'a) -> 'a) -> ((term -> 'a) -> 'a) -> (term -> 'a) -> 'b
    val StatedConditionalExecution : (state -> (state -> (term * state -> 'a) -> 'b) -> (state -> ('item -> 'a) -> 'a) -> (state -> ('item -> 'a) -> 'a) -> ('item -> 'item -> 'item list) -> ('item  list -> 'a) -> 'b)
    val StatedConditionalExecutionAppend : (state -> (state -> (term * state -> 'a) -> 'b) -> (state -> ('c list -> 'a) -> 'a) -> (state -> ('c list -> 'a) -> 'a) -> ('c list -> 'a) -> 'b)

    val GuardedApplyExpression : term -> (term -> term) -> term
    val GuardedApplyExpressionWithPC : pathCondition -> term -> (term -> term) -> term
    val GuardedStatedApplyStatementK : state -> term -> (state -> term -> (term * state -> 'a) -> 'a) -> ((term * state) list -> 'a) -> 'a
    val GuardedStatedApplyk : (state -> term -> ('item -> 'a) -> 'a) -> state -> term -> ('item list -> 'item list) -> ('item list -> 'a) -> 'a

    val ReleaseBranches : unit -> unit
    val AcquireBranches : unit -> unit

    val PerformBinaryOperation : OperationType -> term -> term -> (term -> 'a) -> 'a
    val PerformUnaryOperation : OperationType -> term -> (term -> 'a) -> 'a

    val SolveGenericMethodParameters : typeStorage -> IMethod -> (symbolicType[] * symbolicType[]) option
    val SolveThisType : state -> term -> unit
    val ResolveCallVirt : state -> term -> Type -> IMethod -> symbolicType seq
    val KeepOnlyMock : state -> term -> unit

    val MethodMockAndCall : state -> IMethod -> term option -> term list -> term option
    val ExternMockAndCall : state -> IMethod -> term option -> term list -> term option

    [<AutoOpen>]
    module Terms =
        val Nop : unit ->term
        val Concrete : 'a -> Type -> term
        val Constant : string -> ISymbolicConstantSource -> Type -> term
        val Expression : operation -> term list -> Type -> term
        val Struct : pdict<fieldId, term> -> Type -> term
        val Ref : address -> term
        val Ptr : pointerBase -> Type -> term -> term
        val HeapRef : heapAddress -> Type -> term
        val Union : (term * term) list -> term

        val True : unit -> term
        val False : unit -> term
        val NullRef : Type -> term
        val MakeNullPtr : Type -> term
        val ConcreteHeapAddress : concreteHeapAddress -> term

        val MakeBool : bool -> term
        val MakeNumber : 'a -> term
        val MakeIntPtr : term -> term
        val MakeUIntPtr : term -> term
        val NativeToPtr : term -> term
        val AddressToBaseAndOffset : address -> pointerBase * term

        val TypeOf : term -> Type
        val TypeOfLocation : term -> Type
        val MostConcreteTypeOfRef : state -> term -> Type
        val TypeOfAddress : state -> term -> Type

        val GetHashCode : term -> term

        val ReinterpretConcretes : term list -> Type -> obj

        val TryPtrToRef : state -> pointerBase -> Type -> term -> option<address>
        val PtrToRefFork : state -> pointerBase -> Type -> term -> list<option<address> * state>

        val TryTermToObj : state -> term -> obj option

        val IsStruct : term -> bool
        val IsReference : term -> bool
        val IsPtr : term -> bool
        val IsRefOrPtr : term -> bool
        val IsConcrete : term -> bool
        val IsNullReference : term -> term
        val IsBadRef : term -> term

        val (|ConcreteHeapAddress|_|) : termNode -> concreteHeapAddress option

        val (|Combined|_|) : termNode -> (term list * Type) option
        val (|CombinedTerm|_|) : term -> (term list * Type) option

        val (|CombinedDelegate|_|) : term -> term list option
        val (|ConcreteDelegate|_|) : term -> delegateInfo option

        val (|True|_|) : term -> unit option
        val (|False|_|) : term -> unit option
        val (|Negation|_|) : term -> term option
        val (|Conjunction|_|) : term -> term list option
        val (|Disjunction|_|) : term -> term list option
        val (|NullRef|_|) : term -> Type option
        val (|NonNullRef|_|) : term -> unit option
        val (|NullPtr|_|) : term -> Type option
        val (|DetachedPtr|_|) : termNode -> term option
        val (|DetachedPtrTerm|_|) : term -> term option

        val (|StackReading|_|) : ISymbolicConstantSource -> option<stackKey>
        val (|HeapReading|_|) : ISymbolicConstantSource -> option<heapAddressKey * memoryRegion<heapAddressKey, vectorTime intervals>>
        val (|ArrayRangeReading|_|) : ISymbolicConstantSource -> option<unit>
        val (|ArrayIndexReading|_|) : ISymbolicConstantSource -> option<bool * heapArrayKey * memoryRegion<heapArrayKey, productRegion<vectorTime intervals, int points listProductRegion>>>
        val (|VectorIndexReading|_|) : ISymbolicConstantSource -> option<bool * heapVectorIndexKey * memoryRegion<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>>
        val (|StackBufferReading|_|) : ISymbolicConstantSource -> option<stackBufferIndexKey * memoryRegion<stackBufferIndexKey, int points>>
        val (|StaticsReading|_|) : ISymbolicConstantSource -> option<symbolicTypeKey * memoryRegion<symbolicTypeKey, freeRegion<typeWrapper>>>
        val (|StructFieldSource|_|) : ISymbolicConstantSource -> option<ISymbolicConstantSource * fieldId>
        val (|StructFieldChain|_|) : ISymbolicConstantSource -> option<fieldId list * ISymbolicConstantSource>
        val (|HeapAddressSource|_|) : ISymbolicConstantSource -> option<ISymbolicConstantSource>
        val (|TypeInitializedSource|_|) : IStatedSymbolicConstantSource -> option<Type * symbolicTypeSet>
        val (|TypeSubtypeTypeSource|_|) : ISymbolicConstantSource -> option<Type * Type>
        val (|RefSubtypeTypeSource|_|) : ISymbolicConstantSource -> option<heapAddress * Type>
        val (|RefEqTypeSource|_|) : ISymbolicConstantSource -> option<heapAddress * Type>
        val (|TypeSubtypeRefSource|_|) : ISymbolicConstantSource -> option<Type * heapAddress>
        val (|RefSubtypeRefSource|_|) : ISymbolicConstantSource -> option<heapAddress * heapAddress>
        val (|GetHashCodeSource|_|) : ISymbolicConstantSource -> option<term>
        val (|PointerAddressSource|_|) : ISymbolicConstantSource -> option<ISymbolicConstantSource>
        val (|PointerOffsetSource|_|) : ISymbolicConstantSource -> option<ISymbolicConstantSource>

        val (|Int8T|_|) : term -> option<unit>
        val (|UInt8T|_|) : term -> option<unit>
        val (|Int16T|_|) : term -> option<unit>
        val (|UInt16T|_|) : term -> option<unit>
        val (|Int32T|_|) : term -> option<unit>
        val (|UInt32T|_|) : term -> option<unit>
        val (|Int64T|_|) : term -> option<unit>
        val (|UInt64T|_|) : term -> option<unit>
        val (|BoolT|_|) : term -> option<unit>
        val (|Float32T|_|) : term -> option<unit>
        val (|Float64T|_|) : term -> option<unit>
        val (|FloatT|_|) : term -> option<unit>

        val GetHeapReadingRegionSort : ISymbolicConstantSource -> regionSort

        val SpecializeWithKey : term -> heapArrayKey -> heapArrayKey -> term

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
        val isIntegral : Type -> bool
        val IsReal : Type -> bool
        val IsNumeric : Type -> bool
        val IsPointer : Type -> bool
        val IsValueType : Type -> bool
        val IsArrayType : Type -> bool
        val (|Bool|_|) : Type -> unit option
        val (|StringType|_|) : Type -> unit option

        val ElementType : Type -> Type
        val ArrayTypeToSymbolicType : arrayType -> Type
        val SymbolicTypeToArrayType : Type -> arrayType

        val TypeIsType : Type -> Type -> term
        val IsNullable : Type -> bool
        val TypeIsRef :  state -> Type -> term -> term
        val RefIsType : state -> term -> Type -> term
        val RefEqType : state -> term -> Type -> term
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
        val GreaterOrEqualUn : term -> term -> term
        val Equality : term -> term -> term
        val Inequality : term -> term -> term
        val Less : term -> term -> term
        val LessOrEqual : term -> term -> term
        val Greater : term -> term -> term
        val GreaterOrEqual : term -> term -> term
        val Mul : term -> term -> term
        val Sub : term -> term -> term
        val Add : term -> term -> term
        val Rem : term -> term -> term
        val RemUn : term -> term -> term
        val Div : term -> term -> term
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

        val StringArrayInfo : state -> term -> term option -> term * arrayType

        val ReferenceArrayIndex : state -> term -> term list -> Type option -> term
        val ReferenceField : state -> term -> fieldId -> term

        val TryAddressFromRef : state -> term -> list<address option * state>
        val TryAddressFromRefFork : state -> term -> list<address option * state>

        val ExtractAddress : term -> term
        val ExtractPointerOffset : term -> term

        val Read : state -> term -> term
        val ReadUnsafe : IErrorReporter -> state -> term -> term
        val ReadLocalVariable : state -> stackKey -> term
        val ReadThis : state -> IMethod -> term
        val ReadArgument : state -> ParameterInfo -> term
        val ReadField : state -> term -> fieldId -> term
        val ReadFieldUnsafe : IErrorReporter -> state -> term -> fieldId -> term
        val ReadArrayIndex : state -> term -> term list -> Type option -> term
        val ReadArrayIndexUnsafe : IErrorReporter -> state -> term -> term list -> Type option -> term
        val ReadStringChar : state -> term -> term -> term
        val ReadStaticField : state -> Type -> fieldId -> term
        val ReadDelegate : state -> term -> term option

        val CombineDelegates : state -> term list -> Type -> term
        val RemoveDelegate : state -> term -> term -> Type -> term

        val InitializeArray : state -> term -> term -> unit

        val Write : state -> term -> term -> state list
        val WriteUnsafe : IErrorReporter -> state -> term -> term -> state list
        val WriteStackLocation : state -> stackKey -> term -> unit
        val WriteStructField : term -> fieldId -> term -> term
        val WriteStructFieldUnsafe : IErrorReporter -> state -> term -> fieldId -> term -> term
        val WriteClassField : state -> term -> fieldId -> term -> state list
        val WriteArrayIndex : state -> term -> term list -> term -> Type option -> state list
        val WriteArrayIndexUnsafe : IErrorReporter -> state -> term -> term list -> term -> Type option -> state list
        val WriteStaticField : state -> Type -> fieldId -> term -> unit

        val DefaultOf : Type -> term

        val MakeSymbolicThis : IMethod -> term
        val MakeSymbolicValue : ISymbolicConstantSource -> string -> Type -> term

        val CallStackContainsFunction : state -> IMethod -> bool
        val CallStackSize : state -> int
        val GetCurrentExploringFunction : state -> IMethod
        val EntryFunction : state -> IMethod

        val BoxValueType : state -> term -> term

        val InitializeStaticMembers : state -> Type -> unit
        val MarkTypeInitialized : state -> Type -> unit

        val InitFunctionFrame : state -> IMethod -> term option -> term option list option -> unit
        val AllocateTemporaryLocalVariable : state -> int -> Type -> term -> term
        val AllocateTemporaryLocalVariableOfType : state -> string -> int -> Type -> term
        val AllocateDefaultClass : state -> Type -> term
        val AllocateMock : state -> ITypeMock -> Type -> term
        val AllocateDefaultArray : state -> term list -> Type -> term
        val AllocateVectorArray : state -> term -> Type -> term
        val AllocateConcreteVectorArray : state -> term -> Type -> 'a seq -> term
        val AllocateArrayFromFieldInfo : state -> FieldInfo -> term
        val AllocateString : string -> state -> term
        val AllocateEmptyString : state -> term -> term
        val AllocateDelegate : state -> MethodInfo -> term -> Type -> term
        val CreateStringFromChar : state -> term -> term

        val AllocateConcreteObject : state -> obj -> Type -> term

        val LinearizeArrayIndex : state -> term -> term list -> arrayType -> term

        val IsSafeContextCopy : arrayType -> arrayType -> bool

        val CopyArray : state -> term -> term -> Type -> term -> term -> Type -> term -> unit
        val CopyStringArray : state -> term -> term -> term -> term -> term -> unit

        val ClearArray : state -> term -> term -> term -> unit
        val FillArray : state -> term -> term -> unit

        val StringFromReplicatedChar : state -> term -> term -> term -> unit

        val IsTypeInitialized : state -> Type -> term
        val Dump : state -> string
        val StackTrace : callStack -> IMethod list
        val StackTraceString : callStack -> string
        val StackToString : callStack -> string

        val ArrayRank : state -> term -> term
        val ArrayLengthByDimension : state -> term -> term -> term
        val ArrayLowerBoundByDimension : state -> term -> term -> term

        val CountOfArrayElements : state -> term -> term

        val StringLength : state -> term -> term
        val StringCtorOfCharArray : state -> term -> term -> state list
        val StringCtorOfCharArrayAndLen : state -> term -> term -> term -> state list

        // TODO: get rid of all unnecessary stuff below!
        val ComposeStates : state -> state -> state list
        val WLP : state -> pathCondition -> pathCondition

        val Merge2States : state -> state -> state list
        val Merge2Results : term * state -> term * state -> (term * state) list

        val FillClassFieldsRegion : state -> fieldId -> term -> (IHeapAddressKey -> bool) -> unit
        val FillStaticsRegion : state -> fieldId -> term -> (ISymbolicTypeKey -> bool) -> unit
        val FillArrayRegion : state -> arrayType -> term -> (IHeapArrayKey -> bool) -> unit
        val FillLengthRegion : state -> arrayType -> term -> (IHeapVectorIndexKey -> bool) -> unit
        val FillLowerBoundRegion : state -> arrayType -> term -> (IHeapVectorIndexKey -> bool) -> unit
        val FillStackBufferRegion : state -> stackKey -> term -> (IStackBufferIndexKey -> bool) -> unit
        val FillBoxedRegion : state -> Type -> term -> (IHeapAddressKey -> bool) -> unit

        val ObjectToTerm : state -> obj -> Type -> term
        val TryTermToObject : state -> term -> obj option

        val StateResult : state -> term

    module Print =
        val Dump : state -> string
        val PrintPC : pathCondition -> string

//    module Marshalling =
//        val Unmarshal : state -> obj -> term * state
//        val CanBeCalledViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> bool
//        val CallViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> (term * state -> 'a) -> 'a
