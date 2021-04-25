namespace VSharp.Core

open VSharp
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

    val PerformBinaryOperation : OperationType -> term -> term -> (term -> 'a) -> 'a
    val PerformUnaryOperation : OperationType -> term -> (term -> 'a) -> 'a

    val IsValid : state -> SolverInteraction.smtResult

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
        val MakeNumber : 'a -> term
        val MakeIntPtr : term -> state -> term

        val TypeOf : term -> symbolicType
        val MostConcreteTypeOfHeapRef : state -> term -> symbolicType

        val IsStruct : term -> bool
        val IsReference : term -> bool
        val IsNullReference : term -> term

        val (|ConcreteHeapAddress|_|) : termNode -> concreteHeapAddress option

        val (|True|_|) : term -> unit option
        val (|False|_|) : term -> unit option
        val (|Conjunction|_|) : term -> term list option
        val (|Disjunction|_|) : term -> term list option

        val (|HeapReading|_|) : IMemoryAccessConstantSource -> option<heapAddressKey * memoryRegion<heapAddressKey, vectorTime intervals>>
        val (|ArrayIndexReading|_|) : IMemoryAccessConstantSource -> option<bool * heapArrayIndexKey * memoryRegion<heapArrayIndexKey, productRegion<vectorTime intervals, int points listProductRegion>>>
        val (|VectorIndexReading|_|) : IMemoryAccessConstantSource -> option<bool * heapVectorIndexKey * memoryRegion<heapVectorIndexKey, productRegion<vectorTime intervals, int points>>>
        val (|StackBufferReading|_|) : IMemoryAccessConstantSource -> option<stackBufferIndexKey * memoryRegion<stackBufferIndexKey, int points>>
        val (|StaticsReading|_|) : IMemoryAccessConstantSource -> option<symbolicTypeKey * memoryRegion<symbolicTypeKey, freeRegion<symbolicType>>>
        val (|StructFieldSource|_|) : IMemoryAccessConstantSource -> option<IMemoryAccessConstantSource * fieldId>
        val (|HeapAddressSource|_|) : IMemoryAccessConstantSource -> option<IMemoryAccessConstantSource>
        val (|TypeInitializedSource|_|) : IStatedSymbolicConstantSource -> option<symbolicType * symbolicTypeSet>

        val GetHeapReadingRegionSort : IMemoryAccessConstantSource -> regionSort

        val ConstantsOf : term seq -> term System.Collections.Generic.ISet

        val HeapReferenceToBoxReference : term -> term

        val WithPathCondition : state -> term -> state
        val IsFalsePathCondition : state -> bool
        val PathConditionToSeq : pathCondition -> term seq

    module Types =
        val Numeric : System.Type -> symbolicType
        val ObjectType : symbolicType
        val IndexType : symbolicType

        val FromDotNetType : System.Type -> symbolicType
        val ToDotNetType : symbolicType -> System.Type

        val SizeOf : symbolicType -> int
        val RankOf : symbolicType -> int

        val TLength : symbolicType
        val IsBool : symbolicType -> bool
        val IsInteger : symbolicType -> bool
        val IsReal : symbolicType -> bool
        val IsNumeric : symbolicType -> bool
        val IsPointer : symbolicType -> bool
        val IsValueType : symbolicType -> bool
        val IsArrayType : symbolicType -> bool
        val String : symbolicType
        val (|StringType|_|) : symbolicType -> unit option

        val ElementType : symbolicType -> symbolicType

        val TypeIsType : symbolicType -> symbolicType -> term
        val TypeIsNullable : symbolicType -> bool
        val TypeIsRef :  state -> symbolicType -> term -> term
        val RefIsType : state -> term -> symbolicType -> term
        val RefIsAssignableToType : state -> term -> symbolicType -> term
        val IsCast : state -> term -> symbolicType -> term
        val CanCastImplicitly : term -> symbolicType -> bool
        val Cast : term -> symbolicType -> term
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

        val PopFromOpStack : operationStack -> term * operationStack
        val PopArgumentsFromOpStack : int -> operationStack -> term list * operationStack
        val PushToOpStack : term -> operationStack -> operationStack
        val GetOpStackItem : int -> operationStack -> term
        val FilterActiveFrame : (term -> bool) -> operationStack -> operationStack
        val UnionOpStacks : operationStack -> operationStack -> operationStack
        val MakeSymbolicOpStackActiveFrame : (int -> term -> term) -> operationStack -> operationStack
        val OpStackLength : operationStack -> int
        val OpStackToList : operationStack -> term list

        val PopFrame : state -> state
        val PopTypeVariables : state -> state
        val NewStackFrame : state -> IFunctionIdentifier -> (stackKey * term symbolicValue * symbolicType) list -> bool -> state
        val NewTypeVariables : state -> (typeId * symbolicType) list -> state

        val ReferenceField : state -> term -> fieldId -> term
        val ReferenceArrayIndex : term -> term list -> term

        val ReadSafe : state -> term -> term
        val ReadLocalVariable : state -> stackKey -> term
        val ReadThis : state -> MethodBase -> term
        val ReadArgument : state -> ParameterInfo -> term
        val ReadField : state -> term -> fieldId -> term
        val ReadArrayIndex : state -> term -> term list -> term
        val ReadStringChar : state -> term -> term -> term
        val ReadStaticField : state -> symbolicType -> fieldId -> term
        val ReadDelegate : state -> term -> term

        val WriteSafe : state -> term -> term -> state list
        val WriteLocalVariable : state -> stackKey -> term -> state
        val WriteStructField : term -> fieldId -> term -> term
        val WriteClassField : state -> term -> fieldId -> term -> state list
        val WriteArrayIndex : state -> term -> term list -> term -> state list
        val WriteStringChar : state -> term -> term list -> term -> state list
        val WriteStaticField : state -> symbolicType -> fieldId -> term -> state

        val DefaultOf : symbolicType -> term
        val AllocateOnStack : state -> stackKey -> term -> state
        val AllocateTemporaryLocalVariable : state -> System.Type -> term -> term * state
        val MakeSymbolicThis : System.Reflection.MethodBase -> term

        val MakeSymbolicValue : IMemoryAccessConstantSource -> string -> symbolicType -> term

        val BoxValueType : state -> term -> term * state
        val InitializeStaticMembers : state -> symbolicType -> state
        val AllocateDefaultClass : state -> symbolicType -> term * state
        val AllocateDefaultArray : state -> term list -> symbolicType -> term * state
        val AllocateVectorArray : state -> term -> symbolicType -> term * state
        val AllocateString : string -> state -> term * state
        val AllocateDelegate : state -> term -> term * state

        val CopyArray : state -> term -> term -> term -> term -> term -> state

        val IsTypeInitialized : state -> symbolicType -> term
        val Dump : state -> string

        val ArrayRank : state -> term -> term
        val ArrayLengthByDimension : state -> term -> term -> term
        val ArrayLowerBoundByDimension : state -> term -> term -> term

        val StringLength : state -> term -> term
        val StringCtorOfCharArray : state -> term -> term -> state list

        // TODO: get rid of all unnecessary stuff below!
        val ComposeStates : state -> state -> state list

        val Merge2States : state -> state -> state list
        val Merge2Results : term * state -> term * state -> (term * state) list


//    module Marshalling =
//        val Unmarshal : state -> obj -> term * state
//        val CanBeCalledViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> bool
//        val CallViaReflection : state -> IFunctionIdentifier -> term option -> term list symbolicValue -> (term * state -> 'a) -> 'a
