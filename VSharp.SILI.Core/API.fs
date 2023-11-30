namespace VSharp.Core

open System
open FSharpx.Collections
open VSharp
open VSharp.Core

module API =

    let ConfigureSolver solver =
        SolverInteraction.configureSolver solver

    let ConfigureSimplifier simplifier =
        configureSimplifier simplifier

    let CharsArePretty = charsArePretty
    let ConfigureChars arePretty =
        configureChars arePretty

    let Reset() =
        IdGenerator.reset()
    let SaveConfiguration() =
        IdGenerator.saveConfiguration()
    let Restore() =
        IdGenerator.restore()

    let SetMaxBuferSize size = SolverInteraction.setMaxBufferSize size

    let BranchStatements state condition thenStatement elseStatement k =
        Branching.statedConditionalExecutionWithMergek state condition thenStatement elseStatement k
    let BranchStatementsOnNull state reference thenStatement elseStatement k =
        BranchStatements state (fun state k -> k (Pointers.isNull reference, state)) thenStatement elseStatement k
    let BranchExpressions condition thenBranch elseExpression k =
        Common.statelessConditionalExecutionWithMergek condition thenBranch elseExpression k
    let StatedConditionalExecutionAppend (state : state) conditionInvocation thenBranch elseBranch k =
        Branching.commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch (fun x y -> [x;y]) (List.concat >> k)
    let StatedConditionalExecution = Branching.commonStatedConditionalExecutionk

    let GuardedApplyExpressionWithPC pc term f = Merging.guardedApplyWithPC pc f term
    let GuardedApplyExpression term f = Merging.guardedApply f term
    let GuardedStatedApplyStatementK state term f k = Branching.guardedStatedApplyk f state term k
    let GuardedStatedApplyk f state term mergeStates k =
        Branching.commonGuardedStatedApplyk f state term mergeStates k

    let ReleaseBranches() = Branching.branchesReleased <- true
    let AcquireBranches() = Branching.branchesReleased <- false

    let PerformBinaryOperation op left right k = simplifyBinaryOperation op left right k
    let PerformUnaryOperation op arg k = simplifyUnaryOperation op arg k

    let SolveGenericMethodParameters (typeStorage : typeStorage) (method : IMethod) =
        TypeSolver.solveMethodParameters typeStorage method

    let SolveThisType state thisRef =
        match thisRef.term with
        | HeapRef(address, t) ->
            let constraints = List.singleton t |> typeConstraints.FromSuperTypes
            // TODO: add 'isPublic' constraint, because 'this' may be rendered only with public type
            state.typeStorage.AddConstraint address constraints
            match TypeSolver.solveTypes state.model state with
            | TypeSat -> ()
            | TypeUnsat -> __insufficientInformation__ "SolveThisType: cannot find non-abstract type for 'this'"
        | Ref _ -> ()
        | _ -> internalfail $"unexpected this {ref}"

    let ResolveCallVirt state thisAddress thisType ancestorMethod = TypeSolver.getCallVirtCandidates state thisAddress thisType ancestorMethod

    let KeepOnlyMock state thisAddress = TypeSolver.keepOnlyMock state thisAddress

    let MethodMockAndCall state method this args = MethodMocking.mockAndCall state method this args Default
    let ExternMockAndCall state method this args = MethodMocking.mockAndCall state method this args Extern

    [<AutoOpen>]
    module public Terms =
        let Nop() = Nop()
        let Concrete obj typ = Concrete obj typ
        let Constant name source typ = Constant name source typ
        let Expression op args typ = Expression op args typ
        let Struct fields typ = Struct fields typ
        let Ref address = Ref address
        let Ptr baseAddress typ offset = Ptr baseAddress typ offset
        let HeapRef address baseType = HeapRef address baseType
        let Union gvs = Union gvs

        let True() = True()
        let False() = False()
        let NullRef t = nullRef t
        let MakeNullPtr t = makeNullPtr t
        let ConcreteHeapAddress (address : concreteHeapAddress) = ConcreteHeapAddress address

        let MakeBool b = makeBool b
        let MakeNumber n = makeNumber n

        // This function is used only for creating IntPtr structure
        let MakeIntPtr (value : term) = makeIntPtr value

        // This function is used only for creating UIntPtr structure
        let MakeUIntPtr (value : term) = makeUIntPtr value

        let NativeToPtr (value : term) = nativeToPointer value

        let AddressToBaseAndOffset address = Pointers.addressToBaseAndOffset address
        // NOTE: returns type of value
        let TypeOf term = typeOf term
        // NOTE: returns type of location, referenced by 'ref'
        let TypeOfLocation ref = typeOfRef ref
        let MostConcreteTypeOfRef state ref = Memory.mostConcreteTypeOfRef state ref
        let TypeOfAddress state address = Memory.typeOfHeapLocation state address

        let IsStruct term = isStruct term
        let IsReference term = isReference term
        let IsPtr term = isPtr term
        let IsRefOrPtr term = isRefOrPtr term
        let IsConcrete term =
            match term.term with
            | Concrete _ -> true
            | HeapRef(address, _) when isConcreteHeapAddress address -> true
            | _ -> false
        let IsNullReference term = Pointers.isNull term
        let IsBadRef term = Pointers.isBadRef term

        let GetHashCode term = Memory.getHashCode term

        let ReinterpretConcretes terms t = reinterpretConcretes terms t

        let TryPtrToRef state pointerBase sightType offset =
            Memory.tryPtrToRef state pointerBase sightType offset

        let PtrToRefFork state pointerBase sightType offset =
            let zero = MakeNumber 0
            let ptrToRef sightType offset state k =
                let ref = Memory.tryPtrToRef state pointerBase sightType offset
                k (ref, state)
            let fork state condition sightType offset =
                StatedConditionalExecution state
                    (fun state k -> k (condition, state))
                    (ptrToRef sightType offset)
                    (fun state k -> k (None, state))
                    (fun x y -> [x;y])
                    id
            match pointerBase with
            | HeapLocation(address, t) when address <> zeroAddress() ->
                let typ = Memory.typeOfHeapLocation state address |> TypeUtils.mostConcreteType t
                let isString = typ = typeof<string>
                if TypeUtils.isArrayType typ || isString then
                    let sightType =
                        if sightType = typeof<Void> then
                            if isString then typeof<char> else typ.GetElementType()
                        else sightType
                    let size = TypeUtils.internalSizeOf sightType |> MakeNumber
                    let condition = rem offset size === zero
                    let offset = mul (div offset size) size
                    fork state condition sightType offset
                elif TypeUtils.isValueType typ then
                    let sightType = if sightType = typeof<Void> then typ else sightType
                    let condition = offset === zero
                    fork state condition sightType zero
                else List.singleton (None, state)
            | HeapLocation _ -> List.singleton (None, state)
            | StackLocation key ->
                let sightType = if sightType = typeof<Void> then key.TypeOfLocation else sightType
                let condition = offset === zero
                fork state condition sightType zero
            | StaticLocation t ->
                let sightType = if sightType = typeof<Void> then t else sightType
                let condition = offset === zero
                fork state condition sightType zero

        let TryTermToObj state term = Memory.tryTermToObj state term

        let (|ConcreteHeapAddress|_|) t = (|ConcreteHeapAddress|_|) t

        let (|Combined|_|) t = (|Combined|_|) t
        let (|CombinedTerm|_|) t = (|CombinedTerm|_|) t

        let (|CombinedDelegate|_|) t = (|CombinedDelegate|_|) t.term
        let (|ConcreteDelegate|_|) t = (|ConcreteDelegate|_|) t.term

        let (|True|_|) t = (|True|_|) t
        let (|False|_|) t = (|False|_|) t
        let (|Negation|_|) t = (|NegationT|_|) t
        let (|Conjunction|_|) term = (|Conjunction|_|) term.term
        let (|Disjunction|_|) term = (|Disjunction|_|) term.term
        let (|NullRef|_|) = function
            | {term = HeapRef(addr, t)} when addr = zeroAddress() -> Some(t)
            | _ -> None
        let (|NonNullRef|_|) = function
            | {term = HeapRef(addr, _)} when addr = zeroAddress() -> None
            | _ -> Some()
        let (|NullPtr|_|) = function
            | {term = Ptr(HeapLocation(addr, _), sightType, offset)} when addr = zeroAddress() && offset = makeNumber 0 ->
                Some(sightType)
            | _ -> None

        let (|DetachedPtr|_|) term = (|DetachedPtr|_|) term
        let (|DetachedPtrTerm|_|) term = (|DetachedPtr|_|) term.term

        let (|StackReading|_|) src = Memory.(|StackReading|_|) src
        let (|HeapReading|_|) src = Memory.(|HeapReading|_|) src

        let (|ArrayRangeReading|_|) (src : ISymbolicConstantSource) =
            match src with
            | Memory.ArrayRangeReading _ -> Some()
            | _ -> None

        let (|ArrayIndexReading|_|) src = Memory.(|ArrayIndexReading|_|) src
        let (|VectorIndexReading|_|) src = Memory.(|VectorIndexReading|_|) src
        let (|StackBufferReading|_|) src = Memory.(|StackBufferReading|_|) src
        let (|StaticsReading|_|) src = Memory.(|StaticsReading|_|) src
        let (|StructFieldSource|_|) src = Memory.(|StructFieldSource|_|) src
        let rec (|StructFieldChain|_|) (src : ISymbolicConstantSource) =
            let rec structFieldChainRec acc = function
                | Memory.StructFieldSource(baseSource, field) ->
                    structFieldChainRec (field::acc) baseSource
                | src -> Some(acc, src)
            structFieldChainRec List.empty src
        let (|HeapAddressSource|_|) src = Memory.(|HeapAddressSource|_|) src
        let (|TypeInitializedSource|_|) src = Memory.(|TypeInitializedSource|_|) src
        let (|TypeSubtypeTypeSource|_|) src = TypeCasting.(|TypeSubtypeTypeSource|_|) src
        let (|RefSubtypeTypeSource|_|) src = TypeCasting.(|RefSubtypeTypeSource|_|) src
        let (|RefEqTypeSource|_|) src = TypeCasting.(|RefEqTypeSource|_|) src
        let (|TypeSubtypeRefSource|_|) src = TypeCasting.(|TypeSubtypeRefSource|_|) src
        let (|RefSubtypeRefSource|_|) src = TypeCasting.(|RefSubtypeRefSource|_|) src
        let (|GetHashCodeSource|_|) s = Memory.(|GetHashCodeSource|_|) s
        let (|PointerAddressSource|_|) s = Memory.(|PointerAddressSource|_|) s
        let (|PointerOffsetSource|_|) s = Memory.(|PointerOffsetSource|_|) s

        let (|Int8T|_|) t = if typeOf t = typeof<int8> then Some() else None
        let (|UInt8T|_|) t = if typeOf t = typeof<uint8> then Some() else None
        let (|Int16T|_|) t = if typeOf t = typeof<int16> then Some() else None
        let (|UInt16T|_|) t = if typeOf t = typeof<uint16> then Some() else None
        let (|Int32T|_|) t = if typeOf t = typeof<int32> then Some() else None
        let (|UInt32T|_|) t = if typeOf t = typeof<uint32> then Some() else None
        let (|Int64T|_|) t = if typeOf t = typeof<int64> then Some() else None
        let (|UInt64T|_|) t = if typeOf t = typeof<uint64> then Some() else None
        let (|BoolT|_|) t = if isBool t then Some() else None
        let (|Float32T|_|) t = if typeOf t = typeof<single> then Some() else None
        let (|Float64T|_|) t = if typeOf t = typeof<double> then Some() else None
        let (|FloatT|_|) t = if typeOf t |> TypeUtils.isReal then Some() else None

        let GetHeapReadingRegionSort src = Memory.getHeapReadingRegionSort src

        let SpecializeWithKey constant key writeKey = Memory.specializeWithKey constant key writeKey

        let HeapReferenceToBoxReference reference = Memory.heapReferenceToBoxReference reference

        let AddConstraint conditionState condition =
            Memory.addConstraint conditionState condition
            let constraints = conditionState.typeStorage.Constraints
            TypeStorage.addTypeConstraint constraints condition

        let IsFalsePathCondition conditionState = PC.isFalse conditionState.pc
        let Contradicts state condition = PC.add state.pc condition |> PC.isFalse
        let PathConditionToSeq (pc : pathCondition) = PC.toSeq pc
        let EmptyPathCondition = PC.empty

    module Types =
        let SizeOf t = TypeUtils.internalSizeOf t
        let RankOf t = TypeUtils.rankOf t

        let IndexType = TypeUtils.indexType
        let TLength = TypeUtils.lengthType
        let IsBool t = t = typeof<bool>
        let isIntegral t = TypeUtils.isIntegral t
        let IsReal t = TypeUtils.isReal t
        let IsNumeric t = TypeUtils.isNumeric t
        let IsPointer t = TypeUtils.isPointer t
        let IsValueType t = TypeUtils.isValueType t
        let IsNullable t = TypeUtils.isNullable t
        let IsArrayType t = TypeUtils.isArrayType t
        let (|Bool|_|) t = if t = typeof<bool> then Some() else None
        let (|StringType|_|) t = TypeUtils.(|StringType|_|) t

        let ElementType arrayType = TypeUtils.elementType arrayType
        let ArrayTypeToSymbolicType arrayType = arrayTypeToSymbolicType arrayType
        let SymbolicTypeToArrayType typ = symbolicTypeToArrayType typ

        let TypeIsType leftType rightType = TypeCasting.typeIsType leftType rightType
        let TypeIsRef state typ ref = TypeCasting.typeIsRef state typ ref
        let RefIsType state ref typ = TypeCasting.refIsType state ref typ
        let RefEqType state ref typ = TypeCasting.refEqType state ref typ
        let RefIsRef state ref1 ref2 = TypeCasting.refIsRef state ref1 ref2

        let IsCast state term targetType = TypeCasting.canCast state term targetType
        let Cast term targetType = TypeCasting.cast term targetType

    module public Operators =
        let (!!) x = simplifyNegation x id
        let (&&&) x y = simplifyAnd x y id
        let (|||) x y = simplifyOr x y id
        let (===) x y = ksimplifyEquality x y id
        let (!==) x y = ksimplifyEquality x y (!!)
        let conjunction xs = conjunction xs
        let disjunction xs = disjunction xs

    module public Arithmetics =
        let (===) x y = simplifyEqual x y id
        let Equality x y = simplifyEqual x y id
        let (!==) x y = simplifyNotEqual x y id
        let Inequality x y = simplifyNotEqual x y id
        let (<<) x y = simplifyLess x y id
        let Less x y = simplifyLess x y id
        let (<<=) x y = simplifyLessOrEqual x y id
        let LessOrEqual x y = simplifyLessOrEqual x y id
        let (>>) x y = simplifyGreater x y id
        let Greater x y = simplifyGreater x y id
        let (>>=) x y = simplifyGreaterOrEqual x y id
        let GreaterOrEqual x y = simplifyGreaterOrEqual x y id
        let GreaterOrEqualUn x y = simplifyGreaterOrEqualUn x y id
        let (%%%) x y = simplifyRemainder true (TypeOf x) x y id

        let Mul x y = mul x y
        let Sub x y = sub x y
        let Add x y = add x y
        let Rem x y = rem x y
        let RemUn x y = remUn x y
        let Div x y = div x y
        let IsZero term = checkEqualZero term id

        let Acos x = acos x
        let Asin x = asin x
        let Atan x = atan x
        let Atan2 y x = atan2 y x
        let Ceiling x = ceiling x
        let Cos x = cos x
        let Cosh x = cosh x
        let Floor x = floor x
        let Sin x = sin x
        let Tan x = tan x
        let Sinh x = sinh x
        let Tanh x = tanh x
        let Round x = round x
        let Sqrt x = sqrt x
        let Log x = log x
        let Log10 x = log10 x
        let Exp x = exp x
        let Pow b p = pow b p
        let Abs x = abs x
        let AbsS x = absS x

    module public EvaluationStack =
        let Pop evaluationStack = EvaluationStack.pop evaluationStack
        let PopMany n evaluationStack = EvaluationStack.popMany n evaluationStack
        let Push x evaluationStack =
            let x' = TypeCasting.castToEvaluationStackType x
            EvaluationStack.push x' evaluationStack
        let PushMany xs evaluationStack =
            EvaluationStack.pushMany xs evaluationStack
        let GetItem index evaluationStack = EvaluationStack.item index evaluationStack
        let FilterActiveFrame f evaluationStack = EvaluationStack.filterActiveFrame f evaluationStack
        let Union oldStack newStack = EvaluationStack.union oldStack newStack
        let MakeSymbolicActiveFrame f evaluationStack = EvaluationStack.makeSymbolicActiveFrame f evaluationStack
        let Length evaluationStack = EvaluationStack.length evaluationStack
        let ToList evaluationStack = EvaluationStack.toList evaluationStack
        let ClearActiveFrame evaluationStack = EvaluationStack.clearActiveFrame evaluationStack
        let EmptyStack = EvaluationStack.empty

    module public Memory =

        let EmptyState() = Memory.makeEmpty false
        let EmptyModel method =
            let modelState = Memory.makeEmpty true
            Memory.fillModelWithParametersAndThis modelState method
            StateModel modelState

        let PopFrame state = Memory.popFrame state
        let ForcePopFrames count state = Memory.forcePopFrames count state
        let PopTypeVariables state = Memory.popTypeVariablesSubstitution state
        let NewStackFrame state method parametersAndThis = Memory.newStackFrame state method parametersAndThis
        let NewTypeVariables state subst = Memory.pushTypeVariablesSubstitution state subst

        let StringArrayInfo state stringAddress length = Memory.stringArrayInfo state stringAddress length

        let IsSafeContextWrite actualType neededType =
            Memory.isSafeContextWrite actualType neededType

        let rec ReferenceArrayIndex state arrayRef indices (valueType : Type option) =
            let indices = List.map (fun i -> primitiveCast i typeof<int>) indices
            match arrayRef.term with
            | HeapRef(addr, typ) ->
                let t = Memory.mostConcreteTypeOfHeapRef state addr typ
                let addr, (elemType, dim, _ as arrayType) =
                    if TypeUtils.isArrayType t then
                        addr, symbolicTypeToArrayType t
                    else StringArrayInfo state addr None
                assert(dim = List.length indices)
                match valueType with
                | Some valueType when not (Memory.isSafeContextWrite elemType valueType) ->
                    Ptr (HeapLocation(addr, typ)) valueType (Memory.arrayIndicesToOffset state addr arrayType indices)
                | _ -> ArrayIndex(addr, indices, arrayType) |> Ref
            | Ref(ArrayIndex(address, innerIndices, (elemType, _, _ as arrayType)) as ref) ->
                assert(List.length indices = List.length innerIndices)
                match valueType with
                | None ->
                    let indices = List.map2 add indices innerIndices
                    ArrayIndex(address, indices, arrayType) |> Ref
                | Some typ when Memory.isSafeContextWrite elemType typ ->
                    let indices = List.map2 add indices innerIndices
                    ArrayIndex(address, indices, arrayType) |> Ref
                | Some typ ->
                    assert(List.length indices = 1)
                    let index = indices[0]
                    let pointerBase, refOffset = Pointers.addressToBaseAndOffset ref
                    let indexOffset = mul index (TypeUtils.internalSizeOf typ |> makeNumber)
                    let offset = add refOffset indexOffset
                    Ptr pointerBase typ offset
            | Ptr(HeapLocation(address, t) as baseAddress, _, offset) ->
                assert(TypeUtils.isArrayType t)
                let elemType, _, _ as arrayType = symbolicTypeToArrayType t
                let sightType, indexOffset =
                    match valueType with
                    | None ->
                        elemType, Memory.arrayIndicesToOffset state address arrayType indices
                    | Some t when Memory.isSafeContextWrite elemType t ->
                        t, Memory.arrayIndicesToOffset state address arrayType indices
                    | Some t ->
                        assert(List.length indices = 1)
                        let index = indices[0]
                        t, mul index (TypeUtils.internalSizeOf t |> makeNumber)
                Ptr baseAddress sightType (add offset indexOffset)
            | Union gvs ->
                let referenceArrayIndex term = ReferenceArrayIndex state term indices valueType
                Merging.guardedMap referenceArrayIndex gvs
            | _ -> internalfailf "Referencing array index: expected reference, but got %O" arrayRef

        let ReferenceField state reference fieldId =
            Memory.referenceField state reference fieldId

        let private CommonTryAddressFromRef state ref shouldFork =
            let zero = MakeNumber 0
            let singleton address = List.singleton (address, state)
            match ref.term with
            // Case for char span made from string
            | Ref(ClassField(address, field)) when field = Reflection.stringFirstCharField ->
                let address, arrayType = StringArrayInfo state address None
                ArrayIndex(address, [zero], arrayType) |> Some |> singleton
            | Ref(address) -> Some address |> singleton
            | HeapRef(address, typ) ->
                let t = MostConcreteTypeOfRef state ref
                if TypeUtils.isArrayType t then
                    let _, dim, _ as arrayType = Types.SymbolicTypeToArrayType t
                    let indices = List.init dim (fun _ -> zero)
                    ArrayIndex(address, indices, arrayType) |> Some |> singleton
                elif t = typeof<string> then
                    let address, arrayType = StringArrayInfo state address None
                    ArrayIndex(address, [zero], arrayType) |> Some |> singleton
                elif TypeUtils.isValueType t then
                    BoxedLocation(address, typ) |> Some |> singleton
                else internalfail $"TryAddressFromRef: unexpected pointer {ref}"
            | DetachedPtr _ -> None |> singleton
            | Ptr(pointerBase, sightType, offset) ->
                if shouldFork then
                    PtrToRefFork state pointerBase sightType offset
                else TryPtrToRef state pointerBase sightType offset |> singleton
            | _ -> internalfail $"TryAddressFromRef: unexpected reference to contents {ref}"

        let TryAddressFromRef state ref =
            CommonTryAddressFromRef state ref false

        let TryAddressFromRefFork state ref =
            CommonTryAddressFromRef state ref true

        let private transformBoxedRef ref =
            match ref.term with
            | HeapRef _ -> HeapReferenceToBoxReference ref
            | _ -> ref

        let ExtractAddress ref = Memory.extractAddress ref
        let ExtractPointerOffset ptr = Memory.extractPointerOffset ptr

        let Read state reference =
            transformBoxedRef reference |> Memory.read Memory.emptyReporter state
        let ReadUnsafe (reporter : IErrorReporter) state reference =
            reporter.ConfigureState state
            transformBoxedRef reference |> Memory.read reporter state
        let ReadLocalVariable state location = Memory.readStackLocation state location
        let ReadThis state method = Memory.readStackLocation state (ThisKey method)
        let ReadArgument state parameterInfo = Memory.readStackLocation state (ParameterKey parameterInfo)

        let CommonReadField reporter state term field =
            let doRead target =
                match target.term with
                | HeapRef _
                | Ptr _
                | Ref _ -> ReferenceField state target field |> Memory.read reporter state
                | Struct _ -> Memory.readStruct reporter state target field
                | Combined _ -> Memory.readFieldUnsafe reporter state target field
                | _ -> internalfailf "Reading field of %O" term
            Merging.guardedApply doRead term

        let ReadField state term field =
            CommonReadField Memory.emptyReporter state term field

        let ReadFieldUnsafe (reporter : IErrorReporter) state term field =
            reporter.ConfigureState state
            CommonReadField reporter state term field

        let CommonReadArrayIndex reporter state reference indices valueType =
            let ref = ReferenceArrayIndex state reference indices valueType
            let value = ReadUnsafe reporter state ref
            match valueType with
            | Some valueType when isReference ref -> Types.Cast value valueType
            | _ -> value

        let ReadArrayIndex state reference indices valueType =
            CommonReadArrayIndex Memory.emptyReporter state reference indices valueType

        let ReadArrayIndexUnsafe (reporter : IErrorReporter) state reference indices valueType =
            reporter.ConfigureState state
            CommonReadArrayIndex reporter state reference indices valueType

        let rec ReadStringChar state reference index =
            match reference.term with
            | HeapRef(addr, typ) when Memory.mostConcreteTypeOfHeapRef state addr typ = typeof<string> ->
                let addr, arrayType = Memory.stringArrayInfo state addr None
                Memory.readArrayIndex state addr [index] arrayType
            | Union gvs ->
                let readStringChar term = ReadStringChar state term index
                Merging.guardedMap readStringChar gvs
            | _ -> internalfailf "Reading string char: expected reference, but got %O" reference
        let ReadStaticField state typ field = Memory.readStaticField state typ field
        let ReadDelegate state reference = Memory.readDelegate state reference

        let CombineDelegates state delegates t =
            Memory.combineDelegates state delegates t
        let RemoveDelegate state source toRemove t =
            Memory.removeDelegate state source toRemove t

        let InitializeArray state arrayRef handleTerm = ArrayInitialization.initializeArray state arrayRef handleTerm

        let WriteStackLocation state location value = Memory.writeStackLocation state location value

        let Write state reference value =
            let write state reference = Memory.write Memory.emptyReporter state reference value
            Branching.guardedStatedMap write state reference

        let WriteUnsafe (reporter : IErrorReporter) state reference value =
            reporter.ConfigureState state
            let write state reference = Memory.write reporter state reference value
            Branching.guardedStatedMap write state reference

        let WriteStructField structure field value = Memory.writeStruct structure field value

        let WriteStructFieldUnsafe (reporter : IErrorReporter) state structure field value =
            reporter.ConfigureState state
            Memory.writeStruct structure field value

        let WriteClassField state reference field value =
            Branching.guardedStatedMap
                (fun state reference ->
                    match reference.term with
                    | HeapRef(addr, _) -> Memory.writeClassField state addr field value
                    | _ -> internalfailf "Writing field of class: expected reference, but got %O" reference
                    state)
                state reference

        let CommonWriteArrayIndex reporter state reference indices value valueType =
            let ref = ReferenceArrayIndex state reference indices valueType
            let value =
                if isPtr ref then Option.fold (fun _ -> Types.Cast value) value valueType
                else MostConcreteTypeOfRef state reference |> symbolicTypeToArrayType |> fst3 |> Types.Cast value
            WriteUnsafe reporter state ref value

        let WriteArrayIndex state reference indices value valueType =
            CommonWriteArrayIndex Memory.emptyReporter state reference indices value valueType

        let WriteArrayIndexUnsafe (reporter : IErrorReporter) state reference indices value valueType =
            reporter.ConfigureState state
            CommonWriteArrayIndex reporter state reference indices value valueType

        let WriteStaticField state typ field value = Memory.writeStaticField state typ field value

        let DefaultOf typ = makeDefaultValue typ

        let MakeSymbolicThis m = Memory.makeSymbolicThis m
        let MakeSymbolicValue source name typ = Memory.makeSymbolicValue source name typ

        let CallStackContainsFunction state method = CallStack.containsFunc state.stack method
        let CallStackSize state = CallStack.size state.stack
        let GetCurrentExploringFunction state = CallStack.getCurrentFunc state.stack
        let EntryFunction state = CallStack.entryFunction state.stack

        let BoxValueType state term = Memory.allocateBoxedLocation state term

        let InitializeStaticMembers state targetType =
            Memory.initializeStaticMembers state targetType

        let MarkTypeInitialized state targetType =
            Memory.markTypeInitialized state targetType

        let InitFunctionFrame state (method : IMethod) this paramValues =
            let parameters = method.Parameters
            let values, areParametersSpecified =
                match paramValues with
                | Some values -> values, true
                | None -> [], false
            let localVarsDecl (lvi : System.Reflection.LocalVariableInfo) =
                let stackKey = LocalVariableKey(lvi, method)
                (stackKey, lvi.LocalType |> DefaultOf |> Some, lvi.LocalType)
            let locals =
                match method.LocalVariables with
                | null -> []
                | lvs -> lvs |> Seq.map localVarsDecl |> Seq.toList
            let valueOrFreshConst (param : System.Reflection.ParameterInfo option) value =
                match param, value with
                | None, _ -> internalfail "parameters list is longer than expected!"
                | Some param, None ->
                    let stackKey = ParameterKey param
                    match areParametersSpecified with
                    | true when param.HasDefaultValue ->
                        let typ = param.ParameterType
                        (stackKey, Some(Concrete param.DefaultValue typ), typ)
                    | true -> internalfail "parameters list is shorter than expected!"
                    | _ -> (stackKey, None, param.ParameterType)
                | Some param, Some value -> (ParameterKey param, value, param.ParameterType)
            let parameters = List.map2Different valueOrFreshConst parameters values
            let parametersAndThis =
                match this with
                | Some thisValue ->
                    let thisKey = ThisKey method
                    // TODO: incorrect type when ``this'' is Ref to stack
                    (thisKey, Some thisValue, TypeOfLocation thisValue) :: parameters
                | None -> parameters
            NewStackFrame state (Some method) (parametersAndThis @ locals)

        let AllocateTemporaryLocalVariable state index typ term =
            let tmpKey = TemporaryLocalVariableKey(typ, index)
            let ref = PrimitiveStackLocation tmpKey |> Ref
            Memory.allocateOnStack state tmpKey term
            ref

        let AllocateTemporaryLocalVariableOfType state name index typ =
            Memory.allocateTemporaryLocalVariableOfType state name index typ

        let AllocateDefaultClass state typ =
            if typ = typeof<string> then
                // Allocating not empty string, because it should not be interned
                // Constructor will mutate whole string
                Memory.allocateString state (String('\000', 1))
            else Memory.allocateClass state typ

        let AllocateMock state mock targetType =
            let concreteAddress = Memory.allocateMockType state mock
            HeapRef (ConcreteHeapAddress concreteAddress) targetType

        let AllocateDefaultArray state lengths typ =
            let zeroLowerBounds = List.map (fun _ -> makeNumber 0) lengths
            let address = Memory.allocateArray state typ zeroLowerBounds lengths
            HeapRef address typ

        let AllocateVectorArray state length elementType =
            let address = Memory.allocateVector state elementType length
            HeapRef address (elementType.MakeArrayType())

        let AllocateConcreteVectorArray state length elementType contents =
            let address = Memory.allocateConcreteVector state elementType length contents
            HeapRef address (elementType.MakeArrayType())

        let AllocateArrayFromFieldInfo state fieldInfo =
            ArrayInitialization.allocateOptimizedArray state fieldInfo

        let AllocateDelegate state methodInfo target delegateType =
            Memory.allocateDelegate state methodInfo target delegateType

        let AllocateString string state = Memory.allocateString state string
        let AllocateEmptyString state length = Memory.allocateEmptyString state length

        let CreateStringFromChar state char = Memory.createStringFromChar state char

        let AllocateConcreteObject state (obj : obj) typ = Memory.allocateConcreteObject state obj typ

        let LinearizeArrayIndex state address indices (_, dim, _ as arrayType) =
            let lens = List.init dim (fun dim -> Memory.readLength state address (makeNumber dim) arrayType)
            let lbs = List.init dim (fun dim -> Memory.readLowerBound state address (makeNumber dim) arrayType)
            Memory.linearizeArrayIndex lens lbs indices

        let IsSafeContextCopy (srcArrayType : arrayType) (dstArrayType : arrayType) =
            Copying.isSafeContextCopy srcArrayType dstArrayType

        let CopyArray state src srcIndex srcType dst dstIndex dstType length =
            match src.term, dst.term with
            | HeapRef(srcAddress, _), HeapRef(dstAddress, _) ->
                let srcType = symbolicTypeToArrayType srcType
                let dstType = symbolicTypeToArrayType dstType
                Copying.copyArray state srcAddress srcIndex srcType dstAddress dstIndex dstType length
            | _ -> internalfailf "Coping arrays: expected heapRefs, but got %O, %O" src dst

        let CopyStringArray state src srcIndex dst dstIndex length =
            match src.term, dst.term with
            | HeapRef(srcAddress, srcSightType), HeapRef(dstAddress, dstSightType) ->
                assert(Memory.mostConcreteTypeOfHeapRef state srcAddress srcSightType = typeof<string>)
                assert(Memory.mostConcreteTypeOfHeapRef state dstAddress dstSightType = typeof<string>)
                let srcAddress, srcType = Memory.stringArrayInfo state srcAddress None
                let dstAddress, dstType = Memory.stringArrayInfo state dstAddress None
                assert(srcType = (typeof<char>, 1, true) && dstType = (typeof<char>, 1, true))
                Copying.copyArray state srcAddress srcIndex srcType dstAddress dstIndex dstType length
            | _ -> internalfailf "Coping arrays: expected heapRefs, but got %O, %O" src dst

        let ClearArray state array index length =
            match array.term with
            | HeapRef(address, sightType) ->
                let arrayType = Memory.mostConcreteTypeOfHeapRef state address sightType |> symbolicTypeToArrayType
                let elemType = fst3 arrayType
                let value = makeDefaultValue elemType
                Copying.fillArray state address arrayType index length value
            | _ -> internalfailf "Clearing array: expected heapRef, but got %O" array

        let FillArray state array value =
            match array.term with
            | HeapRef(address, sightType) ->
                let arrayType = Memory.mostConcreteTypeOfHeapRef state address sightType |> symbolicTypeToArrayType
                // Asserting that array is vector (T[])
                assert(thd3 arrayType)
                let zero = makeNumber 0
                let length = Memory.readLength state address zero arrayType
                Copying.fillArray state address arrayType zero length value
            | _ -> internalfailf "Filling array: expected heapRef, but got %O" array

        let StringFromReplicatedChar state string char length =
            let cm = state.concreteMemory
            let concreteChar = Memory.tryTermToObj state char
            let concreteLen = Memory.tryTermToObj state length
            let symbolicCase address =
                let address, arrayType = Memory.stringArrayInfo state address (Some length)
                Copying.fillArray state address arrayType (makeNumber 0) length char
                Memory.writeClassField state address Reflection.stringLengthField length
            match string.term, concreteChar, concreteLen with
            | HeapRef({term = ConcreteHeapAddress a} as address, sightType), Some (:? char as c), Some (:? int as len)
                when cm.Contains a ->
                    assert(Memory.mostConcreteTypeOfHeapRef state address sightType = typeof<string>)
                    let string = String(c, len)
                    cm.Remove a
                    cm.Allocate a string
            | HeapRef({term = ConcreteHeapAddress a} as address, sightType), _, None
            | HeapRef({term = ConcreteHeapAddress a} as address, sightType), None, _
                when cm.Contains a ->
                    assert(Memory.mostConcreteTypeOfHeapRef state address sightType = typeof<string>)
                    Memory.unmarshall state a
                    symbolicCase address
            | HeapRef(address, sightType), _, _ ->
                assert(Memory.mostConcreteTypeOfHeapRef state address sightType = typeof<string>)
                symbolicCase address
            | _ -> internalfailf "Creating string from replicated char: expected heapRef, but got %O" string

        let IsTypeInitialized state typ = Memory.isTypeInitialized state typ
        let Dump state = Memory.dump state
        let StackTrace stack = CallStack.stackTrace stack
        let StackTraceString stack = CallStack.stackTraceString stack
        let StackToString stack = CallStack.toString stack

        let rec ArrayRank state arrayRef =
            match arrayRef.term with
            | HeapRef(addr, typ) -> Memory.mostConcreteTypeOfHeapRef state addr typ |> TypeUtils.rankOf |> makeNumber
            | Union gvs -> Merging.guardedMap (ArrayRank state) gvs
            | _ -> internalfailf "Getting rank of array: expected ref, but got %O" arrayRef
        let rec ArrayLengthByDimension state arrayRef index =
            match arrayRef.term with
            | HeapRef(addr, typ) -> Memory.mostConcreteTypeOfHeapRef state addr typ |> symbolicTypeToArrayType |> Memory.readLength state addr index
            | Union gvs ->
                let arrayLengthByDimension term = ArrayLengthByDimension state term index
                Merging.guardedMap arrayLengthByDimension gvs
            | _ -> internalfailf "reading array length: expected heap reference, but got %O" arrayRef
        let rec ArrayLowerBoundByDimension state arrayRef index =
            match arrayRef.term with
            | HeapRef(addr, typ) -> Memory.mostConcreteTypeOfHeapRef state addr typ |> symbolicTypeToArrayType |> Memory.readLowerBound state addr index
            | Union gvs ->
                let arrayLowerBoundByDimension term = ArrayLowerBoundByDimension state term index
                Merging.guardedMap arrayLowerBoundByDimension gvs
            | _ -> internalfailf "reading array lower bound: expected heap reference, but got %O" arrayRef

        let rec CountOfArrayElements state arrayRef =
            match arrayRef.term with
            | HeapRef(address, typ) ->
                let _, dim, _ as arrayType = Memory.mostConcreteTypeOfHeapRef state address typ |> symbolicTypeToArrayType
                let lens = List.init dim (fun dim -> Memory.readLength state address (makeNumber dim) arrayType)
                List.fold mul (makeNumber 1) lens
            | Union gvs -> Merging.guardedMap (CountOfArrayElements state) gvs
            | _ -> internalfailf "counting array elements: expected heap reference, but got %O" arrayRef

        let StringLength state strRef = Memory.lengthOfString state strRef

        let private CommonStringCtorOfCharArray state arrayRef stringRef length =
            match stringRef.term with
            | HeapRef({term = ConcreteHeapAddress dstAddr} as address, typ) ->
                assert(Memory.mostConcreteTypeOfHeapRef state address typ = typeof<string>)
                let copy arrayRef state k =
                    match arrayRef.term with
                    | HeapRef(arrayAddr, typ) ->
                        assert(let t = Memory.mostConcreteTypeOfHeapRef state arrayAddr typ in t = typeof<char[]> || t = typeof<string>)
                        Copying.copyCharArrayToString state arrayAddr dstAddr (makeNumber 0) length
                        k (Nop(), state)
                    | Ref(ArrayIndex(arrayAddr, indices, (elemType, dim, isVector))) ->
                        assert(isVector && dim = 1 && elemType = typeof<char> && List.length indices = 1)
                        let index = indices[0]
                        Copying.copyCharArrayToString state arrayAddr dstAddr index length
                        k (Nop(), state)
                    | _ -> internalfail $"StringCtorOfCharArray: unexpected array reference {arrayRef}"
                let nullCase state k =
                    let arrayRef = TypeOf arrayRef |> NullRef
                    copy arrayRef state k
                let results = BranchStatementsOnNull state arrayRef nullCase (copy arrayRef) id
                List.map snd results
            | _ -> internalfail $"StringCtorOfCharArray: unexpected string reference {stringRef}"

        let StringCtorOfCharArray state arrayRef stringRef =
            CommonStringCtorOfCharArray state arrayRef stringRef None

        let StringCtorOfCharArrayAndLen state arrayRef stringRef length =
            CommonStringCtorOfCharArray state arrayRef stringRef (Some length)

        let ComposeStates state state' = Memory.composeStates state state'
        let WLP state pc' = PC.mapPC (Memory.fillHoles state) pc' |> PC.union state.pc

        let Merge2States (s1 : state) (s2 : state) = Memory.merge2States s1 s2
        let Merge2Results (r1, s1 : state) (r2, s2 : state) = Memory.merge2Results (r1, s1) (r2, s2)

        let FillClassFieldsRegion state (field : fieldId) value isSuitableKey =
            let defaultValue = MemoryRegion.empty field.typ
            let fill region = MemoryRegion.fillRegion value isSuitableKey region
            state.classFields <- PersistentDict.update state.classFields field defaultValue fill

        let FillStaticsRegion state (field : fieldId) value isSuitableKey =
            let defaultValue = MemoryRegion.empty field.typ
            let fill region = MemoryRegion.fillRegion value isSuitableKey region
            state.staticFields <- PersistentDict.update state.staticFields field defaultValue fill

        let FillArrayRegion state typ value isSuitableKey =
            let defaultValue = fst3 typ |> MemoryRegion.empty
            let fill region = MemoryRegion.fillRegion value isSuitableKey region
            state.arrays <- PersistentDict.update state.arrays typ defaultValue fill

        let FillLengthRegion state typ value isSuitableKey =
            let defaultValue = MemoryRegion.empty TypeUtils.lengthType
            let fill region = MemoryRegion.fillRegion value isSuitableKey region
            state.lengths <- PersistentDict.update state.lengths typ defaultValue fill

        let FillLowerBoundRegion state typ value isSuitableKey =
            let defaultValue = MemoryRegion.empty TypeUtils.lengthType
            let fill region = MemoryRegion.fillRegion value isSuitableKey region
            state.lowerBounds <- PersistentDict.update state.lowerBounds typ defaultValue fill

        let FillStackBufferRegion state key value isSuitableKey =
            let defaultValue = MemoryRegion.empty typeof<int8>
            let fill region = MemoryRegion.fillRegion value isSuitableKey region
            state.stackBuffers <- PersistentDict.update state.stackBuffers key defaultValue fill

        let FillBoxedRegion state typ value isSuitableKey =
            let defaultValue = MemoryRegion.empty typ
            let fill region = MemoryRegion.fillRegion value isSuitableKey region
            state.boxedLocations <- PersistentDict.update state.boxedLocations typ defaultValue fill

        let ObjectToTerm (state : state) (o : obj) (typ : Type) = Memory.objToTerm state typ o
        let TryTermToObject (state : state) term = Memory.tryTermToObj state term

        let StateResult (state : state) =
            let callStackSize = CallStackSize state
            match EvaluationStack.Length state.evaluationStack with
            | _ when callStackSize > 2 -> internalfail "Finished state has many frames on stack! (possibly unhandled exception)"
            | 0 -> Nop()
            | 1 ->
                let result = EvaluationStack.Pop state.evaluationStack |> fst
                let method = GetCurrentExploringFunction state
                let hasByRefParameters = method.Parameters |> Array.exists (fun p -> p.ParameterType.IsByRef)
                let thisIsValueType = method.HasThis && TypeUtils.isValueType method.ReflectedType
                let additionalFrameIsNeeded = hasByRefParameters || thisIsValueType
                match method with
                | _ when callStackSize = 1 || callStackSize = 2 && additionalFrameIsNeeded -> Types.Cast result method.ReturnType
                | _ when state.exceptionsRegister.IsUnhandledError -> Nop()
                | _ -> internalfailf "Method is not finished! Stack trace = %O" CallStack.stackTraceString state.stack
            | _ -> internalfail "EvaluationStack size was bigger than 1"

    module Print =
        let Dump state = Memory.dump state
        let PrintPC pc = PC.toString pc
