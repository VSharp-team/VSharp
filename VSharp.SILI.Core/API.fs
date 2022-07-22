namespace VSharp.Core

open System
open System.Collections.Generic
open FSharpx.Collections
open VSharp

module API =
    let ConfigureSolver solver =
        SolverInteraction.configureSolver solver
    let ConfigureSimplifier simplifier =
        configureSimplifier simplifier

    let Reset() =
        IdGenerator.reset()
        TypeSolver.reset()
    let SaveConfiguration() =
        IdGenerator.saveConfiguration()
    let Restore() =
        IdGenerator.restore()

    let BranchStatements state condition thenStatement elseStatement k =
        Branching.statedConditionalExecutionWithMergek state condition thenStatement elseStatement k
    let BranchStatementsOnNull state reference thenStatement elseStatement k =
        BranchStatements state (fun state k -> k (Pointers.isNull reference, state)) thenStatement elseStatement k
    let BranchExpressions condition thenBranch elseExpression k =
        Common.statelessConditionalExecutionWithMergek condition thenBranch elseExpression k
    let StatedConditionalExecutionAppendResults (state : state) conditionInvocation (thenBranch : state -> (state list -> 'a) -> 'a) elseBranch k =
        Branching.commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch (fun x y -> [x;y]) (List.concat >> k)
    let StatedConditionalExecution = Branching.commonStatedConditionalExecutionk

    let GuardedApplyExpressionWithPC pc term f = Merging.guardedApplyWithPC pc f term
    let GuardedApplyExpression term f = Merging.guardedApply f term
    let GuardedStatedApplyStatementK state term f k = Branching.guardedStatedApplyk f state term k
    let GuardedStatedApplyk f state term mergeStates k =
        Branching.commonGuardedStatedApplyk f state term mergeStates k

    let ReleaseBranches() = Branching.branchesReleased <- true
    let AquireBranches() = Branching.branchesReleased <- false

    let PerformBinaryOperation op left right k = simplifyBinaryOperation op left right k
    let PerformUnaryOperation op arg k = simplifyUnaryOperation op arg k

    let SolveTypes (model : model) (state : state) = TypeCasting.solveTypes model state
    let TryGetModel state =
        match state.model with
        | Some model -> Some model
        | None ->
            match TypeCasting.checkSatWithSubtyping state with
            | SolverInteraction.SmtSat model -> Some model.mdl
            | SolverInteraction.SmtUnknown _ -> None
            // NOTE: irrelevant case, because exploring branch must be valid
            | SolverInteraction.SmtUnsat _ -> __unreachable__()

    let mutable private reportError = fun _ -> ()
    let ConfigureErrorReporter errorReporter =
        reportError <- errorReporter
    let ErrorReporter() =
        let result = fun state failCondition ->
            Branching.commonStatedConditionalExecutionk state
                (fun state k -> k (!!failCondition, state))
                (fun _ k -> k ())
                (fun state k -> k (reportError state))
                (fun _ _ -> [])
                ignore
        reportError <- fun _ -> ()
        result

    [<AutoOpen>]
    module public Terms =
        let Nop = Nop
        let Concrete obj typ = Concrete obj typ
        let Constant name source typ = Constant name source typ
        let Expression op args typ = Expression op args typ
        let Struct fields typ = Struct fields typ
        let Ref address = Ref address
        let Ptr baseAddress typ offset = Ptr baseAddress typ offset
        let Slice term first termSize pos = Slice term first termSize pos
        let HeapRef address baseType = HeapRef address baseType
        let Union gvs = Union gvs

        let True = True
        let False = False
        let NullRef = nullRef
        let MakeNullPtr t = makeNullPtr t
        let ConcreteHeapAddress (address : concreteHeapAddress) = ConcreteHeapAddress address

        let MakeBool b = makeBool b
        let MakeNumber n = makeNumber n
        // NOTE: Ref, Ptr, and nonzero numbers
        let MakeIntPtr (value : term) = value
        let AddressToBaseAndOffset address = Pointers.addressToBaseAndOffset address
        // NOTE: returns type of value
        let TypeOf term = typeOf term
        // NOTE: returns type of location, referenced by 'ref'
        let TypeOfLocation ref = typeOfRef ref
        let rec MostConcreteTypeOfHeapRef state ref =
            let getType ref =
                match ref.term with
                | HeapRef(address, sightType) -> Memory.mostConcreteTypeOfHeapRef state address sightType
                | Ptr(_, t, _) -> t
                | _ -> internalfailf "reading type token: expected heap reference, but got %O" ref
            commonTypeOf getType ref
        let TypeOfAddress state address = Memory.typeOfHeapLocation state address

        let IsStruct term = isStruct term
        let IsReference term = isReference term
        let IsPtr term = isPtr term
        let IsConcrete term =
            match term.term with
            | Concrete _ -> true
            | HeapRef(address, _) when isConcreteHeapAddress address -> true
            | _ -> false
        let IsNullReference term = Pointers.isNull term

        let GetHashCode term = Memory.getHashCode term

        let ReinterpretConcretes terms t = reinterpretConcretes terms t

        let (|ConcreteHeapAddress|_|) t = (|ConcreteHeapAddress|_|) t

        let (|Combined|_|) t = (|Combined|_|) t

        let (|True|_|) t = (|True|_|) t
        let (|False|_|) t = (|False|_|) t
        let (|Negation|_|) t = Terms.(|NegationT|_|) t
        let (|Conjunction|_|) term = Terms.(|Conjunction|_|) term.term
        let (|Disjunction|_|) term = Terms.(|Disjunction|_|) term.term
        let (|NullRef|_|) = function
            | {term = HeapRef(addr, _)} when addr = zeroAddress -> Some()
            | _ -> None
        let (|NullPtr|_|) = function
            | {term = Ptr(HeapLocation(addr, _), _, offset)} when addr = zeroAddress && offset = makeNumber 0 -> Some()
            | _ -> None

        let (|StackReading|_|) src = Memory.(|StackReading|_|) src
        let (|HeapReading|_|) src = Memory.(|HeapReading|_|) src
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
            match src with
            | :? IMemoryAccessConstantSource as ms -> structFieldChainRec [ ] ms
            | _ -> None
        let (|HeapAddressSource|_|) src = Memory.(|HeapAddressSource|_|) src
        let (|TypeInitializedSource|_|) src = Memory.(|TypeInitializedSource|_|) src
        let (|TypeSubtypeTypeSource|_|) src = TypeCasting.(|TypeSubtypeTypeSource|_|) src
        let (|RefSubtypeTypeSource|_|) src = TypeCasting.(|RefSubtypeTypeSource|_|) src
        let (|TypeSubtypeRefSource|_|) src = TypeCasting.(|TypeSubtypeRefSource|_|) src
        let (|RefSubtypeRefSource|_|) src = TypeCasting.(|RefSubtypeRefSource|_|) src

        let GetHeapReadingRegionSort src = Memory.getHeapReadingRegionSort src

        let rec HeapReferenceToBoxReference reference =
            match reference.term with
            | HeapRef({term = ConcreteHeapAddress addr}, typ) -> Ref (BoxedLocation(addr, typ))
            | HeapRef _ -> __insufficientInformation__ "Unable to unbox symbolic ref %O" reference
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, HeapReferenceToBoxReference v)) |> Merging.merge
            | _ -> internalfailf "Unboxing: expected heap reference, but got %O" reference

        let AddConstraint conditionState condition = Memory.addConstraint conditionState condition
        let IsFalsePathCondition conditionState = PC.isFalse conditionState.pc
        let Contradicts state condition = PC.add state.pc condition |> PC.isFalse
        let PathConditionToSeq (pc : pathCondition) = PC.toSeq pc
        let EmptyPathCondition = PC.empty

    module Types =
        let Numeric t = Types.Numeric t
        let ObjectType = Types.objectType
        let IndexType = Types.indexType

        let FromDotNetType t = Types.Constructor.fromDotNetType t
        let ToDotNetType t = Types.toDotNetType t

        let SizeOf t = Types.sizeOf t
        let RankOf t = Types.rankOf t

        let TLength = Types.lengthType
        let IsBool t = Types.isBool t
        let IsInteger t = Types.isInteger t
        let IsReal t = Types.isReal t
        let IsNumeric t = Types.isNumeric t
        let IsPointer t = Types.isPointer t
        let IsValueType t = Types.isValueType t
        let IsArrayType t = Types.isArray t
        let IsNullable t = Types.isNullable t
        let IsEnum t = Types.isEnum t
        let Int8 = Types.Int8
        let Int16 = Types.Int16
        let Int32 = Types.Int32
        let Int64 = Types.Int64
        let Char = Types.Char
        let String = Types.String
        let (|StringType|_|) t = Types.(|StringType|_|) t

        let ElementType arrayType = Types.elementType arrayType
        let ArrayTypeToSymbolicType arrayType = arrayTypeToSymbolicType arrayType

        let TypeIsType leftType rightType = TypeCasting.typeIsType leftType rightType
        let TypeIsRef state typ ref = TypeCasting.typeIsRef state typ ref
        let RefIsType state ref typ = TypeCasting.refIsType state ref typ
        let RefIsAssignableToType state ref typ = TypeCasting.refIsAssignableToType state ref typ
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
        let (!==) x y = simplifyNotEqual x y id
        let (<<) x y = simplifyLess x y id
        let (<<=) x y = simplifyLessOrEqual x y id
        let (>>) x y = simplifyGreater x y id
        let (>>=) x y = simplifyGreaterOrEqual x y id
        let (%%%) x y = simplifyRemainder true (x |> TypeOf |> Types.ToDotNetType) x y id

        let Mul x y = mul x y
        let Sub x y = sub x y
        let Add x y = add x y
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
            Memory.fillWithParametersAndThis modelState method
            {subst = Dictionary<_,_>(); state = modelState}

        let PopFrame state = Memory.popFrame state
        let ForcePopFrames count state = Memory.forcePopFrames count state
        let PopTypeVariables state = Memory.popTypeVariablesSubstitution state
        let NewStackFrame state method parametersAndThis = Memory.newStackFrame state method parametersAndThis
        let NewTypeVariables state subst = Memory.pushTypeVariablesSubstitution state subst

        let rec ReferenceArrayIndex state arrayRef indices (valueType : symbolicType option) =
            match arrayRef.term with
            | HeapRef(addr, typ) ->
                let elemType, dim, _ as arrayType = Memory.mostConcreteTypeOfHeapRef state addr typ |> symbolicTypeToArrayType
                assert(dim = List.length indices)
                match valueType with
                | Some valueType when not (Types.canCastImplicitly valueType elemType && Types.sizeOf elemType = Types.sizeOf valueType) ->
                    Ptr (HeapLocation(addr, typ)) valueType (Memory.arrayIndicesToOffset state addr arrayType indices)
                | _ -> ArrayIndex(addr, indices, arrayType) |> Ref
            | Ptr(HeapLocation(address, _) as baseAddress, t, offset) ->
                assert(Types.IsArrayType t)
                let elemType, _, _ as arrayType = symbolicTypeToArrayType t
                let indexOffset = Memory.arrayIndicesToOffset state address arrayType indices
                Ptr baseAddress elemType (add offset indexOffset)
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ReferenceArrayIndex state v indices valueType)) |> Merging.merge
            | _ -> internalfailf "Referencing array index: expected reference, but got %O" arrayRef

        let rec ReferenceField state reference fieldId =
            let isSuitableField address typ =
                let typ = Memory.mostConcreteTypeOfHeapRef state address typ |> Types.ToDotNetType
                fieldId.declaringType.IsAssignableFrom typ
            match reference.term with
            | HeapRef(address, typ) when isSuitableField address typ |> not ->
                Logger.trace "[WARNING] unsafe cast of term %O in safe context" reference
                let offset = Reflection.getFieldOffset fieldId |> MakeNumber
                Ptr (HeapLocation(address, typ)) (Types.FromDotNetType fieldId.typ) offset
            | HeapRef(address, typ) when fieldId.declaringType.IsValueType ->
                // TODO: Need to check mostConcreteTypeOfHeapRef using pathCondition?
                assert(isSuitableField address typ)
                ReferenceField state (HeapReferenceToBoxReference reference) fieldId
            | HeapRef(address, typ) ->
                // TODO: Need to check mostConcreteTypeOfHeapRef using pathCondition?
                assert(isSuitableField address typ)
                ClassField(address, fieldId) |> Ref
            | Ref address ->
                assert(Memory.baseTypeOfAddress state address |> Types.isStruct)
                StructField(address, fieldId) |> Ref
            | Ptr(baseAddress, _, offset) ->
                let fieldOffset = Reflection.getFieldOffset fieldId |> makeNumber
                Ptr baseAddress (Types.FromDotNetType fieldId.typ) (add offset fieldOffset)
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ReferenceField state v fieldId)) |> Merging.merge
            | _ -> internalfailf "Referencing field: expected reference, but got %O" reference

        let private transformBoxedRef ref =
            match ref.term with
            | HeapRef _ -> HeapReferenceToBoxReference ref
            | _ -> ref

        let Read state reference =
            transformBoxedRef reference |> Memory.read state (ErrorReporter())
        let ReadLocalVariable state location = Memory.readStackLocation state location
        let ReadThis state method = Memory.readStackLocation state (ThisKey method)
        let ReadArgument state parameterInfo = Memory.readStackLocation state (ParameterKey parameterInfo)
        let ReadField state term field =
            let doRead target =
                match target.term with
                | HeapRef _
                | Ref _ -> ReferenceField state target field |> Memory.read state (ErrorReporter())
                | Struct _ -> Memory.readStruct target field
                | _ -> internalfailf "Reading field of %O" term
            Merging.guardedApply doRead term

        let rec ReadArrayIndex state reference indices valueType =
            let ref = ReferenceArrayIndex state reference indices valueType
            let value = Read state ref
            match valueType with
            | Some valueType when isReference ref -> Types.Cast value valueType
            | _ -> value

        let rec ReadStringChar state reference index =
            match reference.term with
            | HeapRef(addr, typ) when Memory.mostConcreteTypeOfHeapRef state addr typ = Types.String ->
                Memory.readArrayIndex state addr [index] (Types.Char, 1, true)
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ReadStringChar state v index)) |> Merging.merge
            | _ -> internalfailf "Reading string char: expected reference, but got %O" reference
        let ReadStaticField state typ field = Memory.readStaticField state typ field
        let ReadDelegate state reference = Memory.readDelegate state reference

        let InitializeArray state arrayRef handleTerm = ArrayInitialization.initializeArray state arrayRef handleTerm

        let WriteLocalVariable state location value = Memory.writeStackLocation state location value
        let Write state reference value =
            let errorReporter = ErrorReporter()
            Branching.guardedStatedMap (fun state reference -> Memory.write state errorReporter reference value) state reference
        let WriteStructField structure field value = Memory.writeStruct structure field value
        let WriteClassField state reference field value =
            Branching.guardedStatedMap
                (fun state reference ->
                    match reference.term with
                    | HeapRef(addr, _) -> Memory.writeClassField state addr field value
                    | _ -> internalfailf "Writing field of class: expected reference, but got %O" reference
                    state)
                state reference
        let WriteArrayIndex state reference indices value valueType =
            let ref = ReferenceArrayIndex state reference indices valueType
            let value =
                if isPtr ref then Option.fold (fun _ -> Types.Cast value) value valueType
                else MostConcreteTypeOfHeapRef state reference |> symbolicTypeToArrayType |> fst3 |> Types.Cast value
            Write state ref value
        let WriteStaticField state typ field value = Memory.writeStaticField state typ field value

        let DefaultOf typ = makeDefaultValue typ

        let MakeSymbolicThis m = Memory.makeSymbolicThis m
        let MakeSymbolicValue source name typ = Memory.makeSymbolicValue source name typ

        let CallStackContainsFunction state method = CallStack.containsFunc state.stack method
        let CallStackSize state = CallStack.size state.stack
        let GetCurrentExploringFunction state = CallStack.getCurrentFunc state.stack

        let BoxValueType state term =
            let address = Memory.freshAddress state
            let reference = HeapRef (ConcreteHeapAddress address) Types.ObjectType
            Memory.writeBoxedLocation state address term
            reference

        let InitializeStaticMembers state targetType =
            Memory.initializeStaticMembers state targetType

        let AllocateTemporaryLocalVariable state typ term =
            let tmpKey = TemporaryLocalVariableKey typ
            let ref = PrimitiveStackLocation tmpKey |> Ref
            Memory.allocateOnStack state tmpKey term
            ref

        let AllocateDefaultClass state typ =
            if typ = Types.String then Memory.allocateString state ""
            else Memory.allocateClass state typ

        let AllocateDefaultArray state lengths typ =
            let zeroLowerBounds = List.map (fun _ -> makeNumber 0) lengths
            let address = Memory.allocateArray state typ zeroLowerBounds lengths
            HeapRef address typ

        let AllocateVectorArray state length elementType =
            let address = Memory.allocateVector state elementType length
            HeapRef address (ArrayType(elementType, Vector))

        let AllocateConcreteVectorArray state length elementType contents =
            let address = Memory.allocateConcreteVector state elementType length contents
            HeapRef address (ArrayType(elementType, Vector))

        let AllocateDelegate state delegateTerm = Memory.allocateDelegate state delegateTerm

        let AllocateString string state = Memory.allocateString state string
        let AllocateEmptyString state length = Memory.allocateEmptyString state length
        let CreateStringFromChar state char = Memory.createStringFromChar state char

        let LinearizeArrayIndex state address indices (_, dim, _ as arrayType) =
            let lens = List.init dim (fun dim -> Memory.readLength state address (makeNumber dim) arrayType)
            let lbs = List.init dim (fun dim -> Memory.readLowerBound state address (makeNumber dim) arrayType)
            Memory.linearizeArrayIndex lens lbs indices

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
                assert(Memory.mostConcreteTypeOfHeapRef state srcAddress srcSightType = Types.String)
                assert(Memory.mostConcreteTypeOfHeapRef state dstAddress dstSightType = Types.String)
                let stringArrayType = (Types.Char, 1, true)
                Copying.copyArray state srcAddress srcIndex stringArrayType dstAddress dstIndex stringArrayType length
            | _ -> internalfailf "Coping arrays: expected heapRefs, but got %O, %O" src dst

        let ClearArray state array index length =
            match array.term with
            | HeapRef(address, sightType) ->
                let arrayType = Memory.mostConcreteTypeOfHeapRef state address sightType |> symbolicTypeToArrayType
                let elemType = fst3 arrayType
                let value = if Types.IsValueType elemType then makeNumber 0 else NullRef
                Copying.fillArray state address arrayType index length value
            | _ -> internalfailf "Clearing array: expected heapRef, but got %O" array

        let StringFromReplicatedChar state string char length =
            match string.term with
            | HeapRef(address, sightType) ->
                assert(Memory.mostConcreteTypeOfHeapRef state address sightType = Types.String)
                let arrayType = Types.Char, 1, true
                Copying.fillArray state address arrayType (makeNumber 0) length char
                Memory.writeLengthSymbolic state address (makeNumber 0) arrayType (add length (makeNumber 1))
                Memory.writeArrayIndex state address [length] arrayType (Concrete '\000' Types.Char)
                Memory.writeClassField state address Reflection.stringLengthField length
            | _ -> internalfailf "Creating string from replicated char: expected heapRef, but got %O" string

        let IsTypeInitialized state typ = Memory.isTypeInitialized state typ
        let Dump state = Memory.dump state
        let StackTrace stack = CallStack.stackTrace stack
        let StackTraceString stack = CallStack.stackTraceString stack

        let rec ArrayRank state arrayRef =
            match arrayRef.term with
            | HeapRef(addr, typ) -> Memory.mostConcreteTypeOfHeapRef state addr typ |> Types.rankOf |> makeNumber
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ArrayRank state v)) |> Merging.merge
            | _ -> internalfailf "Getting rank of array: expected ref, but got %O" arrayRef
        let rec ArrayLengthByDimension state arrayRef index =
            match arrayRef.term with
            | HeapRef(addr, typ) -> Memory.mostConcreteTypeOfHeapRef state addr typ |> symbolicTypeToArrayType |> Memory.readLength state addr index
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ArrayLengthByDimension state v index)) |> Merging.merge
            | _ -> internalfailf "reading array length: expected heap reference, but got %O" arrayRef
        let rec ArrayLowerBoundByDimension state arrayRef index =
            match arrayRef.term with
            | HeapRef(addr, typ) -> Memory.mostConcreteTypeOfHeapRef state addr typ |> symbolicTypeToArrayType |> Memory.readLowerBound state addr index
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ArrayLowerBoundByDimension state v index)) |> Merging.merge
            | _ -> internalfailf "reading array lower bound: expected heap reference, but got %O" arrayRef

        let rec CountOfArrayElements state arrayRef =
            match arrayRef.term with
            | HeapRef(address, typ) ->
                let _, dim, _ as arrayType = Memory.mostConcreteTypeOfHeapRef state address typ |> symbolicTypeToArrayType
                let lens = List.init dim (fun dim -> Memory.readLength state address (makeNumber dim) arrayType)
                List.fold mul (makeNumber 1) lens
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, CountOfArrayElements state v)) |> Merging.merge
            | _ -> internalfailf "counting array elements: expected heap reference, but got %O" arrayRef

        let StringLength state strRef = Memory.lengthOfString state strRef
        let StringCtorOfCharArray state arrayRef dstRef =
            match dstRef.term with
            | HeapRef({term = ConcreteHeapAddress dstAddr} as address, typ) ->
                assert(Memory.mostConcreteTypeOfHeapRef state address typ = Types.String)
                Branching.guardedStatedMap (fun state arrayRef ->
                    match arrayRef.term with
                    | HeapRef(arrayAddr, typ) ->
                        assert(Memory.mostConcreteTypeOfHeapRef state arrayAddr typ = ArrayType(Types.Char, Vector))
                        Copying.copyCharArrayToString state arrayAddr dstAddr
                    | _ -> internalfailf "constructing string from char array: expected array reference, but got %O" arrayRef
                    state)
                    state arrayRef
            | HeapRef _
            | Union _ -> __notImplemented__()
            | _ -> internalfailf "constructing string from char array: expected string reference, but got %O" dstRef

        let ComposeStates state state' = Memory.composeStates state state'
        let WLP state pc' = PC.mapPC (Memory.fillHoles state) pc' |> PC.union state.pc

        let Merge2States (s1 : state) (s2 : state) = Memory.merge2States s1 s2
        let Merge2Results (r1, s1 : state) (r2, s2 : state) = Memory.merge2Results (r1, s1) (r2, s2)
//            let pc1 = PC.squashPC state1.pc
//            let pc2 = PC.squashPC state2.pc
//            if pc1 = Terms.True && pc2 = Terms.True then __unreachable__()
//            __notImplemented__() : state
            //Merging.merge2States pc1 pc2 {state1 with pc = []} {state2 with pc = []}

        let FillRegion state value = function
            | HeapFieldSort field ->
                state.classFields <- PersistentDict.update state.classFields field (field.typ |> Types.FromDotNetType |> MemoryRegion.empty) (MemoryRegion.fillRegion value)
            | StaticFieldSort field ->
                state.staticFields <- PersistentDict.update state.staticFields field (field.typ |> Types.FromDotNetType |> MemoryRegion.empty) (MemoryRegion.fillRegion value)
            | ArrayIndexSort typ ->
                state.arrays <- PersistentDict.update state.arrays typ (typ |> fst3 |> MemoryRegion.empty) (MemoryRegion.fillRegion value)
            | ArrayLengthSort typ ->
                state.lengths <- PersistentDict.update state.lengths typ (MemoryRegion.empty Types.lengthType) (MemoryRegion.fillRegion value)
            | ArrayLowerBoundSort typ ->
                state.lowerBounds <- PersistentDict.update state.lowerBounds typ (MemoryRegion.empty Types.lengthType) (MemoryRegion.fillRegion value)
            | StackBufferSort key ->
                state.stackBuffers <- PersistentDict.update state.stackBuffers key (MemoryRegion.empty Types.Int8) (MemoryRegion.fillRegion value)

    module Print =
        let Dump state = Memory.dump state
        let PrintPC pc = PC.toString pc
