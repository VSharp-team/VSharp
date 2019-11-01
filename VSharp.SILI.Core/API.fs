namespace VSharp.Core

open VSharp

module API =
    let private m = let r = new persistent<_>(always Metadata.empty, id) in r.Reset(); r
    let Enter (location : locationBinding) state k =
        m.Save()
        m.Mutate(State.mkMetadata location state)
        fun x -> m.Restore(); k x

    let Configure activator =
        State.configure activator
    let ConfigureSolver solver =
        Common.configureSolver solver
    let ConfigureSimplifier simplifier =
        Propositional.configureSimplifier simplifier
    let Reset() =
        Memory.reset()
        IdGenerator.reset()
    let SaveConfiguration() =
        Memory.saveConfiguration()
        IdGenerator.saveConfiguration()
    let Restore() =
        Memory.restore()
        IdGenerator.restore()

    let HigherOrderApply codeLoc state k = Explorer.higherOrderApplication m.Value codeLoc state k
    let BranchStatements state condition thenStatement elseStatement k =
        Common.statedConditionalExecutionWithMergek state condition thenStatement elseStatement k
    let BranchStatementsOnNull state reference thenStatement elseStatement k =
        BranchStatements state (fun state k -> k (Pointers.isNull m.Value reference, state)) thenStatement elseStatement k
    let BranchExpressions state condition thenBranch elseExpression k =
        Common.statelessConditionalExecutionWithMergek state.pc condition thenBranch elseExpression k
    let StatedConditionalExecution = Common.commonStatedConditionalExecutionk

    let GuardedApplyExpression term f = Merging.guardedErroredApply f term
    let GuardedStatedApplyStatementK state term f k = Merging.guardedErroredStatedApplyk f state term k
    let GuardedErroredStatedApplyk f errorHandler state term merge mergeStates k =
        Merging.commonGuardedErroredStatedApplyk f errorHandler state term merge mergeStates k

    let PerformBinaryOperation op isChecked state left right k = Operators.simplifyBinaryOperation m.Value op isChecked state left right k
    let PerformUnaryOperation op isChecked state t arg k = Operators.simplifyUnaryOperation m.Value op isChecked state t arg k

    [<AutoOpen>]
    module public Terms =
        let Nop = Nop
        let Error term = Error m.Value term
        let Concrete obj typ = Concrete m.Value obj typ
        let Constant name source typ = Constant m.Value name source typ
        let Expression op args typ = Expression m.Value op args typ
        let Struct fields typ = Struct m.Value fields typ
        let Class fields = Class m.Value fields
        let Union gvs = Union m.Value gvs

        let True = True
        let False = False

        let MakeNullRef () = makeNullRef m.Value
        let MakeDefault typ ref = Merging.guardedErroredApply (getFQLOfRef >> Some >> Memory.mkDefault m.Value typ) ref

        let MakeNumber n = makeNumber m.Value n

        let TypeOf term = typeOf term
        let BaseTypeOfRef ref = baseTypeOfRef ref
        let SightTypeOfRef ref = sightTypeOfRef ref

        let isStruct term = isStruct term
        let isReference term = isReference term
        let IsNullReference term = Pointers.isNull m.Value term

        let (|True|_|) t = (|True|_|) t
        let (|False|_|) t = (|False|_|) t
        let (|LazyInstantiation|_|) s = Memory.(|LazyInstantiation|_|) s
        let (|RecursionOutcome|_|) s = Explorer.(|RecursionOutcome|_|) s
        let (|Conjunction|_|) term = Terms.(|Conjunction|_|) term.term
        let (|Disjunction|_|) term = Terms.(|Disjunction|_|) term.term

        let ConstantsOf terms = discoverConstants terms

        let AddConditionToState conditionState condition = State.withPathCondition conditionState condition

    module Types =
        let FromDotNetType (state : state) t = t |> Types.Constructor.fromDotNetType |> State.substituteTypeVariables State.emptyCompositionContext state
        let ToDotNetType t = Types.toDotNetType t

        let SizeOf t = Types.sizeOf t

        let TLength = Types.lengthType
        let IsBool t = Types.isBool t
        let IsInteger t = Types.isInteger t
        let IsReal t = Types.isReal t
        let IsPointer t = Types.isPointer t
        let IsValueType t = Common.isValueType m.Value t

        let String = Types.String
        let (|StringType|_|) t = Types.(|StringType|_|) t

        let elementType arrayType = Types.elementType arrayType

        let TypeIsType leftType rightType = Common.typeIsType m.Value leftType rightType
        let TypeIsNullable typ = Common.isNullable m.Value typ
        let TypeIsRef typ ref = Common.typeIsRef m.Value typ ref
        let RefIsType ref typ = Common.refIsType m.Value ref typ
        let RefIsRef leftRef rightRef = Common.refIsRef m.Value leftRef rightRef

        let IsCast state targetType term = TypeCasting.isCast m.Value state term targetType
        let Cast state term targetType isChecked fail k = TypeCasting.cast m.Value isChecked state term targetType fail k
        let CastConcrete isChecked value typ = CastConcrete isChecked value typ m.Value
        let CastReferenceToPointer state reference = TypeCasting.castReferenceToPointer m.Value state reference

    module public Operators =
        let (!!) x = Propositional.simplifyNegation m.Value x id
        let (&&&) x y = Propositional.simplifyAnd m.Value x y id
        let (|||) x y = Propositional.simplifyOr m.Value x y id
        let (===) x y = Operators.ksimplifyEquality m.Value x y id
        let (!==) x y = Operators.ksimplifyEquality m.Value x y (!!)

    module public Arithmetics =
        let (===) x y = Arithmetics.simplifyEqual m.Value x y id
        let (!==) x y = Arithmetics.simplifyNotEqual m.Value x y id
        let (<<) x y = Arithmetics.simplifyLess m.Value x y id
        let (<<=) x y = Arithmetics.simplifyLessOrEqual m.Value x y id
        let (>>) x y = Arithmetics.simplifyGreater m.Value x y id
        let (>>=) x y = Arithmetics.simplifyGreaterOrEqual m.Value x y id
        let (%%%) x y = Arithmetics.simplifyRemainder m.Value false State.empty (x |> TypeOf |> Types.ToDotNetType) x y fst

    module public Memory =
        let EmptyState = State.empty

        let PopStack state = State.popStack state
        let PopTypeVariables state = State.popTypeVariablesSubstitution state
        let NewStackFrame state funcId parametersAndThis = Memory.newStackFrame state m.Value funcId parametersAndThis
        let NewScope state frame = Memory.newScope m.Value state frame
        let NewTypeVariables state subst = State.pushTypeVariablesSubstitution state subst

        let ReferenceField parentRef name typ = Memory.referenceField parentRef name typ
        let ReferenceLocalVariable location = Memory.referenceLocalVariable m.Value location
        let ReferenceStaticField targetType fieldName fieldType = Memory.referenceStaticField m.Value targetType fieldName fieldType
        let ReferenceArrayIndex state arrayRef indices = Memory.referenceArrayIndex m.Value state arrayRef indices

        let Dereference state reference = Memory.deref m.Value state reference
        let DereferenceWithoutValidation state reference = Memory.derefWithoutValidation m.Value state reference
        let DereferenceLocalVariable state location = Memory.referenceLocalVariable m.Value location |> Memory.deref m.Value state
        let Mutate state reference value = Memory.mutate m.Value state reference value
        let ReadBlockField blockTerm fieldName fieldType = Memory.readBlockField m.Value blockTerm fieldName fieldType

        let AllocateOnStack state key typ term = Memory.allocateOnStack m.Value state key typ term

        let AllocateInHeap state typ term =
            let address = Memory.freshHeapLocation m.Value
            Memory.allocateInHeap m.Value state address typ term

        let AllocateDefaultStatic state targetType =
            let fql = makeTopLevelFQL TopLevelStatics targetType
            let staticStruct, state = Memory.mkDefaultStatic m.Value state targetType fql
            Memory.allocateInStaticMemory m.Value state targetType staticStruct

        let MakeDefaultBlock termType fql = Memory.mkDefaultBlock m.Value termType (Some fql)

        let AllocateDefaultBlock state typ =
            let address = Memory.freshHeapLocation m.Value
            let fql = TopLevelHeap(address, typ, typ), []
            MakeDefaultBlock typ fql |> Memory.allocateInHeap m.Value state address typ

        let AllocateDefaultArray state dimensions typ =
            let address = Memory.freshHeapLocation m.Value
            let fql = makeTopLevelFQL TopLevelHeap (address, typ, typ)
            Arrays.makeDefault m.Value dimensions typ fql |> Memory.allocateInHeap m.Value state address typ

        let AllocateInitializedArray state dimensions rank typ initializer =
            let address = Memory.freshHeapLocation m.Value
            let fql = makeTopLevelFQL TopLevelHeap (address, typ, typ)
            let ref, state = Arrays.makeDefault m.Value dimensions typ fql |> Memory.allocateInHeap m.Value state address typ
            let state = Arrays.fromInitializer m.Value rank typ initializer fql |> Mutate state ref |> snd
            ref, state

        let AllocateString string state = Memory.allocateString m.Value state string

        let IsTypeNameInitialized termType state = Memory.termTypeInitialized m.Value termType state
        let Dump state = State.dumpMemory state

        let ArrayRank arrayTerm = Arrays.rank arrayTerm
        let ArrayLength arrayTerm = Arrays.length arrayTerm
        let ArrayLengthByDimension state arrayRef index = Memory.referenceArrayLength arrayRef index |> Memory.deref m.Value state
        let ArrayLowerBoundByDimension state arrayRef index = Memory.referenceArrayLowerBound arrayRef index |> Memory.deref m.Value state

        let StringLength state strRef =
            let fql = getFQLOfRef strRef |> Some
            let strStruct, state = Dereference state strRef
            Strings.length fql strStruct, state

        let StringCtorOfCharArray state this arrayRef =
            let fql = getFQLOfRef this |> Some
            BranchStatementsOnNull state arrayRef
                (fun state k -> k (Strings.makeConcreteStringStruct m.Value "" fql, state))
                (fun state k -> Dereference state arrayRef |> mapfst (Strings.ctorOfCharArray m.Value fql) |> k)
                id

    module Database =
        let QuerySummary codeLoc =
            Database.querySummary codeLoc ||?? lazy(internalfailf "database does not contain exploration results for %O" codeLoc)

    module RuntimeExceptions =
        let InvalidCastException state thrower =
            let message, state = Memory.AllocateString "Specified cast is not valid." state
            let term, state = State.createInstance m.Value typeof<System.InvalidCastException> [message] state
            thrower term, state
        let TypeInitializerException qualifiedTypeName innerException state thrower =
            let typeName, state = Memory.AllocateString qualifiedTypeName state
            let args = [typeName; innerException]
            let term, state = State.createInstance m.Value typeof<System.TypeInitializationException> args state
            thrower term, state
        let IndexOutOfRangeException state thrower =
            let term, state = State.createInstance m.Value typeof<System.IndexOutOfRangeException> [] state
            thrower term, state
        let InvalidProgramException state thrower =
            let term, state = State.createInstance m.Value typeof<System.InvalidProgramException> [] state
            thrower term, state
