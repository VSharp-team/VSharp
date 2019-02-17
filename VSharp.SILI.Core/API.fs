namespace VSharp.Core

open VSharp

module API =
    let private m = let r = new persistent<_>(always Metadata.empty, id) in r.Reset(); r
    let Enter (location : locationBinding) state k =
        m.Save()
        m.Mutate(State.mkMetadata location state)
        fun x -> m.Restore(); k x

    let Configure activator interpreter =
        State.configure activator
        Explorer.configure interpreter
    let ConfigureSolver solver =
        Common.configureSolver solver
    let Reset() =
        Memory.reset()
        IdGenerator.reset()
    let SaveConfiguration() =
        Memory.saveConfiguration()
        IdGenerator.saveConfiguration()
    let Restore() =
        Memory.restore()
        IdGenerator.restore()

    let InterpretEntryPoint id k = Explorer.interpretEntryPoint id k
    let Explore id k = Explorer.explore id k

    let Call funcId state body k = Explorer.call m.Value funcId state body k
    let ComposeStatements rs statements isContinueConsumer reduceStatement k =
        ControlFlow.composeStatements statements isContinueConsumer reduceStatement (fun state -> Memory.newScope m.Value state []) rs k
    let HigherOrderApply funcId state k = Explorer.higherOrderApplication m.Value funcId state k
    let BranchStatements state condition thenBranch elseBranch k =
         Common.statedConditionalExecution state condition thenBranch elseBranch ControlFlow.mergeResults ControlFlow.merge2Results ControlFlow.throwOrIgnore k
    let BranchExpressions state condition thenExpression elseExpression k = Common.statedConditionalExecution state condition thenExpression elseExpression Merging.merge Merging.merge2Terms id k
    let BranchStatementsOnNull state reference thenBranch elseBranch k =
        BranchStatements state (fun state k -> k (Pointers.isNull m.Value reference, state)) thenBranch elseBranch k
    let BranchExpressionsOnNull state reference thenExpression elseExpression k =
        BranchExpressions state (fun state k -> k (Pointers.isNull m.Value reference, state)) thenExpression elseExpression k

    let GuardedApplyExpression term f = Merging.guardedErroredApply f term
    let GuardedStatedApplyStatementK state term f k =
        Merging.commonGuardedErroredStatedApplyk f ControlFlow.throwOrIgnore state term ControlFlow.mergeResults k
    let GuardedStatelessApplyStatement term f =
        Merging.commonGuardedErroredApply f ControlFlow.throwOrIgnore term ControlFlow.mergeResults

    let PerformBinaryOperation op isChecked state t left right k = Operators.simplifyBinaryOperation m.Value op isChecked state t left right k
    let PerformUnaryOperation op isChecked state t arg k = Operators.simplifyUnaryOperation m.Value op isChecked state t arg k

    [<AutoOpen>]
    module public Terms =
        let Nop = Nop
        let Error term = Error m.Value term
        let Concrete obj typ = Concrete m.Value obj typ
        let Constant name source typ = Constant m.Value name source typ
        let Expression op args typ = Expression m.Value op args typ
        let Struct fields typ = Struct m.Value fields typ
        let Union gvs = Union m.Value gvs

        let True = True
        let False = False

        let MakeNullRef () = makeNullRef m.Value
        let MakeDefault typ = Memory.mkDefault m.Value typ None
        let MakeNumber n = makeNumber m.Value n
        let MakeLambda body signature = Lambdas.make m.Value body signature

        let TypeOf term = typeOf term
        let (|Lambda|_|) t = Lambdas.(|Lambda|_|) t
        let (|LazyInstantiation|_|) s = Memory.(|LazyInstantiation|_|) s
        let (|RecursionOutcome|_|) s = Explorer.(|RecursionOutcome|_|) s
        let (|Conjunction|_|) term = Terms.(|Conjunction|_|) term.term
        let (|Disjunction|_|) term = Terms.(|Disjunction|_|) term.term

        let PersistentLocalAndConstraintTypes = TypeCasting.persistentLocalAndConstraintTypes m.Value
        let ConstantsOf terms = discoverConstants terms

    module Types =
        let FromDotNetType (state : state) t = t |> Types.Constructor.fromDotNetType |> State.substituteTypeVariables State.emptyCompositionContext state
        let ToDotNetType t = Types.toDotNetType t
        let WrapReferenceType t = Types.wrapReferenceType t

        let SizeOf t = Types.sizeOf t

        let TLength = Types.lengthType
        let IsBool t = Types.isBool t
        let IsInteger t = Types.isInteger t
        let IsReal t = Types.isReal t
        let IsNativeInt t = Pointers.isNativeInt t

        let String = Types.String
        let (|StringType|_|) t = Types.(|StringType|_|) t

        let IsSubtype leftType rightType = Common.is m.Value leftType rightType
        let CanCast state targetType term = TypeCasting.canCast m.Value state targetType term
        let Cast state term targetType isChecked fail k = TypeCasting.cast m.Value state term targetType isChecked (TypeCasting.primitiveCast m.Value isChecked) fail k
        let HierarchyCast state term targetType fail k = TypeCasting.cast m.Value state term targetType false id fail k
        let CastConcrete value typ = CastConcrete value typ m.Value
        let CastReferenceToPointer state reference k = TypeCasting.castReferenceToPointer m.Value state reference k

    module public ControlFlowConstructors =
        let NoComputation = NoResult Metadata.empty
        let NoResult () = NoResult m.Value
        let Break () = Break m.Value
        let Continue () = Continue m.Value
        let Return (term : term) = Return term.metadata term
        let Throw (term : term) = Throw term.metadata term
        let Guarded grs = Guarded m.Value grs

    module public ControlFlow =
        let ResultToTerm r = ControlFlow.resultToTerm r
        let ThrowOrReturn t = ControlFlow.throwOrReturn t
        let ThrowOrIgnore t = ControlFlow.throwOrIgnore t
        let ConsumeErrorOrReturn consumer t = ControlFlow.consumeErrorOrReturn consumer t
        let ComposeSequentially oldRes newRes oldState newState = ControlFlow.composeSequentially oldRes newRes oldState newState
        let ComposeExpressions exprs state exprsMapper k = ControlFlow.composeExpressions exprs state exprsMapper k
        let ConsumeBreak r = ControlFlow.consumeBreak r
        let PickOutExceptions r = ControlFlow.pickOutExceptions r

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

        let ReferenceField state followHeapRefs name typ parentRef = Memory.referenceField m.Value state followHeapRefs name typ parentRef
        let ReferenceLocalVariable state location followHeapRefs = Memory.referenceLocalVariable m.Value state location followHeapRefs
        let ReferenceStaticField state followHeapRefs fieldName fieldType targetType = Memory.referenceStaticField m.Value state followHeapRefs fieldName fieldType targetType
        let ReferenceArrayIndex state arrayRef indices = Memory.referenceArrayIndex m.Value state arrayRef indices

        let Dereference state reference = Memory.deref m.Value state reference
        let DereferenceWithoutValidation state reference = Memory.derefWithoutValidation m.Value state reference
        let DereferenceLocalVariable state id = Memory.referenceLocalVariable m.Value state id false |> Memory.deref m.Value state
        let Mutate state reference value = Memory.mutate m.Value state reference value

        let AllocateOnStack state key term = Memory.allocateOnStack m.Value state key term

        let AllocateInHeap state term =
            let address = Memory.freshHeapLocation m.Value
            Memory.allocateInHeap m.Value state address term

        let AllocateDefaultStatic state targetType =
            let fql = makeTopLevelFQL TopLevelStatics targetType
            let staticStruct, state = Memory.mkDefaultStaticStruct m.Value state targetType fql
            Memory.allocateInStaticMemory m.Value state targetType staticStruct

        let MakeDefaultStruct termType fql = Some fql |> Memory.mkDefaultStruct m.Value termType

        let AllocateDefaultStruct state typ =
            let address = Memory.freshHeapLocation m.Value
            let fql = TopLevelHeap(address, typ, typ), []
            MakeDefaultStruct typ fql |> Memory.allocateInHeap m.Value state address

        let AllocateDefaultArray state dimensions typ =
            let address = Memory.freshHeapLocation m.Value
            let fql = makeTopLevelFQL TopLevelHeap (address, typ, typ)
            Arrays.makeDefault m.Value dimensions typ fql |> Memory.allocateInHeap m.Value state address

        let AllocateInitializedArray state dimensions rank typ initializer =
            let address = Memory.freshHeapLocation m.Value
            let fql = makeTopLevelFQL TopLevelHeap (address, typ, typ)
            let ref, state = Arrays.makeDefault m.Value dimensions typ fql |> Memory.allocateInHeap m.Value state address
            let state = Arrays.fromInitializer m.Value (Memory.tick()) rank typ initializer fql |> Mutate state ref |> snd
            ref, state

        let AllocateString string state = Memory.allocateString m.Value state string

        let IsTypeNameInitialized termType state = Memory.termTypeInitialized m.Value termType state
        let Dump state = State.dumpMemory state

        let ArrayLength arrayTerm = Arrays.length arrayTerm
        let ArrayLengthByDimension state arrayRef index = Memory.referenceArrayLength arrayRef index |> Memory.deref m.Value state
        let ArrayLowerBoundByDimension state arrayRef index = Memory.referenceArrayLowerBound arrayRef index |> Memory.deref m.Value state

        let StringCtorOfCharArray state this arrayRef =
            let fql = Some <| getFQLOfRef this
            BranchExpressionsOnNull state arrayRef
                (fun state k -> k (Strings.makeConcreteStringStruct m.Value (Memory.tick()) "" fql, state))
                (fun state k -> Dereference state arrayRef |> mapfst (Strings.ctorOfCharArray m.Value (Memory.tick()) fql) |> k)
                id

    module Database =
        let QuerySummary funcId =
            Database.querySummary funcId ||?? lazy(internalfailf "database does not contain exploration results for %O" funcId)

    module RuntimeExceptions =
        let NullReferenceException state thrower =
            let term, state = Memory.npe m.Value state
            thrower term, state
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
