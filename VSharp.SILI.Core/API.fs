namespace VSharp.Core

open VSharp

[<AutoOpen>]
module public API =
    let private m = let r = new Persistent<_>(always Metadata.empty, id) in r.Reset(); r
    let Enter location state k =
        m.Save()
        m.Mutate(State.mkMetadata location state)
        fun x -> m.Restore(); k x

    let Configure (activator : IActivator) (interpreter : IInterpreter) =
        State.configure activator
        Explorer.configure interpreter
    let Reset() =
        Memory.reset()
        IdGenerator.reset()
    let SaveConfiguration() =
        Memory.saveConfiguration()
        IdGenerator.saveConfiguration()
    let Restore() =
        Memory.restore()
        IdGenerator.restore()

    let InterpretEntryPoint = Explorer.interpretEntryPoint
    let Explore = Explorer.explore

    let Call funcId state body k = Explorer.call m.Value funcId state body k
    let InvokeAfter consumeContinue (result, state) statement k = ControlFlow.invokeAfter consumeContinue (result, state) statement k

    let BranchStatements state condition thenBranch elseBranch k =
         Common.reduceConditionalExecution state condition thenBranch elseBranch ControlFlow.mergeResults ControlFlow.merge2Results ControlFlow.throwOrIgnore k
    let BranchExpressions state condition thenExpression elseExpression k = Common.reduceConditionalExecution state condition thenExpression elseExpression Merging.merge Merging.merge2Terms id k
    let BranchStatementsOnNull state reference thenBranch elseBranch k =
        BranchStatements state (fun state k -> k (Pointers.isNull m.Value reference, state)) thenBranch elseBranch k

    let GuardedApplyExpressionK term mapper k =
        match term.term with
        | Error _ -> term
        | Union gvs -> Merging.guardedMapk mapper gvs k
        | _ -> mapper term k
    let GuardedApplyExpression term mapper =
        match term.term with
        | Error _ -> term
        | Union gvs -> Merging.guardedMap mapper gvs
        | _ -> mapper term
    let GuardedApplyStatement state term mapper k =
        match term.term with
        | Error e -> k (Throw term.metadata e, state)
        | Union gvs -> Merging.commonGuardedStateMapk mapper gvs state ControlFlow.mergeResults k
        | _ -> mapper state term k
    let GuardedApplyStatelessStatement term mapper =
        match term.term with
        | Error e -> Throw term.metadata e
        | Union gvs -> Merging.commonGuardedMapk (Cps.ret mapper) gvs ControlFlow.mergeResults id
        | _ -> mapper term

    let PerformBinaryOperation op isChecked state t left right k = Operators.simplifyBinaryOperation m.Value op isChecked state t left right k
    let PerformUnaryOperation op isChecked state t arg k = Operators.simplifyUnaryOperation m.Value op isChecked state t arg k

    [<AutoOpen>]
    module public Terms =
        let Nop = Nop
        let Error term = Error m.Value term
        let Concrete obj typ = Concrete m.Value obj typ
        let Constant name source typ = Constant m.Value name source typ
        let Array lower constant contents lengths typ = Array m.Value lower constant contents lengths typ
        let Expression op args typ = Expression m.Value op args typ
        let Union gvs = Union m.Value gvs

        let True = True
        let False = False

        let MakeNullRef typ = MakeNullRef typ m.Value
        let MakeDefault typ = Memory.mkDefault m.Value typ
        let MakeNumber n = MakeNumber n m.Value
        let MakeString length str = Strings.makeString length str (Memory.tick())
        let MakeLambda body signature = Lambdas.make m.Value body signature
        let MakeDefaultArray dimensions typ = Arrays.makeDefault m.Value dimensions typ
        let MakeInitializedArray rank typ initializer = Arrays.fromInitializer m.Value (Memory.tick()) rank typ initializer

        let TypeOf = TypeOf
        let (|Lambda|_|) = Lambdas.(|Lambda|_|)

    module RuntimeExceptions =
        let NullReferenceException state thrower =
            let term, state = Memory.npe m.Value state
            thrower term, state
        let InvalidCastException state thrower =
            let message = MakeConcreteString "Specified cast is not valid." m.Value
            let term, state = State.createInstance m.Value typeof<System.InvalidCastException> [message] state
            thrower term, state
        let TypeInitializerException qualifiedTypeName innerException state thrower =
            let args = [MakeConcreteString qualifiedTypeName m.Value; innerException]
            let term, state = State.createInstance m.Value typeof<System.TypeInitializationException> args state
            thrower term state
        let IndexOutOfRangeException state thrower =
            let term, state = State.createInstance m.Value typeof<System.IndexOutOfRangeException> [] state
            thrower term, state

    module Types =
        let TLength = Arrays.lengthTermType
        let CanCast state targetType term = TypeCasting.canCast m.Value state targetType term
        let Cast state term targetType isChecked fail k = TypeCasting.cast m.Value state term targetType isChecked (TypeCasting.primitiveCast m.Value isChecked) fail k
        let HierarchyCast state term targetType fail k = TypeCasting.cast m.Value state term targetType false id fail k
        let CastConcrete value typ = CastConcrete value typ m.Value
        let CastReferenceToPointer state reference k = TypeCasting.castReferenceToPointer m.Value state reference k

    [<AutoOpen>]
    module public ControlFlowConstructors =
        let NoComputation = NoResult Metadata.empty
        let NoResult () = NoResult m.Value
        let Break () = Break m.Value
        let Continue () = Continue m.Value
        let Return (term : Term) = Return term.metadata term
        let Throw (term : Term) = Throw term.metadata term
        let Guarded grs = Guarded m.Value grs

    module public ControlFlow =
        let ResultToTerm = ControlFlow.resultToTerm
        let ThrowOrReturn = ControlFlow.throwOrReturn
        let ThrowOrIgnore = ControlFlow.throwOrIgnore
        let ConsumeErrorOrReturn = ControlFlow.consumeErrorOrReturn
        let ComposeSequentially = ControlFlow.composeSequentially
        let ConsumeBreak = ControlFlow.consumeBreak
        let PickOutExceptions = ControlFlow.pickOutExceptions

    [<AutoOpen>]
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
        // Lightweight version: divide by zero exceptions are ignored!
        let (%%%) x y = Arithmetics.simplifyRemainder m.Value false State.empty (x |> TypeOf |> Types.ToDotNetType) x y fst

    module public Memory =
        let PopStack state = State.popStack state
        let NewStackFrame state funcId parametersAndThis = Memory.newStackFrame state m.Value funcId parametersAndThis
        let NewScope state frame = Memory.newScope m.Value state frame

        let ReferenceField state followHeapRefs name typ parentRef = Memory.referenceField m.Value state followHeapRefs name typ parentRef
        let ReferenceLocalVariable state location followHeapRefs = Memory.referenceLocalVariable m.Value state location followHeapRefs
        let ReferenceStaticField state followHeapRefs fieldName typ typeName = Memory.referenceStaticField m.Value state followHeapRefs fieldName typ typeName
        let ReferenceArrayIndex state arrayRef indices = Memory.referenceArrayIndex m.Value state arrayRef indices

        let Dereference state reference = Memory.deref m.Value state reference
        let DereferenceLocalVariable state id = Memory.referenceLocalVariable m.Value state id false |> Memory.deref m.Value state
        let Mutate state reference value = Memory.mutate m.Value state reference value

        let AllocateOnStack state key term = Memory.allocateOnStack m.Value state key term
        let AllocateInHeap state term = Memory.allocateInHeap m.Value state term
        let AllocateDefaultStatic state qualifiedTypeName = Memory.mkDefaultStruct m.Value true qualifiedTypeName |> Memory.allocateInStaticMemory m.Value state qualifiedTypeName
        let MakeDefaultStruct qualifiedTypeName = Memory.mkDefaultStruct m.Value false qualifiedTypeName

        let IsTypeNameInitialized qualifiedTypeName state = Memory.typeNameInitialized m.Value qualifiedTypeName state
        let Dump = State.dumpMemory

        let ArrayLength arrayTerm = Arrays.length m.Value arrayTerm
        let ArrayLengthByDimension state arrayRef index = Memory.referenceArrayLength arrayRef index |> Memory.deref m.Value state
        let ArrayLowerBoundByDimension state arrayRef index = Memory.referenceArrayLowerBound arrayRef index |> Memory.deref m.Value state
