namespace VSharp.Core

open FSharpx.Collections
open VSharp

module API =
    let ConfigureSimplifier simplifier =
        Propositional.configureSimplifier simplifier
    let Reset() =
        IdGenerator.reset()
    let SaveConfiguration() =
        IdGenerator.saveConfiguration()
    let Restore() =
        IdGenerator.restore()

    let BranchStatements state condition thenStatement elseStatement k =
        Memory.statedConditionalExecutionWithMergek state condition thenStatement elseStatement k
    let BranchStatementsOnNull state reference thenStatement elseStatement k =
        BranchStatements state (fun state k -> k (Pointers.isNull reference, state)) thenStatement elseStatement k
    let BranchExpressions condition thenBranch elseExpression k =
        Common.statelessConditionalExecutionWithMergek condition thenBranch elseExpression k
    let StatedConditionalExecutionAppendResults (state : state) conditionInvocation (thenBranch : (state -> (state list -> 'a) -> 'a)) elseBranch k =
        Memory.commonStatedConditionalExecutionk state conditionInvocation thenBranch elseBranch (fun x y -> [x;y]) (List.concat >> k)
    let StatedConditionalExecution = Memory.commonStatedConditionalExecutionk

    let GuardedApplyExpressionWithPC pc term f = Merging.guardedApplyWithPC pc f term
    let GuardedApplyExpression term f = Merging.guardedApply f term
    let GuardedStatedApplyStatementK state term f k = Memory.guardedStatedApplyk f state term k
    let GuardedStatedApplyk f state term mergeStates k =
        Memory.commonGuardedStatedApplyk f state term mergeStates k

    let PerformBinaryOperation op left right k = Operators.simplifyBinaryOperation op left right k
    let PerformUnaryOperation op t arg k = Operators.simplifyUnaryOperation op t arg k

    [<AutoOpen>]
    module public Terms =
        let Nop = Nop
        let Concrete obj typ = Concrete obj typ
        let Constant name source typ = Constant name source typ
        let Expression op args typ = Expression op args typ
        let Struct fields typ = Struct fields typ
        let Ref address = Ref address
        let Ptr baseAddress typ offset = Ptr baseAddress typ offset
        let HeapRef address baseType = HeapRef address baseType
        let Union gvs = Union gvs

        let True = True
        let False = False
        let NullRef = nullRef

        let MakeBool b = makeBool b
        let MakeNumber n = makeNumber n

        let TypeOf term = typeOf term
        let rec BaseTypeOfHeapRef state ref =
            match ref.term with
            | HeapRef(addr, _) -> Memory.typeOfHeapLocation state addr
            | Union gvs ->
                let ts = List.map (fun (_, v) -> BaseTypeOfHeapRef state v) gvs
                match ts with
                | [] -> __unreachable__()
                | t::ts ->
                    assert(List.forall ((=)t) ts)
                    t
            | _ -> internalfailf "reading type token: expected heap reference, but got %O" ref

        // TODO: maybe transfer time from interpreter?
        let MakeFunctionResultConstant state (callSite : callSite) =
            Memory.makeFunctionResultConstant state.currentTime callSite

        let isStruct term = isStruct term
        let isReference term = isReference term
        let IsNullReference term = Pointers.isNull term

        let (|True|_|) t = (|True|_|) t
        let (|False|_|) t = (|False|_|) t
        let (|Conjunction|_|) term = Terms.(|Conjunction|_|) term.term
        let (|Disjunction|_|) term = Terms.(|Disjunction|_|) term.term

        let ConstantsOf terms = discoverConstants terms

        let rec HeapReferenceToBoxReference reference =
            match reference.term with
            | HeapRef({term = ConcreteHeapAddress addr}, typ) -> Ref (BoxedLocation(addr, typ))
            | HeapRef _ -> __insufficientInformation__ "Unable to unbox symbolic ref %O" reference
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, HeapReferenceToBoxReference v)) |> Merging.merge
            | _ -> internalfailf "Unboxing: expected heap reference, but got %O" reference

        let WithPathCondition conditionState condition = Memory.withPathCondition conditionState condition

    module Types =
        let Numeric t = Types.Numeric t
        let ObjectType = Types.objectType

        let FromDotNetType (state : state) t = t |> Types.Constructor.fromDotNetType |> Memory.substituteTypeVariables state
        let ToDotNetType t = Types.toDotNetType t

        let SizeOf t = Types.sizeOf t
        let RankOf t = Types.rankOf t

        let TLength = Types.lengthType
        let IsBool t = Types.isBool t
        let IsInteger t = Types.isInteger t
        let IsReal t = Types.isReal t
        let IsPointer t = Types.isPointer t
        let IsValueType t = Types.isValueType t

        let String = Types.String
        let (|StringType|_|) t = Types.(|StringType|_|) t

        let ElementType arrayType = Types.elementType arrayType

        let TypeIsType leftType rightType = TypeCasting.typeIsType leftType rightType
        let TypeIsNullable typ = TypeCasting.isNullable typ
        let rec TypeIsRef typ ref = TypeCasting.typeIsRef typ ref
        let RefIsType ref typ = TypeCasting.refIsType ref typ
        let RefIsRef leftRef rightRef = TypeCasting.refIsRef leftRef rightRef

        let IsCast targetType term = TypeCasting.canCast term targetType
        let Cast term targetType = TypeCasting.cast term targetType
        let CastConcrete value typ = castConcrete value typ
        let CastReferenceToPointer state reference = TypeCasting.castReferenceToPointer state reference

    module public Operators =
        let (!!) x = Propositional.simplifyNegation x id
        let (&&&) x y = Propositional.simplifyAnd x y id
        let (|||) x y = Propositional.simplifyOr x y id
        let (===) x y = Operators.ksimplifyEquality x y id
        let (!==) x y = Operators.ksimplifyEquality x y (!!)
        let conjunction xs = conjunction xs
        let disjunction xs = disjunction xs

    module public Arithmetics =
        let (===) x y = Arithmetics.simplifyEqual x y id
        let (!==) x y = Arithmetics.simplifyNotEqual x y id
        let (<<) x y = Arithmetics.simplifyLess x y id
        let (<<=) x y = Arithmetics.simplifyLessOrEqual x y id
        let (>>) x y = Arithmetics.simplifyGreater x y id
        let (>>=) x y = Arithmetics.simplifyGreaterOrEqual x y id
        let (%%%) x y = Arithmetics.simplifyRemainder (x |> TypeOf |> Types.ToDotNetType) x y id

        let Mul x y = Arithmetics.mul x y
        let IsZero term = Arithmetics.checkEqualZero term id

    module public Memory =
        let EmptyState = Memory.empty

        let PopStack state = Memory.popStack state
        let PopTypeVariables state = Memory.popTypeVariablesSubstitution state
        let NewStackFrame state funcId parametersAndThis isEffect = Memory.newStackFrame state funcId parametersAndThis isEffect
        let NewTypeVariables state subst = Memory.pushTypeVariablesSubstitution state subst

        let rec ReferenceArrayIndex arrayRef indices =
            match arrayRef.term with
            | HeapRef(addr, typ) -> ArrayIndex(addr, indices, symbolicTypeToArrayType typ) |> Ref
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ReferenceArrayIndex v indices)) |> Merging.merge
            | _ -> internalfailf "Referencing array index: expected reference, but got %O" arrayRef
        let rec ReferenceField reference fieldId =
            match reference.term with
            | HeapRef(address, typ) ->
                let dnt = Types.ToDotNetType typ
                let dnt = if dnt.IsGenericType then dnt.GetGenericTypeDefinition() else dnt
                assert(fieldId.declaringType.IsAssignableFrom dnt)
                ClassField(address, fieldId) |> Ref
            | Ref addr ->
                assert(typeOfAddress addr |> Types.isStruct)
                StructField(addr, fieldId) |> Ref
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ReferenceField v fieldId)) |> Merging.merge
            | _ -> internalfailf "Referencing field: expected reference, but got %O" reference

        let ReadSafe state reference = Memory.readSafe state reference
        let ReadLocalVariable state location = Memory.readStackLocation state location
        let ReadThis state methodBase = Memory.readStackLocation state (ThisKey methodBase)
        let ReadArgument state parameterInfo = Memory.readStackLocation state (ParameterKey parameterInfo)
        let ReadField state term field =
            let doRead target =
                match target.term with
                | HeapRef(addr, _) -> Memory.readClassField state addr field
                | Struct _ -> Memory.readStruct target field
                | Ref addr -> StructField(addr, field) |> Ref |> Memory.readSafe state
                | _ -> internalfailf "Reading field of %O" term
            Merging.guardedApply doRead term

        let rec ReadArrayIndex state reference indices =
            match reference.term with
            | HeapRef(addr, typ) ->
                let (_, dim, _) as arrayType = symbolicTypeToArrayType typ
                assert(dim = List.length indices)
                Memory.readArrayIndex state addr indices arrayType
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ReadArrayIndex state v indices)) |> Merging.merge
            | _ -> internalfailf "Reading field of class: expected reference, but got %O" reference
        let ReadStaticField state typ field = Memory.readStaticField state typ field
        let ReadDelegate state reference = Memory.readDelegate state reference

        let WriteLocalVariable state location value = Memory.writeStackLocation state location value
        let WriteSafe state reference value = Memory.writeSafe state reference value
        let WriteStructField structure field value = Memory.writeStruct structure field value
        let rec WriteClassField state reference field value =
            Memory.guardedStatedMap
                (fun state reference ->
                    match reference.term with
                    | HeapRef(addr, _) -> Memory.writeClassField state addr field value
                    | _ -> internalfailf "Writing field of class: expected reference, but got %O" reference)
                state reference
        let rec WriteArrayIndex state reference indices value =
            Memory.guardedStatedMap
                (fun state reference ->
                    match reference.term with
                    | HeapRef(addr, typ) ->
                        let (_, dim, _) as arrayType = symbolicTypeToArrayType typ
                        assert(dim = List.length indices)
                        Memory.writeArrayIndex state addr indices arrayType value
                    | _ -> internalfailf "Writing field of class: expected reference, but got %O" reference)
                state reference
        let WriteStaticField state typ field value = Memory.writeStaticField state typ field value

        let DefaultOf typ = makeDefaultValue typ
        let AllocateOnStack state key term = Memory.allocateOnStack state key term
        let AllocateTemporaryLocalVariable state typ term =
            let tmpKey = TemporaryLocalVariableKey typ
            let ref = PrimitiveStackLocation tmpKey |> Ref
            ref, Memory.allocateOnStack state tmpKey term

        let MakeSymbolicThis m = Memory.makeSymbolicThis m

        let BoxValueType state term =
            let address, state = Memory.freshAddress state
            let reference = HeapRef (ConcreteHeapAddress address) (typeOf term)
            reference, Memory.writeBoxedLocation state address term

        let AllocateDefaultStatic state targetType =
            Memory.initializeStaticMembers state targetType

        let AllocateDefaultClass state typ =
            Memory.allocateClass state typ

        let AllocateDefaultArray state dimensions typ =
            let address, state = Memory.allocateArray state typ None dimensions
            HeapRef address typ, state

        let AllocateDelegate state delegateTerm =
            Memory.allocateDelegate state delegateTerm

        let AllocateString string state = Memory.allocateString state string

        let IsTypeInitialized state typ = Memory.isTypeInitialized state typ
        let Dump state = Memory.dump state

        let rec ArrayRank state arrayRef =
            match arrayRef.term with
            | HeapRef(addr, _) -> Memory.typeOfHeapLocation state addr |> Types.rankOf |> makeNumber
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ArrayRank state v)) |> Merging.merge
            | _ -> internalfailf "Getting rank of array: expected ref, but got %O" arrayRef
        let rec ArrayLengthByDimension state arrayRef index =
            match arrayRef.term with
            | HeapRef(addr, _) -> Memory.readLength state addr index (symbolicTypeToArrayType (Memory.typeOfHeapLocation state addr))
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ArrayLengthByDimension state v index)) |> Merging.merge
            | _ -> internalfailf "reading array length: expected heap reference, but got %O" arrayRef
        let rec ArrayLowerBoundByDimension state arrayRef index =
            match arrayRef.term with
            | HeapRef(addr, _) -> Memory.readLowerBound state addr index (symbolicTypeToArrayType (Memory.typeOfHeapLocation state addr))
            | Union gvs -> gvs |> List.map (fun (g, v) -> (g, ArrayLowerBoundByDimension state v index)) |> Merging.merge
            | _ -> internalfailf "reading array lower bound: expected heap reference, but got %O" arrayRef

        let StringLength state strRef = Memory.lengthOfString state strRef
        let rec StringCtorOfCharArray state arrayRef dstRef =
            match dstRef.term with
            | HeapRef({term = ConcreteHeapAddress dstAddr}, typ) ->
                assert(typ = Types.String)
                Memory.guardedStatedMap (fun state arrayRef ->
                    match arrayRef.term with
                    | HeapRef(arrayAddr, typ) ->
                        assert(typ = ArrayType(Types.Char, Vector))
                        Memory.copyCharArrayToString state arrayAddr dstAddr
                    | _ -> internalfailf "constructing string from char array: expected array reference, but got %O" arrayRef) state arrayRef
            | HeapRef _
            | Union _ -> __notImplemented__()
            | _ -> internalfailf "constructing string from char array: expected string reference, but got %O" dstRef

//        let ThrowException state typ = __notImplemented__() //Memory.allocateException m.Value state typ

        let ComposeStates state state1 k = Memory.composeStates state state1 |> k

        let Merge2States (state1 : state) (state2 : state) =
            let pc1 = PC.squashPC state1.pc
            let pc2 = PC.squashPC state2.pc
            if pc1 = Terms.True && pc2 = Terms.True then __unreachable__()
            __notImplemented__() : state
            //Merging.merge2States pc1 pc2 {state1 with pc = []} {state2 with pc = []}

    module Options =
        let HandleNativeInt f g = Options.HandleNativeInt f g
