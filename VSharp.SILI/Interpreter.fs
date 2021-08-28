namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections
open InstructionsSet
open CilStateOperations
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open ipOperations
open Instruction

type cfg = CFG.cfgData

type public MethodInterpreter(maxBound, forwardSearcher : IForwardSearcher (*ilInterpreter : ILInterpreter, funcId : IFunctionIdentifier, cfg : cfg*)) as this =
    inherit ExplorerBase()
    let interpreter = ILInterpreter(this)
    member x.GetResults initialState =
        let (|CilStateWithIIE|_|) (cilState : cilState) = cilState.iie
        let isStartingDescender (s : cilState) = s.startingIP = initialState.startingIP
        let allStates = List.filter isStartingDescender (forwardSearcher.FinishedStates() |> List.ofSeq)
        let iieStates, nonIIEstates = List.partition isIIEState allStates
        let isFinished (s : cilState) = s.ipStack = [Exit <| methodOf initialState.startingIP]
        let finishedStates = List.filter isFinished nonIIEstates
        let isValid (cilState : cilState) =
           match IsValid cilState.state with
           | SolverInteraction.SmtUnsat _ -> false
           | _ -> true
        let validStates = List.filter isValid finishedStates
        let printInfoForDebug () =
            let allStatesInQueue = forwardSearcher.FinishedStates() |> List.ofSeq
            Logger.info "No states were obtained. Most likely such a situation is a bug. Check it!"
            Logger.info "Indexed queue size = %d\n" (List.length allStatesInQueue)
            List.iteri (fun i -> dump >> Logger.info "Queue.[%d]:\n%s\n" i) allStatesInQueue
            true
        match iieStates with // TODO: write error states? #do
        | CilStateWithIIE iie :: _ -> raise iie
        | _ :: _ -> __unreachable__()
        | _ when validStates = [] ->
            assert(printInfoForDebug())
            internalfailf "No states were obtained. Most likely such a situation is a bug. Check it!"
        | _ -> validStates

    member x.Interpret (main : MethodBase) (initialState : cilState) =
        forwardSearcher.Init main Seq.empty
        while forwardSearcher.ShouldWork() do
            let s = forwardSearcher.Pick()
            let goodStates, incompleteStates, errors = interpreter.ExecuteOneInstruction s
            let allStates = goodStates @ incompleteStates @ errors |> List.toSeq
            forwardSearcher.Update s allStates
        x.GetResults initialState

    override x.Invoke method initialState k =
        let k x = forwardSearcher.Reset(); k x
        let cilStates = x.InitializeStatics initialState method.DeclaringType List.singleton
        assert(List.length cilStates = 1)
        let cilState = List.head cilStates
        let results = x.Interpret method cilState
        let printResults (cilStates : cilState list) =
            let states = List.fold (fun acc (cilState : cilState) -> acc + Print.Dump cilState.state + "\n") "" cilStates
            let fullMethodName = Reflection.getFullMethodName method
            Logger.info "For method %O got %i states :\n%s" fullMethodName (List.length cilStates) states
        printResults results
        k results

    override x.StepInstruction state =
        // TODO: schedule instruction, send response after its execution to client machine.
        // TODO: the response is a priority-sorted list of cilStates, client machine chooses the first one
        //       it can own.
        // TODO: also, filter out incomplete states (error too?)
        interpreter.MakeStep state

and public ILInterpreter(methodInterpreter : ExplorerBase) as this =

    let cilStateImplementations : Map<string, (cilState -> term option -> term list -> cilState list)> =
        Map.ofList [
            "System.Int32 System.Array.GetLength(this, System.Int32)", this.CommonGetArrayLength
            "System.Int32 System.Array.GetLowerBound(this, System.Int32)", this.GetArrayLowerBound
            "System.Void System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)", this.CommonInitializeArray
            "System.Void System.String.FillStringChecked(System.String, System.Int32, System.String)", this.FillStringChecked
            "System.Void System.Array.Clear(System.Array, System.Int32, System.Int32)", this.ClearArray
            "System.Void System.Array.Copy(System.Array, System.Int32, System.Array, System.Int32, System.Int32, System.Boolean)", this.CopyArrayExtendedForm
            "System.Void System.Array.Copy(System.Array, System.Array, System.Int32)", this.CopyArrayShortForm
        ]

    member private x.Raise createException (cilState : cilState) k =
        let statesWithCreatedExceptions = createException cilState
        k statesWithCreatedExceptions

    member private x.AccessMultidimensionalArray accessor (cilState : cilState) upperBounds indices (k : cilState list -> 'a) =
        let checkArrayBounds upperBounds indices =
            let checkOneBound acc (upperBound, index) =
                let lowerBound = Concrete 0 Types.TLength
                let notTooSmall = Arithmetics.(>>=) index lowerBound
                let notTooLarge = Arithmetics.(<<) index upperBound
                acc &&& notTooSmall &&& notTooLarge
            assert(List.length upperBounds = List.length indices)
            let upperBoundsAndIndices = List.zip upperBounds indices
            List.fold checkOneBound True upperBoundsAndIndices
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (checkArrayBounds upperBounds indices, state))
            accessor
            (x.Raise x.IndexOutOfRangeException)
            k

    member private x.AccessArray accessor (cilState : cilState) upperBound index k =
        x.AccessMultidimensionalArray accessor cilState [upperBound] [index] k

    member private x.AccessArrayDimension accessor (cilState : cilState) (this : term) (dimension : term) =
        let upperBound = Memory.ArrayRank cilState.state this
        x.AccessArray (accessor this dimension) cilState upperBound dimension id

    member private x.CommonGetArrayLength (cilState : cilState) thisOption args =
        match args with
        | [dimensionsKey] ->
            let arrayLengthByDimension arrayRef index cilState (k : cilState list -> 'a) =
                push (Memory.ArrayLengthByDimension cilState.state arrayRef index) cilState
                k [cilState]
            x.AccessArrayDimension arrayLengthByDimension cilState (Option.get thisOption) dimensionsKey
        | _ -> internalfail "unexpected number of arguments"

    member private x.GetArrayLowerBound (cilState : cilState) (this : term option) args =
        match args with
        | [dimension] ->
            let arrayLowerBoundByDimension arrayRef index (cilState : cilState) k =
                push (Memory.ArrayLowerBoundByDimension cilState.state arrayRef index) cilState
                k [cilState]
            x.AccessArrayDimension arrayLowerBoundByDimension cilState (Option.get this) dimension
        | _ -> internalfail "unexpected number of arguments"

    member private x.NpeOrInvokeStatementCIL (cilState : cilState) (this : term) statement (k : cilState list -> 'a) =
         StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference this, state))
            (x.Raise x.NullReferenceException)
            statement
            k

    member private x.CommonInitializeArray (cilState : cilState) _ (args : term list) =
        match args with
        | [arrayRef; handleTerm] ->
            x.NpeOrInvokeStatementCIL cilState arrayRef (fun cilState k ->
            x.NpeOrInvokeStatementCIL cilState handleTerm (fun cilState k ->
            let results : state list = VSharp.System.Runtime_CompilerServices_RuntimeHelpers.InitializeArray cilState.state arrayRef handleTerm
            let cilResults = List.map (changeState cilState) results
            k cilResults) k) id
        | _ -> internalfail "unexpected number of arguments"

    member private x.FillStringChecked (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let state = cilState.state
        let dest, destPos, src = args.[0], args.[1], args.[2]
        let srcPos = MakeNumber 0
        let srcLength = Memory.StringLength state src
        let destLength = Memory.StringLength state dest
        let (<<=) = Arithmetics.(<<=)
        let check = srcLength <<= (Arithmetics.Sub destLength destPos)
        let copy (cilState : cilState) k =
            Memory.CopyStringArray cilState.state src srcPos dest destPos srcLength
            k [cilState]
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (check, state))
            copy
            (x.Raise x.IndexOutOfRangeException)
            id

    member private x.ClearArray (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let array, index, length = args.[0], args.[1], args.[2]
        let (>>) = API.Arithmetics.(>>)
        let (<<) = API.Arithmetics.(<<)
        let clearCase (cilState : cilState) k =
            Memory.ClearArray cilState.state array index length
            k [cilState]
        let nonNullCase (cilState : cilState) k =
            let zero = MakeNumber 0
            let lb = Memory.ArrayLowerBoundByDimension cilState.state array zero
            let numOfAllElements = Memory.CountOfArrayElements cilState.state array
            let check = index << lb ||| (Arithmetics.Add index length) >> numOfAllElements ||| length << zero
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (check, state))
                (x.Raise x.IndexOutOfRangeException)
                clearCase
                k
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (IsNullReference array, state))
            (x.Raise x.ArgumentNullException)
            nonNullCase
            id

    member private x.CommonCopyArray (cilState : cilState) src srcIndex dst dstIndex length =
        let state = cilState.state
        let srcType = MostConcreteTypeOfHeapRef state src
        let dstType = MostConcreteTypeOfHeapRef state dst
        let (>>) = API.Arithmetics.(>>)
        let (<<) = API.Arithmetics.(<<)
        let add = Arithmetics.Add
        let zero = TypeUtils.Int32.Zero
        let srcLB = Memory.ArrayLowerBoundByDimension state src zero
        let dstLB = Memory.ArrayLowerBoundByDimension state dst zero
        let srcNumOfAllElements = Memory.CountOfArrayElements state src
        let dstNumOfAllElements = Memory.CountOfArrayElements state dst
        let defaultCase (cilState : cilState) k =
            Memory.CopyArray cilState.state src srcIndex srcType dst dstIndex dstType length
            k [cilState]
        let lengthCheck (cilState : cilState) =
            let check = ((add srcIndex length) >> srcNumOfAllElements) ||| ((add dstIndex length) >> dstNumOfAllElements)
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (check, state))
                (x.Raise x.ArgumentException)
                defaultCase
        let indicesCheck (cilState : cilState) =
            let primitiveLengthCheck = (length << zero) ||| (length >> TypeUtils.Int32.MaxValue)
            let srcIndexCheck = (srcIndex << srcLB) ||| (srcIndex >> srcNumOfAllElements)
            let dstIndexCheck = (dstIndex << dstLB) ||| (dstIndex >> dstNumOfAllElements)
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (primitiveLengthCheck ||| srcIndexCheck ||| dstIndexCheck, state))
                (x.Raise x.ArgumentOutOfRangeException)
                lengthCheck
        let assignableCheck (cilState : cilState) =
            let srcElemType = Types.ElementType srcType
            let dstElemType = Types.ElementType dstType
            let condition =
                if Types.IsValueType srcElemType then True
                else Types.TypeIsType srcElemType dstElemType
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (condition, state))
                indicesCheck
                (x.Raise x.InvalidCastException)
        let rankCheck (cilState : cilState) =
            if Types.RankOf srcType = Types.RankOf dstType then assignableCheck cilState
            else x.Raise x.RankException cilState
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (IsNullReference src ||| IsNullReference dst, state))
            (x.Raise x.ArgumentNullException)
            rankCheck
            id

    member private x.CopyArrayExtendedForm (cilState : cilState) _ (args : term list) =
        assert(List.length args = 6)
        let src, srcIndex, dst, dstIndex, length = args.[0], args.[1], args.[2], args.[3], args.[4]
        x.CommonCopyArray cilState src srcIndex dst dstIndex length

    member private x.CopyArrayShortForm (cilState : cilState) _ (args : term list) =
        assert(List.length args = 3)
        let src, dst, length = args.[0], args.[1], args.[2]
        let state = cilState.state
        let zero = TypeUtils.Int32.Zero
        let srcLB = Memory.ArrayLowerBoundByDimension state src zero
        let dstLB = Memory.ArrayLowerBoundByDimension state src zero
        x.CommonCopyArray cilState src srcLB dst dstLB length

    // TODO: make faster #do
    member private x.TrustedIntrinsics =
        let intPtr = Reflection.getAllMethods typeof<IntPtr> |> Array.map Reflection.getFullMethodName
        let volatile = Reflection.getAllMethods typeof<System.Threading.Volatile> |> Array.map Reflection.getFullMethodName
//        let comparerType = typeof<System.Collections.Generic.Comparer<obj>>.GetGenericTypeDefinition()
//        let defaultComparer = Reflection.getAllMethods comparerType |> Array.map (fun mi -> mi :> MethodBase)
        let defaultComparer = [|"System.Collections.Generic.Comparer`1[T] System.Collections.Generic.Comparer`1[T].get_Default()"|];
        Array.concat [intPtr; volatile; defaultComparer]

    member private x.IsNotImplementedIntrinsic (methodBase : MethodBase) fullMethodName =
        let isIntrinsic =
            let intrinsicAttr = "System.Runtime.CompilerServices.IntrinsicAttribute"
            methodBase.CustomAttributes |> Seq.exists (fun m -> m.AttributeType.ToString() = intrinsicAttr)
        isIntrinsic && (Array.contains fullMethodName x.TrustedIntrinsics |> not)

    member private x.IsExternalMethod (methodBase : MethodBase) =
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        let isInternalCall = methodBase.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall
        let isPInvokeImpl = methodBase.Attributes.HasFlag(MethodAttributes.PinvokeImpl)
        int isInternalCall <> 0 || isPInvokeImpl

    member private x.InstantiateThisIfNeed state thisOption (methodBase : MethodBase) =
        match thisOption with
        | Some this ->
            let thisType = TypeOf this
            if Types.IsValueType thisType && methodBase.IsConstructor then
                let newThis = Memory.DefaultOf thisType
                let states = Memory.WriteSafe state this newThis
                assert(List.length states = 1 && LanguagePrimitives.PhysicalEquality state (List.head states))
        | None -> ()

    member private x.GetFullMethodNameArgsAndThis state (methodBase : MethodBase) =
        let fullyGenericMethod, genericArgs, _ = Reflection.generalizeMethodBase methodBase
        let fullGenericMethodName = Reflection.getFullMethodName fullyGenericMethod
        let wrapType arg = Concrete arg (Types.FromDotNetType typeof<Type>)
        let typeArgs = genericArgs |> Seq.map wrapType |> List.ofSeq
        let termArgs = methodBase.GetParameters() |> Seq.map (Memory.ReadArgument state) |> List.ofSeq
        let args = typeArgs @ termArgs
        let thisOption = if methodBase.IsStatic then None else Some <| Memory.ReadThis state methodBase
        x.InstantiateThisIfNeed state thisOption methodBase
        fullGenericMethodName, args, thisOption

    member private x.InvokeCSharpImplementation (cilState : cilState) fullMethodName thisOption args =
        // TODO: check that all parameters were specified
        let methodInfo = Loader.CSharpImplementations.[fullMethodName]
        let thisOption, args =
            match thisOption, methodInfo.IsStatic with
            | Some this, true -> None, this :: args
            | None, false -> internalfail "Calling non-static concrete implementation for static method"
            | _ -> thisOption, args
        Memory.PopFrame cilState.state
        ExplorerBase.InitFunctionFrame cilState.state methodInfo thisOption (Some args)
        methodInterpreter.InitializeStatics cilState methodInfo.DeclaringType (fun cilState ->
            setCurrentIp (instruction methodInfo 0) cilState
            [cilState])

    member private x.IsArrayGetOrSet (methodBase : MethodBase) =
        let name = methodBase.Name
        (name = "Set" || name = "Get") && typeof<Array>.IsAssignableFrom(methodBase.DeclaringType)

    member private x.InvokeArrayGetOrSet (cilState : cilState) (methodBase : MethodBase) thisOption args =
        let name = methodBase.Name
        match thisOption with
        | Some arrayRef when name = "Get" ->
            let cast value state =
                let typ = Reflection.getMethodReturnType methodBase |> Types.FromDotNetType
                castUnchecked typ value state
            x.LdElemCommon cast cilState arrayRef args
        | Some arrayRef when name = "Set" ->
            let value, indices = List.lastAndRest args
            x.StElemCommon cilState arrayRef indices value
        | _ -> __unreachable__()

    member private x.InlineMethodBaseCallIfNeeded (methodBase : MethodBase) (cilState : cilState) k =
        // [NOTE] Asserting correspondence between ips and frames
        assert(currentMethod cilState = methodBase && currentOffset cilState = Some 0)
        let fullMethodName, args, thisOption = x.GetFullMethodNameArgsAndThis cilState.state methodBase
        let moveIpToExit (cilState : cilState) =
            // [NOTE] else current method non method
            if currentMethod cilState = methodBase then
                setCurrentIp (Exit methodBase) cilState
        if Map.containsKey fullMethodName cilStateImplementations then
            let states = cilStateImplementations.[fullMethodName] cilState thisOption args
            List.iter moveIpToExit states
            k states
        elif Map.containsKey fullMethodName Loader.FSharpImplementations then
            let thisAndArguments = optCons args thisOption
            let moveIp states =
                moveIpToExit cilState
                List.map (changeState cilState) states |> k
            internalCall Loader.FSharpImplementations.[fullMethodName] thisAndArguments cilState.state moveIp
        elif Map.containsKey fullMethodName Loader.CSharpImplementations then
            x.InvokeCSharpImplementation cilState fullMethodName thisOption args |> k
        elif x.IsArrayGetOrSet methodBase then
            let cilStates = x.InvokeArrayGetOrSet cilState methodBase thisOption args
            List.iter moveIpToExit cilStates
            k cilStates
        elif x.IsExternalMethod methodBase then internalfailf "new extern method: %s" fullMethodName
        elif x.IsNotImplementedIntrinsic methodBase fullMethodName then internalfailf "new intrinsic method: %s" fullMethodName
        elif methodBase.GetMethodBody() <> null then cilState |> List.singleton |> k
        else internalfailf "non-extern method %s without body!" (Reflection.getFullMethodName methodBase)

    member private x.ArrayMethods (arrayType : Type) =
        let methodsFromHelper = Type.GetType("System.SZArrayHelper") |> Reflection.getAllMethods
        let makeSuitable (m : MethodInfo) =
            if m.IsGenericMethod then m.MakeGenericMethod(arrayType.GetElementType()) else m
        let concreteMethods = Array.map makeSuitable methodsFromHelper
        Array.concat [concreteMethods; Reflection.getAllMethods typeof<Array>; Reflection.getAllMethods arrayType]

    member private x.FindSuitableForInterfaceMethod (targetType : Type) (method : MethodInfo) =
        let interfaceType = method.DeclaringType
        assert(interfaceType.IsInterface)
        let createSignature (m : MethodInfo) =
            m.GetParameters()
            |> Seq.map (fun p -> p.ParameterType |> Reflection.getFullTypeName)
            |> join ","
        let onlyLastName (m : MethodInfo) =
            match m.Name.LastIndexOf('.') with
            | i when i < 0 -> m.Name
            | i -> m.Name.Substring(i + 1)
        let sign = createSignature method
        let lastName = onlyLastName method
        let methods =
            match targetType with
            | _ when targetType.IsArray -> x.ArrayMethods targetType
            | _ -> targetType.GetInterfaceMap(interfaceType).TargetMethods
        methods |> Seq.find (fun mi -> createSignature mi = sign && onlyLastName mi = lastName)

    member private x.InvokeVirtualMethod (cilState : cilState) calledMethod targetMethod k =
        // Getting this and arguments values by old keys
        let this = Memory.ReadThis cilState.state calledMethod
        let args = calledMethod.GetParameters() |> Seq.map (Memory.ReadArgument cilState.state) |> List.ofSeq
        // Popping frame created for ancestor calledMethod
        popFrameOf cilState
        // Creating valid frame with stackKeys corresponding to actual targetMethod
        methodInterpreter.InitFunctionFrameCIL cilState targetMethod (Some this) (Some args)
        x.InlineMethodBaseCallIfNeeded targetMethod cilState k

    member x.CallVirtualMethodFromTermType (cilState : cilState) termType (calledMethod : MethodInfo) k =
        let targetType = termType |> Types.ToDotNetType
        let genericCalledMethod = if calledMethod.IsGenericMethod then calledMethod.GetGenericMethodDefinition() else calledMethod
        let genericMethodInfo =
            match genericCalledMethod.DeclaringType with
            | i when i.IsInterface -> x.FindSuitableForInterfaceMethod targetType genericCalledMethod
            | _ ->
                let allMethods = Reflection.getAllMethods targetType
                allMethods |> Seq.find (fun mi -> mi.GetBaseDefinition() = genericCalledMethod.GetBaseDefinition())
        let targetMethod =
            if genericMethodInfo.IsGenericMethodDefinition then
                genericMethodInfo.MakeGenericMethod(calledMethod.GetGenericArguments())
            else genericMethodInfo
        if targetMethod.IsAbstract
            then x.CallAbstract targetMethod cilState k
            else x.InvokeVirtualMethod cilState calledMethod targetMethod k

    member x.CallVirtualMethod (ancestorMethod : MethodInfo) (cilState : cilState) (k : cilState list -> 'a) =
        let this = Memory.ReadThis cilState.state ancestorMethod
        let callVirtual (cilState : cilState) this k =
            let baseType = MostConcreteTypeOfHeapRef cilState.state this
            let callForConcreteType typ state k =
                x.CallVirtualMethodFromTermType state typ ancestorMethod k
            let tryToCallForBaseType (cilState : cilState) (k : cilState list -> 'a) =
                StatedConditionalExecutionAppendResultsCIL cilState
                    (fun state k -> k (API.Types.TypeIsRef state baseType this, state))
                    (callForConcreteType baseType)
                    (x.CallAbstract ancestorMethod)
                    k
            let baseDotNetType = Types.ToDotNetType baseType
            if baseDotNetType.IsInterface
                then x.CallAbstract ancestorMethod cilState k
                else tryToCallForBaseType cilState k
        GuardedApplyCIL cilState this callVirtual k

    member x.CallAbstract method cilState k =
        methodInterpreter.CallAbstractMethod method cilState k

    member private x.ConvOvf targetType (cilState : cilState) =
        let supersetsOf =
            PersistentDict.ofSeq [
                TypeUtils.int8Type,    [|TypeUtils.int8Type; TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int16Type,   [|TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int32Type,   [|TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int64Type,   [|TypeUtils.int64Type|]
                TypeUtils.uint8Type,   [|TypeUtils.uint8Type; TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint16Type,  [|TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint32Type,  [|TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint64Type,  [|TypeUtils.uint64Type|]
                TypeUtils.float32Type, [|TypeUtils.float32Type; TypeUtils.float64Type|]
                TypeUtils.float64Type, [|TypeUtils.float64Type|] ]
        let isSubset leftTyp rightTyp = Array.contains rightTyp supersetsOf.[leftTyp]
        let minMaxOf = // TODO: implement big numbers, instead of double #hack
            PersistentDict.ofSeq [
                TypeUtils.int8Type,    (SByte.MinValue  |> double, SByte.MaxValue  |> double)
                TypeUtils.int16Type,   (Int16.MinValue  |> double, Int16.MaxValue  |> double)
                TypeUtils.int32Type,   (Int32.MinValue  |> double, Int32.MaxValue  |> double)
                TypeUtils.int64Type,   (Int64.MinValue  |> double, Int64.MaxValue  |> double)
                TypeUtils.uint8Type,   (Byte.MinValue   |> double, Byte.MaxValue   |> double)
                TypeUtils.uint16Type,  (UInt16.MinValue |> double, UInt16.MaxValue |> double)
                TypeUtils.uint32Type,  (UInt32.MinValue |> double, UInt32.MaxValue |> double)
                TypeUtils.uint64Type,  (UInt64.MinValue |> double, UInt64.MaxValue |> double)
                TypeUtils.float32Type, (Single.MinValue |> double, Single.MaxValue |> double)
                TypeUtils.float64Type, (Double.MinValue |> double, Double.MaxValue |> double) ]
        let getSegment leftTyp rightTyp =
            let min1, max1 = minMaxOf.[leftTyp]
            let min2, max2 = minMaxOf.[rightTyp]
            match min1 < min2, max1 < max2 with
            | true, true   -> min2, max1
            | true, false  -> min2, max2
            | false, true  -> min1, max1
            | false, false -> min1, max2
        let canCastWithoutOverflow term targetTermType =
            let (<<=) = API.Arithmetics.(<<=)
            assert(Terms.TypeOf term |> Types.IsNumeric)
            let termType = Terms.TypeOf term
            if isSubset termType targetTermType then True
            elif termType = TypeUtils.int64Type && targetTermType = TypeUtils.uint64Type then
                let int64Zero = MakeNumber (0 |> int64)
                int64Zero <<= term
            elif termType = TypeUtils.uint64Type && targetTermType = TypeUtils.int64Type then
                let uint64RightBorder = MakeNumber (System.Int64.MaxValue |> uint64)
                term <<= uint64RightBorder
            else
                let min, max = getSegment termType targetTermType
                let leftBorder  = Concrete min termType // must save type info, because min is int64
                let rightBorder = Concrete max termType // must save type info, because max is int64
                (leftBorder <<= term) &&& (term <<= rightBorder)
        let t = pop cilState
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (canCastWithoutOverflow t targetType, state))
            (fun cilState k ->
                let castedResult = Types.Cast t targetType
                push castedResult cilState
                k [cilState])
            (x.Raise x.OverflowException)
            id

    member private x.ConvOvfUn unsignedSightType targetType (cilState : cilState) =
        let t = pop cilState
        let unsignedT = castUnchecked unsignedSightType t cilState.state
        push unsignedT cilState
        x.ConvOvf targetType cilState

    member private x.CommonCastClass (cilState : cilState) (term : term) (typ : symbolicType) k =
        let term = castReferenceToPointerIfNeeded term typ cilState.state
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (IsNullReference term ||| Types.IsCast state term typ, state))
            (fun cilState k ->
                push (Types.Cast term typ) cilState
                k [cilState])
            (x.Raise x.InvalidCastException)
            k
    member private x.CastClass (cfg : cfg) offset (cilState : cilState) : cilState list =
        let term = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Castclass.Size)
        x.CommonCastClass cilState term typ id

    member private x.PushNewObjResultOnEvaluationStack (cilState : cilState) reference (calledMethod : MethodBase) =
        let valueOnStack =
            if calledMethod.DeclaringType.IsValueType then
                  Memory.ReadSafe cilState.state reference
            else reference
        push valueOnStack cilState

    member x.CommonCall (calledMethodBase : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let call cilState k = x.InlineMethodBaseCallIfNeeded calledMethodBase cilState k
        match calledMethodBase.IsStatic with
        | true -> call cilState k
        | false ->
            let this = Memory.ReadThis cilState.state calledMethodBase
            x.NpeOrInvokeStatementCIL cilState this call k
    member x.RetrieveCalledMethodAndArgs (opCode : OpCode) (calledMethodBase : MethodBase) (cilState : cilState) =
        let args = retrieveActualParameters calledMethodBase cilState
        let hasThis = not (calledMethodBase.IsStatic || opCode = OpCodes.Newobj)
        let this = if hasThis then pop cilState |> Some else None
        this, args

    member x.Call (cfg : cfg) offset (cilState : cilState) =
        let calledMethod = resolveMethodFromMetadata cfg (offset + OpCodes.Call.Size)
        methodInterpreter.InitializeStatics cilState calledMethod.DeclaringType (fun cilState ->
        let this, args = x.RetrieveCalledMethodAndArgs OpCodes.Call calledMethod cilState
        methodInterpreter.InitFunctionFrameCIL cilState calledMethod this (Some args)
        x.CommonCall calledMethod cilState id)
    member x.CommonCallVirt (ancestorMethodBase : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let this = Memory.ReadThis cilState.state ancestorMethodBase
        let call (cilState : cilState) k =
            if ancestorMethodBase.IsVirtual && not ancestorMethodBase.IsFinal then
                let methodInfo = ancestorMethodBase :?> MethodInfo
                x.CallVirtualMethod methodInfo cilState k
            else
                x.InlineMethodBaseCallIfNeeded ancestorMethodBase cilState k
        x.NpeOrInvokeStatementCIL cilState this call k
    member x.CallVirt (cfg : cfg) offset (cilState : cilState) =
        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? Tuple<MethodInfo, term> as tuple, _) -> snd tuple, (fst tuple :> MethodBase)
            | _ -> __unreachable__()
        let ancestorMethod = resolveMethodFromMetadata cfg (offset + OpCodes.Call.Size)
        let thisOption, args = x.RetrieveCalledMethodAndArgs OpCodes.Callvirt ancestorMethod cilState
        let this, methodToCall =
            match thisOption with
            | Some this when Reflection.isDelegate ancestorMethod && this <> NullRef ->
                let deleg = Memory.ReadDelegate cilState.state this
                let target, mi = retrieveMethodInfo deleg
                // [NOTE] target is ref to closure: when we have it, 'this' = target, otherwise 'this' = thisOption
                if target = NullRef then thisOption, mi else Some target, mi
            | _ -> thisOption, ancestorMethod
        // NOTE: there is no need to initialize statics, because they were initialized before ``newobj'' execution
        // NOTE: It is not quite strict to InitFunctionFrame here because, but it does not matter because signatures of virtual methods are the same
        methodInterpreter.InitializeStatics cilState methodToCall.DeclaringType (fun cilState ->
        // [NOTE] If DeclaringType has static constructor, InitializeStatics will add new state to queue.
        //        But arguments and this was already popped, so when execution will return to callvirt,
        //        evaluation stack won't contain arguments and this.
        //        For this purpose initializing statics on cilState with this and arguments,
        //        after that popping them again.
///// TODO: can we pop args BEFORE calling static constructor? if yes, remove the comment above. If not, this code should be overwritten
//        let _, _ = x.RetrieveCalledMethodAndArgs OpCodes.Callvirt ancestorMethod cilState
        methodInterpreter.InitFunctionFrameCIL cilState methodToCall this (Some args)
        x.CommonCallVirt methodToCall cilState id)
    member x.ReduceArrayCreation (arrayType : System.Type) (cilState : cilState) (parameters : term list) k =
        let arrayTyp = Types.FromDotNetType arrayType
        Memory.AllocateDefaultArray cilState.state parameters arrayTyp |> k
    member x.CommonCreateDelegate (ctor : ConstructorInfo) (cilState : cilState) (args : term list) k =
        let target, methodPtr =
            assert(List.length args = 2)
            args.[0], args.[1]
        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? MethodInfo as mi, _) -> mi
            | _ -> __unreachable__()
        let typ = Types.FromDotNetType ctor.DeclaringType
        let lambda = Lambdas.make (retrieveMethodInfo methodPtr, target) typ
        Memory.AllocateDelegate cilState.state lambda |> k

    member x.CommonNewObj isCallNeeded (constructorInfo : ConstructorInfo) (cilState : cilState) (args : term list) (k : cilState list -> 'a) : 'a =
        __notImplemented__()
//        let typ = constructorInfo.DeclaringType
//        let constructedTermType = typ |> Types.FromDotNetType cilState.state
//        let blockCase (cilState : cilState) =
//            let callConstructor (cilState : cilState) reference afterCall =
//                if isCallNeeded then
//                    methodInterpreter.ReduceFunctionSignatureCIL cilState constructorInfo (Some reference) (Specified args) false (fun cilState ->
//                    x.InlineMethodBaseCallIfNeeded constructorInfo cilState afterCall)
//                else withResultState reference cilState.state |> changeState cilState |> List.singleton
//            let referenceTypeCase (cilState : cilState) =
//                let ref, state = Memory.AllocateDefaultClass cilState.state constructedTermType
//                callConstructor (withState state cilState) ref (List.map (pushToOpStack ref))
//            let valueTypeCase (cilState : cilState) =
//                let freshValue = Memory.DefaultOf constructedTermType
//                let ref, state = Memory.AllocateTemporaryLocalVariable cilState.state typ freshValue
//                let modifyResult (cilState : cilState) =
//                    let value = Memory.ReadSafe cilState.state ref
//                    pushToOpStack value cilState
//                callConstructor (withState state cilState) ref (List.map modifyResult)
//            if Types.IsValueType constructedTermType then valueTypeCase cilState
//            else referenceTypeCase cilState
//        let nonDelegateCase (cilState : cilState) =
//            if typ.IsArray && constructorInfo.GetMethodBody() = null
//                then x.ReduceArrayCreation typ cilState args id
//                else blockCase cilState
//        if Reflection.IsDelegateConstructor constructorInfo
//            then x.CommonCreateDelegate constructorInfo cilState args k
//            else nonDelegateCase cilState |> k

    member x.NewObj (cfg : cfg) offset (cilState : cilState) =
        let calledMethod = resolveMethodFromMetadata cfg (offset + OpCodes.Newobj.Size)
        assert calledMethod.IsConstructor
        let constructorInfo = calledMethod :?> ConstructorInfo
        let typ = constructorInfo.DeclaringType
        methodInterpreter.InitializeStatics cilState constructorInfo.DeclaringType (fun cilState ->
        let this, args = x.RetrieveCalledMethodAndArgs OpCodes.Newobj calledMethod cilState
        assert(Option.isNone this)
        let constructedTermType = Types.FromDotNetType typ
        let wasConstructorInlined stackSizeBefore (afterCall : cilState) =
            // [NOTE] For example, if constructor is external call, it will be inlined and executed simultaneously
            Memory.CallStackSize afterCall.state = stackSizeBefore
        let modifyValueResultIfConstructorWasCalled stackSizeBefore (afterCall : cilState) =
            if wasConstructorInlined stackSizeBefore afterCall then pushNewObjForValueTypes afterCall
            else ()

        let blockCase (cilState : cilState) =
            let callConstructor (cilState : cilState) reference afterCall =
                methodInterpreter.InitFunctionFrameCIL cilState constructorInfo (Some reference) (Some args)
                x.InlineMethodBaseCallIfNeeded constructorInfo cilState afterCall

            if Types.IsValueType constructedTermType || typ = typeof<IntPtr> then
                let freshValue = Memory.DefaultOf constructedTermType
                let ref = Memory.AllocateTemporaryLocalVariable cilState.state typ freshValue
                push ref cilState // NOTE: ref is used to retrieve constructed struct
                let stackSizeBefore = Memory.CallStackSize cilState.state
                callConstructor cilState ref (List.map (fun afterCall -> modifyValueResultIfConstructorWasCalled stackSizeBefore afterCall; afterCall))
            else
                let ref = Memory.AllocateDefaultClass cilState.state constructedTermType
                push ref cilState // NOTE: ref is used as result afterCall
                callConstructor cilState ref id

        let k reference =
            let newIp = moveInstruction (fallThroughTarget cfg.methodBase offset) (currentIp cilState)
            push reference cilState
            setCurrentIp newIp cilState
            [cilState]

        if Reflection.isDelegateConstructor constructorInfo then
            x.CommonCreateDelegate constructorInfo cilState args k
        elif typ.IsArray && constructorInfo.GetMethodBody() = null then
            x.ReduceArrayCreation typ cilState args k
        else blockCase cilState)

    member x.LdsFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let newIp = moveInstruction (fallThroughTarget cfg.methodBase offset) (currentIp cilState)
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldsfld.Size)
        assert (fieldInfo.IsStatic)
        methodInterpreter.InitializeStatics cilState fieldInfo.DeclaringType (fun cilState ->
        let declaringTermType = Types.FromDotNetType fieldInfo.DeclaringType
        let fieldId = Reflection.wrapField fieldInfo
        let value = if addressNeeded
                    then StaticField(declaringTermType, fieldId) |> Ref
                    else Memory.ReadStaticField cilState.state declaringTermType fieldId
        push value cilState
        setCurrentIp newIp cilState
        [cilState])
    member private x.StsFld (cfg : cfg) offset (cilState : cilState) =
        let newIp = moveInstruction (fallThroughTarget cfg.methodBase offset) (currentIp cilState) // TODO: remove this copy-paste
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stsfld.Size)
        assert(fieldInfo.IsStatic)
        methodInterpreter.InitializeStatics cilState fieldInfo.DeclaringType (fun cilState ->
        let declaringTermType = Types.FromDotNetType fieldInfo.DeclaringType
        let fieldId = Reflection.wrapField fieldInfo
        let value = pop cilState
        let fieldType = Types.FromDotNetType fieldInfo.FieldType
        let value = castUnchecked fieldType value cilState.state
        Memory.WriteStaticField cilState.state declaringTermType fieldId value
        setCurrentIp newIp cilState
        [cilState])
    member x.LdFldWithFieldInfo (fieldInfo : FieldInfo) addressNeeded (cilState : cilState) =
        assert(not fieldInfo.IsStatic)
        let target = pop cilState
        let loadWhenTargetIsNotNull (cilState : cilState) k =
            let createCilState value =
                push value cilState
                k [cilState]
            let fieldId = Reflection.wrapField fieldInfo
            if fieldInfo.DeclaringType = typeof<IntPtr> then
                if addressNeeded then createCilState target
                else Memory.ReadSafe cilState.state target |> createCilState
            else
                if addressNeeded then Memory.ReferenceField cilState.state target fieldId |> createCilState
                else Memory.ReadField cilState.state target fieldId |> createCilState
        x.NpeOrInvokeStatementCIL cilState target loadWhenTargetIsNotNull id
    member x.LdFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldfld.Size)
        x.LdFldWithFieldInfo fieldInfo addressNeeded cilState
    member x.StFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stfld.Size)
        assert(not fieldInfo.IsStatic)
        let value, targetRef = pop2 cilState
        let storeWhenTargetIsNotNull (cilState : cilState) k =
            let fieldType = Types.FromDotNetType fieldInfo.FieldType
            let value = castUnchecked fieldType value cilState.state
            let reference =
                if fieldInfo.DeclaringType = typeof<IntPtr> then targetRef
                else Reflection.wrapField fieldInfo |> Memory.ReferenceField cilState.state targetRef
            Memory.WriteSafe cilState.state reference value |> List.map (changeState cilState) |> k
        x.NpeOrInvokeStatementCIL cilState targetRef storeWhenTargetIsNotNull id
    member private x.LdElemCommon cast (cilState : cilState) arrayRef indices =
        let arrayType = MostConcreteTypeOfHeapRef cilState.state arrayRef
        let uncheckedLdElem (cilState : cilState) k =
            let value = Memory.ReadArrayIndex cilState.state arrayRef indices
            let castedValue = cast value cilState.state
            push castedValue cilState
            k [cilState]
        let checkedLdElem (cilState : cilState) k =
            let dims = List.init (Types.RankOf arrayType) MakeNumber
            let lengths = List.map (Memory.ArrayLengthByDimension cilState.state arrayRef) dims
            x.AccessMultidimensionalArray uncheckedLdElem cilState lengths indices k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkedLdElem id
    member private x.LdElemWithCast cast (cilState : cilState) : cilState list =
        let index, arrayRef = pop2 cilState
        x.LdElemCommon cast cilState arrayRef [index]
    member private x.LdElemTyp typ (cilState : cilState) = x.LdElemWithCast (castUnchecked typ) cilState
    member private x.LdElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldelem.Size)
        x.LdElemTyp typ cilState
    member private x.LdElemRef = x.LdElemWithCast always
    member private x.LdElema (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldelema.Size)
        let index, arrayRef = pop2 cilState
        let referenceLocation (cilState : cilState) k =
            let value = Memory.ReferenceArrayIndex arrayRef [index]
            push value cilState
            k [cilState]
        let checkTypeMismatch (cilState : cilState) (k : cilState list -> 'a) =
            let elementType = MostConcreteTypeOfHeapRef cilState.state arrayRef |> Types.ElementType
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (Types.TypeIsType typ elementType &&& Types.TypeIsType elementType typ, state))
                referenceLocation
                (x.Raise x.ArrayTypeMismatchException)
                k
        let checkIndex (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            x.AccessArray checkTypeMismatch cilState length index k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkIndex id
    member private x.StElemCommon (cilState : cilState) arrayRef indices value =
        let arrayType = MostConcreteTypeOfHeapRef cilState.state arrayRef
        let baseType = Types.ElementType arrayType
        let checkedStElem (cilState : cilState) (k : cilState list -> 'a) =
            let typeOfValue = TypeOf value
            let uncheckedStElem (cilState : cilState) (k : cilState list -> 'a) =
                let casted = castUnchecked baseType value cilState.state
                Memory.WriteArrayIndex cilState.state arrayRef indices casted |> List.map (changeState cilState) |> k
            let checkTypeMismatch (cilState : cilState) (k : cilState list -> 'a) =
                let condition =
                    if Types.IsValueType typeOfValue then True
                    else Types.RefIsAssignableToType cilState.state value baseType
                StatedConditionalExecutionAppendResultsCIL cilState
                    (fun state k -> k (condition, state))
                    uncheckedStElem
                    (x.Raise x.ArrayTypeMismatchException)
                    k
            let dims = List.init (Types.RankOf arrayType) MakeNumber
            let lengths = List.map (Memory.ArrayLengthByDimension cilState.state arrayRef) dims
            x.AccessMultidimensionalArray checkTypeMismatch cilState lengths indices k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkedStElem id
    member private x.StElemWithCast (cilState : cilState) =
        let value, index, arrayRef = pop3 cilState
        x.StElemCommon cilState arrayRef [index] value
    member private x.StElemTyp _ (cilState : cilState) =
        x.StElemWithCast cilState
    member private x.StElem (cilState : cilState) =
//        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Stelem.Size)
        x.StElemWithCast cilState
    member private x.StElemRef = x.StElemWithCast
    member private x.LdLen (cilState : cilState) =
        let arrayRef = pop cilState
        let ldlen (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            push length cilState
            k [cilState]
        x.NpeOrInvokeStatementCIL cilState arrayRef ldlen id
    member private x.LdVirtFtn (cfg : cfg) offset (cilState : cilState) =
        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Ldvirtftn.Size)
        let this = pop cilState
        let ldvirtftn (cilState : cilState) k =
            assert(IsReference this)
            let thisType = this |> MostConcreteTypeOfHeapRef cilState.state |> Types.ToDotNetType
            let methodInfo = thisType.GetMethod(ancestorMethodBase.Name, Reflection.allBindingFlags)
            let methodInfoType = methodInfo.GetType() |> Types.FromDotNetType
            let methodPtr = Terms.Concrete methodInfo methodInfoType
            push methodPtr cilState
            k [cilState]
        x.NpeOrInvokeStatementCIL cilState this ldvirtftn id

    member x.BoxNullable (t : System.Type) (v : term) (cilState : cilState) : cilState list =
        // TODO: move it to Reflection.fs; add more validation in case if .NET implementation does not have these fields
        let boxValue (cilState : cilState) =
            let value = pop cilState
            let address = Memory.BoxValueType cilState.state value
            push address cilState
        let hasValueCase (cilState : cilState) k =
            let valueFieldInfo = t.GetField("value", Reflection.instanceBindingFlags)
            push v cilState
            let cilStates = x.LdFldWithFieldInfo valueFieldInfo false cilState
            List.iter boxValue cilStates
            k cilStates
        let boxNullable (hasValue, cilState : cilState) k =
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (hasValue, state))
                hasValueCase
                (fun cilState k ->
                    push NullRef cilState
                    k [cilState])
                k
        let hasValueFieldInfo = t.GetField("hasValue", Reflection.instanceBindingFlags)
        let hasValueResults =
            push v cilState
            x.LdFldWithFieldInfo hasValueFieldInfo false cilState  |> List.map (fun cilState -> (pop cilState, cilState))
        Cps.List.mapk boxNullable hasValueResults List.concat

    member x.Box (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Box.Size)
        let termType = Types.FromDotNetType t
        if Types.IsValueType termType then
            let v = pop cilState
            if Types.TypeIsNullable termType then x.BoxNullable t v cilState
            else
                allocateValueTypeInHeap v cilState
                [cilState]
        else [cilState]
    member private x.UnboxCommon cilState obj t handleRestResults k =
        let nonExceptionCont (cilState : cilState) res k =
            push res cilState
            k [cilState]
        let termType = Types.FromDotNetType t
        assert(IsReference obj)
        assert(Types.IsValueType termType)
        let nullCase (cilState : cilState) k =
            if Types.TypeIsNullable termType then
                let nullableTerm = Memory.DefaultOf termType
                let address = Memory.BoxValueType cilState.state nullableTerm
                let res = handleRestResults cilState (HeapReferenceToBoxReference address)
                nonExceptionCont cilState res k
            else
                x.Raise x.NullReferenceException cilState k
        let nullableCase (cilState : cilState) =
            let underlyingTypeOfNullableT = System.Nullable.GetUnderlyingType t
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (Types.RefIsType state obj (Types.FromDotNetType underlyingTypeOfNullableT), state))
                (fun cilState k ->
                    let value = HeapReferenceToBoxReference obj |> Memory.ReadSafe cilState.state
                    let nullableTerm = Memory.DefaultOf termType
                    let valueField, hasValueField = Reflection.fieldsOfNullable t
                    let nullableTerm = Memory.WriteStructField nullableTerm valueField value
                    let nullableTerm = Memory.WriteStructField nullableTerm hasValueField (MakeBool true)
                    let address = Memory.BoxValueType cilState.state nullableTerm
                    let res = handleRestResults cilState (HeapReferenceToBoxReference address)
                    nonExceptionCont cilState res k)
                (x.Raise x.InvalidCastException)
        let nonNullCase (cilState : cilState) =
            if Types.TypeIsNullable termType then
                nullableCase cilState
            else
                StatedConditionalExecutionAppendResultsCIL cilState
                    (fun state k -> k (Types.IsCast state obj termType, state))
                    (fun cilState k ->
                        let res = handleRestResults cilState (Types.Cast obj termType |> HeapReferenceToBoxReference)
                        push res cilState
                        k [cilState])
                    (x.Raise x.InvalidCastException)
        BranchOnNullCIL cilState obj
            nullCase
            nonNullCase
            k

    member private x.Throw (cilState : cilState) =
        let error = peek cilState
        BranchOnNullCIL cilState error
            (x.Raise x.NullReferenceException)
            (fun cilState k ->
                //TODO: change current ip
                setException (Unhandled error) cilState
                clearEvaluationStackLastFrame cilState
                k [cilState])
            id
    member private x.Unbox (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox.Size)
        let obj = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then
            let iie = createInsufficientInformation "Unboxing generic parameter"
            {cilState with iie = Some iie} |> List.singleton
        else
            x.UnboxCommon cilState obj t (fun _ x -> x) id
    member private x.UnboxAny (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox_Any.Size)
        let termType = Types.FromDotNetType t
        let valueType = Types.FromDotNetType typeof<System.ValueType>
        let obj = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then
            let iie = createInsufficientInformation "Can't introduce generic type X for equation: T = Nullable<X>"
            {cilState with iie = Some iie} |> List.singleton
        else
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (Types.TypeIsType termType valueType, state))
                (fun cilState k ->
                    let handleRestResults cilState address = Memory.ReadSafe cilState.state address
                    x.UnboxCommon cilState obj t handleRestResults k)
                (fun state k -> x.CommonCastClass state obj termType k)
                id

    member private this.CommonDivRem performAction (cilState : cilState) =
        let integerCase (cilState : cilState) x y minusOne minValue =
            assert(TypeOf x = TypeOf y)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.InvalidCastException)
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k ((x === minValue) &&& (y === minusOne), state))
                        (this.Raise this.InvalidCastException)
                        (fun cilState k ->
                            push (performAction x y) cilState
                            k [cilState]))
                id
        let y, x = pop2 cilState
        match y, x with
        | TypeUtils.Float, TypeUtils.Float ->
            push (performAction x y) cilState
            [cilState]
        | TypeUtils.Int64, _
        | TypeUtils.UInt64, _
        | _, TypeUtils.Int64
        | _, TypeUtils.UInt64 -> integerCase cilState x y TypeUtils.Int64.MinusOne TypeUtils.Int64.MinValue
        | _ -> integerCase cilState x y TypeUtils.Int32.MinusOne TypeUtils.Int32.MinValue
        | _ -> __unreachable__()
    member private this.Div (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonDivRem div cilState

    member private this.Rem (cilState : cilState) =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonDivRem rem cilState

    member private this.CommonUnsignedDivRem isRem performAction (cilState : cilState) =
        let y, x = pop2 cilState
        match y, x with
        | _ when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState k ->
                    push (performAction x y) cilState
                    k [cilState])
                id
        | TypeUtils.Float, _
        | _, TypeUtils.Float when isRem -> internalfailf "Rem.Un is unspecified for Floats"
        | _ -> internalfailf "incompatible operands for %s" (if isRem then "Rem.Un" else "Div.Un")

    member private this.DivUn (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide_Un x y id
        this.CommonUnsignedDivRem false div cilState

    member private this.RemUn cilState =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder_Un x y id
        this.CommonUnsignedDivRem true rem cilState

    member private this.UnsignedCheckOverflow checkOverflowForUnsigned (cilState : cilState) =
        let y, x = pop2 cilState
        match y, x with
        | TypeUtils.Int64, _
        | _, TypeUtils.Int64
        | TypeUtils.UInt64, _
        | _, TypeUtils.UInt64 ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            let max = TypeUtils.UInt64.MaxValue
            let zero = TypeUtils.UInt64.Zero
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x, y = makeUnsignedInteger x id, makeUnsignedInteger y id
            let max = TypeUtils.UInt32.MaxValue
            let zero = TypeUtils.UInt32.Zero
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> internalfailf "incompatible operands for UnsignedCheckOverflow"
    member private this.SignedCheckOverflow checkOverflow (cilState : cilState) =
        let y, x = pop2 cilState
        match y, x with
        | TypeUtils.Int64, _
        | _, TypeUtils.Int64 ->
            let min = TypeUtils.Int64.MinValue
            let max = TypeUtils.Int64.MaxValue
            let zero = TypeUtils.Int64.Zero
            let minusOne = TypeUtils.Int64.MinusOne
            checkOverflow min max zero minusOne x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | TypeUtils.UInt64, _
        | _, TypeUtils.UInt64 -> __unreachable__() // instead of add_ovf should be called add_ovf_un
        | TypeUtils.Float, _
        | _, TypeUtils.Float -> __unreachable__() // only integers
        | _ ->
            let min = TypeUtils.Int32.MinValue
            let max = TypeUtils.Int32.MaxValue
            let zero = TypeUtils.Int32.Zero
            let minusOne = TypeUtils.Int32.MinusOne
            checkOverflow min max zero minusOne x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
    member private this.Add_ovf (cilState : cilState) =
        // min <= x + y <= max
        let checkOverflow min max zero _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let xMoreThan0 state k = k (x >>= zero, state)
            let yMoreThan0 state k = k (y >>= zero, state)
            let checkOverflowWhenMoreThan0 (state : state) k = // x >= 0 && y >= 0
                PerformBinaryOperation OperationType.Subtract max y (fun diff ->
                k (diff >>= x, state))
            let checkOverflowWhenLessThan0 (state : state) k =
                PerformBinaryOperation OperationType.Subtract min y (fun diff ->
                k (x >>= diff, state))
            let add (cilState : cilState) k = // no overflow
                PerformBinaryOperation OperationType.Add x y (fun sum ->
                    push sum cilState
                    k [cilState])
            StatedConditionalExecutionCIL cilState xMoreThan0
                (fun cilState -> // x >= 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        (fun cilState -> // y >= 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenMoreThan0
                                add
                                (this.Raise this.OverflowException))
                        add)
                (fun cilState -> // x < 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        add
                        (fun cilState -> // x < 0 && y < 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenLessThan0
                                add
                                (this.Raise this.OverflowException)))
                id
        this.SignedCheckOverflow checkOverflow cilState
    member private this.Mul_ovf (cilState : cilState) =
        // min <= x * y <= max
        let checkOverflow min max zero _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let (>>) = API.Arithmetics.(>>)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            let xMoreThan0 state k = k (x >> zero, state)
            let yMoreThan0 state k = k (y >> zero, state)
            let checkOverflowWhenXM0YM0 (state : state) k = // x > 0 && y > 0
                PerformBinaryOperation OperationType.Divide max y (fun quotient ->
                k (quotient >>= x, state))
            let checkOverflowWhenXL0YL0 (state : state) k = // x < 0 && y < 0
                PerformBinaryOperation OperationType.Divide max y (fun quotient ->
                k (x >>= quotient, state))
            let checkOverflowWhenXM0YL0 (state : state) k = // x > 0 && y < 0
                PerformBinaryOperation OperationType.Divide min x (fun quotient ->
                k (y >>= quotient, state))
            let checkOverflowWhenXL0YM0 (state : state) k = // x < 0 && y > 0
                PerformBinaryOperation OperationType.Divide min y (fun quotient ->
                k (x >>= quotient, state))
            let mul (cilState : cilState) k = // no overflow
                PerformBinaryOperation OperationType.Multiply x y (fun res ->
                    push res cilState
                    k [cilState])
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k ->
                    push zero cilState
                    k [cilState])
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        xMoreThan0
                        (fun cilState -> // x > 0
                            StatedConditionalExecutionCIL cilState yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YM0
                                        mul
                                        (this.Raise this.OverflowException))
                                (fun cilState -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YL0
                                        mul
                                        (this.Raise this.OverflowException)))
                        (fun cilState -> // x < 0
                            StatedConditionalExecutionCIL cilState
                                yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YM0
                                        mul
                                        (this.Raise this.OverflowException))
                                (fun cilState k -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YL0
                                        mul
                                        (this.Raise this.OverflowException)
                                        k)))
                id
        this.SignedCheckOverflow checkOverflow cilState
    member private this.Add_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k ->
                    PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                    k (diff >>= y, state)))
                (fun cilState k -> PerformBinaryOperation OperationType.Add x y (fun res ->
                    push res cilState
                    k [cilState]))
                (this.Raise this.OverflowException)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Mul_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned zero max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k ->
                    push zero cilState
                    k [cilState])
                (fun cilState k ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k ->
                            PerformBinaryOperation OperationType.Divide max x (fun quotient ->
                            k (quotient >>= y, state)))
                        (fun cilState k ->
                            PerformBinaryOperation OperationType.Multiply x y (fun res ->
                                push res cilState
                                k [cilState]))
                        (this.Raise this.OverflowException)
                        k)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (x >>= y, state))
                (fun (cilState : cilState) k -> // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    push res cilState
                    k [cilState]))
                (this.Raise this.OverflowException)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf (cilState : cilState) =
        // there is no way to reduce current operation to [x `Add_Ovf` (-y)]
        // min <= x - y <= max
        let checkOverflowForSigned min max zero minusOne x y cilState =
                let (>>=) = API.Arithmetics.(>>=)
                let xGreaterEqualZero state k = k (x >>= zero, state)
                let sub (cilState : cilState) k = // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    push res cilState
                    k [cilState])
                StatedConditionalExecutionCIL cilState
                    xGreaterEqualZero
                    (fun cilState -> // x >= 0 => max - x >= 0 => no overflow for [-1 * (max - x)]
                        StatedConditionalExecutionCIL cilState
                            (fun state k ->
                                PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (y >>= minusDiff, state)))) // y >= -(max - x)
                            sub
                            (this.Raise this.OverflowException))
                    (fun cilState -> // x < 0 => no overflow for [min - x] # x < 0 => [min - x] != min => no overflow for (-1) * [min - x]
                        StatedConditionalExecutionCIL cilState
                           (fun state k ->
                                PerformBinaryOperation OperationType.Subtract min x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (minusDiff >>= y, state)))) // -(min - x) >= y
                            sub
                            (this.Raise this.OverflowException))
                    id
        this.SignedCheckOverflow checkOverflowForSigned cilState
    member private x.Newarr (cfg : cfg) offset (cilState : cilState) =
        let (>>=) = API.Arithmetics.(>>=)
        let elemType = resolveTermTypeFromMetadata cfg (offset + OpCodes.Newarr.Size)
        let numElements = pop cilState
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (numElements >>= TypeUtils.Int32.Zero, state))
            (fun cilState k ->
                let ref = Memory.AllocateVectorArray cilState.state numElements elemType
                push ref cilState
                k [cilState])
            (this.Raise this.OverflowException)
            id

    member x.InvalidProgramException cilState = methodInterpreter.InvalidProgramException cilState
    member x.NullReferenceException cilState = methodInterpreter.NullReferenceException cilState; [cilState]
    member x.ArgumentException cilState = methodInterpreter.ArgumentException cilState; [cilState]
    member x.ArgumentNullException cilState = methodInterpreter.ArgumentNullException cilState; [cilState]
    member x.ArgumentOutOfRangeException cilState = methodInterpreter.ArgumentOutOfRangeException cilState; [cilState]
    member x.IndexOutOfRangeException cilState = methodInterpreter.IndexOutOfRangeException cilState; [cilState]
    member x.ArrayTypeMismatchException cilState = methodInterpreter.ArrayTypeMismatchException cilState; [cilState]
    member x.RankException cilState = methodInterpreter.RankException cilState; [cilState]
    member x.DivideByZeroException cilState = methodInterpreter.DivideByZeroException cilState; [cilState]
    member x.OverflowException cilState = methodInterpreter.OverflowException cilState; [cilState]
    member x.ArithmeticException cilState = methodInterpreter.ArithmeticException cilState
    member x.TypeInitializerException qualifiedTypeName innerException cilState =
        methodInterpreter.TypeInitializerException qualifiedTypeName innerException cilState
    member x.InvalidCastException cilState = methodInterpreter.InvalidCastException cilState; [cilState]

    // -------------------------------- ExplorerBase operations -------------------------------------

    member x.ExecuteAllInstructionsForCFGEdges (cfg : cfg) (cilState : cilState) : (cilState list * cilState list * cilState list) =
        let ip = currentIp cilState
        assert(ip.CanBeExpanded())
        let startingOffset = ip.Offset()
        let endOffset =
            let lastOffset = Seq.last cfg.sortedOffsets
            let rec binarySearch l r =
                if l + 1 = r then l
                else
                    let mid = (l + r) / 2
                    if cfg.sortedOffsets.[mid] <= startingOffset then binarySearch mid r
                    else binarySearch l mid
            let index = binarySearch 0 (Seq.length cfg.sortedOffsets)
            if cfg.sortedOffsets.[index] = lastOffset then cfg.ilBytes.Length
            else cfg.sortedOffsets.[index + 1]

        let isIpOfCurrentBasicBlock (ip : ip) =
            match ip with
            | Instruction(i, m) when m = cfg.methodBase -> startingOffset <= i && i < endOffset
            | _ -> false
        x.ExecuteAllInstructionsWhile isIpOfCurrentBasicBlock cilState

    member x.ExecuteOneInstruction (cilState : cilState) : (cilState list * cilState list * cilState list) =
        x.ExecuteAllInstructionsWhile (always false) cilState

    // returns finishedStates, incompleteStates, erroredStates
    member x.ExecuteAllInstructionsWhile (condition : ip -> bool) (cilState : cilState) =
        let rec executeAllInstructions (finishedStates, incompleteStates, errors) cilState k =
            let ip = currentIp cilState
            try
                cilState.iie <- None
                let allStates = x.MakeStep cilState
                let newErrors, restStates = List.partition isError allStates
                let errors = errors @ newErrors // TODO: check it
                let newIieStates, goodStates = List.partition isIIEState restStates
                let incompleteStates = newIieStates @ incompleteStates
                match goodStates with
                | _ when List.forall (currentIp >> condition) goodStates ->
                    Cps.List.foldlk executeAllInstructions (finishedStates, incompleteStates, errors) goodStates k
                | _ -> (goodStates @ finishedStates, incompleteStates, errors) |> k
            with
            | :? InsufficientInformationException as iie ->
                cilState.iie <- Some iie
                setCurrentIp ip cilState
                (finishedStates, cilState :: incompleteStates, errors) |> k
        executeAllInstructions ([],[],[]) cilState id

    member private x.IncrementLevelIfNeeded (cfg : cfg) (offset : offset) (cilState : cilState) =
        let isRecursiveVertex offset =
            if cfg.dfsOut.ContainsKey offset then
                let t1 = cfg.dfsOut.[offset]
                cfg.reverseGraph.[offset] |> Seq.exists (fun w -> cfg.dfsOut.[w] <= t1)
            else false
        if offset = 0 || isRecursiveVertex offset then
            incrementLevel cilState {offset = offset; method = cfg.methodBase}

    member private x.DecrementMethodLevel (cilState : cilState) method =
        let key = {offset = 0; method = method}
        decrementLevel cilState key

    member x.MakeStep (cilState : cilState) =
        cilState.stepsNumber <- cilState.stepsNumber + 1u
        let exit m =
            x.DecrementMethodLevel cilState m
            Logger.printLogLazy Logger.Info "Done with method %s" (lazy Reflection.getFullMethodName m)
            match cilState.ipStack with
            // the whole method is executed
            | [ Exit _ ] when startsFromMethodBeginning cilState ->
                setCurrentTime [] cilState
                popFrameOf cilState
            // some part of method is executed
            | [ Exit _ ] -> ()
            | Exit m :: ips' when Reflection.isStaticConstructor m ->
                popFrameOf cilState
                setIpStack ips' cilState
            | Exit _ :: (InstructionEndingIp(offset, caller) as ip) :: _ ->
                // TODO: assert (isCallIp ip)
                let newIp = moveInstruction (fallThroughTarget caller offset) ip
                popFrameOf cilState
                setCurrentIp newIp cilState
                let callSite = parseCallSite caller offset
                if callSite.opCode = OpCodes.Newobj && callSite.calledMethod.DeclaringType.IsValueType then
                    pushNewObjForValueTypes cilState
            | Exit _ :: Exit _ :: _ -> __unreachable__()
            | _ -> __unreachable__()
        let rec makeStep' ip k =
            match ip with
            | Instruction(offset, m) ->
                if offset = 0 then Logger.printLogLazy Logger.Info "Starting to explore method %O" (lazy Reflection.getFullMethodName m)
                x.ExecuteInstruction m offset cilState |> k
            | Exit m ->
                exit m
                k [cilState]
            | Leave(EndFinally, [],  dst, m) ->
                setCurrentIp (instruction m dst) cilState
                clearEvaluationStackLastFrame cilState
                k [cilState]
            | Leave(EndFinally, ehc :: ehcs,  dst, m) ->
                let ip' = leave (instruction m ehc.HandlerOffset) ehcs dst m
                setCurrentIp ip' cilState
                clearEvaluationStackLastFrame cilState
                k [cilState]
            | Leave(ip, ehcs, dst, m) ->
                let oldLength = List.length cilState.ipStack
                let makeLeaveIfNeeded (result : cilState) =
                    if List.length result.ipStack = oldLength then
                        match result.ipStack with
                        | ip :: ips -> result.ipStack <- leave ip ehcs dst m :: ips
                        | _ -> __unreachable__()
                makeStep' ip (fun states ->
                List.iter makeLeaveIfNeeded states
                k states)
            | _ -> __notImplemented__()
        makeStep' (currentIp cilState) id

    member private x.ExecuteInstruction m offset (cilState : cilState) =
//        Logger.trace "ExecuteInstruction:\n%s" (dump cilState)
        let cfg = CFG.findCfg m
        let opCode = parseInstruction m offset
//        let newIps = moveIp cilState |> List.map (fun cilState -> cilState.ipStack)

        let opcodeValue = LanguagePrimitives.EnumOfValue opCode.Value
        let newSts =
            match opcodeValue with
            | OpCodeValues.Br
            | OpCodeValues.Br_S -> br cfg offset cilState; [cilState]
            | OpCodeValues.Add ->
                (fun _ _ -> standardPerformBinaryOperation OperationType.Add) |> fallThrough cfg offset cilState // TODO: check float overflow [spec]
            | OpCodeValues.Mul -> (fun _ _ -> standardPerformBinaryOperation OperationType.Multiply) |> fallThrough cfg offset cilState
            | OpCodeValues.Sub -> (fun _ _ -> standardPerformBinaryOperation OperationType.Subtract) |> fallThrough cfg offset cilState
            | OpCodeValues.Shl -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftLeft) |> fallThrough cfg offset cilState
            | OpCodeValues.Shr -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight) |> fallThrough cfg offset cilState
            | OpCodeValues.Shr_Un -> (fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight_Un) |> fallThrough cfg offset cilState
            | OpCodeValues.Ceq -> (fun _ _ -> ceq) |> fallThrough cfg offset cilState
            | OpCodeValues.Cgt -> (fun _ _ -> cgt) |> fallThrough cfg offset cilState
            | OpCodeValues.Cgt_Un -> (fun _ _ -> cgtun) |> fallThrough cfg offset cilState
            | OpCodeValues.Clt -> (fun _ _ -> clt) |> fallThrough cfg offset cilState
            | OpCodeValues.Clt_Un -> (fun _ _ -> cltun) |> fallThrough cfg offset cilState
            | OpCodeValues.And -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseAnd OperationType.LogicalAnd) |> fallThrough cfg offset cilState
            | OpCodeValues.Or -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseOr OperationType.LogicalOr) |> fallThrough cfg offset cilState
            | OpCodeValues.Xor -> (fun _ _ -> bitwiseOrBoolOperation OperationType.BitwiseXor OperationType.LogicalXor) |> fallThrough cfg offset cilState
            | OpCodeValues.Neg -> (fun _ _ -> performCILUnaryOperation OperationType.UnaryMinus) |> fallThrough cfg offset cilState
            | OpCodeValues.Not -> (fun _ _ -> bitwiseOrBoolNot) |> fallThrough cfg offset cilState
            | OpCodeValues.Stloc -> stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Stloc.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_0 -> stloc (fun _ _ -> 0) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_1 -> stloc (fun _ _ -> 1) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_2 -> stloc (fun _ _ -> 2) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_3 -> stloc (fun _ _ -> 3) |> forkThrough cfg offset cilState
            | OpCodeValues.Stloc_S -> stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Stloc_S.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Starg -> starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Starg.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Starg_S -> starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Starg_S.Size) |> int) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldc_I4 -> ldc (fun ilBytes offset -> NumberCreator.extractInt32 ilBytes (offset + OpCodes.Ldc_I4.Size)) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_0 -> ldc (fun _ _ -> 0) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_1 -> ldc (fun _ _ -> 1) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_2 -> ldc (fun _ _ -> 2) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_3 -> ldc (fun _ _ -> 3) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_4 -> ldc (fun _ _ -> 4) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_5 -> ldc (fun _ _ -> 5) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_6 -> ldc (fun _ _ -> 6) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_7 -> ldc (fun _ _ -> 7) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_8 -> ldc (fun _ _ -> 8) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_M1 -> ldc (fun _ _ -> -1) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I4_S -> ldc (fun ilBytes offset -> NumberCreator.extractInt8 ilBytes (offset + OpCodes.Ldc_I4_S.Size)) typedefof<int32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_I8 -> ldc (fun ilBytes offset -> NumberCreator.extractInt64 ilBytes (offset + OpCodes.Ldc_I8.Size)) typedefof<int64> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_R4 -> ldc (fun ilBytes offset -> NumberCreator.extractFloat32 ilBytes (offset + OpCodes.Ldc_R4.Size)) typedefof<float32> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldc_R8 -> ldc (fun ilBytes offset -> NumberCreator.extractFloat64 ilBytes (offset + OpCodes.Ldc_R8.Size)) typedefof<double> |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg -> ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarg.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_0 -> ldarg (fun _ _ -> 0) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_1 -> ldarg (fun _ _ -> 1) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_2 -> ldarg (fun _ _ -> 2) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_3 -> ldarg (fun _ _ -> 3) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarg_S -> ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarg_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Nop -> fallThrough cfg offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Ldloc -> ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloc.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_0 -> ldloc (fun _ _ -> 0) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_1 -> ldloc (fun _ _ -> 1) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_2 -> ldloc (fun _ _ -> 2) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_3 -> ldloc (fun _ _ -> 3) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloc_S -> ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloc_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloca -> ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloca.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldloca_S -> ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloca_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ret -> ret cfg cilState; [cilState]
            | OpCodeValues.Dup -> (fun _ _ -> dup) |> fallThrough cfg offset cilState

            // branching
            | OpCodeValues.Brfalse -> brfalse cfg offset cilState
            | OpCodeValues.Brfalse_S -> brfalse cfg offset cilState
            | OpCodeValues.Brtrue -> brtrue cfg offset cilState
            | OpCodeValues.Brtrue_S -> brtrue cfg offset cilState
            | OpCodeValues.Beq -> applyAndBranch ceq brtrue cfg offset cilState
            | OpCodeValues.Beq_S -> applyAndBranch ceq brtrue cfg offset cilState
            | OpCodeValues.Bge -> applyAndBranch bgeHelper brfalse cfg offset cilState
            | OpCodeValues.Bge_S -> applyAndBranch bgeHelper brfalse cfg offset cilState

            | OpCodeValues.Bgt -> applyAndBranch cgt brtrue cfg offset cilState
            | OpCodeValues.Bgt_S -> applyAndBranch cgt brtrue cfg offset cilState
            | OpCodeValues.Bgt_Un -> applyAndBranch cgtun brtrue cfg offset cilState
            | OpCodeValues.Bgt_Un_S -> applyAndBranch cgtun brtrue cfg offset cilState
            | OpCodeValues.Ble -> applyAndBranch cgt brfalse cfg offset cilState
            | OpCodeValues.Ble_S -> applyAndBranch cgt brfalse cfg offset cilState
            | OpCodeValues.Ble_Un -> applyAndBranch cgtun brfalse cfg offset cilState
            | OpCodeValues.Ble_Un_S -> applyAndBranch cgtun brfalse cfg offset cilState
            | OpCodeValues.Blt -> applyAndBranch clt brtrue cfg offset cilState
            | OpCodeValues.Blt_S -> applyAndBranch clt brtrue cfg offset cilState
            | OpCodeValues.Blt_Un -> applyAndBranch cltun brtrue cfg offset cilState
            | OpCodeValues.Blt_Un_S -> applyAndBranch cltun brtrue cfg offset cilState
            | OpCodeValues.Bne_Un -> applyAndBranch ceq brfalse cfg offset cilState
            | OpCodeValues.Bne_Un_S -> applyAndBranch ceq brfalse cfg offset cilState
            | OpCodeValues.Bge_Un -> applyAndBranch cltun brfalse cfg offset cilState
            | OpCodeValues.Bge_Un_S -> applyAndBranch cltun brfalse cfg offset cilState

            | OpCodeValues.Ldstr -> ldstr |> fallThrough cfg offset cilState
            | OpCodeValues.Ldnull -> (fun _ _ -> ldnull) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I1 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int8Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I2 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int16Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I4 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int32Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I8 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_R4 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float32Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_R8 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U1 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint8Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U2 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint16Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U4 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint32Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U8 -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_I -> (fun _ _ -> convi) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_U -> (fun _ _ -> convu) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_R_Un -> (fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64Type) |> fallThrough cfg offset cilState
            | OpCodeValues.Switch -> switch cfg offset cilState
            | OpCodeValues.Ldtoken -> ldtoken |> fallThrough cfg offset cilState
            | OpCodeValues.Ldftn -> ldftn |> fallThrough cfg offset cilState
            | OpCodeValues.Pop -> (fun _ _ -> pop >> ignore) |> fallThrough cfg offset cilState
            | OpCodeValues.Initobj -> initobj |> forkThrough cfg offset cilState
            | OpCodeValues.Ldarga -> ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarga.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldarga_S -> ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarga_S.Size) |> int) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I4 -> (fun _ _ -> ldind (castUnchecked TypeUtils.int32Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I1 -> (fun _ _ -> ldind (castUnchecked TypeUtils.int8Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I2 -> (fun _ _ -> ldind (castUnchecked TypeUtils.int16Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I8 -> (fun _ _ -> ldind (castUnchecked TypeUtils.int64Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_U1 -> (fun _ _ -> ldind (castUnchecked TypeUtils.uint8Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_U2 -> (fun _ _ -> ldind (castUnchecked TypeUtils.uint16Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_U4 -> (fun _ _ -> ldind (castUnchecked TypeUtils.uint32Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_R4 -> (fun _ _ -> ldind (castUnchecked TypeUtils.float32Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_R8 -> (fun _ _ -> ldind (castUnchecked TypeUtils.float64Type)) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_Ref -> (fun _ _ -> ldind always) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldind_I -> (fun _ _ -> ldind MakeIntPtr) |> fallThrough cfg offset cilState
            | OpCodeValues.Isinst -> isinst |> forkThrough cfg offset cilState
            | OpCodeValues.Stobj -> stobj |> forkThrough cfg offset cilState
            | OpCodeValues.Ldobj -> ldobj |> fallThrough cfg offset cilState
            | OpCodeValues.Stind_I1 -> (fun _ _ -> stind (castUnchecked TypeUtils.int8Type)) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I2 -> (fun _ _ -> stind (castUnchecked TypeUtils.int16Type)) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I4 -> (fun _ _ -> stind (castUnchecked TypeUtils.int32Type)) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I8 -> (fun _ _ -> stind (castUnchecked TypeUtils.int64Type)) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_R4 -> (fun _ _ -> stind (castUnchecked TypeUtils.float32Type)) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_R8 -> (fun _ _ -> stind (castUnchecked TypeUtils.float64Type)) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_Ref -> (fun _ _ -> stind always) |> forkThrough cfg offset cilState
            | OpCodeValues.Stind_I -> (fun _ _ -> stind MakeIntPtr) |> forkThrough cfg offset cilState
            | OpCodeValues.Sizeof -> sizeofInstruction |> fallThrough cfg offset cilState
            | OpCodeValues.Leave
            | OpCodeValues.Leave_S -> InstructionsSet.leave cfg offset cilState; [cilState]
            | OpCodeValues.Endfinally -> (fun _ _ -> endfinally) |> fallThrough cfg offset cilState
            | OpCodeValues.Rethrow -> (fun _ _ -> rethrow) |> fallThrough cfg offset cilState
            | OpCodeValues.Endfilter -> (fun _ _ -> endfilter) |> fallThrough cfg offset cilState
            | OpCodeValues.Localloc -> (fun _ _ -> localloc) |> fallThrough cfg offset cilState
            // TODO: notImplemented instructions
            | OpCodeValues.Stelem_I -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Ldelem_I -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Arglist -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Jmp -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Break -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Calli -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Ckfinite -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Constrained_ -> constrained |> fallThrough cfg offset cilState
            | OpCodeValues.Cpblk -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Cpobj -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Mkrefany -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix1 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix2 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix3 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix4 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix5 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix6 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefix7 -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Prefixref -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Readonly_ -> fallThrough cfg offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Refanytype -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Refanyval -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Tail_ -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Unaligned_ -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState
            | OpCodeValues.Volatile_ -> fallThrough cfg offset cilState (fun _ _ _ -> ())
            | OpCodeValues.Initblk -> (fun _ _ _ -> __notImplemented__()) |> fallThrough cfg offset cilState

            | OpCodeValues.Call -> this.Call cfg offset cilState
            | OpCodeValues.Callvirt -> this.CallVirt cfg offset cilState
            | OpCodeValues.Newobj -> this.NewObj cfg offset cilState
            | OpCodeValues.Ldsfld -> this.LdsFld false cfg offset cilState
            | OpCodeValues.Ldsflda -> this.LdsFld true cfg offset cilState
            | OpCodeValues.Stsfld -> this.StsFld cfg offset cilState
            | OpCodeValues.Ldfld -> this.LdFld false |> forkThrough cfg offset cilState
            | OpCodeValues.Ldflda -> this.LdFld true |> forkThrough cfg offset cilState
            | OpCodeValues.Stfld -> this.StFld |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem -> this.LdElem |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelema -> this.LdElema |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I1 -> (fun _ _ -> this.LdElemTyp TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I2 -> (fun _ _ -> this.LdElemTyp TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I4 -> (fun _ _ -> this.LdElemTyp TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_I8 -> (fun _ _ -> this.LdElemTyp TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_R4 -> (fun _ _ -> this.LdElemTyp TypeUtils.float32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_R8 -> (fun _ _ -> this.LdElemTyp TypeUtils.float64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_U1 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_U2 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_U4 -> (fun _ _ -> this.LdElemTyp TypeUtils.uint32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldelem_Ref -> (fun _ _ -> this.LdElemRef) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem -> (fun _ _ -> this.StElem) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I1 -> (fun _ _ -> this.StElemTyp TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I2 -> (fun _ _ -> this.StElemTyp TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I4 -> (fun _ _ -> this.StElemTyp TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_I8 -> (fun _ _ -> this.StElemTyp TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_R4 -> (fun _ _ -> this.StElemTyp TypeUtils.float32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_R8 -> (fun _ _ -> this.StElemTyp TypeUtils.float64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Stelem_Ref -> (fun _ _ -> this.StElemRef) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I1 -> (fun _ _ -> this.ConvOvf TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I2 -> (fun _ _ -> this.ConvOvf TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I4 -> (fun _ _ -> this.ConvOvf TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I8 -> (fun _ _ -> this.ConvOvf TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I -> (fun _ _ -> convi) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U1 -> (fun _ _ -> this.ConvOvf TypeUtils.uint8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U2 -> (fun _ _ -> this.ConvOvf TypeUtils.uint16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U4 -> (fun _ _ -> this.ConvOvf TypeUtils.uint32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U8 -> (fun _ _ -> this.ConvOvf TypeUtils.uint64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U -> (fun _ _ -> convu) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I1_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I2_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I4_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I8_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.int64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_I_Un -> (fun _ _ -> convi) |> fallThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U1_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint8Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U2_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint16Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U4_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint32Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U8_Un -> (fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.uint64Type) |> forkThrough cfg offset cilState
            | OpCodeValues.Conv_Ovf_U_Un -> (fun _ _ -> convu) |> fallThrough cfg offset cilState
            | OpCodeValues.Castclass -> this.CastClass |> forkThrough cfg offset cilState
            | OpCodeValues.Ldlen -> (fun _ _ -> this.LdLen) |> forkThrough cfg offset cilState
            | OpCodeValues.Ldvirtftn -> this.LdVirtFtn |> forkThrough cfg offset cilState
            | OpCodeValues.Box -> this.Box |> forkThrough cfg offset cilState
            | OpCodeValues.Unbox -> this.Unbox |> forkThrough cfg offset cilState
            | OpCodeValues.Unbox_Any -> this.UnboxAny |> forkThrough cfg offset cilState
            | OpCodeValues.Add_Ovf_Un -> (fun _ _ -> this.Add_ovf_un) |> forkThrough cfg offset cilState
            | OpCodeValues.Sub_Ovf_Un -> (fun _ _ -> this.Sub_ovf_un) |> forkThrough cfg offset cilState
            | OpCodeValues.Mul_Ovf_Un -> (fun _ _ -> this.Mul_ovf_un) |> forkThrough cfg offset cilState
            | OpCodeValues.Add_Ovf -> (fun _ _ -> this.Add_ovf) |> forkThrough cfg offset cilState
            | OpCodeValues.Sub_Ovf -> (fun _ _ -> this.Sub_ovf) |> forkThrough cfg offset cilState
            | OpCodeValues.Mul_Ovf -> (fun _ _ -> this.Mul_ovf) |> forkThrough cfg offset cilState
            | OpCodeValues.Div -> (fun _ _ -> this.Div) |> forkThrough cfg offset cilState
            | OpCodeValues.Div_Un -> (fun _ _ -> this.DivUn) |> forkThrough cfg offset cilState
            | OpCodeValues.Rem -> (fun _ _ -> this.Rem) |> forkThrough cfg offset cilState
            | OpCodeValues.Rem_Un -> (fun _ _ -> this.RemUn) |> forkThrough cfg offset cilState
            | OpCodeValues.Newarr -> this.Newarr |> forkThrough cfg offset cilState
            | OpCodeValues.Throw -> this.Throw cilState
            | _ -> __unreachable__()

        let originLevel = levelToUnsignedInt cilState.level
        assert(List.forall (fun s -> levelToUnsignedInt s.level = originLevel) newSts)
        let renewInstructionsInfo cilState =
            if not <| isError cilState then
                x.IncrementLevelIfNeeded cfg offset cilState
        newSts |> List.iter renewInstructionsInfo
        newSts
