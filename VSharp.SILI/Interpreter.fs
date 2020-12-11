namespace VSharp.Interpreter.IL

open System.Collections.Generic
open System
open System.Reflection
open System.Reflection.Emit
open InstructionsSet
open VSharp
open VSharp.Core
open VSharp.Reflection

type cfg = CFG.cfgData

type public CodePortionInterpreter(ilInterpreter : ILInterpreter, codeLoc : ICodeLocation, cfg : cfg, rv : int list) =
    inherit VSharp.Core.InterpreterBase<cilState>()
    let mutable results : cilState list = []
    let workingSet = List<cilState>()
    let exceptionsSet = List<cilState>()

    override x.MakeRecursiveState cilState =
        let methodId = ilInterpreter.MakeMethodIdentifier cfg.methodBase
        let ilCodePortion = ILCodePortion(cilState.ip.Offset(), methodId, cilState.state)
        ilInterpreter.ReproduceEffect ilCodePortion cilState.state (List.map (fun (_, state) -> {cilState with state = state}))

    member x.Invoke state k =
        let getResultsAndStates = function
            | [] -> internalfail "Exception handling is not implemented!" // TODO: __unreachable__()
            | cilStates -> List.map (fun (st : cilState) -> st.state.returnRegister |?? Nop, st.state) cilStates
        let interpret state curV targetV rvs =
            cilState.MakeEmpty curV state
            |> x.Interpret
            |> getResultsAndStates
        match codeLoc with
        | :? ILMethodMetadata ->
            ilInterpreter.InitializeStatics state cfg.methodBase.DeclaringType (List.map (fun state ->
            interpret state (Instruction 0) ip.Exit []) >> List.concat >> k)
        | _ -> __notImplemented__()
    override x.MakeEpsilonState (ist : cilState) = internalfail "Explore in isolation is irrelevant"

    override x.EvaluateOneStep cilState =
        let allStates = ilInterpreter.ExecuteAllInstructions cfg {cilState with isCompleted = false}
        let errors = allStates |> List.filter (fun (cilState : cilState) -> cilState.HasException)
        exceptionsSet.AddRange( errors)
        let completedStates = allStates |> List.filter (fun (cilState : cilState) -> cilState.isCompleted && not <| cilState.HasException)
        completedStates

    override x.IsRecursiveState cilState = false
    override x.Add cilState = if cilState.ip <> ip.Exit then workingSet.Add cilState
    override x.ExploreInIsolation cilState = internalfail "Explore in isolation is irrelevant"
    override x.HasNextState () = workingSet.Count <> 0
    override x.FindSimilar cilState =
        let areCapableForMerge (st1 : cilState) (st2 : cilState) =  st1.state.opStack = st2.state.opStack && st1.ip = st2.ip
        match Seq.tryFindIndex (areCapableForMerge cilState) workingSet with
        | None -> None
        | Some i -> let res = Some workingSet.[i]
                    workingSet.RemoveAt i
                    res
    override x.GetResultStates () = results
    override x.SetResultState newRes = results <- newRes :: results
    override x.IsResultState cilState =
        match results with
        | [] -> cilState.isCompleted && cilState.ip = Exit
        | result :: _ -> cilState.isCompleted && result.ip = cilState.ip && result.state.opStack = cilState.state.opStack
    override x.PickNext () =
        let st = workingSet.[0]
        workingSet.RemoveAt 0
        st

and public ILInterpreter() as this =
    inherit VSharp.Core.ExplorerBase()
    do
        opcode2Function.[hashFunction OpCodes.Call]           <- zipWithOneOffset <| this.Call
        opcode2Function.[hashFunction OpCodes.Callvirt]       <- zipWithOneOffset <| this.CallVirt
        opcode2Function.[hashFunction OpCodes.Newobj]         <- zipWithOneOffset <| this.NewObj
        opcode2Function.[hashFunction OpCodes.Ldsfld]         <- zipWithOneOffset <| this.LdsFld false
        opcode2Function.[hashFunction OpCodes.Ldsflda]        <- zipWithOneOffset <| this.LdsFld true
        opcode2Function.[hashFunction OpCodes.Stsfld]         <- zipWithOneOffset <| this.StsFld
        opcode2Function.[hashFunction OpCodes.Ldfld]          <- zipWithOneOffset <| this.LdFld false
        opcode2Function.[hashFunction OpCodes.Ldflda]         <- zipWithOneOffset <| this.LdFld true
        opcode2Function.[hashFunction OpCodes.Stfld]          <- zipWithOneOffset <| this.StFld
        opcode2Function.[hashFunction OpCodes.Ldelem]         <- zipWithOneOffset <| this.LdElem
        opcode2Function.[hashFunction OpCodes.Ldelem_I1]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int8Type
        opcode2Function.[hashFunction OpCodes.Ldelem_I2]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int16Type
        opcode2Function.[hashFunction OpCodes.Ldelem_I4]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Ldelem_I8]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Ldelem_R4]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.float32TermType
        opcode2Function.[hashFunction OpCodes.Ldelem_R8]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.float64TermType
        opcode2Function.[hashFunction OpCodes.Ldelem_U1]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.uint8Type
        opcode2Function.[hashFunction OpCodes.Ldelem_U2]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.uint16Type
        opcode2Function.[hashFunction OpCodes.Ldelem_U4]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.uint32Type
        opcode2Function.[hashFunction OpCodes.Ldelem_Ref]     <- zipWithOneOffset <| fun _ _ -> this.LdElemRef
        opcode2Function.[hashFunction OpCodes.Stelem]         <- zipWithOneOffset <| this.StElem
        opcode2Function.[hashFunction OpCodes.Stelem_I1]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int8Type
        opcode2Function.[hashFunction OpCodes.Stelem_I2]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int16Type
        opcode2Function.[hashFunction OpCodes.Stelem_I4]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Stelem_I8]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Stelem_R4]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.float32TermType
        opcode2Function.[hashFunction OpCodes.Stelem_R8]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.float64TermType
        opcode2Function.[hashFunction OpCodes.Stelem_Ref]     <- zipWithOneOffset <| fun _ _ -> this.StElemRef
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I1]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int8Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I2]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int16Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int32Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int64Type TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U1]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint8Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U2]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint16Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint32Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint64Type TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I1_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int8Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I2_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int16Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int32Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.int64Type TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U1_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint8Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U2_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint16Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint32Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.uint64Type TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I]     <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4]    opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8]
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I_Un]  <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4_Un] opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8_Un]
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U]     <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4]    opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8]
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U_Un]  <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4_Un] opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8_Un]
        opcode2Function.[hashFunction OpCodes.Castclass]      <- zipWithOneOffset <| this.CastClass
        opcode2Function.[hashFunction OpCodes.Ldlen]          <- zipWithOneOffset <| fun _ _ -> this.LdLen
        opcode2Function.[hashFunction OpCodes.Ldvirtftn]      <- zipWithOneOffset <| this.LdVirtFtn
        opcode2Function.[hashFunction OpCodes.Box]            <- zipWithOneOffset <| this.Box
        opcode2Function.[hashFunction OpCodes.Unbox]          <- zipWithOneOffset <| this.Unbox
        opcode2Function.[hashFunction OpCodes.Unbox_Any]      <- zipWithOneOffset <| this.UnboxAny
        opcode2Function.[hashFunction OpCodes.Add_Ovf_Un]     <- zipWithOneOffset <| fun _ _ -> this.Add_ovf_un
        opcode2Function.[hashFunction OpCodes.Sub_Ovf_Un]     <- zipWithOneOffset <| fun _ _ -> this.Sub_ovf_un
        opcode2Function.[hashFunction OpCodes.Mul_Ovf_Un]     <- zipWithOneOffset <| fun _ _ -> this.Mul_ovf_un
        opcode2Function.[hashFunction OpCodes.Add_Ovf]        <- zipWithOneOffset <| fun _ _ -> this.Add_ovf
        opcode2Function.[hashFunction OpCodes.Sub_Ovf]        <- zipWithOneOffset <| fun _ _ -> this.Sub_ovf
        opcode2Function.[hashFunction OpCodes.Mul_Ovf]        <- zipWithOneOffset <| fun _ _ -> this.Mul_ovf
        opcode2Function.[hashFunction OpCodes.Div]            <- zipWithOneOffset <| fun _ _ -> this.Div
        opcode2Function.[hashFunction OpCodes.Div_Un]         <- zipWithOneOffset <| fun _ _ -> this.DivUn
        opcode2Function.[hashFunction OpCodes.Rem]            <- zipWithOneOffset <| fun _ _ -> this.Rem
        opcode2Function.[hashFunction OpCodes.Rem_Un]         <- zipWithOneOffset <| fun _ _ -> this.RemUn
        opcode2Function.[hashFunction OpCodes.Newarr]         <- zipWithOneOffset <| this.Newarr
    let cfgs = Dictionary<ILMethodMetadata, cfg>()
    let findCfg (ilmm : ILMethodMetadata) =
        Dict.getValueOrUpdate cfgs ilmm (fun () -> CFG.build ilmm.methodBase)
    let internalImplementations : Map<string, (state -> term option -> term list -> state list)> =
        Map.ofList [
            "System.Int32 System.Array.GetLength(this, System.Int32)", this.CommonGetArrayLength
            "System.Int32 System.Array.GetLowerBound(this, System.Int32)", this.GetArrayLowerBound
            "System.Void System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)", this.CommonInitializeArray
        ]
    let __corruptedStack__() = raise (InvalidProgramException())

    member private x.Raise createException (state : state) k =
        //TODO: exception handling
        let statesWithCreatedExceptions : state list = createException state
        k statesWithCreatedExceptions

    member private x.AccessArray accessor (state : state) upperBound index k =
        let checkArrayBounds upperBound x =
            let lowerBound = Concrete 0 Types.TLength
            let notTooSmall = Arithmetics.(>>=) x lowerBound
            let notTooLarge = Arithmetics.(<<) x upperBound
            notTooSmall &&& notTooLarge
        StatedConditionalExecutionAppendResults state
            (fun state k -> k (checkArrayBounds upperBound index, state))
            accessor
            (x.Raise x.IndexOutOfRangeException)
            k
    member private x.AccessArrayDimension accessor (state : state) (this : term) (dimension : term) =
//            let array = Memory.ReadSafe cilState.state this
        let upperBound = Memory.ArrayRank state this
        x.AccessArray (accessor this dimension) state upperBound dimension id
    member private x.CommonGetArrayLength (state : state) thisOption args =
        match args with
        | dimensionsKey :: [] ->
            let arrayLengthByDimension arrayRef index state (k : state list -> 'a) =
                k [{state with returnRegister = Some <| Memory.ArrayLengthByDimension state arrayRef index }]
            x.AccessArrayDimension arrayLengthByDimension state (Option.get thisOption) dimensionsKey
        | _ -> internalfail "unexpected number of arguments"

    member private x.GetArrayLowerBound (state : state) (this : term option) args =
        match args with
        | dimension :: [] ->
            let arrayLowerBoundByDimension arrayRef index (state : state) k =
                k [{state with returnRegister = Some <| Memory.ArrayLowerBoundByDimension state arrayRef index }]
            x.AccessArrayDimension arrayLowerBoundByDimension state (Option.get this) dimension
        | _ -> internalfail "unexpected number of arguments"

    member private x.NpeOrInvokeStatement (state : state) (this : term) statement (k : state list -> 'a) =
         BranchOnNull state this
            (x.Raise x.NullReferenceException)
            statement
            k

    member private x.InitializeArray (cilState : cilState) = __notImplemented__
    member private x.CommonInitializeArray (state : state) thisOption (args : term list) =
        match args with
        | arrayRef :: handleTerm :: [] ->
            x.NpeOrInvokeStatement state arrayRef (fun state k ->
            x.NpeOrInvokeStatement state handleTerm (fun state k ->
            let results : state list = VSharp.System.Runtime_CompilerServices_RuntimeHelpers.InitializeArray state arrayRef handleTerm
            k results) k) id
        | _ -> internalfail "unexpected number of arguments"
    member private x.ReduceMethodBaseCall (methodBase : MethodBase) (initialState : state) (k : state list -> 'a) =
        let state = { initialState with opStack = [] }
        let k =
            let restoreOpStack state = { state with opStack = initialState.opStack }
            List.map Memory.PopStack >> List.map restoreOpStack >> k
        let dealWithResult (term : term, state : state) =
            if term <> Nop then {state with returnRegister = Some term}
            else {state with returnRegister = None}
        let thisOption = if methodBase.IsStatic then None else Some <| Memory.ReadThis state methodBase
        let args = methodBase.GetParameters() |> Seq.map (Memory.ReadArgument state) |> List.ofSeq
        let fullMethodName = Reflection.GetFullMethodName methodBase
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        if Map.containsKey fullMethodName internalImplementations then
            (internalImplementations.[fullMethodName] state thisOption args) |> k
        elif Map.containsKey fullMethodName Loader.internalImplementations then
            let thisAndArguments : term list =
                match thisOption with
                | None -> args
                | Some this -> this :: args
            internalCall Loader.internalImplementations.[fullMethodName] thisAndArguments state k
        elif Map.containsKey fullMethodName Loader.concreteExternalImplementations then
//            match args with
//            | Specified parameters ->
//                let parameters' = optCons parameters this
//                x.ReduceFunction state None (Specified parameters') methodId methodInfo invoke k2
//            | _ -> internalfail "internal call with unspecified parameters!"
            // TODO: check that all parameters were specified
            let methodInfo = Loader.concreteExternalImplementations.[fullMethodName]
            let methodId = x.MakeMethodIdentifier methodInfo
            let invoke state k = x.Invoke methodId state k
            x.ReduceFunction state methodId invoke (List.map dealWithResult >> k)
        elif int (methodBase.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall) <> 0 then
            internalfailf "new extern method: %s" fullMethodName
        elif methodBase.GetMethodBody() <> null then
            x.ReduceConcreteCall methodBase state (List.map dealWithResult >> k)
        else
            internalfail "nonextern method without body!"

    member x.CallMethodFromTermType (state : state) (*this parameters *) termType (calledMethod : MethodInfo) (k : state list -> 'a) =
        let t = termType |> Types.ToDotNetType
        let genericCalledMethod = if calledMethod.IsGenericMethod then calledMethod.GetGenericMethodDefinition() else calledMethod
        let genericMethodInfo =
            match genericCalledMethod.DeclaringType with
            | t1 when t1.IsInterface ->
                let createSignature (m : MethodInfo) =
                    m.GetParameters() |> Seq.map (fun p -> (p.ParameterType |> safeGenericTypeDefinition).FullName)
                    |> join ","
                let onlyLastName (m : MethodInfo) =
                    match m.Name.LastIndexOf('.') with
                    | i when i < 0 -> m.Name
                    | i -> m.Name.Substring(i + 1)
                let sign = createSignature genericCalledMethod
                let lastName = onlyLastName genericCalledMethod
                let methods =
                    match t with
                    | _ when t.IsArray -> t.GetMethods()
                    | _ -> t.GetInterfaceMap(t1).TargetMethods
                methods |> Seq.find (fun mi -> createSignature mi = sign && onlyLastName mi = lastName)
            | _ ->
                let (|||) = Microsoft.FSharp.Core.Operators.(|||)
                let allMethods = t.GetMethods(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                Seq.find (fun (mi : MethodInfo) -> mi.GetBaseDefinition() = genericCalledMethod.GetBaseDefinition()) allMethods
        let targetMethod = if genericMethodInfo.IsGenericMethod then genericMethodInfo.MakeGenericMethod(calledMethod.GetGenericArguments()) else genericMethodInfo
        if targetMethod.IsAbstract
            then x.CallAbstract (x.MakeMethodIdentifier targetMethod) state k
            else
                x.ReduceMethodBaseCall targetMethod state k

    member x.CallVirtualMethod (ancestorMethod : MethodInfo) (state : state) (k : state list -> 'a) =
        __notImplemented__()

//        let methodId = x.MakeMethodIdentifier ancestorMethod
//        let this = Memory.ReadLocalVariable state (ThisKey ancestorMethod)
//        let callVirtual cilState this k =
//            let baseType = BaseTypeOfHeapRef state this
////            let sightType = SightTypeOfRef this
//            let callForConcreteType typ state k =
//                x.CallMethodFromTermType state typ ancestorMethod k
//            let tryToCallForBaseType cilState =
//                StatedConditionalExecutionCIL cilState
//                    (fun state k -> k (API.Types.TypeIsRef baseType this &&& API.Types.TypeIsType baseType sightType, state))
//                    (callForConcreteType baseType)
//                    (x.CallAbstract funcId)
//            let tryToCallForSightType cilState =
//                StatedConditionalExecutionCIL cilState
//                    (fun state k -> k (API.Types.TypeIsRef sightType this, state))
//                    (callForConcreteType sightType)
//                    tryToCallForBaseType
//            let sightDotNetType = Types.ToDotNetType sightType
//            let baseDotNetType = Types.ToDotNetType baseType
//            if sightDotNetType.IsInterface && baseDotNetType.IsInterface
//                then x.CallAbstract funcId cilState k
//                else tryToCallForSightType cilState k
//        GuardedApply cilState this callVirtual k

    member x.CallAbstract funcId state k =
        x.CallAbstractMethod funcId state (fun (result, state) ->
             // TODO: get rid of this copy-paste from
            let state =
                match result.term with
                | Nop -> state
                | _ -> withResultState result state
            k [state])

    member private x.ConvOvf targetType typeForStack (cilState : cilState) = // TODO: think about getting rid of typeForStack
        let typIsLessTyp : System.Collections.Generic.Dictionary<symbolicType, list<symbolicType>> = Dictionary<_,_>()
        typIsLessTyp.[TypeUtils.int8Type] <- [TypeUtils.int8Type; TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type]
        typIsLessTyp.[TypeUtils.int16Type] <- [TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type]
        typIsLessTyp.[TypeUtils.int32Type] <- [TypeUtils.int32Type; TypeUtils.int64Type]
        typIsLessTyp.[TypeUtils.int64Type] <- [TypeUtils.int64Type]

        typIsLessTyp.[TypeUtils.uint8Type] <- [TypeUtils.uint8Type; TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type]
        typIsLessTyp.[TypeUtils.uint16Type] <- [TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type]
        typIsLessTyp.[TypeUtils.uint32Type] <- [TypeUtils.uint32Type; TypeUtils.uint64Type]
        typIsLessTyp.[TypeUtils.uint64Type] <- [TypeUtils.uint64Type]
        let less leftTyp rightTyp = List.contains rightTyp typIsLessTyp.[leftTyp]

        let minMax : System.Collections.Generic.Dictionary<symbolicType, int64 * int64> = Dictionary<_,_>()
        minMax.[TypeUtils.int8Type] <- (System.SByte.MinValue |> int64, System.SByte.MaxValue |> int64)
        minMax.[TypeUtils.int16Type] <- (System.Int16.MinValue |> int64, System.Int16.MaxValue |> int64)
        minMax.[TypeUtils.int32Type] <- (System.Int32.MinValue |> int64, System.Int32.MaxValue |> int64)
        minMax.[TypeUtils.int64Type] <- (System.Int64.MinValue, System.Int64.MaxValue)
        minMax.[TypeUtils.uint8Type] <- (System.Byte.MinValue |> int64, System.Byte.MaxValue |> int64)
        minMax.[TypeUtils.uint16Type] <- (System.UInt16.MinValue |> int64, System.UInt16.MaxValue |> int64)
        minMax.[TypeUtils.uint32Type] <- (System.UInt32.MinValue |> int64, System.UInt32.MaxValue |> int64)
        minMax.[TypeUtils.uint64Type] <- (System.UInt64.MinValue |> int64, System.UInt64.MaxValue |> int64)


        let getSegment leftTyp rightTyp =
            let min1, max1 = minMax.[leftTyp]
            let min2, max2 = minMax.[rightTyp]
            match min1 < min2, max1 < max2 with
            | true, true   -> min2, max1
            | true, false  -> min2, max2
            | false, true  -> min1, max1
            | false, false -> min1, max2

        let canCastWithoutOverflow term targetTermType =
            let (<<=) = API.Arithmetics.(<<=)
            assert(TypeUtils.isInteger term)
            let termType = Terms.TypeOf term
            if less termType targetTermType then True
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
        match cilState.state.opStack with
        | t :: stack ->
            let castForStack results =
                mapAndPushFunctionResultsk (fun (term, state) -> castUnchecked typeForStack term state, state) results id
            StatedConditionalExecutionCIL (withOpStack stack cilState)
                (fun state k -> k (canCastWithoutOverflow t targetType, state))
                (fun cilState k -> k [Types.Cast t targetType, cilState])
                (fun (cilState : cilState) k -> x.Raise x.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                castForStack
        | _ -> __corruptedStack__()
    member private x.ConvOvfUn unsignedSightType targetType typeForStack (cilState : cilState) = // TODO: think about getting rid of typeForStack
        match cilState.state.opStack with
        | t :: stack ->
            let unsignedT = castUnchecked unsignedSightType t cilState.state
            x.ConvOvf targetType typeForStack (withOpStack (unsignedT::stack) cilState)
        | _ -> __corruptedStack__()
    member private x.CommonCastClass (state : state) (term : term) (typ : symbolicType) k =
        let term = castReferenceToPointerIfNeeded term typ state
        StatedConditionalExecutionAppendResults state
            (fun state k -> k (IsNullReference term ||| Types.IsCast typ term, state))
            (fun state k -> k [{state with returnRegister = Some <| Types.Cast term typ}])
            (x.Raise x.InvalidCastException)
            k
    member private x.CastClass (cfg : cfg) offset (cilState : cilState) : cilState list =
        match cilState.state.opStack with
        | term :: stack ->
            let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Castclass.Size)
            let state = {cilState.state with opStack = stack}
            x.CommonCastClass state term typ (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()

    member x.CommonCall (calledMethodBase : MethodBase) (state : state) (k : state list -> 'a) =
        let call state k =
            x.InitializeStatics state calledMethodBase.DeclaringType (List.map (fun state ->
            x.ReduceMethodBaseCall calledMethodBase state id) >> List.concat >> k)
        match calledMethodBase.IsStatic with
        | true -> call state k
        | false ->
            let this = Memory.ReadThis state calledMethodBase
            x.NpeOrInvokeStatement state this call k
    member x.Call (cfg : cfg) offset (cilState : cilState) =
        let calledMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Call.Size)
        let args, cilState = retrieveActualParameters calledMethodBase cilState
        let this, cilState = if not calledMethodBase.IsStatic then popOperationalStack cilState else None, cilState
        x.ReduceFunctionSignature cilState.state calledMethodBase this (Specified args) false (fun state ->
        x.CommonCall calledMethodBase state (pushResultFromStateToCilState cilState))
     member x.CommonCallVirt (ancestorMethodBase : MethodBase) stateWithArgsOnFrame k =
        let this = Memory.ReadThis stateWithArgsOnFrame ancestorMethodBase
        let call (state : state) k =
            x.InitializeStatics state ancestorMethodBase.DeclaringType (List.map (fun state ->
            if ancestorMethodBase.DeclaringType.IsSubclassOf typedefof<System.Delegate> then
                Lambdas.invokeDelegate state this id
            elif ancestorMethodBase.IsVirtual && not ancestorMethodBase.IsFinal then
                let methodInfo = ancestorMethodBase :?> MethodInfo
                x.CallVirtualMethod methodInfo state id
            else
                x.ReduceMethodBaseCall ancestorMethodBase state id) >> List.concat >> k)
        x.NpeOrInvokeStatement stateWithArgsOnFrame this call k
    member x.CallVirt (cfg : cfg) offset (cilState : cilState) =
        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Callvirt.Size)
        let args, cilState = retrieveActualParameters ancestorMethodBase cilState
        let this, cilState = popOperationalStack cilState |> mapfst Option.get
        // NOTE: It is not quite strict to ReduceFunctionSignature here because, but it does not matter because signatures of virtual methods are the same
        x.ReduceFunctionSignature cilState.state ancestorMethodBase (Some this) (Specified args) false (fun state ->
        x.CommonCallVirt ancestorMethodBase state (pushResultFromStateToCilState cilState))
    member x.ReduceArrayCreation (arrayType : Type) (methodBase : MethodBase) (state : state) (parameters : term list) k =
        let arrayTyp = Types.FromDotNetType state arrayType
        let reference, state = Memory.AllocateDefaultArray state parameters arrayTyp
        withResultState reference state |> List.singleton |> k
    member x.CommonCreateDelegate (ctor : ConstructorInfo) (state : state) (args : term list) (k : state list -> 'a) =
        let target, methodPtr =
            assert(List.length args = 2)
            args.[0], args.[1]

        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? MethodInfo as mi, _) -> mi
            | _ -> __unreachable__()

        let invoke state =
            GuardedApplyForState state methodPtr
                (fun state methodPtr k ->
                    BranchOnNull state target
                        (x.Raise x.NullReferenceException)
                        (x.ReduceMethodBaseCall (retrieveMethodInfo methodPtr))
                        k)

        let typ = Types.FromDotNetType state ctor.DeclaringType
        Lambdas.make invoke typ (fun lambda ->
        let deleg, state = Memory.AllocateDelegate state lambda
        withResultState deleg state |> List.singleton |> k)
    member private x.CreateDelegate (ctor : ConstructorInfo) (cilState : cilState) k =
        match cilState.state.opStack with
        | methodPtr :: target :: stack ->
            let state = {cilState.state with opStack = stack}
            x.CommonCreateDelegate ctor state [methodPtr; target] (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()
    member x.CommonNewObj isCallNeeded (constructorInfo : ConstructorInfo) (state : state) (args : term list) (k : state list -> 'a) : 'a =
        let typ = constructorInfo.DeclaringType
        let constructedTermType = typ |> Types.FromDotNetType state
        let blockCase (state : state) =
            let callConstructor (state : state) reference afterCall =
                if isCallNeeded then
                    x.ReduceFunctionSignature state constructorInfo (Some reference) (Specified args) false (fun state ->
                    x.ReduceMethodBaseCall constructorInfo state afterCall)
                else withResultState reference state |> List.singleton
            let referenceTypeCase (state : state) =
                let ref, state = Memory.AllocateDefaultClass state constructedTermType
                callConstructor state ref (List.map (withResultState ref))
            let valueTypeCase (state : state) =
                let freshValue = Memory.DefaultOf constructedTermType
                let ref, state = Memory.AllocateTemporaryLocalVariable state typ freshValue
                let modifyResult state =
                    let value = Memory.ReadSafe state ref
                    withResultState value state
                callConstructor state ref (List.map modifyResult)
            if Types.IsValueType constructedTermType then valueTypeCase state
            else referenceTypeCase state
        let nonDelegateCase (state : state) =
            x.InitializeStatics state typ (List.map (fun state ->
            if typ.IsArray && constructorInfo.GetMethodBody() = null
                then x.ReduceArrayCreation typ constructorInfo state args id
                else blockCase state) >> List.concat)
        if Reflection.IsDelegateConstructor constructorInfo
            then x.CommonCreateDelegate constructorInfo state args k
            else nonDelegateCase state |> k

    member x.NewObj (cfg : cfg) offset (cilState : cilState) : cilState list =
        let constructorInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Newobj.Size) :?> ConstructorInfo
        assert (constructorInfo.IsConstructor)
        let args, cilState = retrieveActualParameters constructorInfo cilState
        x.CommonNewObj true constructorInfo cilState.state args (pushResultFromStateToCilState cilState)

    member x.LdsFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldsfld.Size)
        assert (fieldInfo.IsStatic)
        x.InitializeStatics cilState.state fieldInfo.DeclaringType (List.map (fun state ->
        let declaringTermType = fieldInfo.DeclaringType |> Types.FromDotNetType state
        let fieldId = wrapField fieldInfo
        let value = if addressNeeded then StaticField(declaringTermType, fieldId) |> Ref else Memory.ReadStaticField state declaringTermType fieldId
        pushResultOnStack cilState (value, state) :: []) >> List.concat)
    member private x.StsFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stsfld.Size)
        let state = cilState.state
        assert (fieldInfo.IsStatic)
//        let fieldType = fieldInfo.FieldType |> Types.FromDotNetType state
        let declaringTermType = fieldInfo.DeclaringType |> Types.FromDotNetType state
        let fieldId = wrapField fieldInfo
//        let address = Memory.ReferenceStaticField declaringTermType fullName fieldType
        match cilState.state.opStack with
        | value :: stack ->
            x.InitializeStatics state fieldInfo.DeclaringType (List.map (fun state ->
            let fieldType = fieldInfo.FieldType |> Types.FromDotNetType state
            let value = castUnchecked fieldType value state
            let state = Memory.WriteStaticField state declaringTermType fieldId value
            cilState |> withState state |> withOpStack stack))
        | _ -> __corruptedStack__()
    member x.LdFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldfld.Size)
        assert (not fieldInfo.IsStatic)
        match cilState.state.opStack with
        | target :: stack ->
            let loadWhenTargetIsNotNull (state : state) k =
                let k1 value = k [{state with returnRegister = Some value}]
                let fieldId = wrapField fieldInfo
                if addressNeeded then Memory.ReferenceField target fieldId |> k1
                else Memory.ReadField state target fieldId |> k1
            let state = {cilState.state with opStack = stack}
            x.NpeOrInvokeStatement state target loadWhenTargetIsNotNull (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()
    member x.StFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stfld.Size)
        assert (not fieldInfo.IsStatic)
        match cilState.state.opStack with
        | value :: targetRef :: stack ->
            let storeWhenTargetIsNotNull (state : state) k =
                let fieldType = fieldInfo.FieldType |> Types.FromDotNetType state
                let fieldId = wrapField fieldInfo
                let reference = Memory.ReferenceField targetRef fieldId
                let value = castUnchecked fieldType value state
//                let states = Memory.WriteClassField state targetRef fieldId value
                Memory.WriteSafe state reference value |> k
            let state = {cilState.state with opStack = stack}
            x.NpeOrInvokeStatement state targetRef storeWhenTargetIsNotNull (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()
    member private x.LdElemWithCast cast (cilState : cilState) : cilState list =
        match cilState.state.opStack with
        | index :: arrayRef :: stack ->
            let uncheckedLdElem (state : state) k =
//                let reference = Memory.ReferenceArrayIndex cilState.state arrayRef [index]
//                let value = Memory.ReadSafe cilState.state reference
                let value = Memory.ReadArrayIndex state arrayRef [index]
                let castedValue = cast value cilState.state
                k [{state with returnRegister = Some castedValue}]
            let checkedLdElem (state : state) k =
//                let array = Memory.ReadSafe cilState.state arrayRef
//                let length = Memory.ArrayLength array
                let length = Memory.ArrayLengthByDimension state arrayRef (MakeNumber 0)
                x.AccessArray uncheckedLdElem state length index k
            let state = {cilState.state with opStack = stack}
            x.NpeOrInvokeStatement state arrayRef checkedLdElem (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()
    member private x.LdElemTyp typ (cilState : cilState) = x.LdElemWithCast (castUnchecked typ) cilState
    member private x.LdElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Ldelem.Size)
        x.LdElemTyp typ cilState
    member private x.LdElemRef = x.LdElemWithCast always
    member private x.StElemWithCast cast (cilState : cilState) =
        match cilState.state.opStack with
        | value :: index :: arrayRef :: stack ->
            let checkedStElem (state : state) (k : state list -> 'a) =
                let typeOfValue = TypeOf value
                let uncheckedStElem (state : state) (k : state list -> 'a) =
                    let typedValue = cast value state
                    k <| Memory.WriteArrayIndex state arrayRef [index] typedValue
//                    k (states |> List.map (fun s -> value, {cilState with state = s}))
//                    k [t, {cilState with state = state}]
                let checkTypeMismatchBasedOnTypeOfValue cond (state : state) =
                    StatedConditionalExecutionAppendResults state
                        (fun state k -> k (cond, state))
                        uncheckedStElem
                        (fun (state : state) k -> x.Raise x.ArrayTypeMismatchException state k)
//                let reference = Memory.ReferenceArrayIndex arrayRef [index]
                let rec checkTypeMismatch (state : state) (k : state list -> 'a) =
                    let baseType = arrayRef |> BaseTypeOfHeapRef state |> Types.ElementType
                    if Types.IsValueType typeOfValue then
                        checkTypeMismatchBasedOnTypeOfValue (Types.TypeIsType typeOfValue baseType) state k
//                        checkTypeMismatchBasedOnTypeOfValue (Types.TypeIsRef typeOfValue reference) cilState k
                    else
                        checkTypeMismatchBasedOnTypeOfValue (Types.RefIsType value baseType) state k
//                        checkTypeMismatchBasedOnTypeOfValue (Types.RefIsRef value reference) cilState k
                let length = Memory.ArrayLengthByDimension state arrayRef (MakeNumber 0)
                x.AccessArray checkTypeMismatch state length index k
            x.NpeOrInvokeStatement cilState.state arrayRef checkedStElem (List.map (fun state -> cilState |> withState state |> withOpStack stack))
        | _ -> __corruptedStack__()
    member private x.StElemTyp typ (cilState : cilState) =
        x.StElemWithCast (castUnchecked typ) cilState
    member private x.StElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Stelem.Size)
        x.StElemTyp typ cilState
    member private x.StElemRef = x.StElemWithCast always
    member private x.LdLen (cilState : cilState) =
        match cilState.state.opStack with
        | arrayRef :: stack ->
            let ldlen (state : state) k =
                let length = Memory.ArrayLengthByDimension state arrayRef (MakeNumber 0)
                k [{state with returnRegister = Some length}]
            let state = {cilState.state with opStack = stack}
            x.NpeOrInvokeStatement state arrayRef ldlen (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()
    member private x.LdVirtFtn (cfg : cfg) offset (cilState : cilState) =
        __notImplemented__()
//        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Ldvirtftn.Size)
//        match cilState.opStack with
//        | this :: stack ->
//            let ldvirtftn (cilState : cilState) k =
//                assert(isReference this)
//                let t = this |> SightTypeOfRef |> Types.ToDotNetType
//                let methodInfo = t.GetMethod(ancestorMethodBase.Name, allBindingFlags)
//                let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType cilState.state (methodInfo.GetType()))
//                k [methodPtr, cilState]
//            x.NpeOrInvokeStatement {cilState with opStack = stack} this ldvirtftn pushFunctionResults
//        | _ -> __corruptedStack__()
    member x.BoxNullable (t : Type) v (cilState : cilState) : cilState list =
        // TODO: move it to Reflection.fs; add more validation in case if .NET implementation does not have these methods
        let hasValueMethodInfo = t.GetMethod("get_HasValue")
        let hasValueCase (state : state) k =
            let valueMethodInfo = t.GetMethod("get_Value")
            x.ReduceFunctionSignature state valueMethodInfo (Some v) (Specified []) false (fun state ->
            x.ReduceMethodBaseCall valueMethodInfo state k)
        let boxNullable (hasValue, state : state) (k : state list -> 'a) =
            StatedConditionalExecutionAppendResults state
                (fun state k -> k (hasValue, state))
                hasValueCase
                (fun state k -> k [{state with returnRegister = Some NullRef}])
                k

        x.ReduceFunctionSignature cilState.state hasValueMethodInfo (Some v) (Specified []) false (fun state ->
        x.ReduceMethodBaseCall hasValueMethodInfo state (fun hasValueResults ->
        let hasValueResults = hasValueResults |> List.map (fun (state : state) -> Option.get state.returnRegister, state)
        Cps.List.mapk boxNullable hasValueResults (List.concat >> pushResultFromStateToCilState cilState)))


    member x.Box (cfg : cfg) offset (cilState : cilState) =

        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Box.Size)
        let termType = Types.FromDotNetType cilState.state t
        match cilState.state.opStack with
        | v :: stack ->
            if Types.IsValueType termType then
                let cilState = withOpStack stack cilState
                if Types.TypeIsNullable termType then x.BoxNullable t v cilState
                else allocateValueTypeInHeap v cilState
            else [cilState]
        | _ -> __corruptedStack__()
    member private x.UnboxCommon (state : state) (obj : term) (t : System.Type) (handleRestResults : term * state -> term * state) (k : state list -> 'a) =
        let termType = Types.FromDotNetType state t
        assert(IsReference obj)
        assert(Types.IsValueType termType)
        let nullCase (state : state) (k : state list -> 'a) : 'a =
            if Types.TypeIsNullable termType then
                let nullableTerm = Memory.DefaultOf termType
                let address, state = Memory.BoxValueType state nullableTerm
                let res, state = handleRestResults (HeapReferenceToBoxReference address, state)
                k [{state with returnRegister = Some res}]
            else
                x.Raise x.NullReferenceException state k
//                StatedConditionalExecutionCIL cilState
//                    (fun state k -> k (Types.TypeIsNullable termType, state))
//                    (fun cilState k ->
//                        let address, state = Memory.AllocateDefaultBlock cilState.state termType
//                        k [handleRestResults(address, {cilState with state = state})])
//                    (x.Raise x.NullReferenceException)
        let canCastValueTypeToNullableTargetCase (state : state) =
            let underlyingTypeOfNullableT = Nullable.GetUnderlyingType t
            StatedConditionalExecutionAppendResults state
                (fun state k -> k (Types.RefIsType obj (Types.FromDotNetType state underlyingTypeOfNullableT), state))
                (fun state k ->
                    let value = Memory.ReadSafe state obj
                    let nullableTerm = Memory.DefaultOf termType
                    let valueField, hasValueField = Reflection.fieldsOfNullable t
                    let nullableTerm = Memory.WriteStructField nullableTerm valueField value
                    let nullableTerm = Memory.WriteStructField nullableTerm hasValueField (MakeBool true)
                    let address, state = Memory.BoxValueType state nullableTerm
                    let res, state = handleRestResults(address, state)
                    k [{state with returnRegister = Some res}])
//                        let nullableConstructor = t.GetConstructor([| underlyingTypeOfNullableT |])
//                        let modifyResults results = Cps.List.map (fun (_, cilState) -> handleRestResults (address, cilState)) results k
//                        x.ReduceMethodBaseCall nullableConstructor {cilState with state = state} (Some address) (Specified [value]) modifyResults)
                (fun (state : state) k -> x.Raise x.InvalidCastException state k)
//                StatedConditionalExecutionCIL cilState
//                    (fun state k -> k (Types.TypeIsNullable termType, state))
//                    canCastValueTypeToNullableTargetCase
//                    (fun cilState k -> k [handleRestResults(Types.Cast obj termType, cilState)])
        let nonNullCase (state : state) =
            if Types.TypeIsNullable termType then
                canCastValueTypeToNullableTargetCase state
            else
                StatedConditionalExecutionAppendResults state
                    (fun state k -> k (Types.IsCast termType obj, state)) // TODO: Why not Types.RefIsType method?
                    (fun state k ->
                        let res, state = handleRestResults(Types.Cast obj termType |> HeapReferenceToBoxReference, state)
                        k [{state with returnRegister = Some res}])
                    (fun (state : state) k -> x.Raise x.InvalidCastException state k)

        BranchOnNull state obj
            nullCase
            nonNullCase
            k

    member private x.Unbox (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox.Size)
        match cilState.state.opStack with
        | _ :: _ when t.IsGenericParameter -> __notImplemented__() // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        | obj :: stack ->
            let state = {cilState.state with opStack = stack}
            x.UnboxCommon state obj t id (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()

    member private x.UnboxAny (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox_Any.Size)
        let state = cilState.state
        let termType = Types.FromDotNetType state t
        let valueType = Types.FromDotNetType state typeof<System.ValueType>

        match cilState.state.opStack with
        | _ :: _ when t.IsGenericParameter -> __insufficientInformation__ "Can't introduce generic type X for equation: T = Nullable<X>"  // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        | obj :: stack ->
            let state = {state with opStack = stack}
            StatedConditionalExecutionAppendResults state
                (fun state k -> k (Types.TypeIsType termType valueType, state))
                (fun state k ->
                    let handleRestResults (address, state : state) = Memory.ReadSafe state address, state
                    x.UnboxCommon state obj t handleRestResults k)
                (fun state k -> x.CommonCastClass state obj termType k)
                (pushResultFromStateToCilState cilState)
        | _ -> __corruptedStack__()

    member private this.CommonDivRem performAction (cilState : cilState) =
        let integerCase (cilState : cilState) x y minusOne minValue =
            assert(TypeOf x = TypeOf y)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (fun (cilState : cilState) k -> this.Raise this.InvalidCastException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k ((x === minValue) &&& (y === minusOne), state))
                        (fun (cilState : cilState) k -> this.Raise this.InvalidCastException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                        (fun cilState k -> k [performAction x y, cilState]))
                pushFunctionResults
        match cilState.state.opStack with
        | TypeUtils.Float y :: TypeUtils.Float x :: stack ->
            cilState |> withOpStack (performAction x y :: stack) |> List.singleton
        | TypeUtils.Int64 y :: x :: stack
        | TypeUtils.UInt64 y :: x :: stack
        | y :: TypeUtils.Int64 x :: stack
        | y :: TypeUtils.UInt64 x :: stack ->
            integerCase (withOpStack stack cilState) x y TypeUtils.Int64.MinusOne TypeUtils.Int64.MinValue
        | y :: x :: stack ->
            integerCase (withOpStack stack cilState) x y TypeUtils.Int32.MinusOne TypeUtils.Int32.MinValue
        | _ -> __corruptedStack__()
    member private this.Div (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonDivRem div cilState

    member private this.Rem (cilState : cilState) =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonDivRem rem cilState

    member private this.CommonUnsignedDivRem isRem performAction (cilState : cilState) =
        match cilState.state.opStack with
        | y :: x :: stack when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            StatedConditionalExecutionCIL (withOpStack stack cilState)
                (fun state k -> k (Arithmetics.IsZero y, state))
                (fun (cilState : cilState) k -> this.Raise this.DivideByZeroException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                (fun cilState k -> k [performAction x y, cilState])
                pushFunctionResults
        | TypeUtils.Float _ :: _
        | _ :: TypeUtils.Float _ :: _ when isRem -> internalfailf "Rem.Un is unspecified for Floats"
        | _ -> __corruptedStack__()
    member private this.DivUn (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonUnsignedDivRem false div cilState

    member private this.RemUn cilState =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonUnsignedDivRem true rem cilState

    member private this.UnsignedCheckOverflow checkOverflowForUnsigned (cilState : cilState) =
        match cilState.state.opStack with
        | TypeUtils.Int64 y :: x :: stack
        | y :: TypeUtils.Int64 x :: stack
        | TypeUtils.UInt64 y :: x :: stack
        | y :: TypeUtils.UInt64 x :: stack ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            let max = TypeUtils.UInt64.MaxValue
            let zero = TypeUtils.UInt64.Zero
            checkOverflowForUnsigned zero max x y (withOpStack stack cilState)  // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | y :: x :: stack when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x, y = makeUnsignedInteger x id, makeUnsignedInteger y id
            let max = TypeUtils.UInt32.MaxValue
            let zero = TypeUtils.UInt32.Zero
            checkOverflowForUnsigned zero max x y (withOpStack stack cilState) // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> __corruptedStack__()
    member private this.SignedCheckOverflow checkOverflow (cilState : cilState) =
        match cilState.state.opStack with
        | TypeUtils.Int64 y :: x :: stack
        | y :: TypeUtils.Int64 x :: stack ->
            let min = TypeUtils.Int64.MinValue
            let max = TypeUtils.Int64.MaxValue
            let zero = TypeUtils.Int64.Zero
            let minusOne = TypeUtils.Int64.MinusOne
            checkOverflow min max zero minusOne x y (withOpStack stack cilState) // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | TypeUtils.UInt64 _ :: _ :: _
        | _ :: TypeUtils.UInt64 _ :: _ -> __unreachable__() // instead of add_ovf should be called add_ovf_un
        | TypeUtils.Float _ :: _
        | _ :: TypeUtils.Float _ :: _ -> __unreachable__() // only integers
        | y :: x :: stack ->
            let min = TypeUtils.Int32.MinValue
            let max = TypeUtils.Int32.MaxValue
            let zero = TypeUtils.Int32.Zero
            let minusOne = TypeUtils.Int32.MinusOne
            checkOverflow min max zero minusOne x y (withOpStack stack cilState) // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> __corruptedStack__()
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
                k [sum, cilState])
            StatedConditionalExecutionCIL cilState xMoreThan0
                (fun cilState -> // x >= 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        (fun cilState -> // y >= 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenMoreThan0
                                add
                                (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k)))
                        add)
                (fun cilState -> // x < 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        add
                        (fun cilState -> // x < 0 && y < 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenLessThan0
                                add
                                (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))))
                pushFunctionResults
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
                k [res, cilState])
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k -> k [zero, cilState])
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        xMoreThan0
                        (fun cilState -> // x > 0
                            StatedConditionalExecutionCIL cilState yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YM0
                                        mul
                                        (fun cilState k ->
                                            let states = this.Raise this.OverflowException cilState.state id
                                            states |> List.map (fun (state : state) -> (state.exceptionsRegister.GetError(), {cilState with state = state}))
                                                   |> k
                                            ))
//                                        (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun (state : state) -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k)))
                                (fun cilState -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YL0
                                        mul
                                        (fun (cilState : cilState) k ->
                                            let states = this.Raise this.OverflowException cilState.state id
                                            states |> List.map (fun (state : state) -> (state.exceptionsRegister.GetError(), {cilState with state = state}))
                                                   |> k
                                            )))
                        (fun cilState -> // x < 0
                            StatedConditionalExecutionCIL cilState
                                yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YM0
                                        mul
                                        (fun (cilState : cilState) k ->
                                            let states = this.Raise this.OverflowException cilState.state id
                                            states |> List.map (fun (state : state) -> (state.exceptionsRegister.GetError(), {cilState with state = state}))
                                                   |> k
                                            ))
                                (fun cilState k -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YL0
                                        mul
                                        (fun (cilState : cilState) k ->
                                            let states = this.Raise this.OverflowException cilState.state id
                                            states |> List.map (fun (state : state) -> (state.exceptionsRegister.GetError(), {cilState with state = state}))
                                                   |> k)
                                        k)))
                pushFunctionResults
        this.SignedCheckOverflow checkOverflow cilState
    member private this.Add_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k ->
                    PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                    k (diff >>= y, state)))
                (fun cilState k ->
                    PerformBinaryOperation OperationType.Add x y (fun res ->
                    k [res, cilState]))
                (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                pushFunctionResults
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Mul_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned zero max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k -> k [zero, cilState])
                (fun cilState k ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k ->
                            PerformBinaryOperation OperationType.Divide max x (fun quotient ->
                            k (quotient >>= y, state)))
                        (fun cilState k ->
                            PerformBinaryOperation OperationType.Multiply x y (fun res ->
                            k [res, cilState]))
                        (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                        k)
                pushFunctionResults
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (x >>= y, state))
                (fun (cilState : cilState) k -> // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    k [res, cilState]))
                (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                pushFunctionResults
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf (cilState : cilState) =
        // there is no way to reduce current operation to [x `Add_Ovf` (-y)]
        // min <= x - y <= max
        let checkOverflowForSigned min max zero minusOne x y cilState =
                let (>>=) = API.Arithmetics.(>>=)
                let xGreaterEqualZero state k = k (x >>= zero, state)
                let sub (cilState : cilState) k = // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    k [res, cilState])
                StatedConditionalExecutionCIL cilState
                    xGreaterEqualZero
                    (fun cilState -> // x >= 0 => max - x >= 0 => no overflow for [-1 * (max - x)]
                        StatedConditionalExecutionCIL cilState
                            (fun state k ->
                                PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (y >>= minusDiff, state)))) // y >= -(max - x)
                            sub
                            (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k)))
                    (fun cilState -> // x < 0 => no overflow for [min - x] # x < 0 => [min - x] != min => no overflow for (-1) * [min - x]
                        StatedConditionalExecutionCIL cilState
                           (fun state k ->
                                PerformBinaryOperation OperationType.Subtract min x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (minusDiff >>= y, state)))) // -(min - x) >= y
                            sub
                            (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k)))
                    pushFunctionResults
        this.SignedCheckOverflow checkOverflowForSigned cilState
    member private x.Newarr (cfg : cfg) offset (cilState : cilState) =
        let (>>=) = API.Arithmetics.(>>=)
        let elemType = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Newarr.Size)
        match cilState.state.opStack with
        | numElements :: stack ->
            StatedConditionalExecutionCIL (withOpStack stack cilState)
                (fun state k -> k (numElements >>= TypeUtils.Int32.Zero, state))
                (fun cilState k ->
                    let ref, state = Memory.AllocateDefaultArray cilState.state [numElements] (ArrayType(elemType, Vector))
                    k [ref, {cilState with state = state}])
                (fun (cilState : cilState) k -> this.Raise this.OverflowException cilState.state (List.map (fun state -> state.exceptionsRegister.GetError(), {cilState with state = state}) >> k))
                pushFunctionResults
        | _ -> __corruptedStack__()

    // -------------------------------- ExplorerBase operations -------------------------------------
    override x.Invoke codeLoc =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            let interpreter = CodePortionInterpreter(x, ilmm, findCfg ilmm, [])
            interpreter.Invoke
        | :? ILCodePortion as ilcode ->
            let ilmm = ilcode.FuncId :?> ILMethodMetadata
            let interpreter = CodePortionInterpreter(x, ilcode, findCfg ilmm, [])
            interpreter.Invoke
        | _ -> internalfail "unhandled ICodeLocation instance"
    override x.MakeMethodIdentifier m = { methodBase = m } :> IMethodIdentifier

    member x.ExecuteAllInstructions (cfg : cfg) (cilState : cilState) =
        assert (cilState.ip.CanBeExpanded())
        let startingOffset = cilState.ip.Offset ()
        let endOffset =
            let lastOffset = Seq.last cfg.sortedOffsets
            if startingOffset = lastOffset then cfg.ilBytes.Length
            else
                let index = cfg.sortedOffsets.BinarySearch startingOffset
                cfg.sortedOffsets.[index + 1]
        let isIpOfCurrentBasicBlock = function
            | Instruction offset -> startingOffset <= offset && offset < endOffset
            | _ -> false

        let rec executeAllInstructions erroredStates (offset : ip) cilState : cilState list=
            let allStates = x.ExecuteInstruction cfg (offset.Offset()) cilState
            let newErrors, goodStates = allStates |> List.partition (fun (_, cilState : cilState) -> cilState.HasException)
            let allErrors = erroredStates @ List.map (fun (erroredOffset, (cilState : cilState)) -> {cilState with ip = erroredOffset}) newErrors

            match goodStates with
            | list when List.forall (fst >> (=) ip.Exit) list -> List.map (fun (_, state) -> {state with ip = ip.Exit; isCompleted = true}) list @ allErrors
            | (nextIp, _)::xs as list when isIpOfCurrentBasicBlock nextIp && List.forall (fst >> (=) nextIp) xs ->
                List.collect ((<||) (executeAllInstructions allErrors)) list
            | list -> allErrors @ (list |> List.map (fun (ip, cilState) -> {cilState with ip = ip; isCompleted = not <| isIpOfCurrentBasicBlock ip}))
        executeAllInstructions [] (Instruction startingOffset) cilState

    member x.ExecuteInstruction (cfg : cfg) (offset : int) (cilState : cilState) =
        assert(not cilState.isCompleted)
        let opCode = Instruction.parseInstruction cfg.ilBytes offset
//        Logger.printLog Logger.Trace "Executing instruction %O of %O [%O]" opCode cfg.methodBase cfg.methodBase.DeclaringType
        let nextTargets = Instruction.findNextInstructionOffsetAndEdges opCode cfg.ilBytes offset
        let newOffsets : ip list =
            match nextTargets with
            | UnconditionalBranch nextInstruction
            | FallThrough nextInstruction          -> [Instruction nextInstruction]
            | Return -> [Exit]
            | ExceptionMechanism -> [FindingHandler offset]
            | ConditionalBranch targets -> targets |> List.map Instruction
        let newSts = opcode2Function.[hashFunction opCode] cfg offset newOffsets cilState

//        if opCode = OpCodes.Isinst then
//            newSts |> List.iteri (fun i (_, cilState : cilState) -> Logger.printLog Logger.Trace "number = %i state = %O\n" i cilState.state)

        if opCode = Reflection.Emit.OpCodes.Add_Ovf then ()
        let leaveInstructionExecuted = opCode = OpCodes.Leave || opCode = OpCodes.Leave_S
        newSts |> List.map (fun (d, cilState : cilState) -> d, {cilState with leaveInstructionExecuted = leaveInstructionExecuted})
