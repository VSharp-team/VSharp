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
    let mutable result : cilState option = None
    let workingSet : List<cilState> = new List<_>()
    let exceptionsSet : List<cilState> = new List<_>()
    override x.MakeRecursiveState cilState =
        let methodId = ilInterpreter.MakeMethodIdentifier cfg.methodBase
        let ilCodePortion = ILCodePortion(cilState.ip.Vertex(), cilState.recursiveVertices, methodId, cilState.state)
        {cilState with state = ilInterpreter.ReproduceEffect ilCodePortion cilState.state snd}

    member x.Invoke state this k =
        let getResultAndState (cilState : cilState option) =
            match cilState with
            | Some st -> st.state.returnRegister |?? Nop, st.state
            | None -> internalfail "Exception handling is not implemented!" //TODO: __unreachable__()
        let interpret state curV targetV rvs =
            { cilState.MakeEmpty curV targetV state with recursiveVertices = rvs; this = this }
            |> x.Interpret
            |> getResultAndState |> k
        match codeLoc with
        | :? ILMethodMetadata ->
            ilInterpreter.InitEntryPoint state cfg.methodBase.DeclaringType (fun state ->
            interpret state (Instruction 0) ip.Exit [])
        | :? ILCodePortion as ilcode ->
            let u = Instruction ilcode.VertexNumber
            let rvs = ilcode.RecursiveVertices
            interpret state u u rvs
        | _ -> __notImplemented__()
    override x.MakeEpsilonState (ist : cilState) =
        let state = ist.state
        let pcForEpsilon = !! (conjunction state.pc)
        let state = { Memory.EmptyState with
                        pc = [pcForEpsilon]
                        frames = state.frames
                        stack = (fst Memory.EmptyState.stack, snd state.stack)
                    }
        cilState.MakeEmpty ist.ip ist.ip state
    override x.EvaluateOneStep cilState =
        assert (cilState.ip.CanBeExpanded())
        let lastOffset = Seq.last cfg.sortedOffsets
        let startingOffset = cilState.ip.Vertex ()
        let endOffset =
            let u = cfg.sortedOffsets.BinarySearch(startingOffset)
            if startingOffset = lastOffset then lastOffset + 1
            else cfg.sortedOffsets.[u + 1]
        let isOffsetOfCurrentVertex (offset : ip) = startingOffset <= offset.Vertex() && offset.Vertex() < endOffset
        let rec executeAllInstructions (offset : ip) cilState =
            let exceptions, nonErroredStates =
                ilInterpreter.ExecuteInstruction cfg (offset.Vertex()) cilState
                |> List.partition (fun (_, cilState : cilState) -> cilState.HasException)
            exceptionsSet.AddRange(List.map snd exceptions)
            match nonErroredStates with
            | [] -> []
            | list when List.forall (fst >> (=) ip.Exit) list -> List.map (fun (_, state) -> { state with ip = ip.Exit}) list
            | (nextOffset, _)::xs as list when isOffsetOfCurrentVertex nextOffset
                                               && List.forall (fun (offset, _) -> offset = nextOffset && isOffsetOfCurrentVertex offset) xs ->
                List.collect ((<||) executeAllInstructions) list
            | list -> list |> List.map (fun (offset, cilSt) -> {cilSt with ip = offset})
        executeAllInstructions (Instruction startingOffset) cilState
        |> List.filter (fun st -> st.IsFinished || not (st.ip.CanBeExpanded() && List.contains (st.ip.Vertex()) st.recursiveVertices))
    override x.IsRecursiveState cilState =
        let isHeadOfLoop (cfg : cfg) v =
            let vTime = cfg.topologicalTimes.[v]
            cfg.reverseGraph.[v] |> Seq.exists (fun u -> cfg.topologicalTimes.[u] > vTime)
        match cilState.ip with
        | Instruction v ->
            isHeadOfLoop cfg v &&
            let rv = cilState.recursiveVertices
            let methodId = ilInterpreter.MakeMethodIdentifier cfg.methodBase
            let ilCodePortion = ILCodePortion(v, v :: rv, methodId, cilState.state)
            ilInterpreter.ShouldStopUnrolling ilCodePortion cilState.state
        | _ -> false
    override x.Add cilState = if cilState.ip <> ip.Exit then workingSet.Add cilState
    override x.ExploreInIsolation cilState =
        let u = cilState.ip.Vertex()
        let rv = cilState.recursiveVertices
        let methodId = ilInterpreter.MakeMethodIdentifier cfg.methodBase
        let ilCodePortion = ILCodePortion(u, u :: rv, methodId, cilState.state)
        ilInterpreter.ReproduceEffect ilCodePortion cilState.state (fun (_, state) ->
        {cilState with state = state; recursiveVertices = u :: rv})
    override x.HasNextState () = workingSet.Count <> 0
    override x.FindSimilar cilState =
        let areCapableForMerge (st1 : cilState) (st2 : cilState) =
            st2.IsFinished
         || st1.IsFinished
         || st1.recursiveVertices = st2.recursiveVertices && st1.opStack = st2.opStack && st1.ip = st2.ip && st1.isFinished st1.ip && st2.isFinished st2.ip
        match Seq.tryFindIndex (areCapableForMerge cilState) workingSet with
        | None -> None
        | Some i -> let res = Some workingSet.[i]
                    workingSet.RemoveAt i
                    res
    override x.GetResultState () =
        match result with
        | None -> None
        | Some result -> Some ({ result with recursiveVertices = rv})
    override x.SetResultState newRes = result <- Some newRes
    override x.IsResultState cilState =
        match result with
        | None -> cilState.IsFinished
        | Some result -> cilState.IsFinished && result.ip = cilState.ip && result.opStack = cilState.opStack
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
    let cfgs = new Dictionary<ILMethodMetadata, cfg>()
    let findCfg (ilmm : ILMethodMetadata) =
        Dict.getValueOrUpdate cfgs ilmm (fun () -> CFG.build ilmm.methodBase)
    let internalImplementations =
        Map.ofList [
            "System.Int32 System.Array.GetLength(this, System.Int32)", this.GetArrayLength
            "System.Int32 System.Array.GetLowerBound(this, System.Int32)", this.GetArrayLowerBound
            "System.Void System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)", this.InitializeArray
        ]

    member private x.Raise createException (cilState : cilState) k =
        let exc, state = createException cilState.state
        //TODO: exception handling
        k [exc, {cilState with state = state}]

    member private x.AccessArray accessor cilState upperBound index =
        let checkArrayBounds upperBound x =
            let lowerBound = Concrete 0 Types.TLength
            let notTooSmall = Arithmetics.(>>=) x lowerBound
            let notTooLarge = Arithmetics.(<<) x upperBound
            notTooSmall &&& notTooLarge
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (checkArrayBounds upperBound index, state))
            accessor
            (x.Raise x.IndexOutOfRangeException)
    member private x.AccessArrayDimension accessor (cilState : cilState) = function
        | [this; dimension] ->
            let array = Memory.Dereference cilState.state this
            let upperBound = Memory.ArrayRank array
            x.AccessArray (accessor this dimension) cilState upperBound dimension id
        | _ -> __unreachable__()
    member private x.GetArrayLength (cilState : cilState) args =
        let arrayLengthByDimension arrayRef index (cilState : cilState) k =
            k [Memory.ArrayLengthByDimension cilState.state arrayRef index, cilState]
        x.AccessArrayDimension arrayLengthByDimension cilState args
    member private x.GetArrayLowerBound (cilState : cilState) args =
        let arrayLowerBoundByDimension arrayRef index (cilState : cilState) k =
            k [Memory.ArrayLowerBoundByDimension cilState.state arrayRef index, cilState]
        x.AccessArrayDimension arrayLowerBoundByDimension cilState args

    member private x.NpeOrInvokeStatement (cilState : cilState) (this : term) statement k =
         BranchOnNull cilState this
            (x.Raise x.NullReferenceException)
            statement
            k

    member private x.InitializeArray (cilState : cilState) = function
        | arrayRef :: handleTerm :: [] ->
            x.NpeOrInvokeStatement cilState arrayRef (fun cilState ->
            x.NpeOrInvokeStatement cilState handleTerm (fun cilState k ->
            let result, state = VSharp.System.Runtime_CompilerServices_RuntimeHelpers.InitializeArray cilState.state arrayRef handleTerm
            k [result, {cilState with state = state}])) id
        | _ -> __unreachable__()

    member x.ReduceMethodBaseCall (methodBase : MethodBase) (cilState : cilState) this (args : term list symbolicValue) concreteCall k =
//        Logger.printLog Logger.Trace "reducing call for methodBase = %O" methodBase
        let state = cilState.state
        let appendResultToCilState (term : term, cilState : cilState) =
            let state =
                match term.term with
                | Nop -> cilState.state
                | _ -> {cilState.state with returnRegister = Some term}
            term, {cilState with state = state}
        let toCilState (term, state) = term, {cilState with state = state}
        let fullMethodName = Reflection.GetFullMethodName methodBase
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        if Map.containsKey fullMethodName internalImplementations then
            let thisAndArguments =
                match args with
                | Unspecified -> optCons [] this
                | Specified args -> optCons args this
            (internalImplementations.[fullMethodName] cilState thisAndArguments) |> (List.map appendResultToCilState >> k)
        elif Map.containsKey fullMethodName Loader.internalImplementations then
            let thisAndArguments =
                match args with
                | Unspecified -> optCons [] this
                | Specified args -> optCons args this
            internalCall Loader.internalImplementations.[fullMethodName] thisAndArguments state (toCilState >> appendResultToCilState >> List.singleton >> k)
        elif Map.containsKey fullMethodName Loader.concreteExternalImplementations then
            match args with
            | Specified parameters ->
                let parameters' = optCons parameters this
                let methodInfo = Loader.concreteExternalImplementations.[fullMethodName]
                let methodId = x.MakeMethodIdentifier methodInfo
                let invoke state k = x.Invoke methodId state this k
                x.ReduceFunction state None (Specified parameters') methodId methodInfo invoke (toCilState >> appendResultToCilState >> List.singleton >> k)
            | _ -> internalfail "internal call with unspecified parameters!"
        elif int (methodBase.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall) <> 0 then
            internalfailf "new extern method: %s" fullMethodName
        elif methodBase.GetMethodBody() <> null then
//            x.ReduceConcreteCall methodBase state this args k1
            concreteCall methodBase state this args (toCilState >> appendResultToCilState >> List.singleton >> k)
        else
            internalfail "nonextern method without body!"

    member x.CallMethodFromTermType (caller : locationBinding) (funcId : IFunctionIdentifier) (cilState : cilState) this parameters termType (calledMethod : MethodInfo) k =
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
            then x.CallAbstract caller funcId cilState k
            else x.ReduceMethodBaseCall targetMethod cilState (Some this) parameters x.ReduceConcreteCall k

    member x.CallVirtualMethod (caller : locationBinding) (funcId : IFunctionIdentifier) cilState this parameters (methodInfo : MethodInfo) k =
        let callVirtual cilState this k =
            let baseType = BaseTypeOfRef this
            let sightType = SightTypeOfRef this
            let callForConcreteType typ state k =
                x.CallMethodFromTermType caller funcId state this parameters typ methodInfo k
            let tryToCallForBaseType cilState =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (API.Types.TypeIsRef baseType this &&& API.Types.TypeIsType baseType sightType, state))
                    (callForConcreteType baseType)
                    (x.CallAbstract caller funcId)
            let tryToCallForSightType cilState =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (API.Types.TypeIsRef sightType this, state))
                    (callForConcreteType sightType)
                    tryToCallForBaseType
            let sightDotNetType = Types.ToDotNetType sightType
            let baseDotNetType = Types.ToDotNetType baseType
            if sightDotNetType.IsInterface && baseDotNetType.IsInterface
                then x.CallAbstract caller funcId cilState k
                else tryToCallForSightType cilState k
        GuardedApply cilState this callVirtual k

    member x.CallAbstract caller funcId (cilState : cilState) k =
        x.CallAbstractMethod caller funcId cilState.state (fun (result, state) ->
            // TODO: get rid of this copy-paste from
            let state =
                match result.term with
                | Nop -> state
                | _ -> withResult state (Some result)
            k [result, {cilState with state = state}])

    member private x.ConvOvf targetType typeForStack (cilState : cilState) = // TODO: think about getting rid of typeForStack
        let typIsLessTyp : System.Collections.Generic.Dictionary<termType, list<termType>> = new Dictionary<_,_>()
        typIsLessTyp.[TypeUtils.int8Type] <- [TypeUtils.int8Type; TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type]
        typIsLessTyp.[TypeUtils.int16Type] <- [TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type]
        typIsLessTyp.[TypeUtils.int32Type] <- [TypeUtils.int32Type; TypeUtils.int64Type]
        typIsLessTyp.[TypeUtils.int64Type] <- [TypeUtils.int64Type]

        typIsLessTyp.[TypeUtils.uint8Type] <- [TypeUtils.uint8Type; TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type]
        typIsLessTyp.[TypeUtils.uint16Type] <- [TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type]
        typIsLessTyp.[TypeUtils.uint32Type] <- [TypeUtils.uint32Type; TypeUtils.uint64Type]
        typIsLessTyp.[TypeUtils.uint64Type] <- [TypeUtils.uint64Type]
        let less leftTyp rightTyp = List.contains rightTyp typIsLessTyp.[leftTyp]

        let minMax : System.Collections.Generic.Dictionary<termType, int64 * int64> = new Dictionary<_,_>()
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
        match cilState.opStack with
        | t :: stack ->
            let castForStack results =
                mapAndPushFunctionResultsk (fun (term, state) -> castUnchecked typeForStack term state, state) results id
            StatedConditionalExecutionCIL {cilState with opStack = stack}
                (fun state k -> k (canCastWithoutOverflow t targetType, state))
                (fun cilState k -> k [Types.Cast cilState.state.pc t targetType, cilState])
                (x.Raise x.OverflowException)
                castForStack
        | _ -> __notImplemented__()
    member private x.ConvOvfUn unsignedSightType targetType typeForStack (cilState : cilState) = // TODO: think about getting rid of typeForStack
        match cilState.opStack with
        | t :: stack ->
            let unsignedT = castUnchecked unsignedSightType t cilState.state
            x.ConvOvf targetType typeForStack {cilState with opStack = unsignedT::stack}
        | _ -> __notImplemented__()
    member private x.CastClass (cfg : cfg) offset (cilState : cilState) : cilState list =
        match cilState.opStack with
        | term :: stack ->
            let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Castclass.Size)
            let term = castReferenceToPointerIfNeeded term typ cilState.state
            StatedConditionalExecutionCIL {cilState with opStack = stack}
                (fun state k -> k (IsNullReference term ||| Types.IsCast typ term, state))
                (fun cilState k -> k [Types.Cast cilState.state.pc term typ, cilState])
                (x.Raise x.InvalidCastException)
                pushFunctionResults
        | _ -> __notImplemented__()

    member x.CommonCall (calledMethodBase : MethodBase) cilState this args k =
        let call (cilState : cilState) k =
            x.InitEntryPoint cilState.state calledMethodBase.DeclaringType (fun state ->
            x.ReduceMethodBaseCall calledMethodBase {cilState with state = state} this (Specified args) x.ReduceConcreteCall k)
        match this with
        | None -> call cilState k
        | Some this -> x.NpeOrInvokeStatement cilState this call k
    member x.Call (cfg : cfg) offset (cilState : cilState) =
        let calledMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Call.Size)
        let args, cilState = retrieveActualParameters calledMethodBase cilState
        let this, cilState = if not calledMethodBase.IsStatic then popStack cilState else None, cilState
        x.CommonCall calledMethodBase cilState this args pushFunctionResults
    member x.CommonCallVirt (ancestorMethodBase : MethodBase) cilState this args k =
        let call (cilState : cilState) k =
            x.InitEntryPoint cilState.state ancestorMethodBase.DeclaringType (fun state ->
            let cilState = {cilState with state = state}
            if ancestorMethodBase.DeclaringType.IsSubclassOf typedefof<System.Delegate> then
                Lambdas.invokeDelegate "callvirt" args cilState this k
            elif ancestorMethodBase.IsVirtual && not ancestorMethodBase.IsFinal then
                let ilmm = x.MakeMethodIdentifier ancestorMethodBase
                let methodInfo = ancestorMethodBase :?> MethodInfo
                x.CallVirtualMethod "callvirt" ilmm cilState this (Specified args) methodInfo k
            else
                x.ReduceMethodBaseCall ancestorMethodBase cilState (Some this) (Specified args) x.ReduceConcreteCall k)
        x.NpeOrInvokeStatement cilState this call k
    member x.CallVirt (cfg : cfg) offset (cilState : cilState) =
        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Callvirt.Size)
        let args, cilState = retrieveActualParameters ancestorMethodBase cilState
        let this, cilState = popStack cilState |> mapfst Option.get
        x.CommonCallVirt ancestorMethodBase cilState this args pushFunctionResults
    member private x.CreateDelegate (cilState : cilState) typ k =
        match cilState.opStack with
        | methodPtr :: target :: stack ->
            let getMethodInfo methodPtr =
                match methodPtr.term with
                | Concrete(:? MethodInfo as mi, _) -> mi
                | _ -> __unreachable__()
            let invoke _ cilState args k =
                GuardedApply cilState methodPtr
                    (fun cilState methodPtr k ->
                        let methodInfo = getMethodInfo methodPtr
                        let invoke this cilState k = x.ReduceMethodBaseCall methodInfo cilState this args x.ReduceConcreteCall k
                        BranchOnNull cilState target
                            (invoke None)
                            (invoke (Some target))
                            k)
                    k
            Lambdas.make invoke typ (fun lambda ->
            let lambdaRefAndState = Memory.AllocateReferenceTypeInHeap cilState.state typ lambda
            pushResultOnStack {cilState with opStack = stack} lambdaRefAndState :: [] |> k)
        | _ -> __notImplemented__()
    member x.CommonNewObj (constructorInfo : MethodBase) (cilState : cilState) (k : cilState list -> 'a) : 'a =
        let typ = constructorInfo.DeclaringType
        let constructedTermType = typ |> Types.FromDotNetType cilState.state
        let blockCase (cilState : cilState) =
            let args, cilState = retrieveActualParameters constructorInfo cilState
            let callConstructor cilState reference = x.ReduceMethodBaseCall constructorInfo cilState (Some reference) (Specified args) x.ReduceConcreteCall
            let commonCase isReferenceType (cilState : cilState) k =
                let ref, state = Memory.AllocateDefaultBlock cilState.state constructedTermType
                let k results =
                    match isReferenceType with
                    | true -> mapAndPushFunctionResultsk (fun (_, state) -> ref, state) results k
                    | _ ->
                        let modifyResult (_, state) =
                            let value = Memory.Dereference state ref
                            // TODO: call GC to clean up heap
                            value, state
                        mapAndPushFunctionResultsk modifyResult results k
                callConstructor {cilState with state = state} ref k
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.IsValueType constructedTermType, state))
                (commonCase false)
                (commonCase true)
                id
        let nonDelegateCase (cilState : cilState) =
            x.InitEntryPoint cilState.state typ (fun state ->
            if typ.IsArray && constructorInfo.GetMethodBody() = null
                then reduceArrayCreation typ constructorInfo {cilState with state = state} List.singleton
                else blockCase {cilState with state = state})
        if constructorInfo.DeclaringType.IsSubclassOf typedefof<System.Delegate>
            then x.CreateDelegate cilState constructedTermType k
            else nonDelegateCase cilState |> k
    member x.NewObj (cfg : cfg) offset (cilState : cilState) =
        let constructorInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Newobj.Size) :?> ConstructorInfo
        assert (constructorInfo.IsConstructor)
        x.CommonNewObj constructorInfo cilState id

    member x.LdsFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldsfld.Size)
        assert (fieldInfo.IsStatic)
        x.InitEntryPoint cilState.state fieldInfo.DeclaringType (fun state ->
        let fieldType = fieldInfo.FieldType |> Types.FromDotNetType state
        let declaringTermType = fieldInfo.DeclaringType |> Types.FromDotNetType state
        let fullName = getFullNameOfField fieldInfo
        let address = Memory.ReferenceStaticField declaringTermType fullName fieldType
        let value = if addressNeeded then address else Memory.Dereference state address
        pushResultOnStack cilState (value, state) :: [])
    member private x.StsFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stsfld.Size)
        let state = cilState.state
        assert (fieldInfo.IsStatic)
        let fieldType = fieldInfo.FieldType |> Types.FromDotNetType state
        let declaringTermType = fieldInfo.DeclaringType |> Types.FromDotNetType state
        let fullName = getFullNameOfField fieldInfo
        let address = Memory.ReferenceStaticField declaringTermType fullName fieldType
        match cilState.opStack with
        | value :: stack ->
            x.InitEntryPoint state fieldInfo.DeclaringType (fun state ->
            let value = castUnchecked fieldType value state
            let _, state = Memory.Mutate state address value
            {cilState with state = state; opStack = stack} :: [])
        | _ -> __notImplemented__()
    member x.LdFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldfld.Size)
        assert (not fieldInfo.IsStatic)
        match cilState.opStack with
        | target :: stack ->
            let loadWhenTargetIsNotNull (cilState : cilState) k =
                let state = cilState.state
                let k1 value = k [value, cilState]
                let fieldType = fieldInfo.FieldType |> Types.FromDotNetType state
                let fullName = getFullNameOfField fieldInfo
                match addressNeeded, isStruct target with
                | false, true -> k1 <| Memory.ReadBlockField target fullName fieldType
                | false, false -> Memory.ReferenceField target fullName fieldType |> Memory.Dereference state |> k1
                | true, _ -> k1 <| Memory.ReferenceField target fullName fieldType
            x.NpeOrInvokeStatement {cilState with opStack = stack} target loadWhenTargetIsNotNull pushFunctionResults
        | _ -> __notImplemented__()
    member x.StFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stfld.Size)
        assert (not fieldInfo.IsStatic)
        match cilState.opStack with
        | value :: targetRef :: stack ->
            let storeWhenTargetIsNotNull (cilState : cilState) k =
                let state = cilState.state
                let fieldType = fieldInfo.FieldType |> Types.FromDotNetType state
                let fullName = getFullNameOfField fieldInfo
                let address = Memory.ReferenceField targetRef fullName fieldType
                let value = castUnchecked fieldType value state
                let state = Memory.Mutate state address value |> snd
                k [Nop, {cilState with state = state}]
            x.NpeOrInvokeStatement {cilState with opStack = stack} targetRef storeWhenTargetIsNotNull getCilStateFromResult
        | _ -> __notImplemented__()
    member private x.LdElemWithCast cast (cilState : cilState) : cilState list =
        match cilState.opStack with
        | index :: arrayRef :: stack ->
            let uncheckedLdElem (cilState : cilState) k =
                let reference = Memory.ReferenceArrayIndex cilState.state arrayRef [index]
                let value = Memory.Dereference cilState.state reference
                k [cast value cilState.state, cilState]
            let checkedLdElem (cilState : cilState) k =
                let array = Memory.Dereference cilState.state arrayRef
                let length = Memory.ArrayLength array
                x.AccessArray uncheckedLdElem cilState length index k
            x.NpeOrInvokeStatement {cilState with opStack = stack} arrayRef checkedLdElem pushFunctionResults
        | _ -> __notImplemented__()
    member private x.LdElemTyp typ (cilState : cilState) = x.LdElemWithCast (castUnchecked typ) cilState
    member private x.LdElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Ldelem.Size)
        x.LdElemTyp typ cilState
    member private x.LdElemRef = x.LdElemWithCast always
    member private x.StElemWithCast cast (cilState : cilState) =
        match cilState.opStack with
        | value :: index :: arrayRef :: stack ->
            let checkedStElem (cilState : cilState) k =
                let reference = Memory.ReferenceArrayIndex cilState.state arrayRef [index]
                let typeOfValue = TypeOf value
                let isTypeOfValueValueType = Types.IsValueType typeOfValue
                let uncheckedStElem (cilState : cilState) k =
                    let typedValue = cast value cilState.state
                    let t, state = Memory.Mutate cilState.state reference typedValue
                    k [t, {cilState with state = state}]
                let checkTypeMismatchBasedOnTypeOfValue cond (cilState : cilState) =
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k (cond, state))
                        uncheckedStElem
                        (x.Raise x.ArrayTypeMismatchException)
                let checkTypeMismatch (cilState : cilState) =
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k (isTypeOfValueValueType, state))
                        (fun cilState -> checkTypeMismatchBasedOnTypeOfValue (Types.TypeIsRef typeOfValue reference) cilState)
                        (fun cilState -> checkTypeMismatchBasedOnTypeOfValue (Types.RefIsRef value reference) cilState)
                let array = Memory.Dereference cilState.state arrayRef
                let length = Memory.ArrayLength array
                x.AccessArray checkTypeMismatch cilState length index k
            x.NpeOrInvokeStatement {cilState with opStack = stack} arrayRef checkedStElem getCilStateFromResult
        | _ -> __notImplemented__()
    member private x.StElemTyp typ (cilState : cilState) =
        x.StElemWithCast (castUnchecked typ) cilState
    member private x.StElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Stelem.Size)
        x.StElemTyp typ cilState
    member private x.StElemRef = x.StElemWithCast always
    member private x.LdLen (cilState : cilState) =
        match cilState.opStack with
        | arrayRef :: stack ->
            let ldlen (cilState : cilState) k =
                let array = Memory.Dereference cilState.state arrayRef
                let length = Memory.ArrayLength array
                k [length, cilState]
            x.NpeOrInvokeStatement {cilState with opStack = stack} arrayRef ldlen pushFunctionResults
        | _ -> __notImplemented__()
    member private x.LdVirtFtn (cfg : cfg) offset (cilState : cilState) =
        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Ldvirtftn.Size)
        match cilState.opStack with
        | this :: stack ->
            let ldvirtftn (cilState : cilState) k =
                assert(isReference this)
                let t = this |> SightTypeOfRef |> Types.ToDotNetType
                let methodInfo = t.GetMethod(ancestorMethodBase.Name, allBindingFlags)
                let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType cilState.state (methodInfo.GetType()))
                k [methodPtr, cilState]
            x.NpeOrInvokeStatement {cilState with opStack = stack} this ldvirtftn pushFunctionResults
        | _ -> __notImplemented__()
    member x.BoxNullable (t : Type) v (cilState : cilState) =
        let hasValueMethodInfo = t.GetMethod("get_HasValue")
        let hasValueCase (cilState : cilState) k =
            let valueMethodInfo = t.GetMethod("get_Value")
            let underlyingTermType = Nullable.GetUnderlyingType(t) |> Types.FromDotNetType cilState.state
            let allocateResults results = mapAndPushFunctionResultsk (fun (res, state) -> Memory.AllocateValueTypeInHeap state underlyingTermType res) results k
            x.ReduceMethodBaseCall valueMethodInfo cilState (Some v) (Specified []) x.ReduceConcreteCall allocateResults
        let boxNullable (hasValue, cilState) =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (hasValue, state))
                hasValueCase
                (fun cilState k -> k [{cilState with opStack = MakeNullRef() :: cilState.opStack}])
        x.ReduceMethodBaseCall hasValueMethodInfo cilState (Some v) (Specified []) x.ReduceConcreteCall (fun hasValueResults ->
        Cps.List.mapk boxNullable hasValueResults List.concat)

    member x.Box (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Box.Size)
        let termType = Types.FromDotNetType cilState.state t
        match cilState.opStack with
        | v :: stack ->
            let boxValueType (cilState : cilState) =
                let cilState' = {cilState with opStack = stack}
                StatedConditionalExecutionCIL cilState'
                    (fun state k -> k (Types.TypeIsNullable termType, state))
                    (fun cilState k -> x.BoxNullable t v cilState |> k)
                    (fun cilState k -> allocateValueTypeInHeap v cilState |> k)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.IsValueType termType, state))
                boxValueType
                (fun cilState k -> k [cilState])
                id
        | _ -> __notImplemented__()
    member private x.UnboxCommon (op : OpCode) handleReferenceType handleRestResults (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + op.Size)
        let termType = Types.FromDotNetType cilState.state t
        let InvalidCastException state = x.InvalidCastException state
        match cilState.opStack with
        | _ :: _ when t.IsGenericParameter -> __notImplemented__() //TODO: Nullable.GetUnderlyingType for generics
        | obj :: stack ->
            assert(isReference obj)
            let nullCase (cilState : cilState) =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.TypeIsNullable termType, state))
                    (fun cilState k ->
                        let address, state = Memory.AllocateDefaultBlock cilState.state termType
                        k [handleRestResults(address, {cilState with state = state})])
                    (x.Raise x.NullReferenceException)
            let canCastValueTypeToNullableTargetCase (cilState : cilState) =
                let underlyingTypeOfNullableT = Nullable.GetUnderlyingType(t)
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.RefIsType obj (Types.FromDotNetType state underlyingTypeOfNullableT), state))
                    (fun cilState k ->
                        let value = Memory.Dereference cilState.state obj
                        let address, state = Memory.AllocateDefaultBlock cilState.state termType
                        let nullableConstructor = t.GetConstructor([| underlyingTypeOfNullableT |])
                        let modifyResults results = Cps.List.map (fun (_, cilState) -> handleRestResults (address, cilState)) results k
                        x.ReduceMethodBaseCall nullableConstructor {cilState with state = state} (Some address) (Specified [value]) x.ReduceConcreteCall modifyResults)
                    (x.Raise x.InvalidCastException)
            let canCastValueTypeToTargetCase (cilState : cilState) =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.TypeIsNullable termType, state))
                    canCastValueTypeToNullableTargetCase
                    (fun cilState k -> k [handleRestResults(Types.Cast cilState.state.pc obj termType, cilState)])
            let valueTypeCase (cilState : cilState) =
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.IsCast termType obj, state))
                    canCastValueTypeToTargetCase
                    (x.Raise x.InvalidCastException)
            let nonNullCase (cilState : cilState) =
                let SystemValueType = Types.FromDotNetType cilState.state typedefof<System.ValueType>
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.RefIsType obj SystemValueType, state))
                    valueTypeCase
                    (handleReferenceType obj termType)
            BranchOnNull {cilState with opStack = stack} obj
                nullCase
                nonNullCase
                pushFunctionResults
        | _ -> __notImplemented__()
    member private x.Unbox (cfg : cfg) offset (cilState : cilState) =
        x.UnboxCommon OpCodes.Unbox (fun _ _ -> x.Raise x.InvalidCastException) id cfg offset cilState
    member private x.UnboxAny (cfg : cfg) offset (cilState : cilState) =
        let handleReferenceTypeResults obj termType cilState =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Types.IsCast termType obj, state))
                (fun cilState k -> k [Types.Cast cilState.state.pc obj termType, cilState])
                (x.Raise x.InvalidCastException)
        let handleRestResults (address, cilState : cilState) = Memory.Dereference cilState.state address, cilState
        x.UnboxCommon OpCodes.Unbox_Any handleReferenceTypeResults handleRestResults cfg offset cilState

    member private this.CommonDivRem performAction (cilState : cilState) =
        let integerCase (cilState : cilState) x y minusOne minValue =
            assert(TypeOf x = TypeOf y)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k ((x === minValue) &&& (y === minusOne), state))
                        (this.Raise this.ArithmeticException)
                        (fun cilState k -> k [performAction x y, cilState]))
                pushFunctionResults
        match cilState.opStack with
        | TypeUtils.Float y :: TypeUtils.Float x :: stack ->
            {cilState with opStack = performAction x y :: stack} :: []
        | TypeUtils.Int64 y :: x :: stack
        | TypeUtils.UInt64 y :: x :: stack
        | y :: TypeUtils.Int64 x :: stack
        | y :: TypeUtils.UInt64 x :: stack ->
            integerCase {cilState with opStack = stack} x y TypeUtils.Int64.MinusOne TypeUtils.Int64.MinValue
        | y :: x :: stack ->
            integerCase {cilState with opStack = stack} x y TypeUtils.Int32.MinusOne TypeUtils.Int32.MinValue
        | _ -> __notImplemented__()
    member private this.Div (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonDivRem div cilState

    member private this.Rem (cilState : cilState) =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonDivRem rem cilState

    member private this.CommonUnsignedDivRem isRem performAction (cilState : cilState) =
        let pc = cilState.state.pc
        match cilState.opStack with
        | y :: x :: stack when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x = makeUnsignedInteger pc x id
            let y = makeUnsignedInteger pc y id
            StatedConditionalExecutionCIL {cilState with opStack = stack}
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState k -> k [performAction x y, cilState])
                pushFunctionResults
        | TypeUtils.Float _ :: _
        | _ :: TypeUtils.Float _ :: _ when isRem -> internalfailf "Rem.Un is unspecified for Floats"
        | _ -> __notImplemented__()
    member private this.DivUn (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonUnsignedDivRem false div cilState

    member private this.RemUn cilState =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonUnsignedDivRem true rem cilState

    member private this.UnsignedCheckOverflow checkOverflowForUnsigned (cilState : cilState) =
        let pc = cilState.state.pc
        match cilState.opStack with
        | TypeUtils.Int64 y :: x :: stack
        | y :: TypeUtils.Int64 x :: stack
        | TypeUtils.UInt64 y :: x :: stack
        | y :: TypeUtils.UInt64 x :: stack ->
            let x = makeUnsignedInteger pc x id
            let y = makeUnsignedInteger pc y id
            let max = TypeUtils.UInt64.MaxValue
            let zero = TypeUtils.UInt64.Zero
            checkOverflowForUnsigned zero max x y {cilState with opStack = stack} // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | y :: x :: stack when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x, y = makeUnsignedInteger pc x id, makeUnsignedInteger pc y id
            let max = TypeUtils.UInt32.MaxValue
            let zero = TypeUtils.UInt32.Zero
            checkOverflowForUnsigned zero max x y {cilState with opStack = stack} // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> __notImplemented__()
    member private this.SignedCheckOverflow checkOverflow cilState =
        match cilState.opStack with
        | TypeUtils.Int64 y :: x :: stack
        | y :: TypeUtils.Int64 x :: stack ->
            let min = TypeUtils.Int64.MinValue
            let max = TypeUtils.Int64.MaxValue
            let zero = TypeUtils.Int64.Zero
            let minusOne = TypeUtils.Int64.MinusOne
            checkOverflow min max zero minusOne x y {cilState with opStack = stack} // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | TypeUtils.UInt64 _ :: _ :: _
        | _ :: TypeUtils.UInt64 _ :: _ -> __unreachable__() // instead of add_ovf should be called add_ovf_un
        | TypeUtils.Float _ :: _
        | _ :: TypeUtils.Float _ :: _ -> __unreachable__() // only integers
        | y :: x :: stack ->
            let min = TypeUtils.Int32.MinValue
            let max = TypeUtils.Int32.MaxValue
            let zero = TypeUtils.Int32.Zero
            let minusOne = TypeUtils.Int32.MinusOne
            checkOverflow min max zero minusOne x y {cilState with opStack = stack} // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> __notImplemented__()
    member private this.Add_ovf (cilState : cilState) =
        // min <= x + y <= max
        let checkOverflow min max zero _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let xMoreThan0 state k = k (x >>= zero, state)
            let yMoreThan0 state k = k (y >>= zero, state)
            let noOverflowWhenMoreThan0 (state : state) k = // x >= 0 && y >= 0
                PerformBinaryOperation OperationType.Subtract max y (fun diff ->
                k (diff >>= x, state))
            let noOverflowWhenLessThan0 (state : state) k =
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
                                noOverflowWhenMoreThan0
                                add
                                (this.Raise this.OverflowException))
                        add)
                (fun cilState -> // x < 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        add
                        (fun cilState -> // x < 0 && y < 0
                            StatedConditionalExecutionCIL cilState
                                noOverflowWhenLessThan0
                                add
                                (this.Raise this.OverflowException)))
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
                                (fun cilState -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YL0
                                        mul
                                        (this.Raise this.OverflowException))))
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
                (this.Raise this.OverflowException)
                pushFunctionResults
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Mul_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned zero max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k -> k [zero, cilState])
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k ->
                            PerformBinaryOperation OperationType.Divide max x (fun quotient ->
                            k (quotient >>= y, state)))
                        (fun cilState k ->
                            PerformBinaryOperation OperationType.Multiply x y (fun res ->
                            k [res, cilState]))
                        (this.Raise this.OverflowException))
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
                (this.Raise this.OverflowException)
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
                            (this.Raise this.OverflowException))
                    (fun cilState -> // x < 0 => no overflow for [min - x] # x < 0 => [min - x] != min => no overflow for (-1) * [min - x]
                        StatedConditionalExecutionCIL cilState
                           (fun state k ->
                                PerformBinaryOperation OperationType.Subtract min x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (minusDiff >>= y, state)))) // -(min - x) >= y
                            sub
                            (this.Raise this.OverflowException))
                    pushFunctionResults
        this.SignedCheckOverflow checkOverflowForSigned cilState
    member private x.Newarr (cfg : cfg) offset (cilState : cilState) =
        let (>>=) = API.Arithmetics.(>>=)
        let elemType = resolveTermTypeFromMetadata cilState.state cfg (offset + OpCodes.Newarr.Size)
        match cilState.opStack with
        | numElements :: stack ->
            StatedConditionalExecutionCIL {cilState with opStack = stack}
                (fun state k -> k (numElements >>= TypeUtils.Int32.Zero, state))
                (fun cilState k ->
                    let ref, state = Memory.AllocateDefaultArray cilState.state [numElements] (ArrayType(elemType, Vector))
                    k [ref, {cilState with state = state}])
                (x.Raise x.OverflowException)
                pushFunctionResults
        | _ -> __notImplemented__()

    // -------------------------------- ExplorerBase operations -------------------------------------
    override x.Invoke codeLoc =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            let interpreter = new CodePortionInterpreter(x, ilmm, findCfg ilmm, [])
            interpreter.Invoke
        | :? ILCodePortion as ilcode ->
            let ilmm = ilcode.FuncId :?> ILMethodMetadata
            let interpreter = new CodePortionInterpreter(x, ilcode, findCfg ilmm, ilcode.RecursiveVertices)
            interpreter.Invoke
        | _ -> internalfail "unhandled ICodeLocation instance"
    override x.MakeMethodIdentifier m = { methodBase = m } :> IMethodIdentifier
    member x.ExecuteInstruction (cfg : cfg) (offset : int) (cilState : cilState) =



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
        let k = API.Enter opCode cilState.state id
        let newSts = opcode2Function.[hashFunction opCode] cfg offset newOffsets cilState
        k ()

//        if opCode = OpCodes.Isinst then
//            newSts |> List.iteri (fun i (_, cilState : cilState) -> Logger.printLog Logger.Trace "number = %i state = %O\n" i cilState.state)

        if opCode = Reflection.Emit.OpCodes.Add_Ovf then ()
        let leaveInstructionExecuted = opCode = OpCodes.Leave || opCode = OpCodes.Leave_S
        newSts |> List.map (fun (d, cilState : cilState) -> d, {cilState with leaveInstructionExecuted = leaveInstructionExecuted})
