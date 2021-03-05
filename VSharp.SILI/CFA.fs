namespace VSharp.Analyzer

open System
open System.Reflection
open System.Collections.Generic
open System.Reflection.Emit
open VSharp.Interpreter.IL
open FSharpx.Collections
open VSharp
open VSharp.Core
open CilStateOperations

module TermUtils =
    let internal term (t : term) = t.term

    let internal getAddressTermFromRefOrPtr getTerm refOrPtr =
        let rec getLastAddress = function
            | StructField(addr, _) -> getLastAddress addr
            | addr -> addr
        let getAddressTerm = term >> function
            | Ptr(Some addr, _, _)
            | Ref addr -> getLastAddress addr |> getTerm
            | _ -> __unreachable__()
        GuardedApplyExpression refOrPtr getAddressTerm

    let internal isConcrete (term : term) =
        match term.term with
        | Concrete _ -> true
        | _ -> false


    let internal (|ConcreteHeapAddress|_|) = term >> (|ConcreteHeapAddress|_|)
    let internal isConcreteHeapAddress = function
        | ConcreteHeapAddress _ -> true
        | _ -> false

    let rec internal isConcreteAddress (address : address) =
        match address with
        | PrimitiveStackLocation _
        | BoxedLocation _
        | StaticField _ -> true
        | StackBufferIndex (_, term) -> isConcrete term
        | StructField (address, _) -> isConcreteAddress address
        | ClassField (heapAddress, _)
        | ArrayIndex (heapAddress, _, _)
        | ArrayLowerBound (heapAddress, _, _)
        | ArrayLength (heapAddress, _, _) -> isConcreteHeapAddress heapAddress

    let internal isConcretePtr = function
        | Ptr(None, _, None) -> true
        | Ptr(None, _, Some shift) -> isConcrete shift
        | Ptr(Some address, _, None) -> isConcreteAddress address
        | Ptr(Some address, _, Some shift) -> isConcreteAddress address && isConcrete shift
        | _ -> false

[<StructuralEquality;NoComparison>]
type opStackSource =
    {shift : uint32; typ : symbolicType; time : vectorTime}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.time
        override x.TypeOfLocation = x.typ
        override x.Compose state =
            let result = Memory.GetOpStackItem (int x.shift) state.opStack
            assert(Types.canCastImplicitly result x.typ)
            Types.Cast result x.typ

[<StructuralEquality;NoComparison>]
type stackBufferIndexAddress =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = Types.IndexType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | StackBufferIndex(_, term) -> term
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type classFieldAddress =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | ClassField(term, _) -> term
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type arrayIndexHeapAddress =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | ArrayIndex(term, _, _) -> term
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type arrayIndexItem =
    {baseSource : IMemoryAccessConstantSource; i : int}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | ArrayIndex(_, indices, _) -> List.item x.i indices
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type arrayLowerBoundHeapAddress =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | ArrayLowerBound(term, _, _) -> term
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type arrayLowerBoundDimension =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | ArrayLowerBound(_, dim, _) -> dim
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type arrayLengthAddress =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | ArrayLength(term, _, _) -> term
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type arrayLengthDimension =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getTerm = function
                | ArrayLength(_, dim, _) -> dim
                | _ -> __unreachable__()
            TermUtils.getAddressTermFromRefOrPtr getTerm baseTerm

[<StructuralEquality;NoComparison>]
type pointerShift =
    {baseSource : IMemoryAccessConstantSource}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.baseSource.Time
        override x.TypeOfLocation = AddressType
        override x.Compose state =
            let baseTerm = x.baseSource.Compose state
            let getShift term =
                match term.term with
                | Ptr(_, _, Some shift) -> shift
                | _ -> __unreachable__()
            GuardedApplyExpression baseTerm getShift

module Properties =
    let internal exitVertexOffset = -1
    let internal exitVertexNumber = -1

    let internal initialVertexOffset = 0
    let internal initialVertexNumber = 0

module public CFA =
    open TermUtils

    let pushNewObjResultOnOpStack (cilState : cilState) reference (calledMethod : MethodBase) =
        let valueOnStack =
            if calledMethod.DeclaringType.IsValueType then
                  Memory.ReadSafe cilState.state reference
            else reference
        push valueOnStack cilState

    type Vertex private(id, m : MethodBase, ip : ip, opStack : operationStack) =
        static let ids : Dictionary<MethodBase, int> = Dictionary<_,_>()
        let lemmas = Lemmas(m, ip)
        let paths = Paths(m, ip)
        let queries = Queries(m, ip)
        let solver = null //Solver.SolverPool.mkSolver()
        let errors = List<cilState>()
        let incomingEdges: List<Edge> = List<_>()
        let outgoingEdges: List<Edge> = List<_>()

        override x.GetHashCode() = (m, ip).GetHashCode()
        override x.Equals(o : obj) =
            match o with
            | :? Vertex as other -> x.Method = other.Method && x.Ip = other.Ip
            | _ -> false
        interface IComparable with
            override x.CompareTo(other) =
                match other with
                | :? Vertex as other when x.Method.Equals(other.Method) -> x.GetHashCode().CompareTo(other.GetHashCode())
                | :? Vertex as other -> x.Method.MetadataToken.CompareTo(other.Method.MetadataToken)
                | _ -> -1
        member x.AddErroredStates (newErrors : cilState list) =
            errors.AddRange(newErrors)

        member x.Id with get() = id
        member x.Lemmas = lemmas
        member x.Paths with get () = paths
        member x.Queries = queries
        member x.Solver = solver
        member x.IncomingEdges = incomingEdges
        member x.OutgoingEdges = outgoingEdges
        member x.Ip with get() = ip
        member x.OpStack with get() = opStack
        member x.Method with get() = m
        member x.IsMethodStartVertex with get() = ip = Instruction 0
        member x.IsMethodExitVertex with get() = ip = Exit
        override x.ToString() =
            sprintf "(Method = %O, ip = %O, id = %d)\n" m ip id +
            sprintf "Edges count = %d\n" x.OutgoingEdges.Count +
            "Edges: \n" + Seq.fold (fun acc edge -> acc + edge.ToString() + "\n") "" x.OutgoingEdges
        static member CreateVertex method offset opStack =
            let id = Dict.getValueOrUpdate ids method (always 0)
            ids.[method] <- id + 1
            Vertex(id, method, offset, opStack)
    and
        [<AbstractClass>]
        Edge(src : Vertex, dst : Vertex) =
        abstract member Type : string
        abstract member PropagatePath : cilState -> cilState list
        member x.PrintLog msg obj = Logger.trace "[%s] %s: %O" (x.commonToString()) msg obj
        member x.Src = src
        member x.Dst = dst
        member x.Method = x.Src.Method
        member x.CommonFilterStates state =
            let newPc = PC.squashPC state.pc
            if newPc <> Terms.False then
//                if dst.IsMethodExitVertex then ()
//                dst.Paths.Add {lvl = lvl; state = state}
                true
            else false
        override x.ToString() = x.commonToString()
        member x.commonToString() =
            sprintf "%s ID:[%d --> %d] IP:[%O --> %O] Method:%s"
                x.Type x.Src.Id x.Dst.Id x.Src.Ip x.Dst.Ip (Reflection.GetFullMethodName x.Method)


    type 'a unitBlock =
        {
            entity : 'a
            entryPoint : Vertex
            exitVertex : Vertex
            vertices : Dictionary<int, Vertex>
        }
        with
        member x.AddVertex (v : Vertex) =
            if not <| x.vertices.ContainsKey v.Id then
                x.vertices.Add(v.Id, v)
        member x.WithEntryPoint v = {x with entryPoint = v}
        member x.WithExitVertex v = {x with exitVertex = v}
        member x.FindCallVertex id = x.vertices.[id]

        static member private CreateEmpty entity method entryOffset exitOffset =
            let entry = Vertex.CreateVertex method entryOffset emptyOpStack
            let exit = Vertex.CreateVertex method exitOffset emptyOpStack
            let vertices = Dictionary<int, Vertex>()
            vertices.Add(entry.Id, entry)
            vertices.Add(exit.Id, exit)
            {
                entity = entity
                entryPoint = entry
                exitVertex = exit
                vertices = vertices
            }
        static member CreateEmptyForMethod (method : MethodBase) =
            unitBlock<'a>.CreateEmpty method method (Instruction Properties.initialVertexOffset) Exit
        static member CreateEmptyForFinallyClause (m : MethodBase) (ehc : ExceptionHandlingClause) =
            let entryOffset = ehc.HandlerOffset
            // TODO: check if this formula is forever true
            let exitOffset = ehc.HandlerOffset + ehc.HandlerLength - 1
            unitBlock<'a>.CreateEmpty ehc m (Instruction entryOffset) (Instruction exitOffset)
        override x.ToString() =
            Seq.fold (fun acc vertex -> acc + "\n" + vertex.ToString()) "" x.vertices.Values

    type cfa =
            {
              cfg : CFG.cfgData
              body : MethodBase unitBlock
              finallyHandlers : Dictionary<offset, ExceptionHandlingClause unitBlock>
            }
        with
            member x.Method = x.cfg.methodBase
            member x.FindOrCreateFinallyHandler (ehc : ExceptionHandlingClause) =
                let offset = ehc.HandlerOffset
                if x.finallyHandlers.ContainsKey offset then x.finallyHandlers.[offset]
                else
                    let block = unitBlock<ExceptionHandlingClause>.CreateEmptyForFinallyClause x.Method ehc
                    x.finallyHandlers.Add(offset, block)
                    block

            member x.WithEntryPoint v = {x with body = x.body.WithEntryPoint v}
            member x.WithExitVertex v = {x with body = x.body.WithExitVertex v}
            member x.WithFinallyEntryPoint offset v = x.finallyHandlers.[offset].WithEntryPoint v |> ignore; x
            member x.WithFinallyExitVertex offset v = x.finallyHandlers.[offset].WithExitVertex v |> ignore; x
            override x.ToString() =
                let separator = "\n"
                "Method: " + x.Method.ToString() + separator +
                x.body.ToString() + separator +
                Seq.fold (fun acc block -> acc + block.ToString() + separator) "" (x.finallyHandlers.Values)



    type StepEdge(src : Vertex, dst : Vertex, effect : cilState) =
        inherit Edge(src, dst)
        do
            Prelude.releaseAssert(Map.isEmpty effect.state.callSiteResults)
        override x.Type = "StepEdge"
        override x.PropagatePath (cilState : cilState) =
            compose cilState effect (fun cilStates ->
                x.PrintLog "composition left"  <| dump cilState
                x.PrintLog "composition right" <| dump effect
                List.iter (dump >> x.PrintLog (sprintf "composition resulted")) cilStates

                assert(List.forall (fun (cilState' : cilState) -> cilState'.state.frames = cilState.state.frames) cilStates)

                // Do NOT turn this List.fold into List.exists to be sure that EVERY returned state is propagated
                let goodStates = List.filter (stateOf >> x.CommonFilterStates) cilStates
                if List.length goodStates <> List.length cilStates then Logger.trace "Some states were not propagated from %O to %O" src.Ip cilState.ip
                cilStates
            )

        member x.Effect = effect
        member x.VisibleVariables() = __notImplemented__()

        override x.ToString() = dump effect

    type CallEdge(src : Vertex, dst : Vertex, callSite : callSite, stateWithArgsOnFrameAndAllocatedType : state, numberToDrop, interpreter : ILInterpreter) =
        inherit Edge(src, dst)
        do
           assert(List.length stateWithArgsOnFrameAndAllocatedType.frames = 2)
        override x.Type = "Call"
        override x.PropagatePath (cilStateBeforeCall : cilState) =
            let k (cilStates : cilState list) =
                let propagateStateAfterCall acc (resultCilState : cilState) =
                    let resultState = resultCilState.state
                    let initialState = cilStateBeforeCall.state
                    assert(initialState.frames = resultState.frames)
                    x.PrintLog "propagation through callEdge" callSite
                    x.PrintLog "call edge: composition left" (dump cilStateBeforeCall)
                    x.PrintLog "call edge: composition result" (dump resultCilState)
                    let stateAfterCall =
                        let _, opStack = Memory.PopArgumentsFromOpStack numberToDrop initialState.opStack
                        match resultState.returnRegister with
                        | Some r ->
                            assert(callSite.HasNonVoidResult)
                            { resultState with callSiteResults = Map.add callSite resultState.returnRegister initialState.callSiteResults
                                               returnRegister = None
                                               opStack = Memory.PushToOpStack r opStack }
                        | None when callSite.opCode = OpCodes.Newobj ->
                            let reference = Memory.ReadThis stateWithArgsOnFrameAndAllocatedType callSite.calledMethod
                            let modifiedCilState = pushNewObjResultOnOpStack (withOpStack opStack resultCilState) reference callSite.calledMethod
                            { modifiedCilState.state with callSiteResults = initialState.callSiteResults }
                        | None -> { resultState with callSiteResults = initialState.callSiteResults; opStack = opStack}
                    if x.CommonFilterStates stateAfterCall then {resultCilState with state = stateAfterCall; ip = dst.Ip} :: acc
                    else acc
                List.fold propagateStateAfterCall [] cilStates

            let states = Memory.ComposeStates cilStateBeforeCall.state stateWithArgsOnFrameAndAllocatedType id
            match states with
            | [state] ->
                let cilState = {cilStateBeforeCall with state = state}
                match callSite.opCode with
                | Instruction.NewObj   when Reflection.IsDelegateConstructor callSite.calledMethod -> k [popStackOf cilState]
                | Instruction.NewObj   when Reflection.IsArrayConstructor callSite.calledMethod -> k [popStackOf cilState]
                | Instruction.Call
                | Instruction.NewObj   ->
                    interpreter.CommonCall callSite.calledMethod cilState k
                | Instruction.CallVirt -> interpreter.CommonCallVirt callSite.calledMethod cilState k
                | _ ->  __notImplemented__()
            | _ -> internalfailf "Calling %s: composition with frames unexpectedly forked!" callSite.calledMethod.Name
        member x.ExitNodeForCall() = __notImplemented__()
        member x.CallVariables() = __notImplemented__()
        override x.ToString() =
            sprintf "%s\nstate = %O\n" (base.ToString()) (API.Memory.Dump stateWithArgsOnFrameAndAllocatedType)

    module cfaBuilder =
        let private alreadyComputedCFAs = Dictionary<MethodBase, cfa>()

        [<CustomEquality; CustomComparison>]
        type bypassDataForEdges =
            { u : ip
              srcVertex : Vertex
              v : ip
              uOut : int
              vOut : int
              minSCCs : int
              opStack : operationStack
              allocatedTypes : pdict<concreteHeapAddress, symbolicType>
              lengths : pdict<arrayType, vectorRegion>
              lowerBounds : pdict<arrayType, vectorRegion>
            }
            override x.ToString() =
                let emptyState = {API.Memory.EmptyState with opStack = x.opStack }
                sprintf "u = %O, id = %d; v = %O; state = %s" x.u x.srcVertex.Id x.v (Memory.Dump emptyState)
            override x.GetHashCode() = (x.u, x.v, x.srcVertex).GetHashCode()
            override x.Equals y =
                match y with
                | :? bypassDataForEdges as y -> x.u = y.u && x.srcVertex.Equals(y.srcVertex) && x.v = y.v && x.opStack = y.opStack
                | _ -> false
            interface IComparable with
                override x.CompareTo (obj : obj) =
                    let rec compareData a1 a2 a3 b1 b2 b3 =
                        if a1 > b1 || a1 = b1 && a2 > b2 || a1 = b1 && a2 = b2 && a3 > b3 then -1
                        elif a1 = b1 && a2 = b2 && a3 = b3 then 0
                        else 1

                    match obj with
                    | :? bypassDataForEdges as other -> compareData x.minSCCs x.uOut x.vOut other.minSCCs other.uOut other.vOut
                    | _ -> -1

        let private createEmptyCFA cfg method =
            {
              cfg = cfg
              body = unitBlock<MethodBase>.CreateEmptyForMethod method
              finallyHandlers = Dictionary<_,_>()
            }

        let private addEdge (edge : Edge) =
            edge.Src.OutgoingEdges.Add edge
            edge.Dst.IncomingEdges.Add edge

        let internal shouldRemainOnOpStack (term : term) =
            match term.term with
            | Concrete _ -> true
            | Ref addr -> isConcreteAddress addr
            | Ptr _ as v -> isConcretePtr v
            | HeapRef(heapAddress, _) -> isConcreteHeapAddress heapAddress
            | _ -> false

        let private updateLI makeSource term name typ =
            let isConcrete =
                match typ with
                | AddressType -> isConcreteHeapAddress
                | _ when Types.IsInteger typ -> isConcrete
                | _ -> __notImplemented__()
            if isConcrete term then term
            else Constant name (makeSource()) typ

        let rec private makeSymbolicAddress source = function
            | StackBufferIndex(key, term) when isConcrete term |> not ->
                let source : stackBufferIndexAddress = {baseSource = source}
                StackBufferIndex(key, Constant "StackBufferIndex" source Types.IndexType)
            | StructField(address, fieldId) ->
                StructField(makeSymbolicAddress source address, fieldId)
            | ClassField(heapAddress, fieldId) when isConcreteHeapAddress heapAddress |> not ->
                let source : classFieldAddress = {baseSource = source}
                ClassField(Constant "ClassFieldAddress" source AddressType, fieldId)
            | ArrayIndex(heapAddress, indices, aType) ->
                let makeAddressSource () : arrayIndexHeapAddress = {baseSource = source}
                let address = updateLI makeAddressSource heapAddress "ArrayIndexHeapAddress" AddressType
                let makeIndexSource i () : arrayIndexItem = {baseSource = source; i = i}
                let indexName i = sprintf "ArrayIndex.[%d]" i
                let indices = List.mapi (fun i x -> updateLI (makeIndexSource i) x (indexName i) Types.IndexType) indices
                ArrayIndex(address, indices, aType)
            | ArrayLowerBound(heapAddress, dim, aType) ->
                let makeAddressSource () : arrayLowerBoundHeapAddress = {baseSource = source}
                let address = updateLI makeAddressSource heapAddress "ArrayLowerBoundHeapAddress" AddressType
                let makeDimSource () : arrayLowerBoundDimension = {baseSource = source}
                let dim = updateLI makeDimSource dim "ArrayLowerBoundDimension" Types.IndexType
                ArrayLowerBound(address, dim, aType)
            | ArrayLength(heapAddress, dim, aType) ->
                let makeAddressSource () : arrayLengthAddress = {baseSource = source}
                let address = updateLI makeAddressSource heapAddress "ArrayLengthAddress" AddressType
                let makeDimSource () : arrayLengthDimension = {baseSource = source}
                let dim = updateLI makeDimSource dim "ArrayLengthDimension" Types.IndexType
                ArrayLength(address, dim, aType)
            | address -> address

        let makeSymbolicOpStack time (opStack : operationStack) : operationStack =
            let mkSource (index : int) typ =
                {shift = uint32 index; typ = typ; time = time}

            let makeSymbolic index (v : term) =
                if shouldRemainOnOpStack v then v
                else
                    let shift = index
                    let typ = TypeOf v
                    let source = mkSource shift typ
                    let name = sprintf "opStack.[%d] from top" shift
                    match v.term with
                    | Ref address ->
                        makeSymbolicAddress source address |> Ref
                    | Ptr(address, typ, shift) ->
                        let addr = Option.map (makeSymbolicAddress source) address
                        let makeShiftSource () : pointerShift = {baseSource = source}
                        let shift = Option.map (fun shift -> updateLI makeShiftSource shift "PointerShift" Types.IndexType) shift
                        Ptr addr typ shift
                    | _ -> Memory.MakeSymbolicValue source name typ

            let symbolicOpStack = Memory.MapiOpStack makeSymbolic opStack
            symbolicOpStack

        let private executeSeparatedOpCode (methodInterpreter : MethodInterpreter) (cfg : cfg) (cilState : cilState) =
            let offset = cilState.ip.Offset()
            let opCode, calledMethod = cfg.offsetsDemandingCall.[offset]
            let callSite = { sourceMethod = cfg.methodBase; offset = offset; calledMethod = calledMethod; opCode = opCode }
            let pushFunctionResultOnOpStackIfNeeded (cilState : cilState) (methodInfo : MethodInfo) =
                if methodInfo.ReturnType = typeof<System.Void> then cilState
                else push (Terms.MakeFunctionResultConstant cilState.state callSite) cilState

            let args, cilStateWithoutArgs = InstructionsSet.retrieveActualParameters calledMethod cilState
            let this, cilState =
                match calledMethod with
                | _ when opCode = OpCodes.Newobj ->
                    let ilInterpreter = ILInterpreter(methodInterpreter)
                    let cilStates = ilInterpreter.CommonNewObj false (calledMethod :?> ConstructorInfo) cilStateWithoutArgs args id
                    assert (List.length cilStates = 1)
                    let cilState = List.head cilStates
                    assert(Option.isSome cilState.state.returnRegister)
                    let reference = Option.get cilState.state.returnRegister
                    let cilState = pushNewObjResultOnOpStack cilState reference calledMethod
                    Some reference, withNoResult cilState
                | :? ConstructorInfo -> pop cilStateWithoutArgs |> mapfst Some
                | :? MethodInfo as methodInfo when not calledMethod.IsStatic ->
                    let this, cilState = pop cilStateWithoutArgs
                    Some this, pushFunctionResultOnOpStackIfNeeded cilState methodInfo
                | :? MethodInfo as methodInfo ->
                    None, pushFunctionResultOnOpStackIfNeeded cilStateWithoutArgs methodInfo
                | _ -> internalfailf "unknown methodBase %O" calledMethod

            let numberToDrop = List.length args + if Option.isNone this || callSite.opCode = OpCodes.Newobj then 0 else 1
            let stateWithArgsOnFrame : state = methodInterpreter.ReduceFunctionSignature cilState.state calledMethod this (Specified args) false id
            let nextOffset =
                assert(cfg.graph.[offset].Count = 1)
                cfg.graph.[offset].[0]
            { cilState with ip = Instruction nextOffset; state = stateWithArgsOnFrame }, callSite, numberToDrop

        let private createVertexIfNeeded methodBase opStack (v : ip) (vertices : pdict<ip * operationStack, Vertex>)  =
            let concreteOpStack = Memory.FilterOpStack shouldRemainOnOpStack opStack
            if PersistentDict.contains (v, concreteOpStack) vertices then
                PersistentDict.find vertices (v, concreteOpStack), vertices
            else
                let dstVertex = Vertex.CreateVertex methodBase v opStack
                dstVertex, PersistentDict.add (v, concreteOpStack) dstVertex vertices

        let updateQueue (cfg : cfg) newU (d : bypassDataForEdges) (q, used) =
            let changeData w =
                match w with
                | Instruction wOffset -> {d with v = w; vOut = cfg.dfsOut.[wOffset]; minSCCs = min cfg.sccOut.[wOffset] cfg.sccOut.[wOffset]}
                | Exit -> {d with v = Exit; vOut = -1; minSCCs = -1}
                | _ -> __notImplemented__()

            let addIfNotUsed (q, used) d =
                if PersistentSet.contains d used then q, used
                else PriorityQueue.insert d q, PersistentSet.add used d

            match newU with
            | Exit -> q, used
            | Instruction offset when cfg.graph.[offset].Count = 0 -> (changeData Exit) |> addIfNotUsed (q, used)
            | Instruction vOffset -> cfg.graph.[vOffset] |> Seq.fold (fun acc wOffset -> changeData (Instruction wOffset) |> addIfNotUsed acc) (q, used)
            | _ -> __notImplemented__()

        let addEdgeAndRenewQueue createEdge (d : bypassDataForEdges) (cfg : cfg) (currentTime, vertices, q, used) (cilState' : cilState) =
//            assert(cilState'.ip = d.v)
            let s' = cilState'.state
            let dstVertex, vertices = createVertexIfNeeded cfg.methodBase s'.opStack d.v vertices
            addEdge <| createEdge cilState' dstVertex

            let bypassData = {d with u = d.v; srcVertex = dstVertex; uOut = d.vOut; opStack = s'.opStack
                                     allocatedTypes = s'.allocatedTypes; lengths = s'.lengths; lowerBounds = s'.lowerBounds }

            let newQ, newUsed =
                match cilState'.iie with
                | None -> updateQueue cfg d.v bypassData (q, used)
                | Some _ -> q, used
            VectorTime.max currentTime s'.currentTime, vertices, newQ, newUsed

        let private isConcreteHeapRef (term : term) =
            match term.term with
            | HeapRef (addr, _) -> isConcreteHeapAddress addr
            | _ -> false

        let private getTermConcreteHeapAddress (term : term) =
            match term.term with
            | HeapRef (ConcreteHeapAddress addr, _) -> addr
            | _ -> __unreachable__()

        let private prepareStateWithConcreteInfo (s : state) (d : bypassDataForEdges) =
            let concreteHeapAddresses = s.opStack |> Memory.OpStackToList |> List.filter isConcreteHeapRef |> List.map getTermConcreteHeapAddress
            let appendAllocatedTypes acc k v = if List.contains k concreteHeapAddresses then PersistentDict.add k v acc else acc
            let allocatedTypes = PersistentDict.fold appendAllocatedTypes PersistentDict.empty d.allocatedTypes
            let allocatedTypesValues = PersistentDict.values allocatedTypes

            let appendLengths acc ((t,_,_) : arrayType as k) v = if Seq.contains t allocatedTypesValues then PersistentDict.add k v acc else acc
            let appendLowerBounds acc ((t,_,_) : arrayType as k) v = if Seq.contains t allocatedTypesValues then PersistentDict.add k v acc else acc

            let lengths = PersistentDict.fold appendLengths PersistentDict.empty d.lengths
            let lowerBounds = PersistentDict.fold appendLowerBounds PersistentDict.empty d.lowerBounds
            {s with allocatedTypes = allocatedTypes; lengths = lengths; lowerBounds = lowerBounds}


        // note: entry point and exit vertex must be added to unit block
        let private computeCFAForBlock (methodInterpreter : MethodInterpreter) (initialState : state) (cfa : cfa) (block : unitBlock<'a>) =
            let ilInterpreter = ILInterpreter(methodInterpreter)
            let cfg = cfa.cfg
            let rec bypass (cfg : cfg) (q : IPriorityQueue<bypassDataForEdges>) (used : pset<bypassDataForEdges>) (vertices : pdict<ip * operationStack, Vertex>) currentTime =
                let d, q = PriorityQueue.pop q
                assert(PersistentSet.contains d used)
                let srcVertex = d.srcVertex
                assert(d.u <> Exit)
                let offset = d.u.Offset()

                let currentTime = VectorTime.advance currentTime
                let symbolicOpStack = makeSymbolicOpStack currentTime d.opStack
                let modifiedState = prepareStateWithConcreteInfo {initialState with currentTime = currentTime; startingTime = currentTime; opStack = symbolicOpStack} d

                let initialCilState = makeCilState d.u modifiedState
                if cfg.offsetsDemandingCall.ContainsKey offset then
                    let cilState', callSite, numberToDrop = executeSeparatedOpCode methodInterpreter cfg initialCilState
                    let createEdge (cilState' : cilState) dstVertex = CallEdge (srcVertex, dstVertex, callSite, cilState'.state, numberToDrop, ilInterpreter)
                    let currentTime, vertices, q, used = addEdgeAndRenewQueue createEdge d cfg (currentTime, vertices, q, used) cilState'
                    if not <| PriorityQueue.isEmpty q then bypass cfg q used vertices currentTime
                    else vertices
                else
                    let finishedStates, incompleteStates, erroredStates = ilInterpreter.ExecuteAllInstructions cfg initialCilState
                    let incompleteStates = List.filter (fun (cilState : cilState) -> cilState.ip <> srcVertex.Ip) incompleteStates
                    let goodStates = finishedStates |> List.filter (fun (cilState : cilState) -> cilState.ip = d.v)
                    srcVertex.AddErroredStates erroredStates

                    let createEdge (cilState' : cilState) dstVertex = StepEdge(d.srcVertex, dstVertex, cilState')
                    let currentTime, vertices, q, used = (goodStates @ incompleteStates) |> List.fold (addEdgeAndRenewQueue createEdge d cfg) (currentTime, vertices, q, used)
                    if not <| PriorityQueue.isEmpty q then bypass cfg q used vertices currentTime
                    else vertices
            let offset = block.entryPoint.Ip.Offset()
            let d0 = { u = block.entryPoint.Ip; srcVertex = block.entryPoint; uOut = cfg.dfsOut.[offset]
                       v = block.entryPoint.Ip; vOut = 0; minSCCs = 0; opStack = initialState.opStack
                       allocatedTypes = initialState.allocatedTypes; lengths = initialState.lengths; lowerBounds = initialState.lowerBounds }

            let q, used = updateQueue cfg block.entryPoint.Ip d0 (PriorityQueue.empty false, PersistentSet.empty)
            let vertices = PersistentDict.empty
                           |> PersistentDict.add (block.entryPoint.Ip, initialState.opStack) block.entryPoint
                           |> PersistentDict.add (block.exitVertex.Ip, block.exitVertex.OpStack) block.exitVertex
            let vertices = bypass cfg q used vertices initialState.currentTime
            vertices |> PersistentDict.values |> Seq.iter block.AddVertex

        let computeCFA (methodInterpreter : MethodInterpreter) (funcId: IFunctionIdentifier) : cfa =
            let methodBase = funcId.Method
            match alreadyComputedCFAs.ContainsKey methodBase with
            | true -> alreadyComputedCFAs.[methodBase]
            | _ ->
                let initialState, _, _ = methodInterpreter.FormInitialStateWithoutStatics funcId
                Prelude.releaseAssert(Map.isEmpty initialState.callSiteResults && Option.isNone initialState.returnRegister)

                let cfg = CFG.build methodBase
                let cfa = createEmptyCFA cfg methodBase

                computeCFAForBlock methodInterpreter initialState cfa cfa.body
                alreadyComputedCFAs.[methodBase] <- cfa
                Logger.printLog Logger.Trace "Computed cfa: %O" cfa
                cfa

type StepInterpreter() =
    inherit MethodInterpreter()

    override x.CreateInstance t args (cilState : cilState) =
        let state = {cilState.state with exceptionsRegister = Unhandled Nop}
        List.singleton <| {cilState with state = state}

    override x.EvaluateOneStep (funcId, cilState : cilState) =
        try
            let cfa : CFA.cfa = CFA.cfaBuilder.computeCFA x funcId
            let ip = cilState.ip
            let vertexWithSameOpStack (v : CFA.Vertex) =
                let vOpStack = Memory.OpStackToList v.OpStack
                let opStack = Memory.OpStackToList cilState.state.opStack
                assert(List.length opStack = List.length vOpStack)
                vOpStack
                |> List.zip opStack
                |> List.forall (fun (elementOnStateOpSTack, elementOnVertexOpStack) ->
                    if CFA.cfaBuilder.shouldRemainOnOpStack elementOnVertexOpStack then elementOnStateOpSTack = elementOnVertexOpStack else true)
            let vertices = cfa.body.vertices.Values |> Seq.filter (fun (v : CFA.Vertex) ->
                v.Ip = ip && v.OutgoingEdges.Count > 0 && vertexWithSameOpStack v) |> List.ofSeq

            match vertices with
            | [] -> base.EvaluateOneStep (funcId, cilState)
            | _ ->
                let propagateThroughEdge acc (edge : CFA.Edge) =
                    acc @ edge.PropagatePath cilState
                List.fold (fun acc (v : CFA.Vertex) -> Seq.fold propagateThroughEdge acc v.OutgoingEdges) [] vertices
        with
        | :? InsufficientInformationException as iie -> base.EvaluateOneStep (funcId, cilState)
