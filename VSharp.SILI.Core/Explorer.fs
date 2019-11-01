namespace VSharp.Core

#nowarn "69"

open VSharp
open System.Reflection

type IMethodIdentifier =
    inherit IFunctionIdentifier
    abstract IsStatic : bool
    abstract DeclaringType : System.Type
    abstract DeclaringAssembly : Assembly
    abstract Token : string
type IDelegateIdentifier =
    inherit IFunctionIdentifier
    abstract ContextFrames : frames

type ILCodePortion(vertexNumber : int, recursiveVertices : int list, funcId : IFunctionIdentifier, state : state) =
    member x.VertexNumber with get() = vertexNumber
    member x.RecursiveVertices with get() = recursiveVertices
    member x.Frames with get() = state.frames
    member x.FuncId with get() = funcId
    override x.Equals(b) =
        match b with
        | :? ILCodePortion as ilcode -> ilcode.VertexNumber = vertexNumber && ilcode.RecursiveVertices = recursiveVertices && ilcode.FuncId = funcId
        | _ -> false
    override x.GetHashCode() =
        let codeLoc = x :> ICodeLocation
        codeLoc.Location.GetHashCode()
    override x.ToString() = sprintf "Vertex = %d, RV = %O" vertexNumber recursiveVertices
    interface ICodeLocation with
        override x.Location = (vertexNumber, recursiveVertices) :> obj

module internal Explorer =
    let formInitialStatics metadata typ =
        let staticMemoryEntry = Memory.staticMemoryLazyInstantiator metadata typ ()
        let key = makeTopLevelKey TopLevelStatics typ typ
        Heap.add key staticMemoryEntry Heap.empty

    type private recursionOutcomeSource = // TODO: delete this and use LI (using new TopLevelAddress)
        {id : ICodeLocation; state : state; name : string transparent; typ : termType;
            location : term option; extractor : TermExtractor; typeExtractor : TypeExtractor}
        interface IExtractingSymbolicConstantSource with
            override x.SubTerms = Seq.empty
            override x.WithExtractor e = {x with extractor = e} :> IExtractingSymbolicConstantSource
        interface IExtractingSymbolicTypeSource with
            override x.WithTypeExtractor e = {x with typeExtractor = e} :> IExtractingSymbolicTypeSource
            override x.TypeCompose ctx state =
                (x :> IStatedSymbolicConstantSource).Compose ctx state |> typeOf |> x.typeExtractor.TypeExtract
            override x.TypeEquals other =
                match other with
                | :? recursionOutcomeSource as ros -> x.id = ros.id && x.typ = ros.typ
                | _ -> false
        override x.ToString () = sprintf "id = %O, name = %O, location = %O" x.id x.name x.location

    let (|RecursionOutcome|_|) (src : ISymbolicConstantSource) =
        match src with
        | :? recursionOutcomeSource as ro -> Some(ro.id, ro.state, ro.location, ro.extractor :? IdTermExtractor)
        | _ -> None

    let private mutateStackClosure mtd (codeLoc : ICodeLocation) state =
        let mutateLocation st (frame : entry) =
            let location = StackRef mtd frame.key []
            let name = sprintf "μ[%O, %s]" codeLoc (fst frame.key)
            let typ = frame.typ
            let source = {id = codeLoc; state = state; name = {v=name}; typ = typ; location = Some location; extractor = IdTermExtractor(); typeExtractor = IdTypeExtractor()}
            let fql = makeTopLevelFQL TopLevelStack frame.key
            let value = Memory.makeSymbolicInstance mtd source name typ fql
            Memory.mutateStack mtd st frame.key [] value
        let frames =
            match codeLoc with
            | :? IDelegateIdentifier as di -> di.ContextFrames.f
            | :? ILCodePortion as ilcode -> [List.head ilcode.Frames.f]
            | _ -> []
        frames |> List.fold (fun state frame -> List.fold mutateLocation state frame.entries) state

    let recursionApplicationResult mtd (codeLoc : ICodeLocation) name state k =
        let typ =
            match codeLoc with
            | :? IFunctionIdentifier as funcId -> funcId.ReturnType
            | :? ILCodePortion -> typedefof<System.Void>
            | _ -> internalfail "some new ICodeLocation"
            |> Types.Constructor.fromDotNetType
            |> State.substituteTypeVariables State.emptyCompositionContext state
        let source = {id = codeLoc; state = state; name = {v=name}; typ = typ; location = None; extractor = IdTermExtractor(); typeExtractor = IdTypeExtractor()}
        Memory.makeSymbolicInstance mtd source name typ None |> k

    let recursionApplication mtd codeLoc state addr k =
        let name = IdGenerator.startingWith <| sprintf "μ[%O]_" codeLoc
        recursionApplicationResult mtd codeLoc name state (fun res ->
        let heapSymbol = RecursiveApplication(codeLoc, addr)
        let staticsSymbol = RecursiveApplication(codeLoc, addr)
        let ctx : compositionContext = { mtd = mtd; addr = addr }
        let heap = Memory.composeHeapsOf ctx state heapSymbol
        let statics = Memory.composeStaticsOf ctx state staticsSymbol
        let recursiveState = { mutateStackClosure mtd codeLoc state with heap = heap; statics = statics }
        k (res, recursiveState))

    let higherOrderApplication mtd funcId (state : state) k =
        let addr = [Memory.freshAddress()]
        let name = IdGenerator.startingWith <| sprintf "λ[%O]_" funcId
        recursionApplicationResult mtd funcId name state (fun res ->
        let higherOrderState =
            { mutateStackClosure mtd funcId state with
                heap = HigherOrderApplication(res, addr);
                statics = HigherOrderApplication(res, addr) }
        k (res , higherOrderState))

    type recursionOutcomeSource with
        interface IExtractingSymbolicConstantSource with
            override x.Compose ctx state =
                let state' = Memory.composeStates ctx state x.state
                let source' = {x with state = state'}
                Constant ctx.mtd x.name.v source' x.typ
