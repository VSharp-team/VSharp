namespace VSharp.Core

open VSharp
open System.Text
open System.Collections.Generic
open VSharp.Utils

type compositionContext =
    { mtd : termMetadata; addr : concreteHeapAddress }
    static member Empty = { mtd = Metadata.empty; addr = [] }

type stack = mappedStack<stackKey, term>
type pathCondition = term list
type entry = { key : stackKey; mtd : termMetadata; typ : termType }
type stackFrame = { func : (IFunctionIdentifier * pathCondition) option; entries : entry list }
type frames = { f : stackFrame stack; sh : stackHash }
type 'key generalizedHeap when 'key : equality =
    | Defined of bool * 'key heap // bool = restricted
    | HigherOrderApplication of term * concreteHeapAddress
    | RecursiveApplication of ICodeLocation * concreteHeapAddress
    | Composition of state * compositionContext * 'key generalizedHeap
    | Mutation of 'key generalizedHeap * 'key heap
    | Merged of (term * 'key generalizedHeap) list
and staticMemory = termType generalizedHeap
and typeVariables = mappedStack<typeId, termType> * typeId list stack
and state = { stack : stack; heap : term generalizedHeap; statics : staticMemory; frames : frames; pc : pathCondition; typeVariables : typeVariables }

type IActivator =
    abstract member CreateInstance : locationBinding -> System.Type -> term list -> state -> (term * state)

type IStatedSymbolicConstantSource =
    inherit ISymbolicConstantSource
    abstract Compose : compositionContext -> state -> term

type IStatedSymbolicTypeSource =
    inherit ISymbolicTypeSource
    abstract TypeCompose : compositionContext -> state -> termType

[<AbstractClass>]
type TypeExtractor() =
    abstract TypeExtract : termType -> termType
    override x.Equals other = x.GetType() = other.GetType()
    override x.GetHashCode() = x.GetType().GetHashCode()
type private IdTypeExtractor() =
    inherit TypeExtractor()
    override x.TypeExtract t = t
type private ArrayTypeExtractor() =
    inherit TypeExtractor()
    override x.TypeExtract t =
        match t with
        | ArrayType(e, _) -> e
        | _ -> t
[<AbstractClass>]
type TermExtractor() =
    abstract Extract : term -> term
    override x.Equals other = x.GetType() = other.GetType()
    override x.GetHashCode() = x.GetType().GetHashCode()
type private IdTermExtractor() =
    inherit TermExtractor()
    override x.Extract t = t
type IExtractingSymbolicConstantSource =
    inherit IStatedSymbolicConstantSource
    abstract WithExtractor : TermExtractor -> IExtractingSymbolicConstantSource
type IExtractingSymbolicTypeSource =
    inherit IStatedSymbolicTypeSource
    abstract WithTypeExtractor : TypeExtractor -> IExtractingSymbolicTypeSource

module internal State =

// ------------------------------- Primitives -------------------------------

    let Defined r h = Defined(r, h)

    let empty : state = {
        stack = MappedStack.empty;
        heap = Defined false Heap.empty;
        statics = Defined false Heap.empty;
        frames = { f = Stack.empty; sh = List.empty };
        pc = List.empty;
        typeVariables = (MappedStack.empty, Stack.empty)
    }

    let emptyRestricted : state = {
        stack = MappedStack.empty;
        heap = Defined true Heap.empty;
        statics = Defined true Heap.empty;
        frames = { f = Stack.empty; sh = List.empty };
        pc = List.empty;
        typeVariables = (MappedStack.empty, Stack.empty)
    }

    let emptyCompositionContext : compositionContext = compositionContext.Empty
    let private isZeroAddress (x : concreteHeapAddress) =
        x = [0]
    let composeAddresses (a1 : concreteHeapAddress) (a2 : concreteHeapAddress) : concreteHeapAddress =
        if isZeroAddress a2 then a2 else List.append a1 a2
    let decomposeAddresses (a1 : concreteHeapAddress) (a2 : concreteHeapAddress) : concreteHeapAddress =
        List.minus a1 a2
    let composeContexts (c1 : compositionContext) (c2 : compositionContext) : compositionContext =
        { mtd = Metadata.combine c1.mtd c2.mtd; addr = composeAddresses c1.addr c2.addr }
    let decomposeContexts (c1 : compositionContext) (c2 : compositionContext) : compositionContext =
        { mtd = c1.mtd; addr = decomposeAddresses c1.addr c2.addr }

    let nameOfLocation (topLevel, path) = List.map toString path |> cons (toString topLevel) |> join "."

    let readStackLocation (s : state) key = MappedStack.find key s.stack
    let readHeapLocation (s : term heap) key = s.heap.[key]

    let isAllocatedOnStack (s : state) key = MappedStack.containsKey key s.stack

    let private newStackRegion metadata (s : state) frame frameMetadata sh : state =
        let pushOne (map : stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; mtd = metadata; typ = typ }, MappedStack.push key term map
            | Unspecified -> { key = key; mtd = metadata; typ = typ }, MappedStack.reserve key map
        let locations, newStack = frame |> List.mapFold pushOne s.stack
        let f' = Stack.push s.frames.f { func = frameMetadata; entries = locations }
        { s with stack = newStack; frames = {f = f'; sh = sh} }

    let newStackFrame metadata (s : state) funcId frame : state =
        let frameMetadata = Some(funcId, s.pc)
        let sh' = frameMetadata.GetHashCode()::s.frames.sh
        newStackRegion metadata s frame frameMetadata sh'

    let newScope metadata (s : state) frame : state =
        newStackRegion metadata s frame None s.frames.sh

    let pushToCurrentStackFrame (s : state) key value = MappedStack.push key value s.stack
    let popStack (s : state) : state =
        let popOne (map : stack) entry = MappedStack.remove map entry.key
        let { func = metadata; entries = locations } = Stack.peek s.frames.f
        let f' = Stack.pop s.frames.f
        let sh = s.frames.sh
        let sh' =
            match metadata with
            | Some _ ->
                assert(not <| List.isEmpty sh)
                List.tail sh
            | None -> sh
        { s with stack = List.fold popOne s.stack locations; frames = { f = f'; sh = sh'} }

    let writeStackLocation (s : state) key value : state =
        { s with stack = MappedStack.add key value s.stack }

    let stackFold = MappedStack.fold

    let private heapFold keyFolder termFolder acc h =
        Heap.fold (fun acc k value -> let acc = keyFolder acc k in termFolder acc value) acc h

    let rec private generalizedHeapFold<'a, 'b when 'b : equality> (keyFolder : 'a -> 'b -> 'a) (typeFolder : 'a -> termType -> 'a) (termFolder : 'a -> term -> 'a) (acc : 'a) = function
        | Defined(_, h) -> heapFold keyFolder termFolder acc h
        | Composition(h1, _, h2) ->
            let acc = fold typeFolder termFolder acc h1
            generalizedHeapFold keyFolder typeFolder termFolder acc h2
        | Mutation(h1, h2) ->
            let acc = generalizedHeapFold keyFolder typeFolder termFolder acc h1
            heapFold keyFolder termFolder acc h2
        | Merged ghs ->
            List.fold (fun acc (g, h) -> let acc = termFolder acc g in generalizedHeapFold keyFolder typeFolder termFolder acc h) acc ghs
        | _ -> acc

    and fold typeFolder termFolder acc state =
        let acc = stackFold (fun acc _ v -> termFolder acc v) acc state.stack
        let acc = generalizedHeapFold termFolder typeFolder termFolder acc state.heap
        generalizedHeapFold typeFolder typeFolder termFolder acc state.statics

    let inline private entriesOfFrame f = f.entries
    let inline private keyOfEntry en = en.key

    let typeOfStackLocation (s : state) key =
        let forMatch = List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = _; typ = t } -> if l = key then Some t else None)) s.frames.f
        match forMatch with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let private metadataOfStackLocation (s : state) key =
        match List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = m; typ = _ } -> if l = key then Some m else None)) s.frames.f with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let withPathCondition (s : state) cond : state = { s with pc = cond::s.pc }
    let popPathCondition (s : state) : state =
        match s.pc with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> { s with pc = p' }

    let stackOf (s : state) = s.stack
    let heapOf (s : state) = s.heap
    let staticsOf (s : state) = s.statics
    let framesOf (s : state) = s.frames
    let framesHashOf (s : state) = s.frames.sh
    let pathConditionOf (s : state) = s.pc

    let withHeap (s : state) h' = { s with heap = h' }
    let withStatics (s : state) m' = { s with statics = m' }

    let mkMetadata (location : locationBinding) state =
        { origins = [{ location = location; stack = framesHashOf state}]; misc = null }

    let pushTypeVariablesSubstitution state subst =
        assert (subst <> [])
        let oldMappedStack, oldStack = state.typeVariables
        let newStack = subst |> List.unzip |> fst |> Stack.push oldStack
        let newMappedStack = subst |> List.fold (fun acc (k, v) -> MappedStack.push k v acc) oldMappedStack
        { state with typeVariables = (newMappedStack, newStack) }

    let popTypeVariablesSubstitution state =
        let oldMappedStack, oldStack = state.typeVariables
        let toPop = Stack.peek oldStack
        let newStack = Stack.pop oldStack
        let newMappedStack = List.fold MappedStack.remove oldMappedStack toPop
        { state with typeVariables = (newMappedStack, newStack) }

    let rec substituteTypeVariables ctx (state : state) typ =
        let substituteTypeVariables = substituteTypeVariables ctx state
        let substitute constructor t args = constructor t (List.map substituteTypeVariables args)
        match typ with
        | Void
        | Bottom
        | Null
        | Bool
        | Numeric _ -> typ
        | StructType(t, args) -> substitute Types.StructType t args
        | ClassType(t, args) -> substitute Types.ClassType t args
        | InterfaceType(t, args) -> substitute Types.InterfaceType t args
        | TypeVariable(Id _ as key) ->
            let ms = state.typeVariables |> fst
            if MappedStack.containsKey key ms then MappedStack.find key ms else typ
        | ArrayType(t, dim) -> ArrayType(substituteTypeVariables t, dim)
        | Pointer t -> Pointer(substituteTypeVariables t)

// ------------------------------- Memory layer -------------------------------

    type private NullActivator() =
        interface IActivator with
            member x.CreateInstance _ _ _ _ =
                internalfail "activator is not ready"
    let mutable private activator : IActivator = new NullActivator() :> IActivator
    let configure act = activator <- act
    let createInstance mtd typ args state = activator.CreateInstance (Metadata.firstOrigin mtd) typ args state

    let mutable genericLazyInstantiator : termMetadata -> fql -> termType -> unit -> term =
        fun _ _ _ () -> internalfailf "generic lazy instantiator is not ready"

    let stackLazyInstantiator state key =
        let t = typeOfStackLocation state key
        let metadata = metadataOfStackLocation state key
        let fql = TopLevelStack key, []
        genericLazyInstantiator metadata fql t

    let mutable readHeap : termMetadata -> bool -> term heap -> term -> termType -> term =
        fun _ _ _ -> internalfail "read for heap is not ready"

    let mutable readStatics : termMetadata -> bool -> termType heap -> termType -> termType -> term =
        fun _ _ _ -> internalfail "read for statics is not ready"

    let mutable readTerm : termMetadata -> bool -> term -> fql -> termType -> term =
        fun _ _ _ -> internalfail "read for term is not ready"

    let mutable fillHoles : compositionContext -> state -> term -> term =
        fun _ _ _ -> internalfail "fillHoles is not ready"

// ------------------------------- Pretty-printing -------------------------------

    let private compositionToString s1 s2 =
        sprintf "%s ⚪ %s" s1 s2

    let private dumpHeap<'a, 'b when 'a : equality and 'b : comparison> keyToString valueToString (sorter : 'a -> 'b) prefix n r h (concrete : StringBuilder) (ids : Dictionary<int, string>) =
        let id = ref ""
        if ids.TryGetValue(hash h, id) then !id, n, concrete
        else
            let freshIdentifier = sprintf "%s%d%s" prefix n (if r then "[restr.]" else "")
            ids.Add(hash h, freshIdentifier)
            freshIdentifier, n+1, concrete.Append(sprintf "\n---------- %s = ----------\n" freshIdentifier).Append(Heap.dump h keyToString valueToString sorter)

    let rec private dumpGeneralizedHeap<'a, 'b when 'a : equality and 'b : comparison> keyToString valueToString sorter prefix n (concrete : StringBuilder) (ids : Dictionary<int, string>) = function
        | Defined(r, s) when Heap.isEmpty s -> (if r then "<empty[restr.]>" else "<empty>"), n, concrete
        | Defined(r, s) -> dumpHeap<'a, 'b> keyToString valueToString sorter prefix n r s concrete ids
        | HigherOrderApplication(f, _) -> sprintf "app(%O)" f, n, concrete
        | RecursiveApplication(f, _) -> sprintf "recapp(%O)" f, n, concrete // TODO: add recursive definition into concrete section
        | Mutation(h, h') ->
            let s, n, concrete = dumpGeneralizedHeap keyToString valueToString sorter prefix n concrete ids h
            let s', n, concrete = dumpHeap<'a, 'b> keyToString valueToString sorter prefix n false h' concrete ids
            sprintf "write(%s, %s)" s s', n, concrete
        | Composition(state, _, h') ->
            let s, n, concrete = dumpMemoryRec state n concrete ids
            let s', n, concrete = dumpGeneralizedHeap keyToString valueToString sorter prefix n concrete ids h'
            compositionToString s s', n, concrete
        | Merged ghs ->
            let gss, (n, concrete) =
                List.mapFold (fun (n, concrete) (g, h) ->
                        let s, n, concrete = dumpGeneralizedHeap keyToString valueToString sorter prefix n concrete ids h
                        sprintf "(%O, %s)" g s, (n, concrete))
                    (n, concrete) ghs
            gss |> join ",\n\t" |> sprintf "merge[\n\t%s]", n, concrete

    and private dumpMemoryRec s n concrete ids =
        let heapValueToString typ v =
            match v.term with
            | Class _ -> sprintf "%O %O" typ v
            | _ -> toString v
        let sorter = term >> function
            | Concrete(value, t) when t = Numeric typedefof<int> -> value :?> concreteHeapAddress
            | _ -> [System.Int32.MaxValue]
        let sh, n, concrete = dumpGeneralizedHeap toString heapValueToString sorter "h" n concrete ids s.heap
        let mh, n, concrete = dumpGeneralizedHeap toString (always toString) toString "s" n concrete ids s.statics
        sprintf "{ heap = %s, statics = %s }" sh mh, n, concrete

    let dumpMemory (s : state) =
        let dump, _, concrete = dumpMemoryRec s 0 (new StringBuilder()) (new Dictionary<int, string>())
        if concrete.Length = 0 then dump else sprintf "%s where%O" dump concrete
