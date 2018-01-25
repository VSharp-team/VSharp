namespace VSharp.Core

open VSharp
open System.Text
open System.Collections.Generic
open VSharp.Utils

type CompositionContext = { mtd : TermMetadata; addr : ConcreteHeapAddress; time : timestamp }

type Stack = MappedStack.stack<StackKey, MemoryCell<Term>>
type PathCondition = Term list
type Entry = { key : StackKey; mtd : TermMetadata; typ : TermType option }
type StackFrame = { func : (IFunctionIdentifier * PathCondition) option; entries : list<Entry> ; time : timestamp }
type Frames = { f : Stack.stack<StackFrame>; sh : StackHash }
type GeneralizedHeap =
    | Defined of bool * SymbolicHeap  // bool = restricted
    | HigherOrderApplication of Term * ConcreteHeapAddress * timestamp
    | RecursiveApplication of IFunctionIdentifier * ConcreteHeapAddress * timestamp
    | Composition of State * CompositionContext * GeneralizedHeap
    | Mutation of GeneralizedHeap * SymbolicHeap
    | Merged of (Term * GeneralizedHeap) list
and StaticMemory = GeneralizedHeap
and State = { stack : Stack; heap : GeneralizedHeap; statics : StaticMemory; frames : Frames; pc : PathCondition }

type IActivator =
    abstract member CreateInstance : LocationBinding -> System.Type -> Term list -> State -> (Term * State)

module internal State =
    module SymbolicHeap = Heap

// ------------------------------- Primitives -------------------------------

    let Defined r h = Defined(r, h)

    let empty : State = {
        stack = MappedStack.empty;
        heap = Defined false SymbolicHeap.empty;
        statics = Defined false SymbolicHeap.empty;
        frames = { f = Stack.empty; sh = List.empty };
        pc = List.empty
    }

    let emptyRestricted : State = {
        stack = MappedStack.empty;
        heap = Defined true SymbolicHeap.empty;
        statics = Defined true SymbolicHeap.empty;
        frames = { f = Stack.empty; sh = List.empty };
        pc = List.empty
    }

    let emptyCompositionContext : CompositionContext = { mtd = Metadata.empty; addr = []; time = Timestamp.zero }
    let composeAddresses (a1 : ConcreteHeapAddress) (a2 : ConcreteHeapAddress) : ConcreteHeapAddress =
        List.append a1 a2
    let decomposeAddresses (a1 : ConcreteHeapAddress) (a2 : ConcreteHeapAddress) : ConcreteHeapAddress =
        List.minus a1 a2
    let composeContexts (c1 : CompositionContext) (c2 : CompositionContext) : CompositionContext =
        { mtd = Metadata.combine c1.mtd c2.mtd; addr = composeAddresses c1.addr c2.addr; time = Timestamp.compose c1.time c2.time }
    let decomposeContexts (c1 : CompositionContext) (c2 : CompositionContext) : CompositionContext =
        { mtd = c1.mtd; addr = decomposeAddresses c1.addr c2.addr; time = Timestamp.decompose c1.time c2.time }

    let nameOfLocation = term >> function
        | HeapRef(((_, t), []), _, _) -> toString t
        | StackRef((name, _), [], _) -> name
        | StaticRef(name, [], _) -> System.Type.GetType(name).FullName
        | HeapRef((_, path), _, _)
        | StackRef(_, path, _)
        | StaticRef(_, path, _) -> path |> Seq.map (fst >> toString) |> join "."
        | l ->  internalfailf "requested name of an unexpected location %O" l

    let readStackLocation (s : State) key = MappedStack.find key s.stack
    let readHeapLocation (s : SymbolicHeap) key = s.heap.[key].value

    let isAllocatedOnStack (s : State) key = MappedStack.containsKey key s.stack

    let newStackFrame time metadata (s : State) funcId frame : State =
        let pushOne (map : Stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; mtd = metadata; typ = typ }, MappedStack.push key { value = term; created = time; modified = time } map
            | Unspecified -> { key = key; mtd = metadata; typ = typ }, MappedStack.reserve key map
        let frameMetadata = Some(funcId, s.pc)
        let locations, newStack = frame |> List.mapFold pushOne s.stack
        let f' = Stack.push s.frames.f { func = frameMetadata; entries = locations; time = time }
        let sh' = frameMetadata.GetHashCode()::s.frames.sh
        { s with stack = newStack; frames = {f = f'; sh = sh'} }

    let newScope time metadata (s : State) frame : State =
        let pushOne (map : Stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; mtd = metadata; typ = typ }, MappedStack.push key { value = term; created = time; modified = time } map
            | Unspecified -> { key = key; mtd = metadata; typ = typ }, MappedStack.reserve key map
        let locations, newStack = frame |> List.mapFold pushOne s.stack
        { s with stack = newStack; frames = { s.frames with f = Stack.push s.frames.f { func = None; entries = locations; time = time } } }

    let pushToCurrentStackFrame (s : State) key value = MappedStack.push key value s.stack
    let popStack (s : State) : State =
        let popOne (map : Stack) entry = MappedStack.remove map entry.key
        let { func = metadata; entries = locations; time = _ } = Stack.peek s.frames.f
        let f' = Stack.pop s.frames.f
        let sh = s.frames.sh
        let sh' =
            match metadata with
            | Some _ ->
                assert(not <| List.isEmpty sh)
                List.tail sh
            | None -> sh
        { s with stack = List.fold popOne s.stack locations; frames = { f = f'; sh = sh'} }

    let writeStackLocation (s : State) key value : State =
        { s with stack = MappedStack.add key value s.stack }

    let stackFold = MappedStack.fold

    let inline private entriesOfFrame f = f.entries
    let inline private keyOfEntry en = en.key

    let frameTime (s : State) key =
        match List.tryFind (entriesOfFrame >> List.exists (keyOfEntry >> ((=) key))) s.frames.f with
        | Some { func = _; entries = _; time = t} -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let private typeOfStackLocation (s : State) key =
        let forMatch = List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = _; typ = t } -> if l = key then Some t else None)) s.frames.f
        match forMatch with
        | Some (Some t) -> t
        | Some None -> internalfailf "unknown type of stack location %O!" key
        | None -> internalfailf "stack does not contain key %O!" key

    let private metadataOfStackLocation (s : State) key =
        match List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = m; typ = _ } -> if l = key then Some m else None)) s.frames.f with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let withPathCondition (s : State) cond : State = { s with pc = cond::s.pc }
    let popPathCondition (s : State) : State =
        match s.pc with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> { s with pc = p' }

    let stackOf (s : State) = s.stack
    let heapOf (s : State) = s.heap
    let staticsOf (s : State) = s.statics
    let framesOf (s : State) = s.frames
    let framesHashOf (s : State) = s.frames.sh
    let pathConditionOf (s : State) = s.pc

    let withHeap (s : State) h' = { s with heap = h' }
    let withStatics (s : State) m' = { s with statics = m' }

    let stackLocationToReference state location =
        StackRef (metadataOfStackLocation state location) location []
    let staticLocationToReference term =
        match term.term with
        | Concrete(location, String) -> StaticRef term.metadata (location :?> string) []
        | _ -> __notImplemented__()

    let private heapKeyToString = term >> function
        | Concrete(:? (int list) as k, _) -> k |> List.map toString |> join "."
        | t -> toString t

    let private staticKeyToString = term >> function
        | Concrete(typeName, String) -> System.Type.GetType(typeName :?> string).FullName
        | t -> toString t

    let mkMetadata location state =
        { origins = [{ location = location; stack = framesHashOf state}]; misc = null }

// ------------------------------- Memory layer -------------------------------

    type private NullActivator() =
        interface IActivator with
            member x.CreateInstance _ _ _ _ =
                internalfail "activator is not ready"
    let mutable private activator : IActivator = new NullActivator() :> IActivator
    let configure act = activator <- act
    let createInstance mtd typ args state = activator.CreateInstance (Metadata.firstOrigin mtd) typ args state

    let mutable genericLazyInstantiator : TermMetadata -> GeneralizedHeap option -> timestamp -> Term -> TermType -> unit -> Term =
        fun _ _ _ _ _ () -> internalfailf "generic lazy instantiator is not ready"

    let stackLazyInstantiator state time key =
        let t = typeOfStackLocation state key
        let metadata = metadataOfStackLocation state key
        let fql = StackRef metadata key []
        { value = genericLazyInstantiator metadata None time fql t (); created = time; modified = time }

// ------------------------------- Pretty-printing -------------------------------

    let private compositionToString s1 s2 =
        sprintf "%s ⚪ %s" s1 s2

    let private dumpHeap keyToString prefix n r h (concrete : StringBuilder) (ids : Dictionary<SymbolicHeap, string>) =
        let id = ref ""
        if ids.TryGetValue(h, id) then !id, n, concrete
        else
            let freshIdentifier = sprintf "%s%d%s" prefix n (if r then "[restr.]" else "")
            ids.Add(h, freshIdentifier)
            freshIdentifier, n+1, concrete.Append(sprintf "\n---------- %s = ----------\n" freshIdentifier).Append(Heap.dump h keyToString)

    let rec private dumpGeneralizedHeap keyToString prefix n (concrete : StringBuilder) (ids : Dictionary<SymbolicHeap, string>) = function
        | Defined(r, s) when Heap.isEmpty s -> (if r then "<empty[restr.]>" else "<empty>"), n, concrete
        | Defined(r, s) -> dumpHeap keyToString prefix n r s concrete ids
        | HigherOrderApplication(f, _, _) -> sprintf "app(%O)" f, n, concrete
        | RecursiveApplication(f, _, _) -> sprintf "recapp(%O)" f, n, concrete // TODO: add recursive definition into concrete section
        | Mutation(h, h') ->
            let s, n, concrete = dumpGeneralizedHeap keyToString prefix n concrete ids h
            let s', n, concrete = dumpHeap keyToString prefix n false h' concrete ids
            sprintf "write(%s, %s)" s s', n, concrete
        | Composition(state, _, h') ->
            let s, n, concrete = dumpMemoryRec state n concrete ids
            let s', n, concrete = dumpGeneralizedHeap keyToString prefix n concrete ids h'
            compositionToString s s', n, concrete
        | Merged ghs ->
            let gss, (n, concrete) =
                List.mapFold (fun (n, concrete) (g, h) ->
                        let s, n, concrete = dumpGeneralizedHeap keyToString prefix n concrete ids h
                        sprintf "(%O, %s)" g s, (n, concrete))
                    (n, concrete) ghs
            gss |> join ",\n\t" |> sprintf "merge[\n\t%s]", n, concrete

    and private dumpMemoryRec s n concrete ids =
        let sh, n, concrete = dumpGeneralizedHeap heapKeyToString "h" n concrete ids s.heap
        let mh, n, concrete = dumpGeneralizedHeap staticKeyToString "s" n concrete ids s.statics
        (sprintf "{ heap = %s, statics = %s }" sh mh, n, concrete)

    let dumpMemory (s : State) =
        let dump, _, concrete = dumpMemoryRec s 0 (new StringBuilder()) (new Dictionary<SymbolicHeap, string>())
        if concrete.Length = 0 then dump else sprintf "%s where%O" dump concrete
