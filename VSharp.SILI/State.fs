namespace VSharp

open System.Text
open System.Collections.Generic
open VSharp.Utils
open VSharp.Types

module internal State =
    module SymbolicHeap = Heap

    type internal stack = MappedStack.stack<StackKey, MemoryCell<Term>>
    type internal pathCondition = Term list
    type internal entry = { key : StackKey; mtd : TermMetadata; typ : TermType option }
    type internal stackFrame = { func : (FunctionIdentifier * pathCondition) option; entries : list<entry> ; time : Timestamp }
    type internal frames = { f : Stack.stack<stackFrame>; sh : StackHash }
    type internal GeneralizedHeap =
        | Defined of bool * SymbolicHeap  // bool = restricted
        | HigherOrderApplication of Term * ConcreteHeapAddress * Timestamp
        | RecursiveApplication of FunctionIdentifier * ConcreteHeapAddress * Timestamp
        | Composition of state * GeneralizedHeap
        | Merged of (Term * GeneralizedHeap) list
    and internal heap = GeneralizedHeap
    and internal staticMemory = GeneralizedHeap
    and internal state = { stack : stack; heap : heap; statics : staticMemory; frames : frames; pc : pathCondition }

// ------------------------------- Primitives -------------------------------

    let internal Defined r h = Defined(r, h)

    let internal empty : state = {
        stack = MappedStack.empty;
        heap = Defined false SymbolicHeap.empty;
        statics = Defined false SymbolicHeap.empty;
        frames = { f = Stack.empty; sh = List.empty };
        pc = List.empty
    }

    let internal emptyRestricted : state = {
        stack = MappedStack.empty;
        heap = Defined true SymbolicHeap.empty;
        statics = Defined true SymbolicHeap.empty;
        frames = { f = Stack.empty; sh = List.empty };
        pc = List.empty
    }

    type internal 'a SymbolicValue =
        | Specified of 'a
        | Unspecified

    let internal zeroTime : Timestamp = System.UInt32.MinValue

    let internal nameOfLocation = term >> function
        | HeapRef(((_, t), []), _) -> toString t
        | StackRef((name, _), []) -> name
        | StaticRef(name, []) -> System.Type.GetType(name).FullName
        | HeapRef((_, path), _)
        | StackRef(_, path)
        | StaticRef(_, path) -> path |> Seq.map (fst >> toString) |> join "."
        | l -> "requested name of an unexpected location " + (toString l) |> internalfail

    let internal readStackLocation (s : state) key = MappedStack.find key s.stack
    let internal readHeapLocation (h : SymbolicHeap) key = h.[key] |> fst3

    let internal isAllocatedOnStack (s : state) key = MappedStack.containsKey key s.stack

    let internal newStackFrame time metadata (s : state) funcId frame : state =
        let pushOne (map : stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; mtd = metadata; typ = typ }, MappedStack.push key (term, time, time) map
            | Unspecified -> { key = key; mtd = metadata; typ = typ }, MappedStack.reserve key map
        in
        let frameMetadata = Some(funcId, s.pc) in
        let locations, newStack = frame |> List.mapFold pushOne s.stack in
        let f' = Stack.push s.frames.f { func = frameMetadata; entries = locations; time = time } in
        let sh' = frameMetadata.GetHashCode()::s.frames.sh in
        { s with stack = newStack; frames = {f = f'; sh = sh'} }

    let internal newScope time metadata (s : state) frame : state =
        let pushOne (map : stack) (key, value, typ) =
            match value with
            | Specified term -> { key = key; mtd = metadata; typ = typ }, MappedStack.push key (term, time, time) map
            | Unspecified -> { key = key; mtd = metadata; typ = typ }, MappedStack.reserve key map
        in
        let locations, newStack = frame |> List.mapFold pushOne s.stack in
        { s with stack = newStack; frames = { s.frames with f = Stack.push s.frames.f { func = None; entries = locations; time = time } } }

    let internal pushToCurrentStackFrame (s : state) key value = MappedStack.push key value s.stack
    let internal popStack (s : state) : state =
        let popOne (map : stack) entry = MappedStack.remove map entry.key
        let { func = metadata; entries = locations; time = _ } = Stack.peek s.frames.f in
        let f' = Stack.pop s.frames.f in
        let sh = s.frames.sh in
        let sh' =
            match metadata with
            | Some _ ->
                assert(not <| List.isEmpty sh)
                List.tail sh
            | None -> sh
        { s with stack = List.fold popOne s.stack locations; frames = { f = f'; sh = sh'} }

    let internal writeStackLocation (s : state) key value : state =
        { s with stack = MappedStack.add key value s.stack }

    let internal stackFold = MappedStack.fold

    let inline private entriesOfFrame f = f.entries
    let inline private keyOfEntry en = en.key

    let internal frameTime (s : state) key =
        match List.tryFind (entriesOfFrame >> List.exists (keyOfEntry >> ((=) key))) s.frames.f with
        | Some { func = _; entries = _; time = t} -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal typeOfStackLocation (s : state) key =
        let forMatch = List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = _; typ = t } -> if l = key then Some t else None)) s.frames.f
        match forMatch with
        | Some (Some t) -> t
        | Some None -> internalfailf "unknown type of stack location %O!" key
        | None -> internalfailf "stack does not contain key %O!" key

    let internal metadataOfStackLocation (s : state) key =
        match List.tryPick (entriesOfFrame >> List.tryPick (fun { key = l; mtd = m; typ = _ } -> if l = key then Some m else None)) s.frames.f with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal compareStacks s1 s2 = MappedStack.compare (fun key value -> StackRef Metadata.empty key []) fst3 s1 s2

    let internal withPathCondition (s : state) cond : state = { s with pc = cond::s.pc }
    let internal popPathCondition (s : state) : state =
        match s.pc with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> { s with pc = p' }

    let internal stackOf (s : state) = s.stack
    let internal heapOf (s : state) = s.heap
    let internal staticsOf (s : state) = s.statics
    let private framesOf (s : state) = s.frames
    let private framesHashOf (s : state) = s.frames.sh
    let internal pathConditionOf (s : state) = s.pc

    let internal withHeap (s : state) h' = { s with heap = h' }
    let internal withStatics (s : state) m' = { s with statics = m' }

    let internal stackLocationToReference state location =
        StackRef (metadataOfStackLocation state location) location []
    let internal staticLocationToReference term =
        match term.term with
        | Concrete(location, String) -> StaticRef term.metadata (location :?> string) []
        | _ -> __notImplemented__()

    let private heapKeyToString = term >> function
        | Concrete(:? (int list) as k, _) -> k |> List.map toString |> join "."
        | t -> toString t

    let private staticKeyToString = term >> function
        | Concrete(typeName, String) -> System.Type.GetType(typeName :?> string).FullName
        | t -> toString t

    let internal mkMetadata location state =
        { origins = [{ location = location; stack = framesHashOf state}]; misc = null }

// ------------------------------- Memory layer -------------------------------

    type IActivator =
        abstract member CreateInstance : TermMetadata -> System.Type -> Term list -> state -> (Term * state)
    type private NullActivator() =
        interface IActivator with
            member x.CreateInstance _ _ _ _ =
                internalfail "activator is not ready"
    let mutable activator : IActivator = new NullActivator() :> IActivator
    let mutable genericLazyInstantiator : TermMetadata -> Timestamp -> Term -> TermType -> unit -> Term =
        fun _ _ _ _ () -> internalfailf "generic lazy instantiator is not ready"

    let internal stackLazyInstantiator state time key =
        let time = frameTime state key in
        let t = typeOfStackLocation state key in
        let metadata = metadataOfStackLocation state key in
        let fql = StackRef metadata key [] in
        (genericLazyInstantiator metadata time fql t (), time, time)

// ------------------------------- Pretty-printing -------------------------------

    let private compositionToString s1 s2 =
        sprintf "%s ⚪ %s" s1 s2

    let rec private dumpGeneralizedHeap keyToString prefix n (concrete : StringBuilder) (ids : Dictionary<SymbolicHeap, string>) = function
        | Defined(r, s) when Heap.isEmpty s -> (if r then "<empty[restr.]>" else "<empty>"), n, concrete
        | Defined(r, s) ->
            let id = ref ""
            if ids.TryGetValue(s, id) then !id, n, concrete
            else
                let freshIdentifier = sprintf "%s%d%s" prefix n (if r then "[restr.]" else "") in
                ids.Add(s, freshIdentifier)
                freshIdentifier, n+1, concrete.AppendLine(sprintf "\n---------- %s = ----------" freshIdentifier).Append(Heap.dump s keyToString)
        | HigherOrderApplication(f, _, _) -> sprintf "app(%O)" f, n, concrete
        | RecursiveApplication(f, _, _) -> sprintf "recapp(%O)" f, n, concrete // TODO: add recursive definition into concrete section
        | Composition(state, h') ->
            let s, n, concrete = dumpMemoryRec state n concrete ids in
            let s', n, concrete = dumpGeneralizedHeap keyToString prefix n concrete ids h' in
            compositionToString s s', n, concrete
        | Merged ghs ->
            let gss, (n, concrete) =
                List.mapFold (fun (n, concrete) (g, h) ->
                        let s, n, concrete = dumpGeneralizedHeap keyToString prefix n concrete ids h in
                        sprintf "(%O, %s)" g s, (n, concrete))
                    (n, concrete) ghs
            in gss |> join ",\n\t" |> sprintf "merge[\n\t%s]", n, concrete

    and private dumpMemoryRec s n concrete ids =
        let sh, n, concrete = dumpGeneralizedHeap heapKeyToString "h" n concrete ids s.heap in
        let mh, n, concrete = dumpGeneralizedHeap staticKeyToString "s" n concrete ids s.statics in
        (sprintf "{ heap = %s, statics = %s }" sh mh, n, concrete)

    let internal dumpMemory (s : state) =
        let dump, _, concrete = dumpMemoryRec s 0 (new StringBuilder()) (new Dictionary<SymbolicHeap, string>()) in
        if concrete.Length = 0 then dump else sprintf "%s where%O" dump concrete
