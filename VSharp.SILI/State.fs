namespace VSharp
open System.Collections.Immutable
open VSharp.Utils
open VSharp.Types

module internal State =
    module SymbolicHeap = Heap

    type internal stack = MappedStack.stack<StackKey, MemoryCell<Term>>
    type internal heap = SymbolicHeap
    type internal staticMemory = SymbolicHeap
    type internal pathCondition = Term list
    type internal entry = { key : StackKey; mtd : TermMetadata; typ : TermType option }
    type internal stackFrame = { func : (FunctionIdentifier * pathCondition) option; entries : list<entry> ; time : Timestamp }
    type internal frames = { f : Stack.stack<stackFrame>; sh : StackHash }
    type internal miscellaneous = ImmutableHashSet<obj>
    type internal state = { stack : stack; heap : heap; statics : staticMemory; frames : frames; pc : pathCondition; misc : miscellaneous }

// ------------------------------- Primitives -------------------------------

    let internal empty : state = { stack = MappedStack.empty; heap = SymbolicHeap.empty; statics = SymbolicHeap.empty; frames = { f = Stack.empty; sh = List.empty }; pc = List.empty; misc = miscellaneous.Empty}

    type internal 'a SymbolicValue =
        | Specified of 'a
        | Unspecified

    let internal zeroTime : Timestamp = System.UInt32.MinValue

    let internal nameOfLocation = term >> function
        | HeapRef((_, (x, _)::xs), _) -> toString x
        | HeapRef(((x, _), _), _) -> toString x
        | StackRef((name, _), x::_) -> sprintf "%s.%O" name x
        | StackRef((name, _), _) -> name
        | StaticRef(name, x::_) -> sprintf "%O.%O" name x
        | StaticRef(name, _) -> toString name
        | l -> "requested name of an unexpected location " + (toString l) |> internalfail

    let internal readStackLocation (s : state) key = MappedStack.find key s.stack
    let internal readHeapLocation (s : state) key = s.heap.[key] |> fst3
    let internal readStaticLocation (s : state) key = s.statics.[key] |> fst3

    let internal isAllocatedOnStack (s : state) key = MappedStack.containsKey key s.stack
    let internal staticMembersInitialized (s : state) typeName =
        SymbolicHeap.contains (Terms.MakeStringKey typeName) s.statics

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
        let { func = metadata; entries = locations; time = _ } = Stack.peak s.frames.f in
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

    let inline getEntriesOfFrame f = f.entries
    let inline getKeyOfEntry en = en.key

    let internal frameTime (s : state) key =
        match List.tryFind (getEntriesOfFrame >> List.exists (getKeyOfEntry >> ((=) key))) s.frames.f with
        | Some { func = _; entries = _; time = t} -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal typeOfStackLocation (s : state) key =
        let forMatch = List.tryPick (getEntriesOfFrame >> List.tryPick (fun { key = l; mtd = _; typ = t } -> if l = key then Some t else None)) s.frames.f
        match forMatch with
        | Some (Some t) -> t
        | Some None -> internalfailf "unknown type of stack location %O!" key
        | None -> internalfailf "stack does not contain key %O!" key

    let internal metadataOfStackLocation (s : state) key =
        match List.tryPick (getEntriesOfFrame >> List.tryPick (fun { key = l; mtd = m; typ = _ } -> if l = key then Some m else None)) s.frames.f with
        | Some t -> t
        | None -> internalfailf "stack does not contain key %O!" key

    let internal compareStacks s1 s2 = MappedStack.compare (fun key value -> StackRef key [] Metadata.empty) fst3 s1 s2

    let internal withPathCondition (s : state) cond : state = { s with pc = cond::s.pc }
    let internal popPathCondition (s : state) : state =
        match s.pc with
        | [] -> internalfail "cannot pop empty path condition"
        | _::p' -> { s with pc = p' }

    let private stackOf (s : state) = s.stack
    let internal heapOf (s : state) = s.heap
    let internal staticsOf (s : state) = s.statics
    let private framesOf (s : state) = s.frames
    let private framesHashOf (s : state) = s.frames.sh
    let internal pathConditionOf (s : state) = s.pc

    let internal withHeap (s : state) h' = { s with heap = h' }
    let internal withStatics (s : state) m' = { s with statics = m' }

    let private staticKeyToString = term >> function
        | Concrete(typeName, String) -> System.Type.GetType(typeName :?> string).FullName
        | t -> toString t

    let internal mkMetadata location state =
        { origins = [{ location = location; stack = framesHashOf state}]; misc = null }

// ------------------------------- Memory level -------------------------------

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
        let fql = StackRef key [] metadata in
        (genericLazyInstantiator metadata time fql t (), time, time)

    let internal dumpMemory (s : state) =
        let sh = Heap.dump s.heap toString in
        let mh = Heap.dump s.statics staticKeyToString in
        let separator = if System.String.IsNullOrWhiteSpace(sh) then "" else "\n"
        sh + separator + mh

// ------------------------------- Merging -------------------------------

    let internal merge2 (s1 : state) (s2 : state) resolve : state =
        assert(s1.pc = s2.pc)
        assert(s1.frames = s2.frames)
        let mergedStack = MappedStack.merge2 s1.stack s2.stack resolve (stackLazyInstantiator s1) in
        let mergedHeap = Heap.merge2 s1.heap s2.heap resolve in
        let mergedStatics = Heap.merge2 s1.statics s2.statics resolve in
        let mergedMisc = s1.misc.Union s2.misc in
        { s1 with stack = mergedStack; heap = mergedHeap; statics = mergedStatics; misc = mergedMisc }

    let internal merge guards states resolve : state =
        assert(List.length states > 0)
        let first = List.head states in
        let frames = framesOf first in
        let path = pathConditionOf first in
        assert(List.forall (fun s -> framesOf s = frames) states)
        assert(List.forall (fun s -> pathConditionOf s = path) states)
        let mergedStack = MappedStack.merge guards (List.map stackOf states) resolve (stackLazyInstantiator first) in
        let mergedHeap = Heap.merge guards (List.map heapOf states) resolve in
        let mergedStatics = Heap.merge guards (List.map staticsOf states) resolve in
        let mergedMisc = states |> List.tail |> List.fold (fun (acc : miscellaneous) s -> acc.Union s.misc) first.misc in
        { stack = mergedStack; heap = mergedHeap; statics = mergedStatics; frames = frames; pc = path; misc = mergedMisc }
