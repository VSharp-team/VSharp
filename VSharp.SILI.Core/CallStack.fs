namespace VSharp.Core

open VSharp
open VSharp.Core

// TODO: need type here? we have key.TypeOfLocation
type private entry = internal { value : term option; typ : symbolicType }
type private frame = internal { func : IFunctionIdentifier; entries : pdict<stackKey, entry> }
type callStack = private { frames : frame stack }

// TODO: rename and style #do
module internal CallStack =

    let empty = { frames = Stack.empty }
    let isEmpty (stack : callStack) = Stack.isEmpty stack.frames

    let private pushFrame (stack : callStack) frame =
        {frames = Stack.push stack.frames frame}

    let newStackFrame (stack : callStack) funcId frame : callStack =
        let createEntries dict (key, value, typ) =
            PersistentDict.add key {value = value; typ = typ} dict
        let entries = List.fold createEntries PersistentDict.empty frame
        let frame = { func = funcId; entries = entries }
        { frames = Stack.push stack.frames frame}

    let popFrame (stack : callStack) : callStack =
        let _, frames = Stack.pop stack.frames
        {frames = frames}

    let private tryFindEntryOnFrame (frame : frame) key : entry option =
        PersistentDict.tryFind frame.entries key

    let private writeFrameLocation (frame : frame) key entry =
        let entriesWithoutKey =
            let entries = frame.entries
            assert(PersistentDict.contains key entries)
            // TODO: need to delete entry before add? #do
            PersistentDict.remove key entries
        let newEntries = PersistentDict.add key entry entriesWithoutKey
        {frame with entries = newEntries}

    // NOTE: allocate function is used for adding values to CURRENT frame:
    // - if location has already been on stack, allocate updates it
    // - otherwise, allocate will add location on current frame
    let allocate (stack : callStack) key value =
        let entry = {value = Some value; typ = typeOf value}
        let frame, frames = Stack.pop stack.frames
        let frame' =
            if PersistentDict.contains key frame.entries then writeFrameLocation frame key entry
            else {frame with entries = PersistentDict.add key entry frame.entries}
        {frames = Stack.push frames frame'}

    let private foldFrame folder acc (frame : frame) =
        PersistentDict.fold folder acc frame.entries

    let rec private findFrameAndRead (frames : frame stack) key k =
        if Stack.isEmpty frames then internalfailf "stack does not contain key %O!" key
        let frame, frames = Stack.pop frames
        let entry = tryFindEntryOnFrame frame key
        match entry with
        | Some entry -> k entry
        | None -> findFrameAndRead frames key k

    let typeOfStackLocation (stack : callStack) key =
        let entry = findFrameAndRead stack.frames key id
        entry.typ

    let readStackLocation (stack : callStack) key makeSymbolic =
        let entry = findFrameAndRead stack.frames key id
        match entry.value with
        | Some value -> value
        | None -> makeSymbolic entry.typ

    let rec private findFrameAndWrite (frames : frame stack) key entry k =
        if Stack.isEmpty frames then internalfailf "stack does not contain key %O!" key
        let frame, frames = Stack.pop frames
        if PersistentDict.contains key frame.entries then
            let frame' = writeFrameLocation frame key entry
            Stack.push frames frame' |> k
        else
            findFrameAndWrite frames key entry (fun frames ->
            Stack.push frames frame |> k)

    // NOTE: writeStackLocation function is used for updating location of ANY frame
    let writeStackLocation (stack : callStack) key (value : term) =
        let entry = {value = Some value; typ = typeOf value}
        let newFrames = findFrameAndWrite stack.frames key entry id
        {frames = newFrames}

    let private bottomAndRestFrames (stack : callStack) =
        Stack.bottomAndRest stack.frames

    let private topAndRestFrames (stack : callStack) =
        Stack.pop stack.frames

    let map keyMapper valueMapper typeMapper (stack : callStack) =
        let entryMapper {value = v; typ = t} = {value = Option.map valueMapper v; typ = typeMapper t}
        let frameMapper frame = {frame with entries = PersistentDict.map keyMapper entryMapper frame.entries}
        {frames = Stack.map frameMapper stack.frames}

    let fold folder acc (stack : callStack) =
        let entryFolder acc k entry = folder acc k entry.value entry.typ
        Stack.fold (foldFrame entryFolder) acc stack.frames

    let applyEffect stack stack' : callStack =
        let topFrame, restFrames = topAndRestFrames stack
        let bottomFrame', restFrames' = bottomAndRestFrames stack'
        // NOTE: writeFrameLocation is used here, because
        // bottom frame of effect state affects only top frame of context state
        let updatedTopFrame = foldFrame writeFrameLocation topFrame bottomFrame'
        let frames = Stack.push restFrames updatedTopFrame
        {frames = Stack.union restFrames' frames}

    let containsFunc stack funcId =
        let isRecursiveFrame (frame : frame) = funcId = frame.func
        let _, frames = Stack.pop stack.frames
        Stack.exists isRecursiveFrame frames

    let getCurrentFunc stack =
        let frame, _ = Stack.pop stack.frames
        frame.func

    let size stack = Stack.size stack.frames

    let toString (stack : callStack) =
        let printEntry (k, v) =
            Option.map (fun v -> sprintf "key = %s, value = %O" k v) v
        let keysAndValues = fold (fun acc k v _ -> (toString k, v) :: acc) List.empty stack
        let sorted = List.sortBy fst keysAndValues
        List.choose printEntry sorted
        |> join "\n"
