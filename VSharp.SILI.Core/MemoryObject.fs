namespace VSharp.Core

open VSharp

type updateList<'address, 'value> = ('address * 'value) list

[<StructuralEquality; NoComparison>]
type memoryObject<'key, 'address when 'key : equality> =
    | ArrayOfValues of 'key * symbolicType * updateList<'address, term>
    | ArrayOfRefs of 'key * symbolicType * updateList<'address, heapRefInfo>

module MemoryObject =

    let empty key typ =
        match typ with
        | Types.ReferenceType -> ArrayOfValues(key, typ, [])
        | Types.ValueType -> ArrayOfRefs(key, typ, [])
        | _ -> internalfailf "Lazy instantiating memory object of generic type %O, this should not happen!" typ

    let inline private readUpdateList addr instantiate = function
        | [] -> instantiate()
        | (k, v)::_ -> if k = addr then v else instantiate()

    let rec private maxTimeRec m = function
        | [] -> m
        | (_, v)::ul -> maxTimeRec (max m v.time) ul
    let private maxTime (ul : updateList<'a, heapRefInfo>) = maxTimeRec 0u ul

    let read (mo : memoryObject<'key, 'address>) (addr : 'address) instantiate : term =
        match mo with
        | ArrayOfValues(_, typ, ul) -> readUpdateList addr (fun () -> instantiate typ mo) ul
        | ArrayOfRefs(_, typ, ul) ->
            let instantiateRef () =
                let address = instantiate AddressType mo
                let time = maxTime ul
                {address = address; baseType = typ; time = time}
            let info = readUpdateList addr instantiateRef ul
            HeapRef info

    let private printUpdateList list indent valuePrint =
        List.map (fun (k, v) -> sprintf "%s%O <- %O" indent k (valuePrint v)) list |> join "\n"

    let toString indent = function
        | ArrayOfValues(_, _, ul) -> sprintf "{\n%s\n%s}" indent (printUpdateList ul (indent + "    ") toString)
        | ArrayOfRefs(_, _, ul) -> sprintf "{\n%s\n%s}" indent (printUpdateList ul (indent + "    ") (sprintf "HeapRef %O"))

type memoryObject<'key, 'address when 'key : equality> with
    override x.ToString() = MemoryObject.toString "" x
