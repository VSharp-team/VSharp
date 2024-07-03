namespace VSharp
open System
open VSharp.CSharpUtils

type UnreachableException(msg : string) =
    inherit Exception(msg)

type InternalException(msg : string) =
    inherit Exception(msg)

type InsufficientInformationException(msg : string) =
    inherit Exception(msg)

[<AutoOpen>]
module public Prelude =

    let public internalfail message = raise (InternalException message)
    let public internalfailf format = Printf.ksprintf internalfail format
    let undefinedBehaviour reason = internalfail $"Undefined behaviour: %s{reason}"

    let inline public __notImplemented__() = raise (NotImplementedException())
    let inline public __unreachable__() = raise (UnreachableException "unreachable branch hit!")
    let public __insufficientInformation__ format = Printf.ksprintf (fun reason -> InsufficientInformationException ("Insufficient information! " + reason) |> raise) format
    let public createInsufficientInformation format = Printf.ksprintf (fun reason -> InsufficientInformationException ("Insufficient information! " + reason)) format
    let inline public toString x = x.ToString()
    let inline public join s (ss : seq<string>) = String.Join(s, ss)

    let public always x _ = x

    let public eval x = x ()

    let inline public cons x xs = x :: xs
    let inline public optCons xs = function
        | Some x -> x::xs
        | None -> xs

    let public swap (x : 'a byref) (y : 'a byref) =
        let tmp = x
        x <- y
        y <- tmp

    let inline public withFst x = fun y -> (x, y)
    let inline public withSnd y = fun x -> (x, y)
    let inline public makePair x y = (x, y)

    let public mapfst f (x, y) = (f x, y)
    let public mapsnd f (x, y) = (x, f y)

    let inline public fst3 (x, _, _) = x
    let inline public snd3 (_, y, _) = y
    let inline public thd3 (_, _, z) = z

    let inline public fst4 (x, _, _, _) = x
    let inline public snd4 (_, y, _, _) = y
    let inline public thd4 (_, _, z, _) = z
    let inline public fth4 (_, _, _, a) = a

    let inline public appIfNotNull f lhs rhs =
        if lhs = null then rhs else f lhs

    let inline public (|?) lhs rhs =
        if lhs = null then rhs else lhs
    let inline public (|??) lhs rhs = Option.defaultValue rhs lhs
    let inline public (||??) (lhs : 'a option) (rhs : 'a Lazy) = Option.defaultWith rhs.Force lhs

    type ListMonad() =
       member o.Bind(m : 'a list , f: 'a -> 'b list) = List.collect f m
       member o.Return x = List.singleton x
       member o.Zero() = List.empty
    let list = ListMonad()

    type OptionMonad() =
       member o.Bind(opt, binder) =
           match opt with
           | Some value -> binder value
           | None -> None
       member o.Return x = Some x
       member o.ReturnFrom x = x
       member o.Zero() = None
    let option = OptionMonad()

[<CustomEquality;CustomComparison>]
type 'a transparent =
    { v : 'a }
    override x.ToString() = x.v.ToString()
    override x.GetHashCode() = x.GetType().GetDeterministicHashCode()
    override x.Equals(o : obj) =
        o :? 'a transparent
    interface IComparable with
        override x.CompareTo _ = 0

[<CustomEquality;CustomComparison>]
type fieldId =
    { declaringType : Type; name : string; typ : Type } with
    override x.GetHashCode() =
        HashCode.Combine(x.declaringType, x.name, x.typ)
    override x.Equals y =
        match y with
        | :? fieldId as y -> x.declaringType = y.declaringType && x.name = y.name && x.typ = y.typ
        | _ -> false
    interface IComparable with
        override x.CompareTo y =
            match y with
            | :? fieldId as y ->
                compare
                    (x.declaringType.AssemblyQualifiedName, x.name, x.typ.AssemblyQualifiedName)
                    (y.declaringType.AssemblyQualifiedName, y.name, y.typ.AssemblyQualifiedName)
            | _ -> -1
    override x.ToString() = x.name
    member x.FullName with get() =
        $"({x.typ}){x.declaringType}.{x.name}"
