namespace VSharp.System

open System.Collections.Generic
open global.System
open VSharp
open VSharp.Core
open ChessDotNet


// ------------------------------ System.Collections.Generic.ComparerHelpers --------------------------------

module EqualityComparer =

    let private createEqualityComparer state =
        let genericEqualityComparer = typeof<EqualityComparer<_>>.Assembly.GetType("System.Collections.Generic.ObjectEqualityComparer`1")
        let genericEqualityComparer = genericEqualityComparer.MakeGenericType(typeof<Piece>)
        Types.FromDotNetType genericEqualityComparer |> Memory.AllocateDefaultClass state

    // TODO: now it works only for Piece! #do
    let internal CreateDefaultEqualityComparer (state : state) (args : term list) : term * state =
        assert(List.length args = 1)
        createEqualityComparer state

    let internal get_Default (state : state) (args : term list) : term * state =
        assert(List.length args = 0)
        createEqualityComparer state

    let internal equalsForTwoFieldsBlock (state : state) block1 block2 =
        let block1Type = Terms.MostConcreteTypeOfHeapRef state block1
        let block2Type = Terms.MostConcreteTypeOfHeapRef state block2
        let checkContents () =
            let blockFields = Types.ToDotNetType block1Type |> Reflection.fieldsOf false
            assert(Array.length blockFields = 2)
            let block1FstField = fst blockFields.[0] |> Memory.ReadField state block1
            let block1SndField = fst blockFields.[1] |> Memory.ReadField state block1
            let block2FstField = fst blockFields.[0] |> Memory.ReadField state block2
            let block2SndField = fst blockFields.[1] |> Memory.ReadField state block2
            block1FstField === block2FstField &&& block1SndField === block2SndField
        if block1Type <> block2Type then False // TODO: make this check symbolic #do
        else checkContents ()

    let internal PieceEquals (state : state) (args : term list) : term * state =
        assert(List.length args = 3)
        let piece1, piece2 = List.item 1 args, List.item 2 args
        equalsForTwoFieldsBlock state piece1 piece2, state
