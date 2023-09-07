namespace VSharp.System

open System.Collections.Generic
open global.System
open VSharp
open VSharp.Core


// ------------------------------ System.Collections.Generic.ComparerHelpers --------------------------------

module EqualityComparer =

    let private createEqualityComparer state (typ : Type) =
        let genericEqualityComparer = typeof<EqualityComparer<_>>.Assembly.GetType("System.Collections.Generic.ObjectEqualityComparer`1")
        let genericEqualityComparer = genericEqualityComparer.MakeGenericType(typ)
        Memory.AllocateDefaultClass state genericEqualityComparer

    let internal CreateDefaultEqualityComparer (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let typ = Type.getActualType state runtimeType
        createEqualityComparer state typ

    let internal CreateDefaultComparer (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let runtimeType = List.head args
        let typ = Type.getActualType state runtimeType
        let genericEqualityComparer = typeof<EqualityComparer<_>>.Assembly.GetType("System.Collections.Generic.ObjectComparer`1")
        let genericEqualityComparer = genericEqualityComparer.MakeGenericType(typ)
        Memory.AllocateDefaultClass state genericEqualityComparer

    let internal get_Default (state : state) (args : term list) : term =
        assert(List.length args = 1)
        let wrappedType = List.head args
        let typ = Helpers.unwrapType wrappedType
        createEqualityComparer state typ

    let internal structuralEquality (state : state) block1 block2 =
        let checkContents () =
            let blockType = Terms.MostConcreteTypeOfRef state block1
            let compareOneField acc (field, _) =
                let block1Field = Memory.ReadField state block1 field
                let block2Field = Memory.ReadField state block2 field
                (block1Field === block2Field) &&& acc
            let blockFields = Reflection.fieldsOf false blockType
            Array.fold compareOneField (True()) blockFields
        let typeEquals = Types.RefIsRef state block1 block2 &&& Types.RefIsRef state block2 block1
        if typeEquals = True() then checkContents ()
        elif typeEquals = False() then False()
        else __insufficientInformation__ "unable to check structural equality: %O, %O" block1 block2
