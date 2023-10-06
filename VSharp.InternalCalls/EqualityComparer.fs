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

    let rec internal structuralEquality (state : state) term1 term2 : term =
        let checkFields blockType =
            let compareOneField acc (field, _) =
                let block1Field = Memory.ReadField state term1 field
                let block2Field = Memory.ReadField state term2 field
                if acc <> False() then
                    acc &&& structuralEquality state block1Field block2Field
                else acc
            let blockFields = Reflection.fieldsOf false blockType
            Array.fold compareOneField (True()) blockFields
        let isRef1 = IsRefOrPtr term1
        let isRef2 = IsRefOrPtr term2
        if isRef1 || isRef2 then
            if isRef1 && isRef2 then
                let typeEquals = Types.RefIsRef state term1 term2 &&& Types.RefIsRef state term2 term1
                if typeEquals = True() then
                    let blockType = Terms.MostConcreteTypeOfRef state term1
                    if TypeUtils.isPrimitive blockType || blockType.IsEnum then
                        let value1 = Memory.Read state term1
                        let value2 = Memory.Read state term2
                        structuralEquality state value1 value2
                    else checkFields blockType
                elif typeEquals = False() then False()
                else __insufficientInformation__ $"unable to check structural equality: {term1}, {term2}"
            else False()
        else
            let t1 = TypeOf term1
            let t2 = TypeOf term2
            if (TypeUtils.isPrimitive t1 || t1.IsEnum) && (TypeUtils.isPrimitive t2 || t2.IsEnum) then
                term1 === term2
            else __notImplemented__()
