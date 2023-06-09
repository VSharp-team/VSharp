namespace VSharp

open System
open System.Xml.Serialization
open VSharp
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Collections
open VSharp.CSharpUtils
open MonoMod.RuntimeDetour

[<CLIMutable>]
[<Serializable>]
[<XmlInclude(typeof<structureRepr>)>]
[<XmlInclude(typeof<referenceRepr>)>]
[<XmlInclude(typeof<pointerRepr>)>]
[<XmlInclude(typeof<arrayRepr>)>]
[<XmlInclude(typeof<enumRepr>)>]
[<XmlInclude(typeof<methodRepr>)>]
type extMockRepr = {
    name : string
    isExtern : bool
    baseMethod : methodRepr
    methodImplementation : obj array
}

exception UnexpectedExternCallException of string

module ExtMocking =
    let storageFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Storage>"
    let counterFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Counter>"

    type private PatchMethod(baseMethod : MethodInfo, clausesCount : int) =
        let returnValues : obj[] = Array.zeroCreate clausesCount
        let patchedName = $"{baseMethod.Name}_<patched>"
        let storageFieldName = storageFieldName baseMethod
        let counterFieldName = counterFieldName baseMethod
        let returnType = baseMethod.ReturnType
        let callingConvention = baseMethod.CallingConvention
        let arguments = baseMethod.GetParameters() |> Array.map (fun (p : ParameterInfo) -> p.GetType())
        member x.SetClauses (clauses : obj[]) =
            clauses |> Array.iteri (fun i o -> returnValues.[i] <- o)
        
        member x.InitializeType (typ : Type) =
            if returnType <> typeof<Void> then
                let field = typ.GetField(storageFieldName, BindingFlags.NonPublic ||| BindingFlags.Static)
                if field = null then
                    internalfail $"Could not detect field %s{storageFieldName} of externMock!"
                let storage = Array.CreateInstance(returnType, clausesCount)
                Array.Copy(returnValues, storage, clausesCount)
                field.SetValue(null, storage)
                
        member x.BuildPatch (typeBuilder : TypeBuilder) =
            let methodAttributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Static
            let methodBuilder =
                typeBuilder.DefineMethod(patchedName, methodAttributes, callingConvention)
            
            methodBuilder.SetReturnType returnType
            methodBuilder.SetParameters(arguments)
            
            let ilGenerator = methodBuilder.GetILGenerator()
            let storageField = typeBuilder.DefineField(storageFieldName, returnType.MakeArrayType(), FieldAttributes.Private ||| FieldAttributes.Static)
            let counterField = typeBuilder.DefineField(counterFieldName, typeof<int>, FieldAttributes.Private ||| FieldAttributes.Static)
            
            let normalCase = ilGenerator.DefineLabel()
            let count = returnValues.Length

            ilGenerator.Emit(OpCodes.Ldsfld, counterField)
            ilGenerator.Emit(OpCodes.Ldc_I4, count)
            ilGenerator.Emit(OpCodes.Blt, normalCase)
            
            ilGenerator.Emit(OpCodes.Ldstr, patchedName)
            ilGenerator.Emit(OpCodes.Newobj, typeof<UnexpectedExternCallException>.GetConstructor([|typeof<string>|]))
            ilGenerator.Emit(OpCodes.Ret)
            
            ilGenerator.MarkLabel(normalCase)

            if returnType <> typeof<Void> then
                ilGenerator.Emit(OpCodes.Ldsfld, storageField)
                ilGenerator.Emit(OpCodes.Ldsfld, counterField)
                ilGenerator.Emit(OpCodes.Ldelem, returnType) // Load storage[counter] on stack

            ilGenerator.Emit(OpCodes.Ldsfld, counterField)
            ilGenerator.Emit(OpCodes.Ldc_I4_1)
            ilGenerator.Emit(OpCodes.Add)
            ilGenerator.Emit(OpCodes.Stsfld, counterField)

            ilGenerator.Emit(OpCodes.Ret)
            
            patchedName // return identifier
        
        member x.Build (typeBuilder : TypeBuilder) =
            x.BuildPatch typeBuilder

    type private Type(repr : extMockRepr) =
        let method = PatchMethod(repr.baseMethod.Decode(), repr.methodImplementation.Length)
        let mutable patchType = null
        
        member x.Build(moduleBuilder : ModuleBuilder, testId) =
            let typeBuilder = moduleBuilder.DefineType($"test_{testId}_{repr.name}", TypeAttributes.Public)
            let finalizerName = method.Build typeBuilder
            patchType <- typeBuilder.CreateType()
            patchType, finalizerName
        
        // decode beforehand
        member x.SetClauses clauses =
            method.SetClauses clauses
            method.InitializeType patchType
    
    let mBuilder = lazy(
        let dynamicAssemblyName = "VSharpExternMocks"
        let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(AssemblyName dynamicAssemblyName, AssemblyBuilderAccess.Run)
        assemblyBuilder.DefineDynamicModule dynamicAssemblyName)


    let mutable detours = List.empty : NativeDetour list
    
    let BuildAndPatch (testId : string) decode (repr : extMockRepr) =
        let mockType = Type(repr)
        let methodToPatch = repr.baseMethod.Decode()

        let ptrFrom = 
            if repr.isExtern then ExternMocker.GetExternPtr(methodToPatch)
            else methodToPatch.MethodHandle.GetFunctionPointer()

        let moduleBuilder = mBuilder.Force()
        let patchType, patchName = mockType.Build(moduleBuilder, testId)
        repr.methodImplementation |> Array.map decode |> mockType.SetClauses
        let methodTo = patchType.GetMethod(patchName, BindingFlags.Static ||| BindingFlags.Public)
        let ptrTo = methodTo.MethodHandle.GetFunctionPointer()

        let d = ExternMocker.buildAndApplyDetour(ptrFrom, ptrTo)
        detours <- d::detours

    let Unpatch () =
        List.iter (fun (d : NativeDetour) -> d.Undo()) detours
        detours <- List.Empty

