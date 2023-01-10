namespace VSharp

open System
open System.Xml.Serialization
open VSharp
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Collections
open VSharp.CSharpUtils
open HarmonyLib

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
    baseMethod : methodRepr
    methodImplementation : obj array
}

exception UnexpectedExternCallException of string

module ExtMocking =
    let storageFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Storage>"
    let counterFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Counter>"

    type private PatchMethod(baseMethod : MethodInfo, clausesCount : int) =
        let returnValues : obj[] = Array.zeroCreate clausesCount
        let finalizerName = $"{baseMethod.Name}_<finalizer>"
        let storageFieldName = storageFieldName baseMethod
        let counterFieldName = counterFieldName baseMethod
        let returnType = baseMethod.ReturnType
        let resultArg = baseMethod.ReturnType.MakeByRefType()

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
                typeBuilder.DefineMethod(finalizerName, methodAttributes, CallingConventions.Standard)
            let exType = typeof<Exception>
            methodBuilder.SetReturnType exType
            methodBuilder.SetParameters([|exType; resultArg|])
            methodBuilder.DefineParameter(1, ParameterAttributes.None, "__exception") |> ignore
            methodBuilder.DefineParameter(2, ParameterAttributes.None, "__result") |> ignore
            
            let ilGenerator = methodBuilder.GetILGenerator()
            let storageField = typeBuilder.DefineField(storageFieldName, returnType.MakeArrayType(), FieldAttributes.Private ||| FieldAttributes.Static)
            let counterField = typeBuilder.DefineField(counterFieldName, typeof<int>, FieldAttributes.Private ||| FieldAttributes.Static)
            
            let normalCase = ilGenerator.DefineLabel()
            let count = returnValues.Length

            ilGenerator.Emit(OpCodes.Ldsfld, counterField)
            ilGenerator.Emit(OpCodes.Ldc_I4, count)
            ilGenerator.Emit(OpCodes.Blt, normalCase)
            
            ilGenerator.Emit(OpCodes.Ldstr, finalizerName)
            ilGenerator.Emit(OpCodes.Newobj, typeof<UnexpectedExternCallException>.GetConstructor([|typeof<string>|]))
            ilGenerator.Emit(OpCodes.Ret)
            
            ilGenerator.MarkLabel(normalCase)
            ilGenerator.Emit(OpCodes.Ldarg, 1) // ld __result
            ilGenerator.Emit(OpCodes.Ldsfld, storageField)
            ilGenerator.Emit(OpCodes.Ldsfld, counterField)
            ilGenerator.Emit(OpCodes.Ldelem, returnType) // Load storage[counter] on stack
            ilGenerator.Emit(OpCodes.Stobj, returnType)
            ilGenerator.Emit(OpCodes.Ldsfld, counterField)
            ilGenerator.Emit(OpCodes.Ldc_I4_1)
            ilGenerator.Emit(OpCodes.Add)
            ilGenerator.Emit(OpCodes.Stsfld, counterField)

            ilGenerator.Emit(OpCodes.Ldnull)
            ilGenerator.Emit(OpCodes.Ret)
            finalizerName // return identifier
        
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

    let harmony = lazy(Harmony("vsharp.externMocking"))

    let BuildAndPatch (testId : string) decode (repr : extMockRepr) =
        let mockType = Type(repr)
        let h = harmony.Force()
        let methodToPatch = repr.baseMethod.Decode()
        
        if methodToPatch.ReturnType <> typeof<Void> then
            let moduleBuilder = mBuilder.Force()
            let patchType, finName = mockType.Build(moduleBuilder, testId)
            repr.methodImplementation |> Array.map decode |> mockType.SetClauses
            let finMethod = AccessTools.Method(patchType, finName)
            let transpiler = AccessTools.Method(typeof<ExternMocker>, "FuncTranspiler")
            h.Patch(methodToPatch, null, null, HarmonyMethod(transpiler), HarmonyMethod(finMethod)) |> ignore
        else
            let transpiler = AccessTools.Method(typeof<ExternMocker>, "ProcTranspiler")
            h.Patch(methodToPatch, null, null, HarmonyMethod(transpiler)) |> ignore

    let Unpatch () =
        let h = harmony.Force()
        h.UnpatchAll()
