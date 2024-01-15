namespace VSharp

open System
open VSharp
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Collections
open VSharp.CSharpUtils
open System.Runtime.InteropServices

exception UnexpectedExternCallException of string

module DllManager =

    let private dllImportAttribute = lazy AssemblyManager.NormalizeType(typeof<DllImportAttribute>)

    let private getDllImportNameAndEntry (attr : Attribute) =
        let dllNameField = dllImportAttribute.Value.GetField("dllName")
        let entryPointField = dllImportAttribute.Value.GetField("EntryPoint")
        dllNameField.GetValue(attr) :?> string, entryPointField.GetValue(attr) :?> string

    let parseDllImport (m : MethodBase) =
        // Case for assembly, loaded via default load context (F# internal calls)
        let findViaDefault (attr : Attribute) =
            match attr with
            | :? DllImportAttribute as attr ->
                Some (attr.Value, attr.EntryPoint)
            | _ -> None

        // Case for assembly, loaded via VSharp load context (C# internal calls)
        let findViaVSharpLoadContext (attr : Attribute) = Some (getDllImportNameAndEntry attr)
        let attrFromDefaultContext = m.GetCustomAttributes<DllImportAttribute>() |> Seq.tryPick findViaDefault
        match attrFromDefaultContext with
        | Some info -> Some info
        | None -> m.GetCustomAttributes(dllImportAttribute.Value) |> Seq.tryPick findViaVSharpLoadContext

    let isQCall (m : MethodBase) =
        match parseDllImport m with
        | Some(libName, _) -> libName.Equals("QCall")
        | None -> false

module ExtMocking =

    let storageFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Storage>"
    let counterFieldName (method : MethodInfo) = $"{method.Name}{method.MethodHandle.Value}_<Counter>"

    type PatchMethod(baseMethod : MethodInfo, clausesCount : int) =
        let returnValues : obj[] = Array.zeroCreate clausesCount
        let patchedName = $"{baseMethod.Name}_<patched>"
        let storageFieldName = storageFieldName baseMethod
        let counterFieldName = counterFieldName baseMethod
        let returnType = baseMethod.ReturnType
        let callingConvention = baseMethod.CallingConvention
        let arguments = baseMethod.GetParameters() |> Array.map (fun (p : ParameterInfo) -> p.ParameterType)

        do
            let hasOutParameter =
                baseMethod.GetParameters()
                |> Array.exists (fun x -> x.IsOut)

            if hasOutParameter then internalfail "Extern mocking with out parameters not implemented"

        member x.SetClauses (clauses : obj[]) =
            clauses |> Array.iteri (fun i o -> returnValues[i] <- o)

        member x.InitializeType (typ : Type) =
            if returnType <> typeof<Void> then
                let field = typ.GetField(storageFieldName, BindingFlags.NonPublic ||| BindingFlags.Static)
                if field = null then
                    internalfail $"Could not detect field {storageFieldName} of externMock!"
                let storage = Array.CreateInstance(returnType, clausesCount)
                Array.Copy(returnValues, storage, clausesCount)
                field.SetValue(null, storage)

        member private x.GenerateNonVoidIL (typeBuilder : TypeBuilder) (ilGenerator : ILGenerator) =
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
            ilGenerator.Emit(OpCodes.Ldsfld, storageField)
            ilGenerator.Emit(OpCodes.Ldsfld, counterField)
            ilGenerator.Emit(OpCodes.Ldelem, returnType) // Load storage[counter] on stack

            ilGenerator.Emit(OpCodes.Ldsfld, counterField)
            ilGenerator.Emit(OpCodes.Ldc_I4_1)
            ilGenerator.Emit(OpCodes.Add)
            ilGenerator.Emit(OpCodes.Stsfld, counterField)

            ilGenerator.Emit(OpCodes.Ret)

        member x.BuildPatch (typeBuilder : TypeBuilder) =
            let methodAttributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.Static
            let methodBuilder =
                typeBuilder.DefineMethod(patchedName, methodAttributes, callingConvention)

            methodBuilder.SetReturnType returnType
            methodBuilder.SetParameters(arguments)

            let ilGenerator = methodBuilder.GetILGenerator()

            if returnType = typeof<Void> then
                ilGenerator.Emit(OpCodes.Ret)
            else
                x.GenerateNonVoidIL typeBuilder ilGenerator

            patchedName // return identifier

    type Type(name : string) =
        let mutable patchType = null
        let mutable mockedMethod = null
        let mutable mockImplementations = null
        let mutable outImplementations = null
        let mutable patchMethod = None

        static member Deserialize name (baseMethod : MethodInfo) (methodImplementations : obj[]) =
            let mockType = Type(name)
            mockType.MockedMethod <- baseMethod
            mockType.MockImplementations <- methodImplementations
            mockType.PatchMethod <- PatchMethod(baseMethod, methodImplementations.Length)
            mockType

        member x.MockedMethod
            with get() = mockedMethod
            and private set method =
                mockedMethod <- method

        member x.MockImplementations
            with get() = mockImplementations
            and private set methodImplementations =
                mockImplementations <- methodImplementations

        member x.PatchMethod
            with get() =
                match patchMethod with
                | Some pm -> pm
                | None -> internalfail "ExternMocking patch method called before initialization"
            and private set m =
                patchMethod <- Some m

        member x.Build(moduleBuilder : ModuleBuilder, testId) =
            let typeBuilder = moduleBuilder.DefineType($"test_{testId}_{name}", TypeAttributes.Public)
            let patchName = x.PatchMethod.BuildPatch typeBuilder
            patchType <- typeBuilder.CreateType()
            patchType, patchName

        member x.SetClauses decode =
            let decodedClauses = x.MockImplementations |> Array.map decode
            x.PatchMethod.SetClauses decodedClauses
            x.PatchMethod.InitializeType patchType

    let moduleBuilder = lazy(
        let dynamicAssemblyName = "VSharpExternMocks"
        let assemblyName = AssemblyName(dynamicAssemblyName)
        let assemblyBuilder = AssemblyBuilder.DefineDynamicAssembly(assemblyName, AssemblyBuilderAccess.Run)
        assemblyBuilder.DefineDynamicModule dynamicAssemblyName)

    let buildAndPatch (testId : string) decode (mockType : Type) =
        let methodToPatch = mockType.MockedMethod
        let ptrFrom =
            if Reflection.isExternalMethod methodToPatch then
                let libName, methodName =
                    match DllManager.parseDllImport methodToPatch with
                    | Some p -> p
                    | None -> internalfail "External method without DllImport attribute"
                ExternMocker.GetExternPtr(libName, methodName)
            else
                methodToPatch.MethodHandle.GetFunctionPointer()

        let moduleBuilder = moduleBuilder.Value
        let patchType, patchName = mockType.Build(moduleBuilder, testId)
        mockType.SetClauses decode
        let methodTo = patchType.GetMethod(patchName, BindingFlags.Static ||| BindingFlags.Public)
        let ptrTo = methodTo.MethodHandle.GetFunctionPointer()
        ExternMocker.BuildAndApplyDetour(ptrFrom, ptrTo)

    let unPatch () =
        ExternMocker.UnPatch()
