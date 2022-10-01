namespace VSharp

open global.System
open System.Reflection

module Loader =
    let private collectImplementations (ts : Type seq) =
        let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
        ts
        |> Seq.collect (fun t ->
            t.GetMethods(bindingFlags)
            |> Seq.choose (fun m ->
                match m.GetCustomAttributes(typedefof<ImplementsAttribute>) with
                | Seq.Cons(:? ImplementsAttribute as attr, _) -> Some (attr.Name, m)
                | _ -> None))
        |> Map.ofSeq

    let public CSharpImplementations =
        seq [
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Array")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Monitor")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.RuntimeHelpersUtils")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.CLRConfig")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Interop")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.NumberFormatInfo")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.StringUtils")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.CharUnicodeInfo")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.BlockChain")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.GC")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.DateTimeUtils")
            Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.ThreadUtils")
        ]
        |> collectImplementations

    let public FSharpImplementations =
        Assembly.GetExecutingAssembly().GetTypes()
        |> Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule
        |> collectImplementations

    let private runtimeExceptionsConstructors =
        Assembly.Load(AssemblyName("VSharp.CSharpUtils")).GetType("VSharp.CSharpUtils.Exceptions")
        |> Seq.singleton
        |> collectImplementations

    let mutable public CilStateImplementations : string seq =
        Seq.empty

    let public hasRuntimeExceptionsImplementation (fullMethodName : string) =
        Map.containsKey fullMethodName runtimeExceptionsConstructors

    let public getRuntimeExceptionsImplementation (fullMethodName : string) =
        runtimeExceptionsConstructors.[fullMethodName]

    let public ConcreteInvocations =
        set [
            // Types
            "System.Type System.Type.GetTypeFromHandle(System.RuntimeTypeHandle)"
            "System.Type System.Type.GetTypeFromHandle(System.RuntimeTypeHandle)"
            "System.Reflection.RuntimeAssembly System.RuntimeTypeHandle.GetAssembly(System.RuntimeType)"
            "System.Type System.RuntimeType.GetElementType(this)"
            "System.Boolean System.Type.op_Inequality(System.Type, System.Type)"
            "System.Boolean System.Type.op_Equality(System.Type, System.Type)"
            "System.Boolean System.RuntimeTypeHandle.IsGenericTypeDefinition(System.RuntimeType)"
            "System.Boolean System.RuntimeTypeHandle.IsInterface(System.RuntimeType)"
            "System.Boolean System.RuntimeTypeHandle.IsGenericVariable(System.RuntimeType)"
            "System.String System.RuntimeType.get_Name(this)"
            "System.Boolean System.Type.get_IsValueType(this)"
            "System.Array System.RuntimeType.GetEnumValues(this)"
            "System.Type System.RuntimeType.GetEnumUnderlyingType(this)"
            "System.Boolean System.RuntimeTypeHandle.CanCastTo(System.RuntimeType, System.RuntimeType)"
            "System.Reflection.RuntimeModule System.RuntimeTypeHandle.GetModule(System.RuntimeType)"
            "System.Void System.ModuleHandle.GetModuleType(System.Runtime.CompilerServices.QCallModule, System.Runtime.CompilerServices.ObjectHandleOnStack)"

            // EqualityComparer
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultEqualityComparer(System.Type)"
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultComparer(System.Type)"
            "System.Collections.Generic.EqualityComparer`1[T] System.Collections.Generic.EqualityComparer`1[T].get_Default()"

            // Thread
            "System.Threading.Thread System.Threading.Thread.get_CurrentThread()"

            // Interop
            "System.Int32 Interop+Sys.LChflagsCanSetHiddenFlag()"
            "System.Byte* Interop+Sys.GetCwd(System.Byte*, System.Int32)"
        ]

    let isInvokeInternalCall (fullMethodName : string) =
        ConcreteInvocations.Contains fullMethodName
