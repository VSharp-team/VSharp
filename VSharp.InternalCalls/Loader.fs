namespace VSharp

open global.System
open System.Reflection

module Loader =

    let private implementsAttribute = lazy AssemblyManager.NormalizeType(typeof<ImplementsAttribute>)

    let private getImplementsName (attr : Attribute) =
        let implementsNameField = implementsAttribute.Value.GetField("Name")
        implementsNameField.GetValue(attr) :?> string

    let private collectImplementations (ts : Type seq) =
        let bindingFlags = BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
        let internalCalls = ResizeArray<string * MethodInfo>()
        for t in ts do
            for m in t.GetMethods(bindingFlags) do
                let mutable found = false
                for attr in m.GetCustomAttributes<ImplementsAttribute>() do
                    internalCalls.Add(attr.Name, m)
                    found <- true
                if not found then
                    for attr in m.GetCustomAttributes(implementsAttribute.Value) do
                        internalCalls.Add(getImplementsName attr, m)
        Map.ofSeq internalCalls

    let private CSharpUtilsAssembly =
        AssemblyName("VSharp.CSharpUtils").FullName |> AssemblyManager.LoadFromAssemblyName

    let public CSharpImplementations =
        // Loading assembly and collecting methods via VSharp assembly load context,
        // because C# internal calls will be explored by VSharp
        seq [
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.Array")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.Monitor")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.CLRConfig")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.Interop")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.NumberFormatInfo")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.StringUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.CharUnicodeInfo")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.BlockChain")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.GC")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.DateTimeUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.ThreadUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.DelegateUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.DiagnosticsUtils")
        ]
        |> collectImplementations

    let public FSharpImplementations =
        // Loading assembly and collecting methods via default assembly load context,
        // because all VSharp types, like VSharp.Core.state are loaded via default load context,
        // so F# internal calls (which use state) must be loaded via default load context
        Assembly.GetExecutingAssembly().GetTypes()
        |> Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule
        |> collectImplementations

    let private runtimeExceptionsConstructors =
        // Loading assembly and collecting methods via VSharp assembly load context,
        // because exceptions constructors will be explored by VSharp
        CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.Exceptions")
        |> Seq.singleton
        |> collectImplementations

    let private runtimeExceptionsImplementations =
        runtimeExceptionsConstructors.Values |> Seq.map Reflection.getFullMethodName |> set

    let mutable public CilStateImplementations : string seq =
        Seq.empty

    let public hasRuntimeExceptionsImplementation (fullMethodName : string) =
        Map.containsKey fullMethodName runtimeExceptionsConstructors

    let public isRuntimeExceptionsImplementation (fullMethodName : string) =
        Set.contains fullMethodName runtimeExceptionsImplementations

    let public getRuntimeExceptionsImplementation (fullMethodName : string) =
        runtimeExceptionsConstructors.[fullMethodName]

    let private shimImplementations =
        set [
            "System.DateTime System.DateTime.get_Now()"
            "System.String System.IO.File.ReadAllText(System.String)"
            "System.String[] System.IO.File.ReadAllLines(System.String)"
            "System.String[] System.IO.File.ReadLines(System.String)"
            "System.Byte[] System.IO.File.ReadAllBytes(System.String)"
            "System.String System.Console.ReadLine()"
            // Socket.Read  TODO: writing to the out parameters
        ]

    let public isShimmed (fullMethodName : string) =
        Set.contains fullMethodName shimImplementations

    let private concreteInvocations =
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
            "System.Reflection.PropertyInfo System.Type.GetProperty(this, System.String, System.Reflection.BindingFlags)"
            "System.Type[] System.RuntimeType.GetInterfaces(this)"
            "System.Boolean System.Type.get_IsByRef(this)"
            "System.RuntimeType System.RuntimeTypeHandle.GetBaseType(System.RuntimeType)"
            "System.TypeCode System.Type.GetTypeCode(System.Type)"
            "System.Reflection.CorElementType System.RuntimeTypeHandle.GetCorElementType(System.RuntimeType)"
            "System.Reflection.CorElementType System.Enum.InternalGetCorElementType(this)"
            "System.String System.RuntimeType.ToString(this)"

            // EqualityComparer
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultEqualityComparer(System.Type)"
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultComparer(System.Type)"
            "System.Collections.Generic.EqualityComparer`1[T] System.Collections.Generic.EqualityComparer`1[T].get_Default()"

            // Thread
            "System.Threading.Thread System.Threading.Thread.get_CurrentThread()"
            "System.Int32 System.Threading.Thread.get_OptimalMaxSpinWaitsPerSpinIteration()"

            // Interop
//            "System.Int32 Interop+Sys.LChflagsCanSetHiddenFlag()"
//            "System.Byte* Interop+Sys.GetCwd(System.Byte*, System.Int32)"
//            "System.Object System.Runtime.InteropServices.GCHandle.InternalCompareExchange(System.IntPtr, System.Object, System.Object)"
//            "System.Boolean System.Runtime.Intrinsics.X86.Sse2.get_IsSupported()"

            // Diagnostics
//            "System.IntPtr System.Diagnostics.Tracing.EventPipeInternal.CreateProvider(System.String, Interop+Advapi32+EtwEnableCallback)"
//            "System.Void System.Diagnostics.StackTrace.GetStackFramesInternal(System.Diagnostics.StackFrameHelper, System.Int32, System.Boolean, System.Exception)"

            // LINQ
            "System.Void System.Linq.Expressions.Expression.Validate(System.Type, System.Boolean)"

            // Environment
            "System.Int32 System.Environment.get_TickCount()"
            "System.Boolean System.Numerics.Vector.get_IsHardwareAccelerated()"

            // VSharp
            "System.Int32 IntegrationTests.ExceptionsControlFlow.ConcreteThrow()"
            "System.Void VSharp.CSharpUtils.Exceptions.CreateNullReferenceException()"
            "System.Void VSharp.CSharpUtils.Exceptions.CreateInvalidCastException(System.String)"
            "System.Void VSharp.CSharpUtils.Exceptions.CreateOverflowException()"
            "System.Void VSharp.CSharpUtils.Exceptions.CreateIndexOutOfRangeException()"
            "System.Void VSharp.CSharpUtils.Exceptions.ArgumentOutOfRangeException()"
            "System.Void VSharp.CSharpUtils.Exceptions.ArgumentException()"
            "System.Void VSharp.CSharpUtils.Exceptions.DivideByZeroException()"
            "System.Void VSharp.CSharpUtils.Exceptions.ArithmeticException()"
            "System.Void VSharp.CSharpUtils.Exceptions.CreateArrayTypeMismatchException()"
            "System.Void VSharp.CSharpUtils.Exceptions.CreateArgumentNullException()"
            "System.Void VSharp.CSharpUtils.Exceptions.CreateOutOfMemoryException()"
        ]

    let isInvokeInternalCall (fullMethodName : string) =
        concreteInvocations.Contains fullMethodName
