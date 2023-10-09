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

    let internal CSharpImplementations =
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
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.DiagnosticsUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.CultureInfoUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.DebugProviderUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.EnvironmentUtils")
            CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.JetBrainsDiagnosticsUtils")
        ]
        |> collectImplementations

    // Loading assembly and collecting methods via default assembly load context,
    // because all VSharp types, like VSharp.Core.state are loaded via default load context,
    // so F# internal calls (which use state) must be loaded via default load context
    let mutable internal FSharpImplementations : Map<string, MethodInfo> = Map.empty

    let public SetInternalCallsAssembly (asm : Assembly) =
        assert asm.FullName.Contains("VSharp.InternalCalls")
        let implementations =
            asm.GetTypes()
            |> Array.filter Microsoft.FSharp.Reflection.FSharpType.IsModule
            |> collectImplementations
        assert(Map.isEmpty implementations |> not)
        FSharpImplementations <- implementations

    let internal runtimeExceptionsConstructors =
        // Loading assembly and collecting methods via VSharp assembly load context,
        // because exceptions constructors will be explored by VSharp
        CSharpUtilsAssembly.GetType("VSharp.CSharpUtils.Exceptions")
        |> Seq.singleton
        |> collectImplementations

    let private runtimeExceptionsImplementations =
        runtimeExceptionsConstructors.Values |> Seq.map Reflection.getFullMethodName |> set

    let internal isRuntimeExceptionsImplementation (fullMethodName : string) =
        Set.contains fullMethodName runtimeExceptionsImplementations

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

    let internal isShimmed (fullMethodName : string) =
        Set.contains fullMethodName shimImplementations

    let internal trustedIntrinsics =
        let intPtr = Reflection.getAllMethods typeof<IntPtr> |> Array.map Reflection.getFullMethodName
        let volatile = Reflection.getAllMethods typeof<System.Threading.Volatile> |> Array.map Reflection.getFullMethodName
        let defaultComparer = [|"System.Collections.Generic.Comparer`1[T] System.Collections.Generic.Comparer`1[T].get_Default()"|]
        let string = [|"System.Boolean System.String.StartsWith(this, System.String, System.StringComparison)"|]
        let span = [|
            "System.Boolean System.MemoryExtensions.StartsWith(System.ReadOnlySpan`1[T], System.ReadOnlySpan`1[T])"
            "System.Boolean System.SpanHelpers.SequenceEqual(System.Byte&, System.Byte&, System.UIntPtr)"
        |]
        let vector = [|
            "System.Void System.Numerics.Vector`1[T]..ctor(this, T)"
            "System.Void System.Numerics.Vector`1[T]..ctor(this, T[])"
            "System.Void System.Numerics.Vector`1[T]..ctor(this, T[], System.Int32)"
            "System.Int32 System.Numerics.Vector`1[T].get_Count()"
            "System.Boolean System.Numerics.Vector`1[T].op_Inequality(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Boolean System.Numerics.Vector`1[T].op_Equality(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
        |]
        let runtimeHelpers = [|
             "System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsKnownConstant(System.Char)"
             "System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsKnownConstant(System.String)"
        |]
        let interlocked = [|
            "System.Int32 System.Threading.Interlocked.Or(System.Int32&, System.Int32)"
        |]
        Array.concat [intPtr; volatile; defaultComparer; string; span; vector; runtimeHelpers; interlocked]

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
            "System.Boolean System.RuntimeType.IsDefined(this, System.Type, System.Boolean)"
            "System.Reflection.CustomAttribute.IsDefined(System.RuntimeType, System.RuntimeType, System.Boolean)"
            "System.Void System.RuntimeType.set_GenericCache(this, System.Object)"
            "System.Enum+EnumInfo System.Enum.GetEnumInfo(System.RuntimeType, System.Boolean)"
            "System.Collections.Generic.IArraySortHelper`1[T] System.Collections.Generic.ArraySortHelper`1[T].CreateArraySortHelper()"
            "System.Void System.Buffers.ArrayPoolEventSource..ctor(this)"
            "System.Type[] System.RuntimeType.GetGenericArguments(this)"
            "System.Boolean System.RuntimeType.get_IsGenericType(this)"
            "System.RuntimeType System.ModuleHandle.GetModuleType(System.Reflection.RuntimeModule)"
            "System.Type System.RuntimeType.get_DeclaringType(this)"
            "System.String System.RuntimeType.get_Namespace(this)"
            "System.Boolean System.Type.get_IsAbstract(this)"
            "System.Object[] System.Reflection.RuntimeAssembly.GetCustomAttributes(this, System.Type, System.Boolean)"

            // Assembly
            "System.String System.Reflection.RuntimeAssembly.get_Location(this)"

            // EqualityComparer
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultEqualityComparer(System.Type)"
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultComparer(System.Type)"
            "System.Collections.Generic.EqualityComparer`1[T] System.Collections.Generic.EqualityComparer`1[T].get_Default()"

            // Thread
            "System.Threading.Thread System.Threading.Thread.get_CurrentThread()"
            "System.Int32 System.Threading.Thread.get_OptimalMaxSpinWaitsPerSpinIteration()"
            "System.Int32 System.Threading.Thread.GetCurrentProcessorId()"

            // Interop
            "System.String System.Runtime.InteropServices.RuntimeInformation.get_OSDescription()"
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
            "System.String System.Environment.GetEnvironmentVariable(System.String)"
            "System.OperatingSystem System.Environment.GetOSVersion()"

            // Reflection
            "System.Reflection.AssemblyName System.Reflection.Assembly.GetName(this)"
            "System.Reflection.AssemblyName System.Reflection.RuntimeAssembly.GetName(this, System.Boolean)"

            // Guid
            "System.Guid System.Guid.NewGuid()"

            // CultureInfo
            "System.Globalization.CultureInfo System.Globalization.CultureInfo.get_CurrentCulture()"
            "System.Globalization.CultureInfo System.Globalization.CultureInfo.get_InvariantCulture()"
            "System.Globalization.CompareInfo System.Globalization.CultureInfo.get_CompareInfo(this)"
            "System.Boolean System.Globalization.GlobalizationMode.get_Invariant()"
            "System.String System.Globalization.CultureInfo.get_Name(this)"
            "System.Globalization.CultureInfo System.Globalization.CultureInfo.GetCultureInfo(System.String)"
            "System.Globalization.CultureData System.Globalization.CultureData.GetCultureData(System.String, System.Boolean)"

            // ResourceManager
            "System.Void System.Resources.ResourceManager..ctor(this, System.String, System.Reflection.Assembly)"
            "System.String System.Resources.ResourceManager.GetString(this, System.String, System.Globalization.CultureInfo)"

            // Buffers
            "System.Buffers.TlsOverPerCoreLockedStacksArrayPool`1+ThreadLocalArray[T][] System.Buffers.TlsOverPerCoreLockedStacksArrayPool`1[T].InitializeTlsBucketsAndTrimming(this)"

            // Random
            "System.Void System.Random..ctor(this)"
            "System.UInt64 System.Marvin.GenerateSeed()"

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

    let internal isInvokeInternalCall (fullMethodName : string) =
        concreteInvocations.Contains fullMethodName
