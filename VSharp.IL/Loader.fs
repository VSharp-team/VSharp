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
            "System.Boolean System.MemoryExtensions.Equals(System.ReadOnlySpan`1[System.Char], System.ReadOnlySpan`1[System.Char], System.StringComparison)"
            "System.Boolean System.SpanHelpers.SequenceEqual(System.Byte&, System.Byte&, System.UIntPtr)"
            "System.Boolean System.MemoryExtensions.SequenceEqual(System.ReadOnlySpan`1[T], System.ReadOnlySpan`1[T])"
        |]
        let vector = [|
            "System.Void System.Numerics.Vector`1[T]..ctor(this, T)"
            "System.Void System.Numerics.Vector`1[T]..ctor(this, T[])"
            "System.Void System.Numerics.Vector`1[T]..ctor(this, T[], System.Int32)"
            "System.Int32 System.Numerics.Vector`1[T].get_Count()"
            "System.Boolean System.Numerics.Vector`1[T].op_Inequality(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Boolean System.Numerics.Vector`1[T].op_Equality(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector.BitwiseOr(System.Numerics.Vector`1[T, System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector.BitwiseAnd(System.Numerics.Vector`1[T, System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector.Equals(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector`1[T].op_BitwiseAnd(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector`1[T].op_BitwiseOr(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector`1[T].op_ExclusiveOr(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector.ConditionalSelect(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Numerics.Vector`1[T] System.Numerics.Vector`1[T].get_AllBitsSet()"
            "System.Numerics.Vector`1[T] System.Numerics.Vector.LessThanOrEqual(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
            "System.Boolean System.Numerics.Vector.LessThanOrEqualAll(System.Numerics.Vector`1[T], System.Numerics.Vector`1[T])"
        |]
        let runtimeHelpers = [|
             "System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsKnownConstant(System.Char)"
             "System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsKnownConstant(System.String)"
             "System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.EnumEquals(T, T)"
        |]
        let interlocked = [|
            "System.Int32 System.Threading.Interlocked.Or(System.Int32&, System.Int32)"
        |]
        Array.concat [intPtr; volatile; defaultComparer; string; span; vector; runtimeHelpers; interlocked]

    let private concreteInvocations =
        set [
            // Types
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
            "System.Type System.RuntimeType.GetGenericTypeDefinition(this)"
            "System.String System.RuntimeType.get_FullName(this)"
            "System.Boolean System.RuntimeTypeHandle.ContainsGenericVariables(System.RuntimeType)"
            "System.Boolean System.RuntimeType.get_ContainsGenericParameters(this)"
            "System.Boolean System.RuntimeType.IsSubclassOf(this, System.Type)"
            "System.Boolean System.RuntimeType.get_IsActualEnum(this)"
            "System.Boolean System.Enum.IsDefined(System.Type, System.Object)"
            "System.Boolean System.Type.get_IsPublic(this)"
            "System.Reflection.MemberTypes System.RuntimeType.get_MemberType(this)"
            "System.Int32 System.RuntimeType.get_MetadataToken(this)"
            "System.Object System.Enum.InternalBoxEnum(System.RuntimeType, System.Int64)"
            "System.Object System.Reflection.RtFieldInfo.GetValue(this, System.Object)"
            "System.Reflection.Assembly System.RuntimeType.get_Assembly(this)"
            "System.Void System.Type+<>c..ctor(this)"

            // Object
            "System.Object System.Object.MemberwiseClone(this)"

            // Assembly
            "System.String System.Reflection.RuntimeAssembly.get_Location(this)"
            "System.Void System.Runtime.Loader.AssemblyLoadContext..ctor(this, System.Boolean, System.Boolean, System.String)"
            "System.Void System.Runtime.Loader.DefaultAssemblyLoadContext..ctor(this)"

            // EqualityComparer
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultEqualityComparer(System.Type)"
            "System.Object System.Collections.Generic.ComparerHelpers.CreateDefaultComparer(System.Type)"
            "System.Collections.Generic.EqualityComparer`1[T] System.Collections.Generic.EqualityComparer`1[T].get_Default()"
            "System.Void System.Collections.Generic.NonRandomizedStringEqualityComparer+OrdinalComparer..ctor(this, System.Collections.Generic.IEqualityComparer`1[System.String])"
            "System.Void System.StringComparer..ctor(this)"

            // Threading
            "System.Threading.Thread System.Threading.Thread.get_CurrentThread()"
            "System.Int32 System.Threading.Thread.get_OptimalMaxSpinWaitsPerSpinIteration()"
            "System.Int32 System.Threading.Thread.GetCurrentProcessorId()"
            "System.Void System.Threading.AutoResetEvent..ctor(this, System.Boolean)"
            "System.Void System.Threading.EventWaitHandle.CreateEventCore(this, System.Boolean, System.Threading.EventResetMode, System.String, System.Boolean&)"
            "System.Void System.Threading.EventWaitHandle..ctor(this, System.Boolean, System.Threading.EventResetMode, System.String, System.Boolean&)"
            "System.Void System.Threading.EventWaitHandle..ctor(this, System.Boolean, System.Threading.EventResetMode)"
            "System.Void System.Threading.ManualResetEvent..ctor(this, System.Boolean)"
            "System.Void System.Threading.Thread.Initialize(this)"
            "System.Byte System.Threading.ThreadPool.InitializeConfigAndDetermineUsePortableThreadPool()"
            "System.Void System.Threading.Thread.set_Name(this, System.String)"
            "System.Void System.Threading.LowLevelMonitor.Initialize(this)"
            "System.Void System.Threading.Semaphore.CreateSemaphoreCore(this, System.Int32, System.Int32, System.String, System.Boolean&)"
            "System.Void System.Threading.Semaphore..ctor(this, System.Int32, System.Int32, System.String, System.Boolean&)"
            "System.Void System.Threading.Semaphore..ctor(this, System.Int32, System.Int32, System.String)"
            "System.Void System.Threading.Semaphore..ctor(this, System.Int32, System.Int32)"
            "System.Void System.Threading.LowLevelLifoSemaphore.Create(this, System.Int32)"
            "System.Void System.Threading.Thread.set_IsThreadPoolThread(this, System.Boolean)"
            "System.Void System.Threading.Thread.set_IsBackground(this, System.Boolean)"
            "System.Int32 System.Threading.Thread.GetCurrentProcessorNumber()"
            "System.Boolean System.Threading.ProcessorIdCache.ProcessorNumberSpeedCheck()"

            // Interop
            "System.String System.Runtime.InteropServices.RuntimeInformation.get_OSDescription()"
            "System.String Interop+Sys.GetHostName()"
            "System.String Interop+Sys.GetDomainName()"
            "System.IntPtr System.RuntimeTypeHandle.GetGCHandle(System.Runtime.CompilerServices.QCallTypeHandle, System.Runtime.InteropServices.GCHandleType)"
            "System.Object System.Runtime.InteropServices.GCHandle.InternalGet(System.IntPtr)"
            "System.Span`1[System.Byte] System.Runtime.InteropServices.MemoryMarshal.AsBytes(System.Span`1[T]))"
//            "System.Int32 Interop+Sys.LChflagsCanSetHiddenFlag()"
//            "System.Byte* Interop+Sys.GetCwd(System.Byte*, System.Int32)"
//            "System.Object System.Runtime.InteropServices.GCHandle.InternalCompareExchange(System.IntPtr, System.Object, System.Object)"

            // Diagnostics
            "System.Byte[] System.Diagnostics.Tracing.Statics.MetadataForString(System.String, System.Int32, System.Int32, System.Int32)"
            "System.Int64 System.Diagnostics.Stopwatch.QueryPerformanceCounter()"
//            "System.IntPtr System.Diagnostics.Tracing.EventPipeInternal.CreateProvider(System.String, Interop+Advapi32+EtwEnableCallback)"
//            "System.Void System.Diagnostics.StackTrace.GetStackFramesInternal(System.Diagnostics.StackFrameHelper, System.Int32, System.Boolean, System.Exception)"

            // LINQ
            "System.Void System.Linq.Expressions.Expression.Validate(System.Type, System.Boolean)"

            // Delegates
            "System.IntPtr System.Runtime.InteropServices.Marshal.GetFunctionPointerForDelegate(System.Delegate)"
            "System.Reflection.MethodInfo System.Delegate.get_Method(this)"
            "System.Reflection.MethodInfo System.Reflection.RuntimeReflectionExtensions.GetMethodInfo(System.Delegate)"

            // Environment
            "System.Int32 System.Environment.get_TickCount()"
            "System.Boolean System.Numerics.Vector.get_IsHardwareAccelerated()"
            "System.String System.Environment.GetEnvironmentVariable(System.String)"
            "System.OperatingSystem System.Environment.GetOSVersion()"
            "System.Int32 System.Environment.GetProcessId()"
            "System.String System.Environment.get_UserName()"
            "System.String System.Net.NetworkInformation.HostInformationPal.GetHostName()"
            "System.String System.Environment.get_SystemDirectory()"
            "System.Void System.Diagnostics.StackTrace..ctor(this)"
            "System.String System.BadImageFormatException.get_Message(this)"
            "System.String System.Environment.GetFolderPath(System.Environment+SpecialFolder)"
            "System.String System.AppContext.GetBaseDirectoryCore()"
            "System.String System.IO.Path.GetFullPath(System.String)"
            "System.Boolean System.OperatingSystem.IsOSPlatform(System.String)"
            "System.Collections.IDictionary System.Environment.GetEnvironmentVariables()"
            "System.IO.TextWriter System.Console.get_Out()"
            "System.String System.IO.Path.GetTempPath()"
            // TODO: move to symbolic internal calls, it's dangerous
            "System.Void System.Environment.SetEnvironmentVariable(System.String, System.String)"
            "System.Int64 System.Environment.get_TickCount64()"
            "System.Void System.Threading.TimerQueueTimer..ctor(this, System.Threading.TimerCallback, System.Object, System.UInt32, System.UInt32, System.Boolean)"
            "System.Void System.ConsolePal.EnsureInitializedCore()"
            "System.Void System.Console.add_CancelKeyPress(System.ConsoleCancelEventHandler)"
            "System.Boolean System.IO.File.Exists(System.String)"

            // Text
            "System.Int32 System.Text.UTF8Encoding.GetBytes(this, System.String, System.Int32, System.Int32, System.Byte[], System.Int32)"

            // Exceptions
            "System.String System.Exception.get_Source(this)"
            "System.String System.BadImageFormatException.ToString(this)"

            // Reflection
            "System.Reflection.AssemblyName System.Reflection.Assembly.GetName(this)"
            "System.Reflection.AssemblyName System.Reflection.RuntimeAssembly.GetName(this, System.Boolean)"
            "System.Byte[] System.Reflection.AssemblyName.GetPublicKeyToken(this)"
            "System.Reflection.Assembly System.Reflection.Assembly.Load(System.Reflection.AssemblyName)"
            "System.Reflection.Assembly System.Reflection.Assembly.GetEntryAssembly()"
            "System.Runtime.Loader.AssemblyLoadContext System.Runtime.Loader.AssemblyLoadContext.GetLoadContext(System.Reflection.Assembly)"
            "System.Boolean System.Reflection.RuntimeAssembly.get_IsDynamic(this)"
            "System.Boolean System.Reflection.Metadata.MetadataUpdater.IsApplyUpdateSupported()"
            "System.Reflection.MethodInfo System.Type.GetMethod(this, System.String, System.Reflection.BindingFlags)"
            "System.Reflection.FieldInfo System.Type.GetField(this, System.String)"

            // DependencyInjection
            "Microsoft.Extensions.DependencyInjection.ServiceLookup.ServiceProviderEngine Microsoft.Extensions.DependencyInjection.ServiceProvider.GetEngine(this)"
            "System.Void Microsoft.Extensions.DependencyInjection.ServiceProvider..ctor(this, System.Collections.Generic.ICollection`1[Microsoft.Extensions.DependencyInjection.ServiceDescriptor], Microsoft.Extensions.DependencyInjection.ServiceProviderOptions)"
            "Microsoft.Extensions.DependencyInjection.ServiceProvider Microsoft.Extensions.DependencyInjection.ServiceCollectionContainerBuilderExtensions.BuildServiceProvider(Microsoft.Extensions.DependencyInjection.IServiceCollection, Microsoft.Extensions.DependencyInjection.ServiceProviderOptions)"
            "Microsoft.Extensions.DependencyInjection.ServiceProvider Microsoft.Extensions.DependencyInjection.ServiceCollectionContainerBuilderExtensions.BuildServiceProvider(Microsoft.Extensions.DependencyInjection.IServiceCollection)"
            "System.Void Microsoft.Extensions.DependencyInjection.ServiceLookup.CompiledServiceProviderEngine..ctor(this, Microsoft.Extensions.DependencyInjection.ServiceProvider)"
            "System.Void Microsoft.Extensions.DependencyInjection.ServiceLookup.DynamicServiceProviderEngine..ctor(this, Microsoft.Extensions.DependencyInjection.ServiceProvider)"
            "System.Object Microsoft.Extensions.DependencyInjection.ServiceProvider.GetService(this, System.Type)"
            "Microsoft.Extensions.DependencyInjection.IServiceScope Microsoft.Extensions.DependencyInjection.ServiceProviderServiceExtensions.CreateScope(System.IServiceProvider)"

            // Activator
            "T System.Activator.CreateInstance()"

            // Guid
            "System.Guid System.Guid.NewGuid()"
            "System.Void System.Guid..ctor(this, System.String)"
            "System.Guid System.Diagnostics.Tracing.EventSource.GetGuid(System.Type)"
            "System.Guid System.Guid.Parse(System.String)"

            // CultureInfo
            "System.Globalization.CultureInfo System.Globalization.CultureInfo.get_CurrentCulture()"
            "System.Globalization.CultureInfo System.Globalization.CultureInfo.get_InvariantCulture()"
            "System.Globalization.CompareInfo System.Globalization.CultureInfo.get_CompareInfo(this)"
            "System.Boolean System.Globalization.GlobalizationMode.get_Invariant()"
            "System.String System.Globalization.CultureInfo.get_Name(this)"
            "System.Globalization.CultureInfo System.Globalization.CultureInfo.GetCultureInfo(System.String)"
            "System.Globalization.CultureData System.Globalization.CultureData.GetCultureData(System.String, System.Boolean)"
            "System.String System.Globalization.CultureInfo.GetUserDefaultLocaleName()"
            "System.String System.Globalization.CultureData.GetLocaleInfoEx(System.String, System.UInt32)"
            "System.Globalization.CultureData System.Globalization.CultureData.get_Invariant()"
            "System.Globalization.CalendarData System.Globalization.CalendarData.CreateInvariant()"
            "System.Object System.Globalization.CultureInfo.GetFormat(this, System.Type)"
            "System.Void System.Globalization.CultureInfo..ctor(this, System.Globalization.CultureData, System.Boolean)"

            // ResourceManager
            "System.Void System.Resources.ResourceManager..ctor(this, System.String, System.Reflection.Assembly)"
            "System.String System.Resources.ResourceManager.GetString(this, System.String, System.Globalization.CultureInfo)"

            // Buffers
            "System.Buffers.TlsOverPerCoreLockedStacksArrayPool`1+ThreadLocalArray[T][] System.Buffers.TlsOverPerCoreLockedStacksArrayPool`1[T].InitializeTlsBucketsAndTrimming(this)"

            // Random
            "System.Void System.Random..ctor(this)"
            "System.UInt64 System.Marvin.GenerateSeed()"
            "System.UInt32 System.HashCode.GenerateGlobalSeed()"

            // Time
            // TODO: this should be extern mocks
            "System.DateTime System.DateTime.get_Now()"
            "System.DateTime System.DateTime.get_UtcNow()"
            "System.DateTime System.Diagnostics.Process.get_StartTime(this)"

            // FileSystem
            "System.String System.IO.FileSystemInfo.get_LinkTarget(this)"
            "System.String System.Environment.get_CurrentDirectory()"
            // TODO: should be extern mocked
            "System.Boolean System.IO.Directory.Exists(System.String)"

            // EventSource
            "System.String System.Diagnostics.Tracing.EventSource.GetName(System.Type)"
            "System.Void Microsoft.Extensions.Logging.EventSource.LoggingEventSource..ctor(this)"
            "System.Void System.Diagnostics.DiagnosticSourceEventSource..ctor(this)"
            "System.Void System.Diagnostics.Tracing.NativeRuntimeEventSource..ctor(this)"
            "System.Void System.Diagnostics.Tracing.EventSource..ctor(this, System.Guid, System.String)"
            "System.Void System.Diagnostics.Tracing.EventSource.Initialize(this, System.Guid, System.String, System.String[])"
            "System.Void System.Diagnostics.Tracing.EventSource..ctor(this, System.String)"
            "System.Void Microsoft.EntityFrameworkCore.Infrastructure.EntityFrameworkEventSource..ctor(this)"

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

            // Arithmetics
            "System.Double System.Math.Pow(System.Double, System.Double)"
            "System.Double System.Math.Min(System.Double, System.Double)"

            // ASP.NET Core
            // Configuration builder
            "System.Void Microsoft.Extensions.Configuration.ConfigurationManager+ConfigurationSources.Add(this, Microsoft.Extensions.Configuration.IConfigurationSource)"

            // ConfigurationManager
            "System.Void Microsoft.Extensions.Configuration.ConfigurationManager.ReloadSources(this)"

            // EntityFramework
            "System.Void Microsoft.EntityFrameworkCore.DbContext..ctor(this)"
            "Microsoft.EntityFrameworkCore.Internal.IDbContextDependencies Microsoft.EntityFrameworkCore.DbContext.get_DbContextDependencies(this)"
            "Microsoft.EntityFrameworkCore.ChangeTracking.EntityEntry`1[TEntity] Microsoft.EntityFrameworkCore.DbContext.Add(this, TEntity)"
            "Microsoft.EntityFrameworkCore.ChangeTracking.Internal.InternalEntityEntry Microsoft.EntityFrameworkCore.ChangeTracking.Internal.StateManager.GetOrCreateEntry(this, System.Object)"
            "System.Void Microsoft.EntityFrameworkCore.DbContext.Dispose(this)"

            // Collections
            // Enumerable
            "TSource System.Linq.Enumerable.Single(System.Collections.Generic.IEnumerable`1[TSource], System.Func`2[TSource,System.Boolean])"
            "TSource System.Linq.Enumerable.Max(System.Collections.Generic.IEnumerable`1[TSource]))"

            // Dictionary
            "System.Void System.Collections.Generic.Dictionary`2[TKey,TValue].Add(this, TKey, TValue)"
            "System.Void System.Collections.Generic.Dictionary`2[TKey,TValue]..ctor(this)"
            "System.Void System.Collections.Generic.Dictionary`2[TKey,TValue]..ctor(this, System.Int32, System.Collections.Generic.IEqualityComparer`1[TKey])"
            "System.Void System.Collections.Generic.Dictionary`2[TKey,TValue]..ctor(this, System.Collections.Generic.IEqualityComparer`1[TKey])"
            "System.Boolean System.Collections.Generic.Dictionary`2[TKey,TValue].TryInsert(this, TKey, TValue, System.Collections.Generic.InsertionBehavior)"
            "System.Boolean System.Collections.Generic.Dictionary`2[TKey,TValue].ContainsKey(this, TKey)"
            "System.Boolean System.Collections.Generic.Dictionary`2[TKey,TValue].TryGetValue(this, TKey, TValue&)"
            "System.Void System.Collections.Generic.SortedDictionary`2+KeyValuePairComparer[TKey,TValue]..ctor(this, System.Collections.Generic.IComparer`1[TKey])"

            // Set
            "System.Int32 System.Collections.Generic.HashSet`1[T].Initialize(this, System.Int32)"
            "System.Void System.Collections.Generic.HashSet`1[T].UnionWith(this, System.Collections.Generic.IEnumerable`1[T])"
            "System.Void System.Collections.Generic.HashSet`1[T]..ctor(this, System.Collections.Generic.IEqualityComparer`1[T])"
            "System.Boolean System.Collections.Generic.HashSet`1[T].Add(this, T)"
            "System.Void System.Collections.Generic.TreeSet`1[T]..ctor(this, System.Collections.Generic.IComparer`1[T])"
            "System.Void System.Collections.Generic.SortedSet`1[T]..ctor(this, System.Collections.Generic.IComparer`1[T])"
            "System.Void System.Collections.Generic.SortedDictionary`2[TKey,TValue]..ctor(this)"
            "System.Void System.Collections.Generic.SortedDictionary`2[TKey,TValue]..ctor(this, System.Collections.Generic.IComparer`1[TKey])"

            // Collection
            "System.Void System.Collections.ObjectModel.Collection`1[T]..ctor(this)"

            // String
            "System.String System.String.ToUpperInvariant(this)"
            "System.Boolean System.Text.Unicode.Utf16Utility.AllCharsInUInt32AreAscii(System.UInt32)"
            "System.String System.Int32.ToString(this, System.IFormatProvider)"
            "System.String System.Number.FormatInt32(System.Int32, System.Int32, System.String, System.IFormatProvider)"
            "System.String System.String.FormatHelper(System.IFormatProvider, System.String, System.ReadOnlySpan`1[System.Object])"
            "System.Void System.Text.ValueStringBuilder.AppendFormatHelper(this, System.IFormatProvider, System.String, System.ReadOnlySpan`1[System.Object])"
            "System.String System.String.Format(System.IFormatProvider, System.String, System.Object)"
            "System.String System.String.InternalSubString(this, System.Int32, System.Int32)"
            "System.String System.String.Substring(this, System.Int32)"
            "System.String System.String.Substring(this, System.Int32, System.Int32)"
            "System.String[] System.String.CreateSplitArrayOfThisAsSoleValue(this, System.StringSplitOptions, System.Int32)"
            "System.String[] System.String.Split(this, System.Char[], System.StringSplitOptions)"
            "System.Int32 System.Globalization.CompareInfo.Compare(this, System.String, System.String, System.Globalization.CompareOptions)"
            "System.Boolean System.String.Contains(this, System.Char)"

            // Array
            "System.UIntPtr System.Array.get_NativeLength(this)"
            "System.Object System.Array.InternalGetValue(this, System.IntPtr)"

            // List
            "System.Void System.Collections.Generic.List`1[T]..ctor(this)"
            "T[] System.Collections.Generic.List`1[T].ToArray(this)"
            "System.Void System.Collections.Generic.List`1[T].Add(this, T)"
            "System.Void System.Collections.Generic.List`1[T].AddWithResize(this, T)"
            "System.Void System.Collections.Generic.List`1[T].Grow(this, System.Int32)"
            "System.Void System.Collections.Generic.List`1[T].RemoveAt(this, System.Int32)"
            "System.Void System.Collections.Generic.List`1+Enumerator[T]..ctor(this, System.Collections.Generic.List`1[T])"

            // Span
            "System.Boolean System.MemoryExtensions.Equals(System.ReadOnlySpan`1[System.Char], System.ReadOnlySpan`1[System.Char], System.StringComparison)"
            "System.Boolean System.MemoryExtensions.SequenceEqual(System.ReadOnlySpan`1[T], System.ReadOnlySpan`1[T])"
            "System.ReadOnlySpan`1[T] System.ReadOnlyMemory`1[T].get_Span(this)"

            // ProfileOptimization
            "System.Void System.Runtime.ProfileOptimization.SetProfileRoot(System.String)"
            "System.Void System.Runtime.ProfileOptimization.StartProfile(System.String)"

            // Enum
            "System.Boolean System.Enum.HasFlag(this, System.Enum)"

            // WeakReference
            "T System.WeakReference`1[T].get_Target(this)"

            // RuntimeHelpers
            "System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.IsReferenceOrContainsReferences()"
            "System.Boolean System.Runtime.CompilerServices.RuntimeHelpers.ObjectHasComponentSize(System.Object)"

            // Infrastructure
            "Microsoft.Management.Infrastructure.Serialization.CimMofDeserializer Microsoft.Management.Infrastructure.Serialization.CimMofDeserializer.Create()"

            // GC
            "T[] System.GC.AllocateUninitializedArray(System.Int32, System.Boolean)"

            // ResourceManager
            "System.Void System.Resources.ResourceManager..ctor(this, System.Type)"

            // Unsafe
            // "T Internal.Runtime.CompilerServices.Unsafe.As(System.Object)"
            // "T System.Runtime.CompilerServices.Unsafe.As(System.Object)"
            // "T& Internal.Runtime.CompilerServices.Unsafe.AsRef(T&)"
            // "T& System.Runtime.CompilerServices.Unsafe.AsRef(T&)"
            // "T& Internal.Runtime.CompilerServices.Unsafe.AsRef(System.Void*)"
            // "T& System.Runtime.CompilerServices.Unsafe.AsRef(System.Void*)"
            // "TTo& Internal.Runtime.CompilerServices.Unsafe.As(TFrom&)"
            // "TTo& System.Runtime.CompilerServices.Unsafe.As(TFrom&)"
            // "System.Boolean Internal.Runtime.CompilerServices.Unsafe.IsNullRef(T&)"
            // "System.Boolean System.Runtime.CompilerServices.Unsafe.IsNullRef(T&)"

            "System.Boolean System.Double.TryParse(System.String, System.Double&)"
            "System.Boolean System.Single.TryParse(System.String, System.Single&)"
            "System.String System.Exception.GetStackTrace(this)"
        ]

    let internal isInvokeInternalCall (fullMethodName : string) =
        concreteInvocations.Contains fullMethodName
