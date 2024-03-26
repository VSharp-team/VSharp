#nullable enable
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;

namespace VSharp.CSharpUtils
{
    public class VSharpAssemblyLoadContext : AssemblyLoadContext, IDisposable
    {
        private static string GetTypeName(Type t)
        {
            return t.AssemblyQualifiedName ?? t.FullName ?? t.Name;
        }

        private readonly Dictionary<string, AssemblyDependencyResolver> _resolvers = new();

        private readonly Dictionary<string, Assembly> _assemblies = new();

        private readonly Dictionary<string, Type> _types = new();

        public event Func<string, string?>? ExtraResolver;

        public IEnumerable<string> DependenciesDirs { get; set; } = new List<string>();

        private const BindingFlags Flags =
            BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance | BindingFlags.Static;

        private Assembly? OnAssemblyResolve(object? sender, ResolveEventArgs args)
        {
            var existingInstance = Assemblies.FirstOrDefault(assembly => assembly.FullName == args.Name);
            if (existingInstance != null)
            {
                return existingInstance;
            }

            var extraResolverPath = ExtraResolver?.Invoke(args.Name);

            if (extraResolverPath is not null && File.Exists(extraResolverPath))
            {
                return LoadFromAssemblyPath(extraResolverPath);
            }

            foreach (var path in DependenciesDirs)
            {
                var assemblyPath = Path.Combine(path, new AssemblyName(args.Name).Name + ".dll");
                if (!File.Exists(assemblyPath))
                    continue;
                var assembly = LoadFromAssemblyPath(assemblyPath);
                return assembly;
            }

            return null;
        }

        public VSharpAssemblyLoadContext(string name) : base(name)
        {
            // Doesn't work with this.Resolving. It is not yet known why.
            AppDomain.CurrentDomain.AssemblyResolve += OnAssemblyResolve;
        }

        protected override Assembly? Load(AssemblyName assemblyName)
        {
            foreach (var (_, resolver) in _resolvers)
            {
                var path = resolver.ResolveAssemblyToPath(assemblyName);
                if (path != null)
                {
                    return LoadFromAssemblyPath(path);
                }
            }

            return null;
        }

        protected override IntPtr LoadUnmanagedDll(string unmanagedDllName)
        {
            foreach (var (_, resolver) in _resolvers)
            {
                var path = resolver.ResolveUnmanagedDllToPath(unmanagedDllName);
                if (path != null)
                {
                    return LoadUnmanagedDllFromPath(path);
                }
            }

            return IntPtr.Zero;
        }

        public new Assembly LoadFromAssemblyPath(string path)
        {
            if (_assemblies.TryGetValue(path, out var assembly))
            {
                return assembly;
            }

            if (!_resolvers.ContainsKey(path))
            {
                _resolvers[path] = new AssemblyDependencyResolver(path);
            }

            var asm = base.LoadFromAssemblyPath(path);
            _assemblies.Add(path, asm);

            foreach (var t in asm.GetTypesChecked())
            {
                var type = t.IsGenericType ? t.GetGenericTypeDefinition() : t;
                if (type.FullName is null)
                {
                    // Case for types, that contains open generic parameters
                    continue;
                }

                _types[GetTypeName(type)] = type;
            }
            return asm;
        }

        // Some types (for example, generic arguments)
        // may be loaded from the default context (AssemblyLoadContext.Default).
        // We have to rebuild them using the twin types from VSharpAssemblyLoadContext.
        public Type NormalizeType(Type t)
        {
            if (t.IsArray)
            {
                var elementType = t.GetElementType();
                Debug.Assert(elementType != null);
                var normalized = NormalizeType(elementType);
                var rank = t.GetArrayRank();
                return t.IsSZArray ? normalized.MakeArrayType() : normalized.MakeArrayType(rank);
            }

            if (t.IsPointer)
            {
                var elementType = t.GetElementType();
                Debug.Assert(elementType != null);
                var normalized = NormalizeType(elementType);
                return normalized.MakePointerType();
            }

            if (t is { IsGenericType: true, IsGenericTypeDefinition: false })
            {
                var normalized = NormalizeType(t.GetGenericTypeDefinition());
                var fixedGenericTypes = t.GetGenericArguments().Select(NormalizeType).ToArray();
                return normalized.MakeGenericType(fixedGenericTypes);
            }

            var name = GetTypeName(t);
            if (_types.TryGetValue(name, out var normalizedType))
            {
                return normalizedType;
            }

            _types.Add(name, t);
            return t;
        }

        public FieldInfo NormalizeField(FieldInfo fieldInfo)
        {
            var reflectedType = fieldInfo.ReflectedType;
            Debug.Assert(reflectedType != null);
            var type = NormalizeType(reflectedType);
            return type.GetFields(Flags)
                .FirstOrDefault(f => f.MetadataToken == fieldInfo.MetadataToken, fieldInfo);
        }

        public MethodBase NormalizeMethod(MethodBase originMethod)
        {
            var reflectedType = originMethod.ReflectedType;
            if (reflectedType is null)
            {
                // Case for dynamic methods
                var asm = LoadFromAssemblyPath(originMethod.Module.Assembly.Location);
                return asm.Modules
                    .SelectMany(m => m.GetMethods(Flags))
                    .FirstOrDefault(m => m.MetadataToken == originMethod.MetadataToken, originMethod);
            }

            LoadFromAssemblyPath(reflectedType.Assembly.Location);
            var type = NormalizeType(reflectedType);
            var method = type.GetMethods(Flags)
                .FirstOrDefault(m => m.MetadataToken == originMethod.MetadataToken,
                    type.GetConstructors(Flags)
                        .FirstOrDefault(m => m.MetadataToken == originMethod.MetadataToken, originMethod));
            return method;
        }

        public void Dispose()
        {
            AppDomain.CurrentDomain.AssemblyResolve -= OnAssemblyResolve;
        }
    }
}
