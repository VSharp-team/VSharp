using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;

namespace VSharp.CSharpUtils
{
    public class VSharpAssemblyLoadContext : AssemblyLoadContext, IDisposable
    {
        private readonly Dictionary<string, AssemblyDependencyResolver> _resolvers = new();

        private readonly Dictionary<string, Type> _types = new();

        public IEnumerable<string> DependenciesDirs { get; set; } = new List<string>();

        private Assembly OnAssemblyResolve(object sender, ResolveEventArgs args)
        {
            var existingInstance = Assemblies.FirstOrDefault(assembly => assembly.FullName == args.Name);
            if (existingInstance != null)
            {
                return existingInstance;
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
            if (!_resolvers.ContainsKey(path))
            {
                _resolvers[path] = new AssemblyDependencyResolver(path);
            }

            var asm = base.LoadFromAssemblyPath(path);
            foreach (var t in asm.GetTypes())
            {
                if (t.FullName is null)
                {
                    continue;
                }

                _types[t.FullName] = t;
            }
            return asm;
        }

        // Some types (for example, generic arguments)
        // may be loaded from the default context (AssemblyLoadContext.Default).
        // We have to rebuild them using the twin types from VSharpAssemblyLoadContext.
        public Type NormalizeType(Type t)
        {
            if (t.IsGenericType)
            {
                var fixedGenericTypes = t.GetGenericArguments().Select(NormalizeType).ToArray();
                return t.GetGenericTypeDefinition().MakeGenericType(fixedGenericTypes);
            }

            return _types.GetValueOrDefault(t.FullName, t);
        }

        public MethodInfo NormalizeMethod(MethodBase originMethod)
        {
            var asm = LoadFromAssemblyPath(originMethod.Module.Assembly.Location);
            if (originMethod.ReflectedType is null)
            {
                return asm.Modules
                    .SelectMany(m => m.GetMethods())
                    .First(m => m.MetadataToken == originMethod.MetadataToken);
            }

            var type = _types[originMethod.ReflectedType.FullName];
            var method = type.GetMethods()
                .First(m => m.MetadataToken == originMethod.MetadataToken);
            return method;
        }

        public void Dispose()
        {
            AppDomain.CurrentDomain.AssemblyResolve -= OnAssemblyResolve;
        }
    }
}
