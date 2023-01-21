using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.Loader;

namespace VSharp.CSharpUtils
{
    public class VSharpAssemblyLoadContext : AssemblyLoadContext
    {
        private readonly Dictionary<string, AssemblyDependencyResolver> _resolvers = new();

        public VSharpAssemblyLoadContext(string name) : base(name)
        {
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

            return base.LoadFromAssemblyPath(path);
        }
    }
}