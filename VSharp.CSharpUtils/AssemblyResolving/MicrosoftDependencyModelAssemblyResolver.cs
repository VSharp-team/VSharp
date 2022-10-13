using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using Microsoft.Extensions.DependencyModel;
using Microsoft.Extensions.DependencyModel.Resolution;

namespace VSharp.CSharpUtils.AssemblyResolving
{
    public class MicrosoftDependencyModelAssemblyResolver : IAssemblyResolver
    {
        private readonly List<ICompilationAssemblyResolver> _resolvers = new();
        private readonly DependencyContext _context;

        public MicrosoftDependencyModelAssemblyResolver(DependencyContext context, params ICompilationAssemblyResolver[] resolvers)
        {
            _resolvers.AddRange(resolvers);
            _context = context;
        }

        public string Resolve(AssemblyName assemblyName)
        {
            if (_context is null)
            {
                return null;
            }

            var compilationLibrary = _context.CompileLibraries.FirstOrDefault(l =>
                l.Name.Equals(assemblyName.Name, StringComparison.OrdinalIgnoreCase));

            if (compilationLibrary is null)
            {
                var runtimeLibrary = _context.RuntimeLibraries.FirstOrDefault(l =>
                    l.Name.Equals(assemblyName.Name, StringComparison.OrdinalIgnoreCase));

                if (runtimeLibrary is not null)
                {
                    compilationLibrary = new CompilationLibrary(
                        runtimeLibrary.Type,
                        runtimeLibrary.Name,
                        runtimeLibrary.Version,
                        runtimeLibrary.Hash,
                        runtimeLibrary.RuntimeAssemblyGroups.SelectMany(g => g.AssetPaths),
                        runtimeLibrary.Dependencies,
                        runtimeLibrary.Serviceable
                    );
                }
            }

            if (compilationLibrary is null)
            {
                return null;
            }

            var assemblies = new List<string>();

            foreach (ICompilationAssemblyResolver resolver in _resolvers)
            {
                try
                {
                    if (resolver.TryResolveAssemblyPaths(compilationLibrary, assemblies))
                    {
                        return assemblies[0];
                    }
                }
                catch { }
            }

            return null;
        }
    }
}
