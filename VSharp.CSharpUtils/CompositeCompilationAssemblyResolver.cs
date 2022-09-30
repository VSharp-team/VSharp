using System.Collections.Generic;
using Microsoft.Extensions.DependencyModel;
using Microsoft.Extensions.DependencyModel.Resolution;

namespace VSharp.CSharpUtils
{
    public class CompositeCompilationAssemblyResolver : ICompilationAssemblyResolver
    {
        private readonly ICompilationAssemblyResolver[] _resolvers;

        public CompositeCompilationAssemblyResolver(ICompilationAssemblyResolver[] resolvers)
        {
            _resolvers = resolvers;
        }

        public bool TryResolveAssemblyPaths(CompilationLibrary library, List<string> assemblies)
        {
            foreach (ICompilationAssemblyResolver resolver in _resolvers)
            {
                try
                {
                    if (resolver.TryResolveAssemblyPaths(library, assemblies))
                    {
                        return true;
                    }
                }
                catch { }
            }

            return false;
        }
    }
}
