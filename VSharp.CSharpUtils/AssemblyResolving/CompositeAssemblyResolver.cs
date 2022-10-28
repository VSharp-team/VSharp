using System.Collections.Generic;
using System.Reflection;

namespace VSharp.CSharpUtils.AssemblyResolving
{
    public class CompositeAssemblyResolver : IAssemblyResolver
    {
        private readonly List<IAssemblyResolver> _resolvers = new();

        public CompositeAssemblyResolver(params IAssemblyResolver[] resolvers)
        {
            _resolvers.AddRange(resolvers);
        }

        public string Resolve(AssemblyName assemblyName)
        {
            foreach (var resolver in _resolvers)
            {
                var resolved = resolver.Resolve(assemblyName);
                if (resolved is not null)
                {
                    return resolved;
                }
            }

            return null;
        }
    }
}
