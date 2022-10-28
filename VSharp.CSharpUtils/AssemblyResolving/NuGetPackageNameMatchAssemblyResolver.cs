using System.IO;
using System.Reflection;

namespace VSharp.CSharpUtils.AssemblyResolving
{
    public class NuGetPackageNameMatchAssemblyResolver : IAssemblyResolver
    {
        private readonly string _baseNuGetDirectory = AssemblyResolverUtils.GetBaseNuGetDirectory();

        public string Resolve(AssemblyName assemblyName)
        {
            // TODO: consider different frameworks versions
            var path = Path.Combine(_baseNuGetDirectory, assemblyName.Name.ToLower());

            if (!Directory.Exists(path))
            {
                return null;
            }

            return AssemblyResolverUtils.FindAssemblyWithName(new DirectoryInfo(path), assemblyName);;
        }
    }
}
