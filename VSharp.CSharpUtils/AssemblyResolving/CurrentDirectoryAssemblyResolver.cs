using System.IO;
using System.Reflection;

namespace VSharp.CSharpUtils.AssemblyResolving
{
    public class CurrentDirectoryAssemblyResolver : IAssemblyResolver
    {
        private readonly string _directoryPath;

        public CurrentDirectoryAssemblyResolver(string directoryPath)
        {
            _directoryPath = directoryPath;
        }

        public string Resolve(AssemblyName assemblyName)
        {
            var dllPath = Path.Combine(_directoryPath, $"{assemblyName.Name}.dll");

            if (File.Exists(dllPath))
            {
                return dllPath;
            }

            return null;
        }
    }
}
