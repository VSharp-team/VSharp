using System.Reflection;

namespace VSharp.CSharpUtils.AssemblyResolving
{
    public interface IAssemblyResolver
    {
        string Resolve(AssemblyName assemblyName);
    }
}
