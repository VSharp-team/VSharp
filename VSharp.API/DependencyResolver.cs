#nullable enable
using System;

namespace VSharp;

public class DependencyResolver : IDisposable
{
    private readonly Func<string, string?> _resolver;

    public DependencyResolver(Func<string, string?> nameToPath)
    {
        _resolver = nameToPath;
        AssemblyManager.AddExtraResolver(_resolver);
    }

    public void Dispose()
    {
        AssemblyManager.RemoveExtraResolver(_resolver);
    }
}
