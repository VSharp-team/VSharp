using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using NuGet.Packaging;
using NuGet.Versioning;

namespace VSharp.CSharpUtils.AssemblyResolving
{
    public class NuGetGraphAssemblyResolver : IAssemblyResolver
    {
        private readonly HashSet<(string path, string pkgName)> _discovered = new();
        private readonly HashSet<string> _withResolvedDependencies = new();
        private readonly bool _fallbackToAllPackages;

        private readonly string _baseNuGetDirectory = AssemblyResolverUtils.GetBaseNuGetDirectory();

        public NuGetGraphAssemblyResolver(Assembly assembly, bool fallbackToAllPackages = true)
        {
            _fallbackToAllPackages = fallbackToAllPackages;

            var dllDirectory = new DirectoryInfo(Path.GetDirectoryName(assembly.Location));
            var currentDir = dllDirectory;

            do
            {
                var nupkg = currentDir.EnumerateFiles("*.nupkg").FirstOrDefault();
                if (nupkg is not null)
                {
                    _discovered.Add((currentDir.FullName, nupkg.Name));
                    break;
                }
                currentDir = currentDir.Parent;

            } while (currentDir.FullName != dllDirectory.Root.FullName);
        }

        private IEnumerable<string> GetDirectories()
        {
            if (_baseNuGetDirectory is null || _discovered.Count == 0)
            {
                yield break;
            }

            var queue = new Queue<(string, string)>(_discovered);

            while (queue.Count > 0)
            {
                var (path, name) = queue.Dequeue();

                if (!_withResolvedDependencies.Contains(name))
                {
                    var deps = GetDependencies(path, name);

                    foreach (var dep in deps)
                    {
                        queue.Enqueue(dep);
                    }
                }

                yield return path;
            }

            if (_fallbackToAllPackages)
            {
                foreach (var packageDir in Directory.EnumerateDirectories(_baseNuGetDirectory))
                {
                    foreach (var versionDir in Directory.EnumerateDirectories(packageDir))
                    {
                        if (_discovered.All(p => p.path != versionDir))
                        {
                            yield return versionDir;
                        }
                    }
                }
            }
        }

        public string Resolve(AssemblyName assemblyName)
        {
            foreach (var dir in GetDirectories())
            {
                var found = AssemblyResolverUtils.FindAssemblyWithName(new DirectoryInfo(dir), assemblyName);
                if (found is not null)
                {
                    return found;
                }
            }

            return null;
        }

        private HashSet<(string, string)> GetDependencies(string path, string name)
        {
            using FileStream inputStream = new FileStream(Path.Combine(path, name), FileMode.Open);
            using PackageArchiveReader reader = new PackageArchiveReader(inputStream);
            NuspecReader nuspec = reader.NuspecReader;
            var toReturn = new HashSet<(string, string)>();

            foreach (var dependencyGroup in nuspec.GetDependencyGroups())
            {
                foreach (var dependency in dependencyGroup.Packages)
                {
                    var basePackagePath = Path.Combine(_baseNuGetDirectory, dependency.Id.ToLower());

                    if (!Directory.Exists(basePackagePath))
                    {
                        continue;
                    }

                    var availableVersions = Directory.EnumerateDirectories(basePackagePath)
                        .Select(Path.GetFileName);

                    foreach (var version in availableVersions)
                    {
                        if (dependency.VersionRange.Satisfies(new NuGetVersion(version)))
                        {
                            var newEntry = (Path.Combine(basePackagePath, version), GetPackageFileName(dependency.Id.ToLower(),  version));

                            if (!_discovered.Contains(newEntry))
                            {
                                _discovered.Add(newEntry);
                                toReturn.Add(newEntry);
                            }

                            break;
                        }
                    }
                }
            }

            _withResolvedDependencies.Add(name);
            return toReturn;
        }

        private static string GetPackageFileName(string id, string version) =>
            $"{id}.{version}.nupkg";
    }
}
