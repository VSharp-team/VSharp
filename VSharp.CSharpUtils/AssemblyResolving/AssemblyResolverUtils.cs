using System;
using System.IO;
using System.Linq;
using System.Reflection;

namespace VSharp.CSharpUtils.AssemblyResolving
{
    public static class AssemblyResolverUtils
    {
        public static string FindAssemblyWithName(DirectoryInfo directory, AssemblyName targetName)
        {
            bool IsTargetAssembly(FileInfo assemblyFile)
            {
                try
                {
                    var foundName = AssemblyName.GetAssemblyName(assemblyFile.FullName);
                    return foundName.Name == targetName.Name &&
                           foundName.Version == targetName.Version &&
                           foundName.ContentType == targetName.ContentType;
                }
                catch (Exception)
                {
                    return false;
                }
            }

            var found = directory.EnumerateFiles("*.dll").FirstOrDefault(IsTargetAssembly);

            if (found is not null)
            {
                return found.FullName;
            }

            foreach (var subDir in directory.EnumerateDirectories())
            {
                var foundInSubDir = FindAssemblyWithName(subDir, targetName);
                if (foundInSubDir is not null)
                {
                    return foundInSubDir;
                }
            }

            return null;
        }

        public static string GetBaseNuGetDirectory()
        {
            var baseDirectory = Environment.GetEnvironmentVariable("NUGET_PACKAGES");

            if (!string.IsNullOrEmpty(baseDirectory))
            {
                return baseDirectory;
            }

            if (OperatingSystem.IsWindows())
            {
                baseDirectory = Environment.GetEnvironmentVariable("USERPROFILE");
            }
            else
            {
                baseDirectory = Environment.GetEnvironmentVariable("HOME");
            }

            if (string.IsNullOrEmpty(baseDirectory))
            {
                return null;
            }

            return Path.Combine(baseDirectory, ".nuget", "packages");
        }
    }
}
