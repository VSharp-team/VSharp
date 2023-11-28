#nullable enable
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

namespace VSharp.CSharpUtils
{
    public static class ReflectionUtils
    {
        /// <summary>
        /// Checks that method is implemented in the given type. Used to filter "uninteresting" methods
        /// like default implementations of ToString, GetHashCode etc. However, it is possible that
        /// not all cases are checked with this condition. TODO: check that nothing else is needed
        /// </summary>
        private static bool DeclaredInType(this MethodBase method, Type type) => method.DeclaringType == type;

        public static IEnumerable<MethodBase> EnumerateExplorableMethods(this Type type)
        {
            const BindingFlags flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public;

            foreach (var m in type.GetMethods(flags).Where(m => m.GetMethodBody() != null && m.DeclaredInType(type)))
            {
                yield return m;
            }

            foreach (var ctor in type.GetConstructors(flags).Where(ctor => ctor.GetMethodBody() != null))
            {
                yield return ctor;
            }
        }

        public static IEnumerable<Type> EnumerateExplorableTypes(this Assembly assembly)
        {
            var types = new List<Type>();
            try
            {
                types.AddRange(assembly.GetTypes());
            }
            catch (ReflectionTypeLoadException e)
            {
                types.AddRange(e.Types.Where(type => type is not null)!);
            }
            return types.Where(IsPublic);
        }

        private static bool IsPublic(Type t)
        {
            Debug.Assert(t != null && t != t.DeclaringType);
            return
                t.IsPublic
                || t is { IsNestedPublic: true, DeclaringType: not null } && IsPublic(t.DeclaringType);
        }

        public static IEnumerable<Type> GetExportedTypesChecked(this Assembly assembly)
        {
            return assembly.GetTypesChecked().Where(IsPublic);
        }

        public static IEnumerable<Type> GetTypesChecked(this Assembly assembly)
        {
            try
            {
                return assembly.GetTypes();
            }
            catch (ReflectionTypeLoadException e)
            {
                // TODO: pass logger here and show warnings
                return e.Types.Where(t => t is not null)!;
            }
        }

        public static MethodBase? ResolveMethod(this Assembly assembly, int methodToken)
        {
            foreach (var module in assembly.GetModules())
            {
                try
                {
                    var method = module.ResolveMethod(methodToken);
                    if (method != null) return method;
                }
                catch
                {
                    // ignored
                }
            }

            return null;
        }

        private static bool MethodIsSuitable(Type t, string methodName, int paramsCount, MethodBase m)
        {
            var nameIsSuitable =
                !m.IsAbstract && $"{t.FullName ?? t.Name}.{m.Name}".Contains(methodName) && m.DeclaredInType(t);
            if (paramsCount > 0)
                return nameIsSuitable && m.GetParameters().Length == paramsCount;
            return nameIsSuitable;
        }

        private static IEnumerable<MethodBase> FindMethodInType(Type t, string methodName, int paramsCount)
        {
            const BindingFlags flags =
                BindingFlags.IgnoreCase | BindingFlags.Static | BindingFlags.Instance
                | BindingFlags.NonPublic | BindingFlags.Public;

            var methods = t.GetConstructors(flags).Concat<MethodBase>(t.GetMethods(flags));
            return methods.Where(m => MethodIsSuitable(t, methodName, paramsCount, m));
        }

        public static MethodBase? ResolveMethod(this Assembly assembly, string methodName)
        {
            var name = methodName;
            var parametersCount = -1;
            var paramCountStart = methodName.IndexOf('[');
            if (paramCountStart > 0 &&
                int.TryParse(methodName[(paramCountStart + 1) .. methodName.IndexOf(']')], out parametersCount))
            {
                name = methodName[.. paramCountStart];
            }

            var methods =
                assembly
                    .GetTypes()
                    .SelectMany(t => FindMethodInType(t, name, parametersCount))
                    .ToArray();

            return methods.Length != 0 ? methods.MinBy(m => m.Name.Length) : null;
        }

        public static IEnumerable<Type> ResolveNamespace(this Assembly assembly, string namespaceName)
        {
            var types =
                assembly.EnumerateExplorableTypes()
                    .Where(t => t.Namespace?.StartsWith(namespaceName) == true);
            return types;
        }

        public static Type? ResolveType(this Assembly assembly, string name)
        {
            var specificClass =
                assembly.GetType(name) ??
                assembly.GetTypes()
                    .Where(t => (t.FullName ?? t.Name).Contains(name))
                    .MinBy(t => t.FullName?.Length ?? t.Name.Length);

            return specificClass;
        }
    }
}
