using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;

namespace VSharp.CSharpUtils
{
    public static class ReflectionUtils
    {
        public static IEnumerable<MethodBase> EnumerateExplorableMethods(this Type type)
        {
            var flags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.DeclaredOnly;

            foreach (var m in type.GetMethods(flags).Where(m => m.GetMethodBody() != null))
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
                foreach (var type in e.Types)
                {
                    if (type is not null)
                    {
                        types.Add(type);
                    }
                }
            }
            return types.Where(IsPublic);
        }

        private static bool IsPublic(Type t)
        {
            Debug.Assert(t != null && t != t.DeclaringType);
            return t.IsPublic || t.IsNestedPublic && IsPublic(t.DeclaringType);
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
                return e.Types.Where(t => t is not null);
            }
        }
    }
}
