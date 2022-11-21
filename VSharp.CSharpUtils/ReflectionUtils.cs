using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using VSharp;

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
            return types.Where(t => t.IsPublic || t.IsNestedPublic);
        }
    }
}
