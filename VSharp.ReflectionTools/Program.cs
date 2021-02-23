using System;
using System.Linq;
using System.Reflection;

namespace VSharp.ReflectionTools
{
    public static class Program
    {
        public static int Main(string[] args)
        {
            string assemblyName;
            string moduleName;
            uint ucontextToken;
            uint umemberRef;
            if (args.Length != 5 || args[1] != "--ref2def"
                                 || !UInt32.TryParse(args[2], out ucontextToken)
                                 || !UInt32.TryParse(args[4], out umemberRef))
            {
                Console.WriteLine("Usage: {0} <assembly> --ref2def contextToken moduleName memberRef", AppDomain.CurrentDomain.FriendlyName);
                return 1;
            }
            try
            {
                assemblyName = args[0];
                moduleName = args[3];
                int contextToken = (unchecked((int) ucontextToken));
                int memberRef = (unchecked((int) umemberRef));
                // Assembly assembly = System.Runtime.Loader.AssemblyLoadContext.Default.LoadFromAssemblyPath(args[0]);//Assembly.LoadFile(args[0]);
                Assembly assembly = null;
                try
                {
                    assembly = Assembly.Load(assemblyName);
                }
                catch (System.IO.FileNotFoundException)
                {
                    assembly = Assembly.LoadFile(moduleName);
                }
                Module module = assembly.Modules.FirstOrDefault(m => m.FullyQualifiedName == moduleName);
                if (module == null)
                {
                    throw new InvalidOperationException("Could not resolve module!");
                }

                MethodBase contextMethod = module.ResolveMethod(contextToken);
                if (contextMethod == null)
                {
                    throw new InvalidOperationException("Could not resolve context method!");
                }

                var methodGenerics = contextMethod is ConstructorInfo ? null : contextMethod.GetGenericArguments();
                MethodBase method = module.ResolveMethod(memberRef, contextMethod.DeclaringType != null ? contextMethod.DeclaringType.GetGenericArguments() : null, methodGenerics);
                if (method == null)
                {
                    throw new InvalidOperationException("Could not resolve method!");
                }

                if (method.DeclaringType != null)
                {
                    Console.WriteLine("{0} {1}", method.MetadataToken, method.DeclaringType.MetadataToken);
                }
                else
                {
                    Console.WriteLine("{0}", method.MetadataToken);
                }

            }
            catch (Exception e)
            {
                Console.WriteLine("Error: {0} {1}", e.Message, e.GetType().FullName);
                return 2;
            }

            return 0;
        }
    }
}