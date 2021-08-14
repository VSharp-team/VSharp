using System;
using System.Reflection;
using System.Linq;

namespace VSharp.Runner
{
    class Program
    {
        static int Main(string[] args)
        {
            string assemblyName;
            string moduleName;
            uint umethodToken;
            if (args.Length != 3 || !UInt32.TryParse(args[2], out umethodToken))
            {
                Console.WriteLine("Usage: {0} <assembly> <moduleName> <methodToken>", AppDomain.CurrentDomain.FriendlyName);
                return 1;
            }
            try
            {
                assemblyName = args[0];
                moduleName = args[1];
                int methodToken = (unchecked((int) umethodToken));
                // Assembly assembly = System.Runtime.Loader.AssemblyLoadContext.Default.LoadFromAssemblyPath(args[0]);//Assembly.LoadFile(args[0]);
                Assembly assembly = null;
                try
                {
                    assembly = Assembly.Load(assemblyName);
                }
                catch (System.IO.IOException)
                {
                    assembly = Assembly.LoadFile(moduleName);
                }
                Module module = assembly.Modules.FirstOrDefault(m => m.FullyQualifiedName == moduleName);
                if (module == null)
                {
                    throw new InvalidOperationException("Could not resolve module!");
                }

                // TODO: support generic arguments?
                MethodBase method = module.ResolveMethod(methodToken);
                if (method == null)
                {
                    throw new InvalidOperationException("Could not resolve method!");
                }
                bool hasThis = method.CallingConvention.HasFlag(CallingConventions.HasThis);

                // TODO: support virtual methods by passing explicit declaring type?
                object thisObj = hasThis ? Activator.CreateInstance(method.DeclaringType) : null;
                object[] parameters = method.GetParameters().Select(t => Activator.CreateInstance(t.ParameterType)).ToArray();
                method.Invoke(thisObj, parameters);
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