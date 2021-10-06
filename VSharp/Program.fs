// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    let message = from "F#" // Call the function
    printfn "Hello world %s" message
    0 // return an integer exit code

//        private void InterpretEntryPoint(MethodInfo m)
//        {
//            Assert.IsTrue(m.IsStatic);
//            PrepareAndInvoke(m, _explorer.InterpretEntryPoint);
//        }
//
//        private void ExploreType(List<string> ignoreList, MethodInfo ep, Type t)
//        {
//            BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public |
//                                        BindingFlags.DeclaredOnly;
//
//            if (ignoreList?.Where(kw => !t.AssemblyQualifiedName.Contains(kw)).Count() == ignoreList?.Count &&
//                t.IsPublic)
//            {
//                foreach (var m in t.GetMethods(bindingFlags))
//                {
//                    // FOR DEBUGGING SPECIFIED METHOD
//                    // if (m != ep && !m.IsAbstract)
//                    if (m != ep && !m.IsAbstract && m.Name != "op_Division")
//                    {
//                        Debug.Print(@"Called interpreter for method {0}", m.Name);
//                        Explore(m);
//                    }
//                }
//            }
//        }
//
//        public void Run(Assembly assembly, List<string> ignoredList)
//        {
//            var ep = assembly.EntryPoint;
//
//            foreach (var t in assembly.GetTypes())
//            {
//                ExploreType(ignoredList, ep, t);
//            }
//
//            if (ep != null)
//            {
//                InterpretEntryPoint(ep);
//            }
//
//            _statistics.PrintExceptionsStats();
//        }
