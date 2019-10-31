using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Reflection;
using VSharp.Core;
using NUnit.Framework;
using System.Text.RegularExpressions;
using Microsoft.FSharp.Core;
using VSharp.Test.Tests;


namespace VSharp.Test
{
    public class SVM
    {
        private ExplorerBase _explorer;

        public SVM(ExplorerBase explorer)
        {
            _explorer = explorer;
            API.Configure(explorer);
        }

        private codeLocationSummary PrepareAndInvoke(IDictionary<MethodInfo, codeLocationSummary> dict, MethodInfo m, Func<IMethodIdentifier, FSharpFunc<codeLocationSummary, codeLocationSummary>, codeLocationSummary> invoke)
        {
            IMethodIdentifier methodIdentifier = _explorer.MakeMethodIdentifier(m);
            if (methodIdentifier == null)
            {
                var format = new PrintfFormat<string, Unit, string, Unit>($"WARNING: metadata method for {m.Name} not found!");
                Logger.printLog(Logger.Warning, format);
                return null;
            }
            dict?.Add(m, null);
            var id = FSharpFunc<codeLocationSummary,codeLocationSummary>.FromConverter(x => x);
            var summary = invoke(methodIdentifier, id);
            if (dict != null)
            {
                dict[m] = summary;
            }
            return summary;
        }

        private void InterpretEntryPoint(IDictionary<MethodInfo, codeLocationSummary> dictionary, MethodInfo m)
        {
            Assert.IsTrue(m.IsStatic);
            PrepareAndInvoke(dictionary, m, _explorer.InterpretEntryPoint);
        }

        private void Explore(IDictionary<MethodInfo, codeLocationSummary> dictionary, MethodInfo m)
        {
            PrepareAndInvoke(dictionary, m, _explorer.Explore);
        }

        private void ExploreType(List<string> ignoreList, MethodInfo ep, IDictionary<MethodInfo, codeLocationSummary> dictionary, Type t)
        {
            BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public | BindingFlags.DeclaredOnly;

            if (ignoreList?.Where(kw => !t.AssemblyQualifiedName.Contains(kw)).Count() == ignoreList?.Count && t.IsPublic)
            {
                foreach (var m in t.GetMethods(bindingFlags))
                {
                    if (m != ep && !m.IsAbstract)
                    {
                        Debug.Print(@"Called interpreter for method {0}", m.Name);
                        Explore(dictionary, m);
                    }
                }
            }
        }

        private static string ReplaceLambdaLines(string str)
        {
            return Regex.Replace(str, @"@\d+(\+|\-)\d*\[Microsoft.FSharp.Core.Unit\]", "");
        }

        private static string ResultToString(codeLocationSummary summary)
        {
            return $"{summary.result}\nHEAP:\n{ReplaceLambdaLines(API.Memory.Dump(summary.state))}";
        }

        public string ExploreOne(MethodInfo m)
        {
            var summary = PrepareAndInvoke(null, m, _explorer.Explore);
            return ResultToString(summary);
        }

        public void ConfigureSolver(ISolver solver)
        {
            API.ConfigureSolver(solver);
        }

        public IDictionary<MethodInfo, string> Run(Assembly assembly, List<string> ignoredList)
        {
            IDictionary<MethodInfo, codeLocationSummary> dictionary = new Dictionary<MethodInfo, codeLocationSummary>();
            var ep = assembly.EntryPoint;

            foreach (var t in assembly.GetTypes())
            {
                ExploreType(ignoredList, ep, dictionary, t);
            }

            if (ep != null)
            {
                InterpretEntryPoint(dictionary, ep);
            }

            return dictionary.ToDictionary(kvp => kvp.Key, kvp => ResultToString(kvp.Value));
        }

    }
}
