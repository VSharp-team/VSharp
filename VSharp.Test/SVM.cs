using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using VSharp.Core;
using NUnit.Framework;
using System.Text.RegularExpressions;
using Microsoft.FSharp.Core;
using CodeLocationSummaries = System.Collections.Generic.IEnumerable<VSharp.Core.codeLocationSummary>;


namespace VSharp.Test
{
    public class SVM
    {
        private readonly ExplorerBase _explorer;
        private readonly Statistics _statistics = new Statistics();

        public SVM(ExplorerBase explorer)
        {
            _explorer = explorer;
        }

        private CodeLocationSummaries PrepareAndInvokeWithoutStatistics(IDictionary<MethodInfo, CodeLocationSummaries> dict,
            MethodInfo m,
            Func<IMethodIdentifier, FSharpFunc<CodeLocationSummaries, CodeLocationSummaries>, CodeLocationSummaries> invoke)
        {
            _statistics.SetupBeforeMethod(m);
            IMethodIdentifier methodIdentifier = _explorer.MakeMethodIdentifier(m);
            if (methodIdentifier == null)
            {
                var format =
                    new PrintfFormat<string, Unit, string, Unit>(
                        $"WARNING: metadata method for {m.Name} not found!");
                Logger.printLog(Logger.Warning, format);
                return null;
            }

            dict?.Add(m, null);
            var id = FSharpFunc<CodeLocationSummaries, CodeLocationSummaries>.FromConverter(x => x);
            var summary = invoke(methodIdentifier, id);
            _statistics.AddSucceededMethod(m);
            if (dict != null)
            {
                dict[m] = summary;
            }

            return summary;
        }

        private CodeLocationSummaries PrepareAndInvoke(IDictionary<MethodInfo, CodeLocationSummaries> dict, MethodInfo m,
            Func<IMethodIdentifier, FSharpFunc<CodeLocationSummaries, CodeLocationSummaries>, CodeLocationSummaries> invoke)
        {
            // try
            // {
                return PrepareAndInvokeWithoutStatistics(dict, m, invoke);
            // }
            // catch (Exception e)
            // {
            //     _statistics.AddException(e, m);
            // }

            return null;
        }

        private void InterpretEntryPoint(IDictionary<MethodInfo, CodeLocationSummaries> dictionary, MethodInfo m)
        {
            Assert.IsTrue(m.IsStatic);
            PrepareAndInvoke(dictionary, m, _explorer.InterpretEntryPoint);
        }

        private void Explore(IDictionary<MethodInfo, CodeLocationSummaries> dictionary, MethodInfo m)
        {
            if (m.GetMethodBody() != null)
                PrepareAndInvoke(dictionary, m, _explorer.Explore);
        }

        private void ExploreType(List<string> ignoreList, MethodInfo ep,
            IDictionary<MethodInfo, CodeLocationSummaries> dictionary, Type t)
        {
            BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public |
                                        BindingFlags.DeclaredOnly;

            if (ignoreList?.Where(kw => !t.AssemblyQualifiedName.Contains(kw)).Count() == ignoreList?.Count &&
                t.IsPublic)
            {
                foreach (var m in t.GetMethods(bindingFlags))
                {
                    // FOR DEBUGGING SPECIFIED METHOD
                    // if (m != ep && !m.IsAbstract)
                    if (m != ep && !m.IsAbstract && m.Name != "op_Division")
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

        private static string SummaryToString(codeLocationSummary summary)
        {
            if (summary == null)
                return "Summary is empty";
            return $"{summary.result}\nMEMORY DUMP:\n{ReplaceLambdaLines(API.Memory.Dump(summary.state))}";
        }

        private static string ResultToString(CodeLocationSummaries summary)
        {
            int count = 0;
            if (summary == null || (count = summary.Count()) == 0)
                return "No states were obtained!";
            var suffix = count > 1 ? "s" : "";
            return $"Totally {count} state{suffix}:\n{String.Join("\n", summary.Select(SummaryToString))}";
        }

        public string ExploreOne(MethodInfo m)
        {
            var summary = PrepareAndInvoke(null, m, _explorer.Explore);
            return ResultToString(summary);
        }

        public string ExploreOneWithoutStatistics(MethodInfo m)
        {
            var summary = PrepareAndInvokeWithoutStatistics(null, m, _explorer.Explore);
            return ResultToString(summary);
        }

        public IDictionary<MethodInfo, string> Run(Assembly assembly, List<string> ignoredList)
        {
            IDictionary<MethodInfo, CodeLocationSummaries> dictionary = new Dictionary<MethodInfo, CodeLocationSummaries>();
            var ep = assembly.EntryPoint;

            foreach (var t in assembly.GetTypes())
            {
                ExploreType(ignoredList, ep, dictionary, t);
            }

            if (ep != null)
            {
                InterpretEntryPoint(dictionary, ep);
            }

            _statistics.PrintExceptionsStats();

            return dictionary.ToDictionary(kvp => kvp.Key, kvp => ResultToString(kvp.Value));
        }
    }
}
