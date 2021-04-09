using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using VSharp.Core;
using VSharp.Interpreter.IL;
using NUnit.Framework;
using System.Text.RegularExpressions;
using Microsoft.FSharp.Core;
using VSharp.CSharpUtils;
using CodeLocationSummaries = System.Collections.Generic.IEnumerable<VSharp.Interpreter.IL.codeLocationSummary>;
using VSharp.Solver;

namespace VSharp.Test
{
    public class TestCodeLocationSummaries
    {
        private InsufficientInformationException _exception;
        private CodeLocationSummaries _summaries;

        public InsufficientInformationException Exception => _exception;
        public CodeLocationSummaries Summaries => _summaries;

        private TestCodeLocationSummaries(InsufficientInformationException exception)
        {
            _exception = exception;
        }

        private TestCodeLocationSummaries(CodeLocationSummaries summaries)
        {
            _summaries = summaries;
        }

        public static TestCodeLocationSummaries WithInsufficientInformationException(InsufficientInformationException exception)
        {
            return new TestCodeLocationSummaries(exception);
        }

        public static TestCodeLocationSummaries WithSummaries(CodeLocationSummaries summaries)
        {
            return new TestCodeLocationSummaries(summaries);
        }
    }

    public class SVM
    {
        private readonly ExplorerBase _explorer;
        private readonly Statistics _statistics = new Statistics();

        public SVM(ExplorerBase explorer)
        {
            _explorer = explorer;
        }

        private TestCodeLocationSummaries PrepareAndInvokeWithoutStatistics(IDictionary<MethodInfo, TestCodeLocationSummaries> dict,
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
            TestCodeLocationSummaries summary = null;

            try
            {
                summary = TestCodeLocationSummaries.WithSummaries(invoke(methodIdentifier, id));
            }
            catch (InsufficientInformationException e)
            {
                summary = TestCodeLocationSummaries.WithInsufficientInformationException(e);
            }

            _statistics.AddSucceededMethod(m);
            if (dict != null)
            {
                dict[m] = summary;
            }

            return summary;
        }

        private TestCodeLocationSummaries PrepareAndInvoke(IDictionary<MethodInfo, TestCodeLocationSummaries> dict, MethodInfo m,
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

        private void InterpretEntryPoint(IDictionary<MethodInfo, TestCodeLocationSummaries> dictionary, MethodInfo m)
        {
            Assert.IsTrue(m.IsStatic);
            PrepareAndInvoke(dictionary, m, _explorer.InterpretEntryPoint);
        }

        private void Explore(IDictionary<MethodInfo, TestCodeLocationSummaries> dictionary, MethodInfo m)
        {
            if (m.GetMethodBody() != null)
                PrepareAndInvoke(dictionary, m, _explorer.Explore);
        }

        private void ExploreType(List<string> ignoreList, MethodInfo ep,
            IDictionary<MethodInfo, TestCodeLocationSummaries> dictionary, Type t)
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
            return $"{summary.Result}\nMEMORY DUMP:\n{ReplaceLambdaLines(API.Memory.Dump(summary.State))}";
        }

        private static string ResultToString(TestCodeLocationSummaries summary)
        {
            if (summary.Exception != null)
            {
                if (summary.Summaries != null)
                    return "Test summary contains both InsInfExc and ordinary Summaries";
                return $"Totally 1 state:\n{summary.Exception.Message}\n";
            }

            int count = 0;
            if ((count = summary.Summaries.Count()) == 0)
                return "No states were obtained!";
            var suffix = count > 1 ? "s" : "";
            var sortedResults = summary.Summaries.Select(SummaryToString).OrderBy(x => x.GetDeterministicHashCode());
            return $"Totally {count} state{suffix}:\n{String.Join("\n", sortedResults)}";
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

        public void ConfigureSolver()
        {
            API.ConfigureSolver(SolverPool.mkSolver());
        }

        public IDictionary<MethodInfo, string> Run(Assembly assembly, List<string> ignoredList)
        {
            IDictionary<MethodInfo, TestCodeLocationSummaries> dictionary = new Dictionary<MethodInfo, TestCodeLocationSummaries>();
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
