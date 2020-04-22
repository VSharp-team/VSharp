using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Reflection;
using VSharp.Core;
using NUnit.Framework;
using System.Text.RegularExpressions;
using System.Timers;
using Microsoft.FSharp.Core;
using VSharp.Interpreter.IL;


namespace VSharp.Test
{
    public class SVM
    {
        private ExplorerBase _explorer;
        private int _methodsNumber = 0;
        private MethodBase _currentlyExplored;
        private Dictionary<Type, List<Exception>> _exceptions = new Dictionary<Type, List<Exception>>();

        private Dictionary<string, List<Exception>> _notImplementedExceptions =
            new Dictionary<string, List<Exception>>();

        public SVM(ExplorerBase explorer)
        {
            _explorer = explorer;
            API.Configure(explorer);
        }

        private void AddException(Exception e)
        {
            Type t = e.GetType();
            if (!_exceptions.ContainsKey(t))
            {
                _exceptions[t] = new List<Exception>();
            }

            _exceptions[t].Add(e);
        }

        private void AddNotImplementedException(NotImplementedException e)
        {
            string stackTrace = e.StackTrace;
            if (!_notImplementedExceptions.ContainsKey(stackTrace))
            {
                _notImplementedExceptions[stackTrace] = new List<Exception>();
            }
            _notImplementedExceptions[stackTrace].Add(e);
        }

        private codeLocationSummary PrepareAndInvoke(IDictionary<MethodInfo, codeLocationSummary> dict, MethodInfo m,
            Func<IMethodIdentifier, FSharpFunc<codeLocationSummary, codeLocationSummary>, codeLocationSummary> invoke)
        {
            // throw new Exception("Create set with exceptions and export them to DOC");
            // throw new Exception("все что __notImplemented__ должно быть __notImplemented__, а не захакано");
            try
            {
                // if (_methodsNumber > 1000)
                // {
                //     return null;
                // }
                //
                // if (_methodsNumber == 1000)
                // {
                //     Console.WriteLine("Got it");
                // }
                _methodsNumber++;
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
                var id = FSharpFunc<codeLocationSummary, codeLocationSummary>.FromConverter(x => x);
                var summary = invoke(methodIdentifier, id);
                Console.WriteLine("DONE: {0}", m);
                if (dict != null)
                {
                    dict[m] = summary;
                }

                return summary;
            }
            catch (NotImplementedException e)
            {
                Console.WriteLine("NotImplementedException in {0} occured: {1}", m, e.StackTrace);
                AddException(e);
                AddNotImplementedException(e);
            }
            catch (Exception e)
            {
                Console.WriteLine("Unhandled Exception in {0} occured: {1}", m, e.Message);
                AddException(e);
            }

            return null;
        }

        private void InterpretEntryPoint(IDictionary<MethodInfo, codeLocationSummary> dictionary, MethodInfo m)
        {
            Assert.IsTrue(m.IsStatic);
            PrepareAndInvoke(dictionary, m, _explorer.InterpretEntryPoint);
        }

        private void Explore(IDictionary<MethodInfo, codeLocationSummary> dictionary, MethodInfo m)
        {
            if (m.GetMethodBody() != null)
                PrepareAndInvoke(dictionary, m, _explorer.Explore);
        }

        private void ExploreType(List<string> ignoreList, MethodInfo ep,
            IDictionary<MethodInfo, codeLocationSummary> dictionary, Type t)
        {
            BindingFlags bindingFlags = BindingFlags.Instance | BindingFlags.Static | BindingFlags.Public |
                                        BindingFlags.DeclaredOnly;

            if (ignoreList?.Where(kw => !t.AssemblyQualifiedName.Contains(kw)).Count() == ignoreList?.Count &&
                t.IsPublic)
            {
                foreach (var m in t.GetMethods(bindingFlags))
                {
                    if (m != ep && !m.IsAbstract)
                    // if (m != ep && !m.IsAbstract && m.Name == "get_MonitoringTotalProcessorTime")
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
            if (summary == null)
                return "summary is null";
            return $"{summary.result}\nHEAP:\n{ReplaceLambdaLines(API.Memory.Dump(summary.state))}";
        }

        public string ExploreOne(MethodInfo m)
        {
            var summary = PrepareAndInvoke(null, m, _explorer.Explore);
            return ResultToString(summary);
        }

        public void ConfigureSolver(ISolver solver)
        {
            // API.ConfigureSolver(solver);
        }

        private void PrintExceptionsStats()
        {
            Type notImplType = typeof(NotImplementedException);
            // var format1 =
            //     new PrintfFormat<string, Unit, string, Unit>(
            //         $"INFO: exceptions types number: {_exceptions.Keys.Count.ToString()}");
            // var format2 =
            //     new PrintfFormat<string, Unit, string, Unit>(
            //         $"INFO: notImplemented number: {_exceptions[notImplType].Count.ToString()}");
            // var format3 =
            //     new PrintfFormat<string, Unit, string, Unit>(
            //         $"INFO: methods number: {_methodsNumber.ToString()}");
            //
            // Logger.printLog(Logger.Trace, format1);
            // Logger.printLog(Logger.Trace, format2);
            // Logger.printLog(Logger.Trace, format3);
            Console.WriteLine($"INFO: exceptions types number: {_exceptions.Keys.Count.ToString()}");
            Console.WriteLine($"INFO: methods number: {_methodsNumber.ToString()}");
            if (_exceptions.ContainsKey(notImplType))
            {
                Console.WriteLine($"INFO: notImplemented number: {_exceptions[notImplType].Count.ToString()}");
                Console.WriteLine($"INFO: notImplemented Types: {_notImplementedExceptions.Keys.Count.ToString()}");
                foreach (var message in _notImplementedExceptions.Keys)
                {
                    Console.WriteLine(message);
                    Console.WriteLine("");
                }
            }
            else
            {
                Console.WriteLine("INFO: NO NOT_IMPL_EXCEPTIONS");
            }
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

            PrintExceptionsStats();

            return dictionary.ToDictionary(kvp => kvp.Key, kvp => ResultToString(kvp.Value));
        }
    }
}