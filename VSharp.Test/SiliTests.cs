using System;
using System.IO;
using System.Reflection;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Threading;
using NUnit.Framework;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using NUnit.Framework.Internal.Commands;
using VSharp.Interpreter.IL;
using VSharp.Core;
using VSharp.Test.Tests;

namespace VSharp.Test
{
    public class TestSvmFixtureAttribute : NUnitAttribute, IFixtureBuilder
    {
        public IEnumerable<TestSuite> BuildFrom(ITypeInfo typeInfo)
        {
            var typ = new Utils.DummyTypeInfo(typeInfo.Type);
            yield return new NUnitTestFixtureBuilder().BuildFrom(typ, new Utils.DummyFilter());
        }
    }

    public class IdealValuesHandler
    {
        private const string MethodSeparator = "METHOD: ";
        private const string ResultSeparator = "RESULT: ";
        private const string GoldsDirectoryName = "Golds";
        private const string IdealTestFileExtension = ".gold";
        private const string IdealTemporaryFileExtension = ".tmp";

        private string _idealValuePath;
        public string ExpectedValue;
        private string _methodName;

        public IdealValuesHandler(MethodInfo methodInfo, [CallerFilePath] string currentFilePath = "")
        {
            var currentFolder = Path.GetDirectoryName(currentFilePath);
            _idealValuePath = GetIdealValuePath(currentFolder, methodInfo);
            ExpectedValue = ReadIdealValue(_idealValuePath);
            _methodName = MethodInfoToString(methodInfo);
        }

        private static int MethodHash(MethodInfo methodInfo)
        {
            return methodInfo
                .GetParameters()
                .Concat(new []{methodInfo.ReturnParameter})
                .Select(p => p.ParameterType.ToString())
                .Aggregate(0, (current, t) =>
                    t.Aggregate(unchecked(current + t.Length), (h, c) => unchecked(h * 314159 + c)));
        }

        private static string GetIdealValuePath(string currentFolder, MethodInfo methodInfo)
        {
            var typeName = methodInfo?.DeclaringType?.FullName?.Split('.');
            if (typeName == null)
                return null;
            var methodNameWin = $"{methodInfo.Name}.{MethodHash(methodInfo)}{PlatformID.Win32NT}{IdealTestFileExtension}";
            var methodNameUnix = $"{methodInfo.Name}.{MethodHash(methodInfo)}{PlatformID.Unix}{IdealTestFileExtension}";
            var typePath = Path.Combine(currentFolder, GoldsDirectoryName, Path.Combine(typeName));
            var idealValuePathWin = Path.Combine(typePath, methodNameWin);
            var idealValuePathUnix = Path.Combine(typePath, methodNameUnix);
            if (File.Exists(idealValuePathWin) || File.Exists(idealValuePathUnix))
                if (Environment.OSVersion.Platform == PlatformID.Unix)
                    return idealValuePathUnix;
                else return idealValuePathWin;
            var methodName = $"{methodInfo.Name}.{MethodHash(methodInfo)}{IdealTestFileExtension}";
            return Path.Combine(typePath, methodName);
        }

        private static string ReadIdealValue(string idealValuePath)
        {
            if (!File.Exists(idealValuePath))
                return null;
            var idealValueContents = File.ReadAllText(idealValuePath);
            if (string.IsNullOrEmpty(idealValueContents))
                return null;

            var idealValue = idealValueContents.Split(new []{ResultSeparator}, 2, StringSplitOptions.None);
            return idealValue[1].Trim();
        }

        private static string MethodInfoToString(MethodInfo methodInfo)
        {
            var parameters = string.Join(", ", methodInfo.GetParameters().Select(p => p.ParameterType.ToString()));
            return $"{methodInfo.ReturnType} {methodInfo.DeclaringType}.{methodInfo.Name}({parameters})";
        }

        public void CreateTemporaryIdealFile(string gotValue)
        {
            var text = $"{MethodSeparator}{_methodName}\n{ResultSeparator}{gotValue}\n";
            var idealValueRoot = Path.GetDirectoryName(_idealValuePath);
            Debug.Assert(idealValueRoot != null);
            Directory.CreateDirectory(idealValueRoot);
            File.WriteAllText(_idealValuePath + IdealTemporaryFileExtension, text);
        }

        public string DiffOfGotAndIdealValues(string gotValue)
        {
            return ExpectedValue == null
                ? $"There is no gold file for {_methodName}!\nGOT: {gotValue}"
                : $"{MethodSeparator}{_methodName}\nEXPECTED: {ExpectedValue}\nGOT:      {gotValue}";
        }
    }

    public class TestSvmAttribute : NUnitAttribute, IWrapTestMethod, ISimpleTestBuilder
    {
        protected static SVM _svm;
        private static uint _maxBound;
        private static IBidirectionalSearcher[] _searchers;//= new INewSearcher[]
        private static Dictionary<IBidirectionalSearcher, TimeSpan> _globalTime;//= new INewSearcher[]
        private static Dictionary<MethodBase, PobsStatistics> _pobsStatistics;
        private List<int> _unreachableLocations = new List<int>();

        public static void SetUpSVM(SVM svm, uint maxBound, IBidirectionalSearcher[] searchers)
        {
            _svm = svm;
            _maxBound = maxBound;
            _searchers = searchers;
            _pobsStatistics = new Dictionary<MethodBase, PobsStatistics>();
            _globalTime = new Dictionary<IBidirectionalSearcher, TimeSpan>();
            foreach (var s in _searchers)
            {
                _globalTime.Add(s, TimeSpan.Zero);
            }
        }

        public TestSvmAttribute() { }

        public TestSvmAttribute(int excludedLoc)
        {
            _unreachableLocations.Add(excludedLoc);
        }

        public TestSvmAttribute(int excludedLoc1, int excludedLoc2)
        {
            _unreachableLocations.Add(excludedLoc1);
            _unreachableLocations.Add(excludedLoc2);
        }

        public TestSvmAttribute(int[] excludedLocations)
        {
            for (int i = 0; i < excludedLocations.Length; ++i)
                _unreachableLocations.Add(excludedLocations[i]);
        }

        public static void PrintStats()
        {
            var searchers = _searchers;
            foreach (var m in _pobsStatistics.Keys)
            {
                foreach (var s in searchers)
                {
                    _pobsStatistics[m].PrintStats(m, s);
                }
                Console.WriteLine();
            }

            foreach (var s in searchers)
            {
                Console.WriteLine($"{s.GetType()} executed {_globalTime[s]}");
            }

            foreach (var s in searchers)
            {
                // Console.WriteLine($"{s.GetType()} executed {s.TotalNumber}");
            }

            PobsStatistics.PrintAccuracy();
        }

        public virtual TestCommand Wrap(TestCommand command)
        {
            return new TestSvmCommand(command, _unreachableLocations);
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            private List<int> _unreachableLocations;
            public TestSvmCommand(TestCommand innerCommand, List<int> unreachableLocations) : base(innerCommand)
            {
                _unreachableLocations = unreachableLocations;
            }

            private TestExecutionContext HandleResult(MethodInfo methodInfo, TestExecutionContext context, string testResult)
            {
                var idealValue = new IdealValuesHandler(methodInfo);

                if (string.Equals(idealValue.ExpectedValue, testResult))
                {
                    context.CurrentResult.SetResult(ResultState.Success);
                }
                else
                {
                    idealValue.CreateTemporaryIdealFile(testResult);
                    var diff = idealValue.DiffOfGotAndIdealValues(testResult);
                    context.CurrentResult.SetResult(ResultState.Failure, diff);
                }

                return context;
            }

            private TestResult Explore(TestExecutionContext context)
            {
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                try
                {
                    var gotValue = _svm.ExploreOne(methodInfo).Trim();
                    context = HandleResult(methodInfo, context, gotValue);
                }
                catch (Exception e)
                {
                    // TODO: add more info
                    context.CurrentResult.SetResult(ResultState.Inconclusive, e.Message);
                }

                return context.CurrentResult;
            }

            // private (bool, string) AnswerPobs(MethodInfo entryMethod, IBidirectionalSearcher searcher
            //     , Dictionary<codeLocation, PobsSetup.DesiredStatus> expectedResults, uint maxBound)
            // {
            //     var stopWatch = new Stopwatch();
            //     stopWatch.Start();
            //     var svm = new SVM(new PobsInterpreter(searcher));
            //     var list = expectedResults.Keys.ToList();
            //     var (dict, testResult) = svm.AnswerPobs(entryMethod, list);
            //     stopWatch.Stop();
            //
            //     Console.WriteLine($"searcher = {searcher.GetType()}, ElapsedTime = {stopWatch.Elapsed}");
            //
            //     _pobsStatistics[entryMethod].AddTime(searcher, entryMethod, stopWatch.Elapsed);
            //     _globalTime[searcher] = _globalTime[searcher] + stopWatch.Elapsed;
            //
            //     bool res = true;
            //     foreach (var loc in list)
            //     {
            //         bool matches = expectedResults[loc].ToString() == dict[loc];
            //         if (!matches)
            //         {
            //             _pobsStatistics[entryMethod].AddWrongAnswer(searcher, loc, stopWatch.Elapsed);
            //             // Console.WriteLine($"Checking location, offset = {exitOffset.ToString(");X4")}, method = {entryMethod});
            //             // var exit = exitOffset.ToString("X4");
            //             // Console.WriteLine($"searсher {searcher.GetType()} could not reach {exit} of method = {entryMethod}");
            //             // Console.WriteLine($"ElapsedTime = {stopWatch.Elapsed}");
            //         }
            //         else
            //         {
            //             _pobsStatistics[entryMethod].AddCorrectAnswer(searcher, loc, stopWatch.Elapsed);
            //         }
            //
            //         res &= matches;
            //     }
            //
            //     return (res, testResult);
            //     // return context.CurrentResult;
            // }

            public override TestResult Execute(TestExecutionContext context)
            {
                return Explore(context);
                // var entryMethod = innerCommand.Test.Method.MethodInfo;
                // var cfg = CFG.findCfg(entryMethod);
                // var codeLocations = new Dictionary<codeLocation, PobsSetup.DesiredStatus>();
                // PobsStatistics.IncrementTestsNumber();
                // foreach (var offset in cfg.sortedOffsets)
                // {
                //     if (offset != 0)
                //     {
                //         var loc = new codeLocation(offset, entryMethod);
                //         if (_unreachableLocations.Contains(offset))
                //         {
                //             // we can not establish ``unreachable'' yet
                //             codeLocations.Add(loc, PobsSetup.DesiredStatus.Unknown);
                //         }
                //         else
                //         {
                //             codeLocations.Add(loc, PobsSetup.DesiredStatus.Witnessed);
                //         }
                //     }
                // }
                //
                // // var exitOffset = entryMethod.GetMethodBody().GetILAsByteArray().Length - 1;
                // _pobsStatistics.Add(entryMethod, new PobsStatistics(_searchers));
                // // bool res = true;
                //
                // try
                // {
                //     Debug.Assert(_searchers.Length == 1);
                //     var searcher = _searchers[0];
                //     var (res, testResult) = AnswerPobs(entryMethod, searcher, codeLocations, _maxBound);
                //     // foreach (var s in _searchers)
                //     // {
                //     //     res &= AnswerPobs(entryMethod, s, codeLocations, _maxBound);
                //     // }
                //     // PrintStats();
                //
                //     if (res)
                //         context = HandleResult(entryMethod, context, testResult);
                //     else context.CurrentResult.SetResult(ResultState.Failure);
                //     return context.CurrentResult;
                // }
                // catch (Exception e)
                // {
                //     // TODO: add more info
                //     context.CurrentResult.SetResult(ResultState.Inconclusive, e.Message);
                //     return context.CurrentResult;
                // }
            }
        }

        private static NUnitTestCaseBuilder _builder = new NUnitTestCaseBuilder();

        public TestMethod BuildFrom(IMethodInfo method, NUnit.Framework.Internal.Test suite)
        {
            var defaultParameters = method.GetParameters().Select(
                parameter => TypeUtils.defaultOf(parameter.ParameterType)).ToArray();
            var parameters = new TestCaseParameters(defaultParameters);
            if (method.ReturnType.Type != typeof(void))
                parameters.ExpectedResult = null;
            return _builder.BuildTestMethod(method, suite, parameters);
        }
    }
}
