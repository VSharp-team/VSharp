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
        private static int _maxBound;
        private static INewSearcher[] _searchers;//= new INewSearcher[]
        private static Dictionary<MethodBase, PobsStatistics> _pobsStatistics;

        public static void SetUpSVM(SVM svm, int maxBound, INewSearcher[] searchers)
        {
            _svm = svm;
            _maxBound = maxBound;
            _searchers = searchers;
            _pobsStatistics = new Dictionary<MethodBase, PobsStatistics>();
        }

        public static void PrintStats()
        {
            var searchers = new INewSearcher[] {_searchers[1], _searchers[2], _searchers[3]};
            foreach (var m in _pobsStatistics.Keys)
            {
                foreach (var s in searchers)
                {
                    _pobsStatistics[m].PrintStats(s);
                }
                Console.WriteLine();
            }
        }

        public virtual TestCommand Wrap(TestCommand command)
        {
            return new TestSvmCommand(command);
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            public TestSvmCommand(TestCommand innerCommand) : base(innerCommand) {}

            private TestResult Explore(TestExecutionContext context)
            {
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                var idealValue = new IdealValuesHandler(methodInfo);
                var gotValue = _svm.ExploreOne(methodInfo).Trim();

                if (string.Equals(idealValue.ExpectedValue, gotValue))
                {
                    context.CurrentResult.SetResult(ResultState.Success);
                }
                else
                {
                    idealValue.CreateTemporaryIdealFile(gotValue);
                    var diff = idealValue.DiffOfGotAndIdealValues(gotValue);
                    context.CurrentResult.SetResult(ResultState.Failure, diff);
                }

                return context.CurrentResult;
            }

            private bool AnswerPobs(MethodInfo entryMethod, INewSearcher searcher)
            {
                var exitOffset = entryMethod.GetMethodBody().GetILAsByteArray().Length - 1;
                var loc = new Core.codeLocation(exitOffset, entryMethod);

                Core.codeLocation[] codeLocations = { loc };

                var stopWatch = new Stopwatch();
                stopWatch.Start();
                var svm = new SVM(new PobsInterpreter(searcher));
                var dict = svm.AnswerPobs(entryMethod, codeLocations);
                stopWatch.Stop();

                Console.WriteLine($"searcher = {searcher.GetType()}, ElapsedTime = {stopWatch.Elapsed}");

                bool res = PobsSetup.DesiredStatus.Witnessed.ToString() == dict[loc];
                if (!res)
                {
                    _pobsStatistics[entryMethod].AddWrongAnswer(searcher, loc, stopWatch.Elapsed);
                    // Console.WriteLine($"Checking location, offset = {exitOffset.ToString(");X4")}, method = {entryMethod});
                    var exit = exitOffset.ToString("X4");
                    Console.WriteLine($"searсher {searcher.GetType()} could not reach {exit} of method = {entryMethod}");
                    // Console.WriteLine($"ElapsedTime = {stopWatch.Elapsed}");
                }
                else
                {
                    _pobsStatistics[entryMethod].AddCorrectAnswer(searcher, loc, stopWatch.Elapsed);
                }
                _pobsStatistics[entryMethod].AddTime(searcher, entryMethod, stopWatch.Elapsed);

                return res;
                // return context.CurrentResult;
            }

            public override TestResult Execute(TestExecutionContext context)
            {
                // return Explore(context);
                var entryMethod = innerCommand.Test.Method.MethodInfo;
                _pobsStatistics.Add(entryMethod, new PobsStatistics(_searchers));
                bool res = true;

                foreach (var s in _searchers)
                {
                    res &= AnswerPobs(entryMethod, s);
                }
                context.CurrentResult.SetResult(res ? ResultState.Success : ResultState.Failure);
                return context.CurrentResult;
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
