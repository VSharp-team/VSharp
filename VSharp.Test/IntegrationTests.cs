﻿using System;
using System.IO;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading;
using Microsoft.FSharp.Core;
using NUnit.Framework;
using NUnit.Framework.Interfaces;
using NUnit.Framework.Internal;
using NUnit.Framework.Internal.Builders;
using NUnit.Framework.Internal.Commands;
using VSharp.Interpreter.IL;
using VSharp.Solver;

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

    public class TestSvmAttribute : NUnitAttribute, IWrapTestMethod, ISimpleTestBuilder
    {
        private static SiliOptions _options = null;

        static TestSvmAttribute()
        {
            Trace.Listeners.Add(new Utils.DumpStackTraceListener());

            var ci = new CultureInfo("en-GB")
            {
                NumberFormat = {
                    PositiveInfinitySymbol = "Infinity",
                    NegativeInfinitySymbol = "-Infinity"
                }
            };
            Thread.CurrentThread.CurrentCulture = ci;

            Logger.ConfigureWriter(TestContext.Progress);
            // SVM.ConfigureSimplifier(new Z3Simplifier()); can be used to enable Z3-based simplification (not recommended)
        }

        // NOTE: expected coverage field in TestSVM attribute indicates dotCover needs to be run:
        //       if it wasn't mentioned -- dotCover will not be started
        //       if it was -- dotCover will be started
        private int? _expectedCoverage = null;
        private uint _recThresholdForTest = 0u;
        private bool _concolicMode = false;

        public TestSvmAttribute(
            int expectedCoverage = -1,
            uint recThresholdForTest = 0u,
            bool concolicMode = false)
        {
            if (expectedCoverage < 0)
                _expectedCoverage = null;
            else if (expectedCoverage > 100)
                _expectedCoverage = 100;
            else _expectedCoverage = expectedCoverage;
            _recThresholdForTest = recThresholdForTest;
            _concolicMode = concolicMode;
        }

        public virtual TestCommand Wrap(TestCommand command)
        {
            executionMode execMode;
            if (_concolicMode)
                execMode = executionMode.ConcolicMode;
            else
                execMode = executionMode.SymbolicMode;
            return new TestSvmCommand(command, _expectedCoverage, _recThresholdForTest, execMode);
        }

        private class TestSvmCommand : DelegatingTestCommand
        {
            private int? _expectedCoverage;
            private uint _recThresholdForTest;
            private executionMode _executionMode;
            public TestSvmCommand(
                TestCommand innerCommand,
                int? expectedCoverage,
                uint recThresholdForTest,
                executionMode execMode) : base(innerCommand)
            {
                _expectedCoverage = expectedCoverage;
                _recThresholdForTest = recThresholdForTest;
                _executionMode = execMode;
            }

            private TestResult Explore(TestExecutionContext context)
            {
                Core.API.ConfigureSolver(SolverPool.mkSolver());
                var methodInfo = innerCommand.Test.Method.MethodInfo;
                try
                {
                    _options = new SiliOptions(
                            explorationMode.NewTestCoverageMode(coverageZone.MethodZone, searchMode.GuidedMode),
                            _executionMode,
                            _recThresholdForTest,
                            new FSharpOption<IDictionary<Type, object>>(null)
                        );
                    SILI explorer = new SILI(_options);
                    UnitTests unitTests = new UnitTests(Directory.GetCurrentDirectory());

                    explorer.InterpretIsolated(methodInfo, unitTests.GenerateTest, unitTests.GenerateError, _ => { }, e => throw e);

                    if (unitTests.UnitTestsCount == 0 && unitTests.ErrorsCount == 0 && explorer.Statistics.IncompleteStates.Count == 0)
                    {
                        throw new Exception("No states were obtained! Most probably this is bug.");
                    }
                    explorer.Statistics.PrintStatistics(TestContext.Out);
                    TestContext.Out.WriteLine("Test results written to {0}", unitTests.TestDirectory.FullName);
                    unitTests.WriteReport(explorer.Statistics.PrintStatistics);
                    if (unitTests.UnitTestsCount != 0 || unitTests.ErrorsCount != 0)
                    {
                        TestContext.Out.WriteLine("Starting coverage tool...");
                        // NOTE: to disable coverage check TestResultsChecker's expected coverage should be null
                        //       to enable coverage check use _expectedCoverage
                        var testChecker = new TestResultsChecker(unitTests.TestDirectory,
                            Directory.GetCurrentDirectory(), _expectedCoverage);
                        if (testChecker.Check(methodInfo))
                            context.CurrentResult.SetResult(ResultState.Success);
                        else
                            context.CurrentResult.SetResult(ResultState.Failure, testChecker.ResultMessage);
                    }
                    else
                    {
                        context.CurrentResult.SetResult(ResultState.Success);
                    }
                }
                catch (Exception e)
                {
                    context.CurrentResult.SetResult(ResultState.Error, e.Message);
                }

                return context.CurrentResult;
            }

            public override TestResult Execute(TestExecutionContext context)
            {
                return Explore(context);
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
