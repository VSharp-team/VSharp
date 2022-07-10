using FsCheck;
using FsCheck.Xunit;
using InstantCredit.Shared.Models.Enums;
using LoanExam;
using LoanExam.ActionPointsHandler;
using LoanExam.Models;
using Microsoft.Extensions.Logging;
using VSharp;
using Employment = InstantCredit.Shared.Models.Enums.Employment;

namespace LoanExamTests;


public class Logger : ILogger
{
    public string A { get; }

    public Logger(string a)
    {
        A = a;
    }
    
    public IDisposable BeginScope<TState>(TState state)
    {
        throw new NotImplementedException();
    }

    public bool IsEnabled(LogLevel logLevel)
    {
        throw new NotImplementedException();
    }

    public void Log<TState>(LogLevel logLevel, EventId eventId, TState state, Exception? exception, Func<TState, Exception?, string> formatter)
    {
        throw new NotImplementedException();
    }
}

public static class Arbitraries
{
    public static Arbitrary<int> LetterGenerator()
    {
        return Gen.Elements(new[] {-1, 0, 1, 20, 21, 25, 27, 28, 29, 30, 31, 58, 59, 60, 61, 62, 65, 70, 71, 72, 75}).ToArbitrary();
    }
    
    public static Arbitrary<decimal> LetterGenerator2()
    {
        return Gen.Elements(new decimal[]
        {
            0, 1, 1_000_000, 1_000_001, 1_500_000, 5_000_000, 5_000_001, 6_000_000,
            9_999_999, 10_000_000//, 10_000_001
        }).ToArbitrary();
    }
}


public class UnitTest1
{
    [Fact]
    public void CoverAndRun_CreditCalculationService_UnreachableBranchHitIsNotThrown()
    {
        TestGenerator.CoverAndRun(typeof(CreditCalculationService));
    }

    [Fact]
    public void QuickCheck_CoversQuiteALot()
    {
        var s = new CreditCalculationService();

        var list = new List<Request>();
        Prop.ForAll<Request>(r =>
        {
            s.Build(r);
            list.Add(r);
        }).QuickCheck();
        
        Assert.True(list.Count > 0);
    }
    
    [Property(Arbitrary = new[] { typeof(Arbitraries) }, Verbose = true, QuietOnSuccess = false, MaxTest = 100000)]
    public bool QuickCheck_CoverEvenMore(Request r)
    {
        var s = new CreditCalculationService();
        s.Build(r);
        return true;
    }
}