using LoanExam.ActionPointsHandler;
using Microsoft.Extensions.Logging;
using VSharp;

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

public class UnitTest1
{
    [Fact]
    public void Test1()
    {
        var map = new Dictionary<Type, object>()
        {
            { typeof(ILogger), new Logger("AAA") }
        };
        
        TestGenerator.CoverAndRun<EmploymentHandler>(map);
    }
}