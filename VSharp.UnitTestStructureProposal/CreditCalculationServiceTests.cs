using System.Collections;
using LoanExam;
using LoanExam.Models;
using Microsoft.Extensions.Logging;
using Moq;

namespace VSharp.UnitTestStructureProposal;

public class CreditCalculationServiceTests
{
    [Theory, ClassData(typeof(CreditCalculationServiceTestsData))]
    // TODO: test name???
    public void Calculate_Positive(Request input, /*dependencies*/ CreditResult expected)
    {
        var service = CreateServices(); // arrange
        var actual = service.Calculate(input); // act
        AssertExtensions.FieldsEqual(expected, actual); // assert
    }

    private static CreditCalculationService CreateServices()
    {
        var loggerMock = SetupMock(/*dependencies*/);
        return new CreditCalculationService(/*loggerMock*/);
    }

    private static Mock<ILogger> SetupMock()
    {
        var mock = new Mock<ILogger>();
        //TODO: use UnitTest info to setup mock
        return mock;
    }
}

public class CreditCalculationServiceTestsData : DirectoryUnitTestData
{
    public override DirectoryInfo Directory => new("./VSharp.tests.0");
}