using System;
using Microsoft.Extensions.Logging;

namespace LoanExam.ActionPointsHandler;

public enum Employment
{
    EmploymentContract=0,
    IndividualEntrepreneur=1,
    Freelancer =2,
    Retired=3,
    Unemployed=4
}

public class EmploymentHandler
{
    private readonly ILogger _logger;

    public EmploymentHandler(ILogger logger)
    {
        _logger = logger ?? throw new ArgumentNullException(nameof(logger));
    }
    
    public int EmploymentActionPoint(Employment personalInfoEmployment, int personalInfoAge)
    {
        int points = 0;
        switch (personalInfoEmployment)
        {
            case Employment.EmploymentContract:
                points += 14;
                break;
            case Employment.IndividualEntrepreneur:
                points += 12;
                break;
            case Employment.Freelancer:
                points += 8;
                break;
            case Employment.Retired:
                points += personalInfoAge switch
                {
                    < 70 => 5,
                    _ => 0
                };
                break;
        }

        return points;
    }
}