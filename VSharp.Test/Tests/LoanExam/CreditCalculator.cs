using InstantCredit.Shared.Models.Enums;
using LoanExam.Models;
using System;
using VSharp.Test;

namespace LoanExam;

public class CreditResult
{
    public bool CreditIssued { get; set; }
    
    public double Percent { get; set; }
}

[TestSvmFixture]
public class CreditCalculationService
{
    private int CalculateByAge(int age, CreditInfo creditInfo)
    {
        var SumPoints = 0;
        if (age.Between(21, 28))
        {
            switch (creditInfo.Sum)
            {
                case < 1_000_000:
                    return 12;
                case > 3_000_000:
                    return 0;
                default:
                    return 9;
            }
        }

        if (age.Between(29, 59))
        {
            SumPoints += 14;
            return SumPoints;
        }

        SumPoints += creditInfo.Deposit == Deposit.None 
            ? 0 
            : 8;
        
        return SumPoints;
    }

    private int CalculateByCriminal(bool certificateOfNoCriminalRecord)
    {
        return certificateOfNoCriminalRecord 
            ? 15 
            : 0;
    }

    private int CalculateByEmployment(Employment employment, int age)
    {
        return employment switch
        {
            Employment.Contract => 14,
            Employment.OwnIndividualEntrepreneurship => 12,
            Employment.Freelancer => 8,
            Employment.Pensioner when age < 70 => 5,
            Employment.Pensioner => 0,
            Employment.Unemployed => 0,
            _ => throw new ArgumentOutOfRangeException(nameof(employment), employment, null)
        };
    }

    private int CalculateByCreditInfo(CreditInfo creditInfo)
    {
        var SumPoints = 0;
        SumPoints += creditInfo.Sum switch
        {
            <= 1_000_000 => 12,
            <= 5_000_000 => 14,
            <= 10_000_000 => 8,
            _ => throw new ArgumentOutOfRangeException()
        };

        SumPoints += creditInfo.Deposit switch
        {
            Deposit.Guarantee => 12,
            Deposit.Realty => 14,
            Deposit.OldCar => 3,
            Deposit.NewCar => 8,
            Deposit.None => 0,
            _ => throw new ArgumentOutOfRangeException()
        };

        SumPoints += creditInfo.Purpose switch
        {
            CreditPurpose.Realty => 8,
            CreditPurpose.ConsumerCredit => 14,
            CreditPurpose.ReCrediting => 12,
            _ => throw new ArgumentOutOfRangeException()
        };

        return SumPoints;
    }

    private int CalculateByOtherCredits(bool otherCredits, CreditPurpose creditPurpose)
    {
        if (!otherCredits)
        {
            return creditPurpose == CreditPurpose.ReCrediting ? 0 : 15;
        }
        else
        {
            return 0;
        }
    }

    [TestSvm(-1, 0, 20, false, SearchStrategy.ShortestDistance)]
    public CreditResult Build(Request request)
    {
        var SumPoints = 0;
        
        SumPoints += CalculateByAge(request.Personality.Age, request.CreditInfo);
        SumPoints += CalculateByCriminal(request.CertificateOfNoCriminalRecord);
        SumPoints += CalculateByEmployment(request.Personality.Employment, request.Personality.Age);
        SumPoints += CalculateByCreditInfo(request.CreditInfo!);
        SumPoints += CalculateByOtherCredits(request.OtherCredits, request.CreditInfo!.Purpose);
       
        var result = new CreditResult
        {
            Percent = 0,
            CreditIssued = SumPoints >= 80
        };
        
        if (SumPoints < 80)
        {
            return result;
        }

        result.Percent = SumPoints switch
        {
            < 84 => 30,
            < 88 => 26,
            < 92 => 22,
            < 96 => 19,
            < 100 => 15,
            100 => 12.5,
            _ => throw new ArgumentOutOfRangeException()
        };

        result.CreditIssued = true;
        SumPoints = 0;
        return result;
    }
}
