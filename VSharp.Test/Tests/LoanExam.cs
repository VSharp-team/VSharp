using System;
using VSharp.Test;

namespace IntegrationTests;

public enum CreditPurpose
{
    ConsumerCredit,
    Realty,
    ReCrediting
}

public enum Deposit
{
    Realty,
    OldCar,
    NewCar,
    Guarantee,
    None
}

public enum Employment
{
    Contract,
    OwnIndividualEntrepreneurship,
    Freelancer,
    Pensioner,
    Unemployed
}

public class CreditResult
{
    public bool CreditIssued { get; set; }

    public double Percent { get; set; }
}


public class CreditInfo
{
    public CreditPurpose Purpose { get; set; }

    public decimal Sum { get; set; }

    public Deposit Deposit { get; set; }
}

public class Personality
{
    public string FirstName { get; set; }

    public string SecondName { get; set; }

    public string Patronymic { get; set; }

    public int Age { get; set; }

    public Employment Employment { get; set; }
}

public class Passport
{
    public string Series { get; set; }

    public string Number { get; set; }

    public DateTime IssueDate { get; set; }

    public string IssuedBy { get; set; }

    public string Registration { get; set; }

    public static bool operator ==(Passport x, Passport y)
    {
        return x.Number == y.Number && x.Series == y.Series;
    }

    public static bool operator !=(Passport x, Passport y)
    {
        return !(x == y);
    }
}

public static class IntExtensions
{
    public static bool Between(this int src, int left, int right)
    {
        return src >= left && src <= right;
    }
}

public record Request(
    Personality Personality,
    CreditInfo CreditInfo,
    Passport Passport,
    bool CertificateOfNoCriminalRecord,
    bool OtherCredits
);

[TestSvmFixture]
public class LoanExam
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

    [TestSvm(93, 0, 10, strat: SearchStrategy.Interleaved, coverageZone: CoverageZone.Class, guidedMode: false, releaseBranches: false)]
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
