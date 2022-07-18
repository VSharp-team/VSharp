using System.ComponentModel.DataAnnotations;

namespace LoanExam.Models;

public record Request(
    [Required]Personality Personality,
    [Required]CreditInfo CreditInfo,
    [Required]Passport Passport,
    bool CertificateOfNoCriminalRecord,
    bool OtherCredits
);