using System.ComponentModel.DataAnnotations;

namespace LoanExam.Models;

public record Request(
    [Required]PersonalInfo PersonalInfo,
    [Required]CreditInfo CreditInfo,
    [Required]PassportInfo PassportInfo,
    bool CertificateOfNoCriminalRecord,
    bool OtherCredits
);