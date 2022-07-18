using InstantCredit.Shared.Models.Enums;

namespace LoanExam.Models;

public class CreditInfo
{
    public CreditPurpose Purpose { get; set; }
    
    public decimal Sum { get; set; }
    
    public Deposit Deposit { get; set; }
}