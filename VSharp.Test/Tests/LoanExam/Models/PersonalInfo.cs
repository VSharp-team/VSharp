using InstantCredit.Shared.Models.Enums;

namespace LoanExam.Models;

public class PersonalInfo
{
    public string FirstName { get; set; }
    
    public string SecondName { get; set; }

    public string Patronymic { get; set; }
    
    public int Age { get; set; }
    
    public Employment Employment { get; set; }
}