using InstantCredit.Shared.Models.Enums;

namespace LoanExam.Models;

public class Personality
{
    public string FirstName { get; set; }
    
    public string SecondName { get; set; }

    public string Patronymic { get; set; }
    
    public int Age { get; set; }
    
    public Employment Employment { get; set; }
}