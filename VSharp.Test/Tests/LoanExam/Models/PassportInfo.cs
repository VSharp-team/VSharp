using System;
#pragma warning disable CS0660, CS0661

namespace LoanExam.Models;

public class PassportInfo
{
    public string Series { get; set; }
    
    public string Number { get; set; }
    
    public DateTime IssueDate { get; set; }
    
    public string IssuedBy { get; set; }
    
    public string Registration { get; set; }

    public static bool operator ==(PassportInfo x, PassportInfo y)
    {
        return x.Number == y.Number && x.Series == y.Series;
    }
    
    public static bool operator !=(PassportInfo x, PassportInfo y)
    {
        return !(x == y);
    }
}