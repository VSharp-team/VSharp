namespace LoanExam.Models;

public static class IntExtensions
{
    public static bool Between(this int src, int left, int right)
    {
        return src >= left && src <= right;
    }
}