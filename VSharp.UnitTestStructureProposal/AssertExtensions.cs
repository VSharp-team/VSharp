using System.Reflection;

namespace VSharp.UnitTestStructureProposal;

public class AssertExtensions
{
    public static void FieldsEqual<T>(T expected, T actual)
    {
        Assert.Equal(expected, actual, new FieldEqualityComparer<T>());
    }

    private class FieldEqualityComparer<T> : IEqualityComparer<T>
    {
        public bool Equals(T? x, T? y)
        {
            if (null == y || null == x)
            {
                return false;
            }
            
            var thisType = x.GetType();
            var thatType = y.GetType();

            if (thatType != thisType)
            {
                return false;
            }

            object thisObj = x;
            object? thisResult, thatResult;


            var thisFields = thisType.GetFields(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic);

            for (int i = 0; i < thisFields.Length; i++)
            {
                thisResult = thisFields[i].GetValue(thisObj);
                thatResult = thisFields[i].GetValue(y);

                if (thisResult == null)
                {
                    if (thatResult != null)
                        return false;
                }
                else
                if (!thisResult.Equals(thatResult))
                {
                    return false;
                }
            }

            return true;
        }

        public int GetHashCode(T obj) => obj?.GetHashCode() ?? 0;
    }
}