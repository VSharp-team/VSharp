using System.Linq;

namespace VSharp.CSharpUtils
{
    public class StringUtils
    {
        [Implements("System.Int32 System.String.GetHashCode(this)")]
        public static int GetHashCode(string str)
        {
            return str.GetDeterministicHashCode();
        }

        [Implements("System.Boolean System.String.EqualsHelper(System.String, System.String)")]
        public static bool Equals(string str1, string str2)
        {
            if (str1.Length != str2.Length)
                return false;

            for (var i = 0; i < str1.Length; i++)
            {
                if (str1[i] != str2[i])
                    return false;
            }

            return true;
        }
    }
}
