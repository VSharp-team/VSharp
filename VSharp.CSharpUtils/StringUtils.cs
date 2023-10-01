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
        [Implements("System.Boolean System.String.Equals(System.String, System.String)")]
        [Implements("System.Boolean System.String.Equals(this, System.String)")]
        public static bool Equals(string str1, string str2)
        {
            if (str1 is null && str2 is null)
                return true;

            if (str1 is null)
                return false;

            if (str2 is null)
                return false;

            if (str1.Length != str2.Length)
                return false;

            for (var i = 0; i < str1.Length; i++)
            {
                if (str1[i] != str2[i])
                    return false;
            }

            return true;
        }

        [Implements("System.Boolean System.String.Equals(System.String, System.String, System.StringComparison)")]
        public static bool EqualsWithComparison(string str1, string str2, System.StringComparison comparison)
        {
            return Equals(str1, str2);
        }

        [Implements("System.Boolean System.String.StartsWith(this, System.String, System.StringComparison)")]
        public static bool StartsWith(string str1, string str2, System.StringComparison comparison)
        {
            var len1 = str1.Length;
            var len2 = str2.Length;

            if (len2 > len1)
                return false;

            for (var i = 0; i < len2; i++)
            {
                if (str1[i] != str2[i])
                    return false;
            }

            return true;
        }
    }
}
