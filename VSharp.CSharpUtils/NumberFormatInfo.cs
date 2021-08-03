namespace VSharp.CSharpUtils
{
    public static class NumberFormatInfo
    {
        [Implements("System.Globalization.NumberFormatInfo System.Globalization.NumberFormatInfo.get_CurrentInfo()")]
        public static System.Globalization.NumberFormatInfo get_CurrentInfo()
        {
            return new System.Globalization.NumberFormatInfo();
        }
    }
}
