using System;

namespace VSharp
{
    [AttributeUsage(AttributeTargets.All, AllowMultiple = true)]
    public class ImplementsAttribute : Attribute
    {
        public readonly string Name;

        public ImplementsAttribute(string name)
        {
            Name = name;
        }
    }
}
