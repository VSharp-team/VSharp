namespace VSharp

open System

module EnumUtils =

    /// <summary>
    /// Gets enum type underlying type throwing InsufficientInformationException if enum is a generic parameter.
    /// TODO: Implement default underlying types values (Int32) using assumptions.
    /// </summary>
    let getEnumUnderlyingTypeChecked (t : Type) =
        if not t.IsEnum then
            invalidArg "t" "Type must be enum"

        if t.IsGenericParameter then
            __insufficientInformation__ $"Cannot determine underlying type for generic enum type {t.Name}"

        t.GetEnumUnderlyingType()

    let getEnumDefaultValue (t : Type) =
        if not t.IsEnum then
            invalidArg "t" "Type must be enum"

        let value = Activator.CreateInstance t

        if t.IsEnumDefined value then
            value
        else
            let allValues = t.GetEnumValues()
            if allValues.Length <> 0 then
                allValues.GetValue(0)
            else
                value
