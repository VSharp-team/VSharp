namespace VSharp.Core.Utils

open System
open System.Collections.Generic

/// <summary>
/// Generates string identifiers unique per application domain.
/// </summary>
module IdGenerator =
    let private defaultPrefix = "v#!"
    let private values = new Dictionary<string, int>()

    /// <summary>
    /// Generates new string identifiers starting from the given prefix unique per application domain
    /// </summary>
    let public startingWith prefix =
        let nonEmptyPrefix = if String.IsNullOrWhiteSpace(prefix) then defaultPrefix else prefix
        // Generated ids may actually still collide if prefix ends with digit. Adding 
        let validPrefix = if Char.IsDigit(nonEmptyPrefix.[nonEmptyPrefix.Length - 1]) then nonEmptyPrefix + "!!" else nonEmptyPrefix
        let id = if values.ContainsKey(validPrefix) then values.[validPrefix] + 1 else 1 
        values.Remove(validPrefix) |> ignore
        values.Add(validPrefix, id)
        validPrefix + string(id)

    /// <summary>
    /// Generates new string identifiers unique per application domain
    /// </summary>
    let public newId() = startingWith(defaultPrefix)

    /// <summary>
    /// Clears internal identifiers cache; the identifiers will duplicate the generated until reset ones
    /// </summary>
    let public reset() = values.Clear()
