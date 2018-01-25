namespace VSharp

open System
open System.Collections.Generic

/// <summary>
/// Generates string identifiers unique per application domain.
/// </summary>
module public IdGenerator =
    let private defaultPrefix = "v#!"
    let private values = persistent((fun () -> new Dictionary<string, uint32>()), fun x -> new Dictionary<string, uint32>(x))

    /// <summary>
    /// Generates new string identifiers starting from the given prefix unique per application domain
    /// </summary>
    let public startingWith prefix =
        let nonEmptyPrefix = if String.IsNullOrWhiteSpace(prefix) then defaultPrefix else prefix
        // Generated ids may actually still collide if prefix ends with digit. Adding
        let validPrefix = if Char.IsDigit(nonEmptyPrefix.[nonEmptyPrefix.Length - 1]) then nonEmptyPrefix + "!!" else nonEmptyPrefix
        let id = if values.Value.ContainsKey(validPrefix) then values.Value.[validPrefix] + 1u else 1u
        values.Value.Remove(validPrefix) |> ignore
        values.Value.Add(validPrefix, id)
        validPrefix + string(id)

    /// <summary>
    /// Generates new string identifiers unique per application domain.
    /// </summary>
    let public newId() = startingWith(defaultPrefix)

    /// <summary>
    /// Clears internal identifiers cache; the identifiers will duplicate ones that were generated until the reset.
    /// </summary>
    let public clear() = values.Value.Clear()

    /// <summary>
    /// Persistent version of clear(): clears cache saving old configuration of IdGenerator until restore() is called.
    /// </summary>
    let public reset() = values.Reset()

    /// <summary>
    /// Stores current peak value: next restore() call will unroll peak value to current one.
    /// </summary>
    let public saveConfiguration() = values.Save()

    /// <summary>
    /// Rolls back to previously stored configuration of IdGenerator.
    /// </summary>
    let public restore() = values.Restore()
