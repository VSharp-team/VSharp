module VSharp.Utils.EnvironmentUtils

open System
open System.Collections.Generic
open System.Diagnostics
open System.Reflection
open VSharp

let EnvVarUnspecified = "\0"

let optionalFromEnv name =
    let value = Environment.GetEnvironmentVariable(name)
    if value = null || value = EnvVarUnspecified then None
    else Some value

let fromEnv name =
    match optionalFromEnv name with
    | Some value -> value
    | None -> internalfail $"Required env var[{name}] not specified"

type EnvironmentConfigurationAttribute() =
    inherit Attribute()

[<AttributeUsage(AttributeTargets.Property)>]
type EnvironmentVariableAttribute(name : string) =
    inherit Attribute()
    member this.Name with get() = name

type EnvironmentCollection = Dictionary<string, string>
    
let isEnvVarSpecified (envCollection: EnvironmentCollection) name =
    envCollection.ContainsKey(name) && envCollection[name] <> EnvVarUnspecified

let private getEnvironmentConfiguration<'a> () =
    let configurationType = typeof<'a>
    assert Attribute.IsDefined(configurationType, typeof<EnvironmentConfigurationAttribute>)

    let result = List<PropertyInfo * string>()

    for property in configurationType.GetProperties(Reflection.allBindingFlags) do
        let envConfigurationAttributes = property.GetCustomAttributes(typeof<EnvironmentVariableAttribute>, false)
        assert (envConfigurationAttributes.Length = 1)
        let envConfigurationAttribute = envConfigurationAttributes[0] :?> EnvironmentVariableAttribute
        let envVarName = envConfigurationAttribute.Name
        result.Add(property, envVarName)

    result

let withConfiguration (configuration: 'a) (env: EnvironmentCollection) =

    for property, envVarName in getEnvironmentConfiguration<'a> () do
        assert (isEnvVarSpecified env envVarName |> not)
        env[envVarName] <- property.GetValue(configuration) |> string

    env
    
let setProcInfoEnvironment (env: EnvironmentCollection) (procInfo: ProcessStartInfo) =
    for KeyValue(envVarName, envValue) in env do
        assert (procInfo.Environment.ContainsKey(envVarName) |> not)
        procInfo.Environment[envVarName] <- envValue

let isConfigured<'a> () =
    let mutable allConfigured = true
    for _, envVarName in getEnvironmentConfiguration<'a> () do
        allConfigured <- allConfigured && Option.isSome (optionalFromEnv envVarName)
    allConfigured

let configurationFromEnv<'a> () =
    let result = Activator.CreateInstance<'a>()
    for property, envVarName in getEnvironmentConfiguration<'a> () do
        property.SetValue(result, fromEnv envVarName)
    result
