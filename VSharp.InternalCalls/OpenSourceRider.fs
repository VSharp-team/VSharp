namespace VSharp.System

open global.System
open VSharp.Core

module internal OpenSourceRider =

// ------------------------------- Logging -------------------------------

    // TODO: support logging
    let logCCtor (state : state) (_ : term list) : term * state =
        Nop, state

    // TODO: add OpenRider to references! #do
    let getLog (state : state) (_ : term list) : term * state =
        let logType = typeof<JetBrains.Diagnostics.Log>.Assembly.GetType("JetBrains.Diagnostics.Log+SwitchingLog")
        Types.FromDotNetType logType |> Memory.AllocateDefaultClass state

    let isTraceEnabled (state : state) (_ : term list) : term * state =
        False, state

//    let ctor (state : state) (args : term list) : (term * state) list =
//        let compareInfoType = Types.FromDotNetType
//        let field = {declaringType = typeof<System.Collections.Comparer>; name = "_compareInfo"; typ = typeof<System.Globalization.CompareInfo>}
//        let this = List.head args
//        let cultureType = Types.FromDotNetType typeof<System.Globalization.CompareInfo>
//        let compareInfo, state = Memory.AllocateDefaultClass state cultureType
//        Memory.WriteClassField state this field compareInfo |> List.map (withFst Nop)
