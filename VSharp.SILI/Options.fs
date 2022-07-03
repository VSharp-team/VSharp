﻿namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Diagnostics

type searchMode =
    | DFSMode
    | BFSMode
    | GuidedMode

type coverageZone =
    | MethodZone
    | ClassZone
    | ModuleZone

type explorationMode =
    | TestCoverageMode of coverageZone * searchMode
    | StackTraceReproductionMode of StackTrace

type executionMode =
    | ConcolicMode
    | SymbolicMode

type SiliOptions = {
    explorationMode : explorationMode
    executionMode : executionMode
    recThreshold : uint32
    preallocatedMap: Option<IDictionary<Type, Object>>
}
