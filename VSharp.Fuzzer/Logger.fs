module internal VSharp.Fuzzer.Logger

let logUnhandledException (e: exn) =
    VSharp.Logger.error $"Unhandled exception: {e}"
    exit 1

let private communicationTraceTag = "Communication"
let traceCommunication msg = VSharp.Logger.traceWithTag communicationTraceTag msg
let errorCommunication msg = VSharp.Logger.errorWithTag communicationTraceTag msg

let private testGenerationTraceTag = "TestGeneration"
let traceTestGeneration msg = VSharp.Logger.traceWithTag testGenerationTraceTag msg

let private fuzzingTag = "Fuzzing"
let errorFuzzing msg = VSharp.Logger.errorWithTag fuzzingTag msg
let traceFuzzing msg = VSharp.Logger.traceWithTag fuzzingTag msg
let infoFuzzing msg = VSharp.Logger.infoWithTag fuzzingTag msg

let private generationTag = "Generation"
let traceGeneration msg = VSharp.Logger.traceWithTag generationTag msg
let infoGeneration msg = VSharp.Logger.infoWithTag generationTag msg

let private typeSolverTag = "TypeSolver"
let traceTypeSolving msg = VSharp.Logger.traceWithTag typeSolverTag msg

let traceFuzzingInteraction msg = VSharp.Logger.traceWithTag VSharp.Logger.fuzzingInteractionTraceTag msg

let setupLogger outputDir =
    #if DEBUG
    let writer = System.IO.File.AppendText $"{outputDir}{System.IO.Path.DirectorySeparatorChar}fuzzer.log"

    VSharp.Logger.configureWriter writer
    // Enable tags here

    VSharp.Logger.changeVerbosity VSharp.Logger.defaultTag VSharp.Logger.Warning
    #endif
    ()
