namespace VSharp.SILI

open System.Reflection
open VSharp
open VSharp.Interpreter.IL

type StandaloneFuzzer(options: FuzzerOptions) =

    let statistics = new SILIStatistics()

    member this.StartFuzzing (cancellationToken, isolated : MethodBase seq) =
        let saveStatistic = statistics.SetBasicBlocksAsCoveredByTest
        let outputDir = options.outputDirectory.FullName
        task {
            let targetAssemblyPath = (Seq.head isolated).Module.Assembly.Location
            let onCancelled () = Logger.warning "Fuzzer canceled"
            let interactor = Fuzzer.Interactor(cancellationToken, outputDir, saveStatistic, onCancelled)
            do! interactor.StartFuzzing targetAssemblyPath isolated
        }
