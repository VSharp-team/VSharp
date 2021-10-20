namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic

open VSharp
open VSharp.Core
open VSharp.Utils

open System.Diagnostics

module EstimatorConstants =
    let usedLocation = -5
    let newFrameOpening = 1000
    let newLocationOpening = 50

type public StackTraceEstimator(stackTrace : StackTrace) =
    let stackFrame2Ip (sf : StackFrame) =
        let offset = sf.GetILOffset()
        let m = sf.GetMethod()
        ipOperations.buildIpFromOffset m offset

    let ipStack = Seq.map stackFrame2Ip (stackTrace.GetFrames()) |> List.ofSeq
    let stackTraceSize = List.length ipStack
    let history = Dictionary<cilState, pset<ip>>()
    let estimations = Dictionary<cilState, int>()
//    let updateHistory (child : cilState) (p : cilState) =
//        let set = history.[p]
//        let newSet = PersistentSet.add set (CilStateOperations.currentIp child)
//        history.Add(child, newSet)
    let updateInfo (parent : cilState option) child estimation =
        estimations.Add(child, estimation)
        let newSet =
            match parent with
            | None -> PersistentSet.add PersistentSet.empty (CilStateOperations.currentIp child)
            | Some p -> PersistentSet.add history.[p] (CilStateOperations.currentIp child)
        history.Add(child, newSet)
    interface IEstimator with
        override x.Estimate(parent, child) =
            let ip = CilStateOperations.currentIp child
            let ipStackSize = List.length child.ipStack
            let newEstimation =
                match parent with
                | None when ipStackSize <= stackTraceSize && List.take ipStackSize ipStack = child.ipStack ->
                    EstimatorConstants.newFrameOpening
                | None -> EstimatorConstants.newLocationOpening
                | Some p when PersistentSet.contains ip history.[p] -> estimations.[p] + EstimatorConstants.usedLocation
                | Some p when ipStackSize <= stackTraceSize && List.take ipStackSize ipStack = child.ipStack ->
                    estimations.[p] + EstimatorConstants.newFrameOpening
                | Some p -> estimations.[p] + EstimatorConstants.newLocationOpening
            updateInfo parent child newEstimation
            newEstimation
