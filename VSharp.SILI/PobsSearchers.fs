namespace VSharp.Interpreter.IL

open System
open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections
open FSharpx.Collections
open FSharpx.Collections
open InstructionsSet
open CilStateOperations
open VSharp
open VSharp.Core
open ipOperations
open Instruction

//type ForwardNewSearcher() =
//    interface INewSearcher with
//        override x.ChooseAction(qf, _) =
//            match qf with
//            | [] -> Stop
//            | s :: _ -> GoForward s
