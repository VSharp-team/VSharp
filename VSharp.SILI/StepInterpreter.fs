namespace VSharp.Interpreter.IL

//open System.Reflection
//open VSharp
//open VSharp.Core.API
//open VSharp.Interpreter.IL.CFG
//type exceptionRegister = VSharp.Core.exceptionRegister
//
//exception PdrNotImplementedException
//type StepInterpreter() =
//    inherit ILInterpreter()
//    override x.CreateInstance exceptionType arguments state =
////        let ilInterpreter = ILInterpreter()
////        ilInterpreter.CreateInstance exceptionType arguments state
//        // TODO: exceptions
//        Nop, {state with exceptionRegister = exceptionRegister.Unhandled Nop}
//
//
//    override x.Invoke codeLoc oldState this k =
//        match codeLoc with
//        | :? ILMethodMetadata as ilmm ->
//            Analyzer.CFA.configureInterpreter x
//            let state, this, thisIsNotNull, _ = x.FormInitialState ilmm
//            let initialState =
//                match this with
//                | None -> state
//                | Some _ -> AddConditionToState state thisIsNotNull
//            let cfa = Analyzer.CFA.cfaBuilder.computeCFA x initialState this ilmm.methodBase
//            Logger.printLog Logger.Trace "Computed cfa: %s" (cfa.ToString())
//            k (Terms.Nop, oldState)
//
////            let interpreter = new CodePortionInterpreter(x, ilmm, findCfg ilmm, [])
////            interpreter.Invoke state this k
//        | _ -> internalfail "unhandled ICodeLocation instance"
//




