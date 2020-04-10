namespace VSharp.Interpreter.IL

open VSharp

type StepInterpreter() =
    inherit ILInterpreter()
    let x = 456
    override x.Invoke codeLoc state this k =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            let initialState, _, _, _ = x.FormInitialState ilmm
            let methodRepr = Engine.MethodRepresentationBuilder.computeRepresentation initialState ilmm.methodBase
            Logger.printLog Logger.Trace "Computed Method Representation: %O" methodRepr
            __notImplemented__()

//            let interpreter = new CodePortionInterpreter(x, ilmm, findCfg ilmm, [])
//            interpreter.Invoke state this k
        | _ -> internalfail "unhandled ICodeLocation instance"





