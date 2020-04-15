namespace VSharp.Interpreter.IL

open VSharp

exception PdrNotImplementedException
type StepInterpreter() =
    inherit ILInterpreter()
    override x.Invoke codeLoc state this k =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            let initialState, _, _, _ = x.FormInitialState ilmm
            let methodRepr = Engine.MethodRepresentationBuilder.computeRepresentation initialState ilmm.methodBase
            Logger.printLog Logger.Trace "Computed Method Representation: %s" (methodRepr.ToString())
            raise PdrNotImplementedException

//            let interpreter = new CodePortionInterpreter(x, ilmm, findCfg ilmm, [])
//            interpreter.Invoke state this k
        | _ -> internalfail "unhandled ICodeLocation instance"





