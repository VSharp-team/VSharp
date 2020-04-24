namespace VSharp.Interpreter.IL

open VSharp
open VSharp.Core.API
open VSharp.Interpreter.IL.CFG

exception PdrNotImplementedException
type StepInterpreter() =
    inherit ILInterpreter()
    override x.Invoke codeLoc oldState this k =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            Engine.configureInterpreter x
            let state, this, thisIsNotNull, _ = x.FormInitialState ilmm
            let initialState =
                match this with
                | None -> state
                | Some _ -> AddConditionToState state thisIsNotNull
            let methodRepr = Engine.MethodRepresentationBuilder.computeRepresentation x initialState this ilmm.methodBase
            Logger.printLog Logger.Trace "Computed Method Representation: %s" (methodRepr.ToString())
            k (Terms.Nop, oldState)

//            let interpreter = new CodePortionInterpreter(x, ilmm, findCfg ilmm, [])
//            interpreter.Invoke state this k
        | _ -> internalfail "unhandled ICodeLocation instance"





