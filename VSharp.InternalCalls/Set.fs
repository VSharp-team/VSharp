namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter.IL
open VSharp.Interpreter.IL.CilState
open VSharp.TypeUtils
open Arithmetics

module internal Set =

    let IsContainsItem (state : state) (args : term list) =
        assert(List.length args = 3)
        let this, item = args[0], args[2]
        Memory.IsSetContains state this item

    let AddItem (state : state) (args : term list) =
        assert(List.length args = 3)
        let this, item = args[0], args[2]
        let isAdd = Memory.AddToSet state this item
        let count = Memory.GetSetCount state this
        let assume = count >> MakeNumber 0
        AssumeStatedConditionalExecution state assume
        isAdd

    let RemoveItem (state : state) (args : term list) =
        assert(List.length args = 3)
        let this, item = args[0], args[2]
        let count = Memory.GetSetCount state this
        let contains = Memory.IsSetContains state this item
        let assume = (!! contains) ||| (count >> MakeNumber 0)
        AssumeStatedConditionalExecution state assume
        Memory.RemoveFromSet state this item

    let GetCount (state : state) (args : term list) =
        assert(List.length args = 2)
        let this = args[0]
        Memory.GetSetCount state this
