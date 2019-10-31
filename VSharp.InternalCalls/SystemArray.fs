namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Array -------------------------------

module internal SystemArray =

    open Arithmetics

    let checkBounds state lower upper x fits k =
        let notTooSmall = x >>= lower
        let notTooLarge = x << upper
        let inBounds = notTooSmall &&& notTooLarge
        BranchStatements state
            (fun state k -> k (inBounds, state))
            fits
            (fun state k -> RuntimeExceptions.IndexOutOfRangeException state Error |> k)
            k

    let private accessBound accessor state args =
        assert(List.length args = 2)
        let this, dimension = List.item 0 args, List.item 1 args
        let array, state = Memory.Dereference state this
        GuardedStatedApplyStatementK state array (fun state term k ->
            match term.term with
            | Array(d, _, _, _, _, _) ->
                let lowerBound = Concrete 0 Types.TLength
                checkBounds state lowerBound d dimension
                    (fun state k -> accessor state this dimension |> k) k
            | term -> internalfailf "expected array, but %O got!" term) id

    let GetLength state args = accessBound Memory.ArrayLengthByDimension state args

    let GetLowerBound state args = accessBound Memory.ArrayLowerBoundByDimension state args

    let GetRank state args =
        assert(List.length args = 1)
        let array, state = Memory.Dereference state (List.head args)
        let result = GuardedApplyExpression array (fun term ->
            match term.term with
            | Array(d, _, _, _, _, _) -> d
            | term -> internalfailf "expected array, but %O got!" term)
        result, state

    let get_Rank state args =
        GetRank state args

    let get_Length state args =
        assert(List.length args = 1)
        let array, state = Memory.Dereference state (List.head args)
        Memory.ArrayLength array, state
