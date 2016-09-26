namespace VSharp.Core.Symbolic.Reduction

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Core.Symbolic

module internal Arithmetics =

// ------------------------------- Simplification of "+" -------------------------------

    let private makeAddition isChecked t x y =
        Terms.MakeBinary OperationType.Add x y false (Types.FromPrimitiveDotNetType t)

    let private simplifyConcreteAddition x y isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.AddChecked(x, y, t, success)
            if !success then Terms.MakeConcrete result t
            else Bottom
        else
            Terms.MakeConcrete (Calculator.Add(x, y, t)) t

    let rec private simplifyAdditionToExpression x y (t : System.Type) matched unmatched =
        let xop = Terms.OperationOf x
        let xargs = Terms.ArgumentsOf x
        let xt = Terms.TypeOf x
        match xop with
        | Operator(OperationType.Add, isExternalChecked) when not isExternalChecked ->
            // Simplifying (a + b) + y at this step
            let a = List.head xargs
            let b = List.item 1 xargs
            match a, b, y with
            // (a + b) + y = (a + y) + b if a and y concrete and unchecked
            | Concrete(a, at), _, Concrete(y, yt) -> simplifyAddition (simplifyConcreteAddition a y false t) b false t |> matched
            // ((-y) + b) + y = b if unchecked
            | Terms.UnaryMinus(a, isMinusChecked, _), b, _ when not isMinusChecked && a = y -> matched b
            // (a + (-y)) + y = a if unchecked
            | a, Terms.UnaryMinus(b, isMinusChecked, _), _ when not isMinusChecked && b = y -> matched a
            // (a + b) + (-a) = b if unchecked
            | _, _, Terms.UnaryMinus(y, isMinusChecked, _) when not isMinusChecked && a = y -> matched b
            // (a + b) + (-b) = a if unchecked
            | _, _, Terms.UnaryMinus(y, isMinusChecked, _) when not isMinusChecked && b = y -> matched a
            | _ ->
                // Trying to simplify pairwise combinations of x- and y-summands
                let summandsOfY =
                    match y with
                    | Terms.Add(c, d, isChecked, yt) when not isChecked -> [c; d]
                    | _ -> [y]
                // simplifySummand tries to simplify x with every summand of y and calls cps with result and unmatched summands
                let rec simplifySummand x summands failedSummands cps =
                    if List.isEmpty summands then cps x failedSummands
                    else simplifyAdditionCps x (List.head summands) false t
                            (fun result -> simplifySummand result (List.tail summands) failedSummands cps)
                            (fun () -> simplifySummand x (List.tail summands) ((List.head summands) :: failedSummands) cps)

                simplifySummand a summandsOfY  [] (fun aSum summandsToGo ->
                simplifySummand b summandsToGo [] (fun bSum summandsToGo ->
                if List.length summandsOfY = List.length summandsToGo then unmatched () // Nothing matched, the whole process is failed
                else
                    // Something matched, the work is done, just combining results together...
                    let toAdd = aSum :: bSum :: summandsToGo
                    // TODO: care about different types...
                    toAdd |> List.reduce (makeAddition false t) |> matched))
        | Operator(OperationType.UnaryMinus, isChecked) when not isChecked ->
            // Simplifying (-x) + y at this step
            let x = List.head xargs
            match x with
            // (-y) + y = 0
            | _ when x = y -> matched (Terms.MakeConcrete 0 t)
            // -(y + b) = -b
            | Terms.Add(a, b, isChecked, _) when not isChecked && a = y -> matched b
            // -(a + y) = -a
            | Terms.Add(a, b, isChecked, _) when not isChecked && b = y -> matched a
            | _ -> unmatched ()
        | Operator(OperationType.Multiply, isChecked) when not isChecked ->
            // Simplifying (a * b) + y at this step
            let a = List.head xargs
            let b = List.item 1 xargs
            match a, b, y with
            // (a * y) + y = (a + 1) * y if unckecked and a is concrete
            | Concrete(a, at), _, _ when b = y ->
                let aPlusOne = simplifyConcreteAddition a 1 false (Types.ToDotNetType at)
                simplifyMultiplication aPlusOne y false t |> matched
            // (a * b) + (c * b) = (a + c) * b if uncheched and a and c are concrete
            | Concrete(a, _), _, Terms.Mul(Concrete(c, _), d, isChecked, _) when not isChecked && d = b ->
                let aPlusC = simplifyConcreteAddition a c false t
                simplifyConcreteMultiplication aPlusC b false t |> matched
            | _ -> unmatched ()
        | Cond ->
            // Simplifying (if c then a else b) + y at this step
            let c = List.head xargs
            let a = List.item 1 xargs
            let b = List.item 2 xargs
            match a, b, y with
            // (if c then a else b) + y if a, b and y are concrete
            | Concrete(a, at), Concrete(b, bt), Concrete(y, yt) ->
                let aPlusY = simplifyConcreteAddition a y false (Types.ToDotNetType at)
                let bPlusY = simplifyConcreteAddition b y false (Types.ToDotNetType bt)
                Expression(Cond, [c; aPlusY; bPlusY], xt) |> matched
            | _ -> unmatched ()
        | _ -> unmatched ()

    and private simplifyAdditionCps x y isChecked t (matched : Term -> Term) unmatched =
        match x, y with
        | Bottom, _
        | _, Bottom -> matched Bottom
        | Nop, _ -> raise(new System.ArgumentException("Invalid left operand!"))
        | _, Nop -> raise(new System.ArgumentException("Invalid right operand!"))
        | Concrete(x, typeOfX), Concrete(y, typeOfY) -> simplifyConcreteAddition x y isChecked t |> matched
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> matched y
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> matched x
        | Expression(_, _, _), Expression(_, _, _) when not isChecked ->
            simplifyAdditionToExpression x y t matched (fun () ->
            simplifyAdditionToExpression y x t matched unmatched)
        | Expression(_, _, _), _ when not isChecked -> simplifyAdditionToExpression x y t matched unmatched
        | _, Expression(_, _, _) when not isChecked -> simplifyAdditionToExpression y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyAddition x y isChecked t =
        let defaultCase () =
            let sorted = if (Terms.IsConcrete y) then (y, x) else (x, y)
            makeAddition isChecked t (fst sorted) (snd sorted)
        simplifyAdditionCps x y isChecked t id defaultCase

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyUnaryMinus x isChecked t =
        match x with
        | Bottom -> Bottom
        | Nop -> raise(new System.ArgumentException("Invalid operand!"))
        | Concrete(x, typeOfX) ->
            if isChecked then
                let success = ref true
                let result = Calculator.UnaryMinusChecked(x, t, success)
                if !success then Terms.MakeConcrete result t
                else Bottom
            else
                Terms.MakeConcrete (Calculator.UnaryMinus(x, t)) t
        // -(-(x)) = x if both unchecked
        | Terms.UnaryMinus(x, isInnerChecked, t) when not isInnerChecked && not isChecked -> x
        // -(a + b) = (-a) + (-b) if all unchecked
        | Terms.Add(a, b, isPlusChecked, _) when not isPlusChecked && not isChecked ->
            let minusA = simplifyUnaryMinus a false t
            let minusB = simplifyUnaryMinus b false t
            simplifyAddition minusA minusB false t
         // -(a * x) = (-a) * x if both unchecked
        | Terms.Mul(Concrete(a, at), y, isMulChecked, _) when not isMulChecked && not isChecked ->
            let minusA = simplifyUnaryMinus (Concrete(a, at)) isChecked (Types.ToDotNetType at)
            simplifyMultiplication minusA y isMulChecked t
        | _ -> Terms.MakeUnary OperationType.UnaryMinus x isChecked (Types.FromPrimitiveDotNetType t)



// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication x y isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.MulChecked(x, y, t, success)
            if !success then Terms.MakeConcrete result t
            else Bottom
        else
            Terms.MakeConcrete (Calculator.Mul(x, y, t)) t

    and private simplifyMultiplicationOnExpression x y t unmatchedHandler =
        x

    and internal simplifyMultiplication x y isChecked t  =
        x

// ------------------------------- General methods -------------------------------
    let internal simplifyBinaryOperation op x y isChecked t =
        match op with
        | OperationType.Add -> simplifyAddition x y isChecked t
        | OperationType.Subtract -> simplifyAddition x (simplifyUnaryMinus y false t) isChecked t
        | OperationType.Divide
        | OperationType.Equal
        | OperationType.Greater
        | OperationType.GreaterOrEqual
        | OperationType.Less
        | OperationType.LessOrEqual
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.Multiply
        | OperationType.NotEqual
        | OperationType.NullCoalescing
        | OperationType.Remainder
        | OperationType.ShiftLeft
        | OperationType.ShiftRight -> raise(new System.NotImplementedException())
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary arithmetic operator"))

    let internal simplifyUnaryOperation op x isChecked t =
        match op with
        | OperationType.LogicalNeg
        | OperationType.PostfixIncrement
        | OperationType.PostfixDecrement
        | OperationType.PrefixIncrement
        | OperationType.PrefixDecrement
        | OperationType.UnaryMinus -> simplifyUnaryMinus x isChecked t
        | OperationType.UnaryPlus -> x
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary arithmetic operator"))
