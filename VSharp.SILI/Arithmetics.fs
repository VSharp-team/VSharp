namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Simplify
open VSharp.Terms

[<AutoOpen>]
module internal Arithmetics =

    let private makeAddition isChecked state t x y k =
        (MakeBinary OperationType.Add x y isChecked (Types.FromDotNetType t), state) |> k

    let private makeProduct isChecked state t x y k =
        (MakeBinary OperationType.Multiply x y isChecked (Types.FromDotNetType t), state) |> k

// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition isChecked state t x y =
        if isChecked then
            let success = ref true
            let result = Calculator.AddChecked(x, y, t, success)
            if !success then MakeConcrete result t, state
            else
                let t, s = State.activator.CreateInstance typeof<System.OverflowException> [] state in
                Error t, s
        else
            MakeConcrete (Calculator.Add(x, y, t)) t, state

    let rec private simplifyAdditionToSum state t a b y matched unmatched =
        // Simplifying (a + b) + y at this step
        match a, b, y with
        // (a + b) + y = (a + y) + b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) ->
            let x, state = simplifyConcreteAddition false state t a y in
            simplifyAddition false state t x b matched
        // ((-y) + b) + y = b if unchecked
        | UnaryMinus(a, false, _), b, _ when a = y -> matched (b, state)
        // (a + (-y)) + y = a if unchecked
        | a, UnaryMinus(b, false, _), _ when b = y -> matched (a, state)
        // (a + b) + (-a) = b if unchecked
        | _, _, UnaryMinus(y, false, _) when a = y -> matched (b, state)
        // (a + b) + (-b) = a if unchecked
        | _, _, UnaryMinus(y, false, _) when b = y -> matched (a, state)
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-summands
            let summandsOfY =
                match y with
                | Add(c, d, false, yt) -> [(c, state); (d, state)]
                | _ -> [(y, state)]
            in
            simplifyPairwiseCombinations
                [(a, state); (b, state)]
                summandsOfY
                t
                fst
                (simplifyAdditionExt false state)
                (simplifyAddition false state t)
                (withSnd state >> matched)
                unmatched

    and private simplifyAdditionToUnaryMinus state t x y matched unmatched =
        // Simplifying (-x) + y at this step
        match x with
        // (-y) + y = 0
        | _ when x = y -> (MakeConcrete 0 t, state) |> matched
        // -(y + b) = -b
        | Add(a, b, false, _) when a = y -> matched (b, state)
        // -(a + y) = -a
        | Add(a, b, false, _) when b = y -> matched (a, state)
        | _ -> unmatched ()

    and private simplifyAdditionToProduct state t a b y matched unmatched =
        // Simplifying (a * b) + y at this step
        match a, b, y with
        // (a * y) + y = (a + 1) * y if unckecked and a is concrete
        | Concrete(a, at), _, _ when b = y ->
            let aPlusOne, state = simplifyConcreteAddition false state (Types.ToDotNetType at) a 1 in
            simplifyMultiplication false state t aPlusOne y matched
        // (a * b) + (c * b) = (a + c) * b if unchecked and a and c are concrete
        | Concrete(a, _), _, Mul(Concrete(c, _), d, false, _) when d = b ->
            let aPlusC, state = simplifyConcreteAddition false state t a c in
            simplifyMultiplication false state t aPlusC b matched
        | _ -> unmatched ()

    and private simplifyAdditionToExpression state x y t matched unmatched =
        match x with
        | Add(a, b, false, _) -> simplifyAdditionToSum state t a b y matched unmatched
        | UnaryMinus(x, false, _) -> simplifyAdditionToUnaryMinus state t x y matched unmatched
        | Mul(a, b, false, _) -> simplifyAdditionToProduct state t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyAdditionExt isChecked state t x y matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), Concrete(y, typeOfY) -> simplifyConcreteAddition isChecked state t x y |> matched
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> matched (y, state)
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> matched (x, state)
        | Expression _, Expression _ when not isChecked ->
            simplifyAdditionToExpression state x y t matched (fun () ->
            simplifyAdditionToExpression state y x t matched unmatched)
        | Expression _, _ when not isChecked -> simplifyAdditionToExpression state x y t matched unmatched
        | _, Expression _ when not isChecked -> simplifyAdditionToExpression state y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyAddition isChecked state t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y) in
                makeAddition isChecked state t (fst sorted) (snd sorted) k
        simplifyGenericBinary "addition" state x y k
                              (fun x y _ _ state -> simplifyConcreteAddition isChecked state t x y)
                              (fun x y state k -> simplifyAdditionExt isChecked state t x y k defaultCase)
                              (fun x y state k -> simplifyAddition isChecked state t x y k)

    and private simplifySubtraction isChecked state t x y k =
        simplifyUnaryMinus isChecked state t y (fun (minusY, state) ->
        simplifyAddition isChecked state t x minusY k)

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus isChecked state t x =
        if isChecked then
            let success = ref true
            let result = Calculator.UnaryMinusChecked(x, t, success)
            if !success then MakeConcrete result t, state
            else
                let t, s = State.activator.CreateInstance typeof<System.OverflowException> [] state in
                Error t, s
        else
            MakeConcrete (Calculator.UnaryMinus(x, t)) t, state

    and private simplifyUnaryMinus isChecked state t x k =
        simplifyGenericUnary "unary minus" state x k (fun x _ state -> simplifyConcreteUnaryMinus isChecked state t x) (fun x state k ->
        match x with
        // -(-(x)) = x if both unchecked
        | UnaryMinus(x, false, t) when not isChecked -> k (x, state)
        // -(a + b) = (-a) + (-b) if all unchecked
        | Add(a, b, false, _) when not isChecked ->
            simplifyUnaryMinus false state t a (fun (minusA, state) ->
            simplifyUnaryMinus false state t b (fun (minusB, state) ->
            simplifyAddition false state t minusA minusB k))
         // -(a * x) = (-a) * x if both unchecked
        | Mul(Concrete(a, at), y, false, _) when not isChecked ->
            simplifyUnaryMinus isChecked state (Types.ToDotNetType at) (Concrete(a, at)) (fun (minusA, state) ->
            simplifyMultiplication false state t minusA y k)
        | _ -> (MakeUnary OperationType.UnaryMinus x isChecked (Types.FromDotNetType t), state) |> k)

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication isChecked state t x y =
        if isChecked then
            let success = ref true
            let result = Calculator.MulChecked(x, y, t, success)
            if !success then MakeConcrete result t, state
            else
                let t, s = State.activator.CreateInstance typeof<System.OverflowException> [] state in
                Error t, s
        else
            MakeConcrete (Calculator.Mul(x, y, t)) t, state

    and private simplifyMultiplicationOfProduct state t a b y matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) ->
            let x, state = simplifyConcreteMultiplication false state t a y in
            simplifyMultiplication false state t x b matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, false, _), b, _ when c = y -> simplifyMultiplication false state t a b matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, false, _), _ when c = y -> simplifyMultiplication false state t a b matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, false, _) when d = a -> simplifyMultiplication false state t b c matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, false, _) when d = b -> simplifyMultiplication false state t a c matched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-factors
            let factorsOfY =
                match y with
                | Mul(c, d, false, yt) -> [(c, state); (d, state)]
                | _ -> [(y, state)]
            in
            simplifyPairwiseCombinations
                [(a, state); (b, state)]
                factorsOfY
                t
                fst
                (simplifyMultiplicationExt false state)
                (simplifyMultiplication false state t)
                (withSnd state >> matched)
                unmatched

    and private simplifyMultiplicationOfDivision state t a b y matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / b) * y = (a * y) / b if a and y are concrete and unckecked
        | Concrete(a, at), b, Concrete(y, yt) ->
            let aMulY, state = simplifyConcreteMultiplication false state t a y
            simplifyDivision false state t aMulY b matched
        // (a / y) * y = a if unchecked
        | a, b, y when b = y -> matched (a, state)
        // (a / (y * d)) * y = a/d if unchecked
        | a, Mul(c, d, false, _), y when c = y -> simplifyDivision false state t a d matched
        // (a / (c * y)) * y = a/c if unchecked
        | a, Mul(c, d, false, _), y when d = y -> simplifyDivision false state t a c matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfExpression state t x y matched unmatched =
        match x with
        | Mul(a, b, false, _) -> simplifyMultiplicationOfProduct state t a b y matched unmatched
        | Div(a, b, false, _) -> simplifyMultiplicationOfDivision state t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt isChecked state t x y matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> (MakeConcrete 0 t, state) |> matched
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> (MakeConcrete 0 t, state) |> matched
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, 1) -> matched (y, state)
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, 1) -> matched (x, state)
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, -1) -> simplifyUnaryMinus isChecked state t y matched
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus isChecked state t x matched
        | Expression _, Expression _ when not isChecked ->
            simplifyMultiplicationOfExpression state t x y matched (fun () ->
            simplifyMultiplicationOfExpression state t y x matched unmatched)
        | Expression _, _ when not isChecked -> simplifyMultiplicationOfExpression state t x y matched unmatched
        | _, Expression _ when not isChecked -> simplifyMultiplicationOfExpression state t y x matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplication isChecked state t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct isChecked state t (fst sorted) (snd sorted) k
        simplifyGenericBinary "product" state x y k
                              (fun x y tx ty state -> simplifyConcreteMultiplication isChecked state t x y)
                              (fun x y state k -> simplifyMultiplicationExt isChecked state t x y k defaultCase)
                              (fun x y state k -> simplifyMultiplication isChecked state t x y k)

// ------------------------------- Simplification of "/" -------------------------------

    and simplifyConcreteDivision isChecked state t x y =
        let success = ref true
        let result =
            if isChecked then
                Calculator.DivChecked(x, y, t, success)
            else
                Calculator.Div(x, y, t, success)
        if !success then MakeConcrete result t, state
        else
        let t, s =
            match result with
            | :? System.DivideByZeroException -> State.activator.CreateInstance typeof<System.DivideByZeroException> [] state
            | :? System.OverflowException -> State.activator.CreateInstance typeof<System.OverflowException> [] state
            | _ -> __notImplemented__()
        in Error t, s

    and private simplifyDivision isChecked state t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y) in
            makeProduct isChecked state t (fst sorted) (snd sorted) k
        in
        simplifyGenericBinary "division" state x y k
            (fun x y _ _ state -> simplifyConcreteDivision isChecked state t x y)
            (fun x y state k ->
                match x, y with
                // 0 / y = 0
                | Concrete(x, _), _ when Calculator.IsZero(x) -> (MakeConcrete 0 t, state) |> k
                // x / 1 = x
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, 1) -> (x, state) |> k
                // x / -1 = -x
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus isChecked state t x k
                // x / x = 1 if unchecked
                | x, y when not isChecked && x = y -> (MakeConcrete 1 t, state) |> k
                // x / -x = -1 if unchecked
                | x, UnaryMinus(y, false, _) when not isChecked && x = y -> (MakeConcrete -1 t, state) |> k
                // (a / b) / y = a / (b * y) if unchecked and b and y concrete
                | Div(a, Concrete(b, _), false, _), Concrete(y, _) when not isChecked ->
                    let bMulY, state = simplifyConcreteMultiplication false state t b y in
                    simplifyDivision false state t a bMulY k
                | _ -> (MakeBinary OperationType.Divide x y isChecked (Types.FromDotNetType t), state) |> k)
            (fun x y state k -> simplifyDivision isChecked state t x y k)

    and private simplifyDivisionAndCheckNotZero isChecked state t x y k =
        simplifyEqual y (Concrete(0, TypeOf y)) (fun yIsZero ->
            if Terms.IsFalse yIsZero then simplifyDivision isChecked state t x y k
            elif Terms.IsTrue yIsZero
            then State.activator.CreateInstance typeof<System.DivideByZeroException> [] state |> (fun (t, s) -> k (Error t, s))
            else
                let errorTerm, errorState = State.activator.CreateInstance typeof<System.DivideByZeroException> [] state in
                let y = Merging.merge2Terms !!yIsZero yIsZero y (Error errorTerm) in
                let state = Merging.merge2States !!yIsZero yIsZero state errorState in
                simplifyDivision isChecked state t x y k)

// ------------------------------- Simplification of "%" -------------------------------

    and simplifyConcreteRemainder isChecked state t x y =
        let success = ref true
        let result =
            if isChecked then
                Calculator.RemChecked(x, y, t, success)
            else
                Calculator.Rem(x, y, t, success)
        if !success then MakeConcrete result t, state
        else
        let t, s =
            match result with
            | :? System.DivideByZeroException -> State.activator.CreateInstance typeof<System.DivideByZeroException> [] state
            | :? System.OverflowException -> State.activator.CreateInstance typeof<System.OverflowException> [] state
            | _ -> __notImplemented__()
        in Error t, s


    and isRemainderZero state t x y =
        let success = ref true
        Calculator.IsZero(Calculator.Rem(x, y, t, success)) && !success

    and private simplifyRemainder isChecked state t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct isChecked state t (fst sorted) (snd sorted) k
        simplifyGenericBinary "remainder" state x y k
            (fun x y tx ty state -> simplifyConcreteRemainder isChecked state t x y)
            (fun x y state k ->
                match x, y with
                // 0 % y = 0
                | Concrete(x, _), _ when Calculator.IsZero(x) -> (MakeConcrete 0 t, state) |> k
                // x % 1 = 0
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, 1) -> (MakeConcrete 0 t, state) |> k
                // x % -1 = 0
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, -1) -> (MakeConcrete 0 t, state) |> k
                // x % x = 0
                | x, y when not isChecked && x = y -> (MakeConcrete 0 t, state) |> k
                // x % -x = 0 if unchecked
                | x, UnaryMinus(y, false, _) when not isChecked && x = y -> (MakeConcrete 0 t, state) |> k
                // (a * b) % y = 0 if unchecked, b and y concrete and a % y = 0
                | Mul(Concrete(a, _), b, false, _), Concrete(y, _) when not isChecked && isRemainderZero state t a y ->
                     (MakeConcrete 0 t, state) |> k
                | _ -> (MakeBinary OperationType.Remainder x y isChecked (Types.FromDotNetType t), state) |> k)
            (fun x y state k -> simplifyRemainder isChecked state t x y k)

    and private simplifyRemainderAndCheckNotZero isChecked state t x y k =
        simplifyEqual y (Concrete(0, TypeOf y)) (fun yIsZero ->
            if Terms.IsFalse yIsZero then simplifyRemainder isChecked state t x y k
            elif Terms.IsTrue yIsZero
            then State.activator.CreateInstance typeof<System.DivideByZeroException> [] state |> (fun (t, s) -> k (Error t, s))
            else
                let errorTerm, errorState = State.activator.CreateInstance typeof<System.DivideByZeroException> [] state in
                let y = Merging.merge2Terms !!yIsZero yIsZero y (Error errorTerm) in
                let state = Merging.merge2States !!yIsZero yIsZero state errorState in
                simplifyRemainder isChecked state t x y k)


// TODO: IMPLEMENT THE REST!

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and private simplifyConcreteComparison operator x y tx ty =
        MakeConcrete (Calculator.Compare(x, y) |> operator) typedefof<bool>

    and private simplifyComparison op x y concrete sameIsTrue k =
        simplifyGenericBinary "comparison" State.empty x y (fst >> k) concrete
            (fun x y s k ->
                match x, y with
                | x, y when x = y -> (MakeConcrete sameIsTrue typedefof<bool>, s) |> k
                | Add(Concrete(c, t), x, false, _), y when x = y ->
                    simplifyComparison op (Concrete(c, t)) (Concrete(0, t)) concrete sameIsTrue (withSnd s >> k)
                | x, Add(Concrete(c, t), y, false, _) when x = y ->
                    simplifyComparison op (Concrete(0, t)) (Concrete(c, t)) concrete sameIsTrue (withSnd s >> k)
                | _ -> (MakeBinary op x y false Bool, s) |> k)
            (fun x y state k -> simplifyComparison op x y concrete sameIsTrue (withSnd state >> k))

    and internal simplifyEqual x y k =
        simplifyComparison OperationType.Equal x y (fun x y tx ty s -> simplifyConcreteComparison ((=) 0) x y tx ty, s) true k
    and internal simplifyNotEqual x y k =
        simplifyComparison OperationType.Equal x y (fun x y tx ty s -> simplifyConcreteComparison ((=) 0) x y tx ty, s) true ((!!) >> k)
    and internal simplifyLess x y k =
        simplifyComparison OperationType.Less x y (fun x y tx ty s -> simplifyConcreteComparison ((>) 0) x y tx ty, s) false k
    and internal simplifyLessOrEqual x y k =
        simplifyComparison OperationType.LessOrEqual x y (fun x y tx ty s -> simplifyConcreteComparison ((>=) 0) x y tx ty, s) true k
    and internal simplifyGreater x y k =
        simplifyComparison OperationType.LessOrEqual x y (fun x y tx ty s -> simplifyConcreteComparison ((>=) 0) x y tx ty, s) true ((!!) >> k)
    and internal simplifyGreaterOrEqual x y k =
        simplifyComparison OperationType.Less x y (fun x y tx ty s -> simplifyConcreteComparison ((>) 0) x y tx ty, s) false ((!!) >> k)

// ------------------------------- General functions -------------------------------

    // WARNING: These operators are safe versions of concrete simplifyBinaryOperation.
    // Use them, only if you can guarantee that operation will complete without exceptions,
    // otherwise you should use simplifyBinaryOperation with state
    let (+++) x y =
        simplifyAddition false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let (---) x y =
        simplifySubtraction false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let ( *** ) x y =
        simplifyMultiplication false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let (%%%) x y =
        simplifyRemainder false State.empty (Types.ToDotNetType (Terms.TypeOf x)) x y fst

    let (===) x y =
        simplifyEqual x y id

    let internal simplifyBinaryOperation op state x y isChecked t k =
        match op with
        | OperationType.Add -> simplifyAddition isChecked state t x y k
        | OperationType.Subtract -> simplifySubtraction isChecked state t x y k
        | OperationType.Multiply -> simplifyMultiplication isChecked state t x y k
        | OperationType.Divide -> simplifyDivisionAndCheckNotZero isChecked state t x y k
        | OperationType.Remainder -> simplifyRemainderAndCheckNotZero isChecked state t x y k
        | OperationType.ShiftLeft
        | OperationType.ShiftRight -> raise(new System.NotImplementedException())
        | OperationType.Equal -> simplifyEqual x y (withSnd state >> k)
        | OperationType.NotEqual -> simplifyNotEqual x y (withSnd state >> k)
        | OperationType.Greater -> simplifyGreater x y (withSnd state >> k)
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual x y (withSnd state >> k)
        | OperationType.Less -> simplifyLess x y (withSnd state >> k)
        | OperationType.LessOrEqual -> simplifyLessOrEqual x y (withSnd state >> k)
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.NullCoalescing -> __notImplemented__()
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary arithmetic operator"))

    let internal simplifyUnaryOperation op state x isChecked t k =
        match op with
        | OperationType.LogicalNeg -> __notImplemented__()
        | OperationType.UnaryMinus -> simplifyUnaryMinus isChecked state t x k
        | OperationType.UnaryPlus -> k (x, state)
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary arithmetic operator"))

    let internal isArithmeticalOperation op t1 t2 =
        (Types.IsNumeric t1 || Types.IsBottom t1) && (Types.IsNumeric t2 || Types.IsBottom t2) &&
        match op with
        | OperationType.Add
        | OperationType.Subtract
        | OperationType.Multiply
        | OperationType.Divide
        | OperationType.Remainder
        | OperationType.ShiftLeft
        | OperationType.ShiftRight
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.Greater
        | OperationType.GreaterOrEqual
        | OperationType.Less
        | OperationType.LessOrEqual
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.NullCoalescing
        | OperationType.LogicalNeg
        | OperationType.UnaryMinus
        | OperationType.UnaryPlus -> true
        | _ -> false
