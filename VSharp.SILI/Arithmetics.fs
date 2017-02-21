namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Simplify
open VSharp.Terms

module internal Arithmetics =

    let private makeAddition isChecked t x y =
        MakeBinary OperationType.Add x y false (Types.FromPrimitiveDotNetType t)

    let private makeProduct isChecked t x y =
        MakeBinary OperationType.Multiply x y false (Types.FromPrimitiveDotNetType t)

// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition x y isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.AddChecked(x, y, t, success)
            if !success then MakeConcrete result t
            else Error (result :?> System.Exception)
        else
            MakeConcrete (Calculator.Add(x, y, t)) t

    let rec private simplifyAdditionToSum a b y state t matched unmatched =
        // Simplifying (a + b) + y at this step
        match a, b, y with
        // (a + b) + y = (a + y) + b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) -> simplifyAddition (simplifyConcreteAddition a y false t) b state false t matched
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
                | Add(c, d, false, yt) -> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations [a; b] summandsOfY state t simplifyAdditionExt (makeAddition false t) matched unmatched

    and private simplifyAdditionToUnaryMinus x y state t matched unmatched =
        // Simplifying (-x) + y at this step
        match x with
        // (-y) + y = 0
        | _ when x = y -> (MakeConcrete 0 t, state) |> matched
        // -(y + b) = -b
        | Add(a, b, false, _) when a = y -> matched (b, state)
        // -(a + y) = -a
        | Add(a, b, false, _) when b = y -> matched (a, state)
        | _ -> unmatched state

    and private simplifyAdditionToProduct a b y state t matched unmatched =
        // Simplifying (a * b) + y at this step
        match a, b, y with
        // (a * y) + y = (a + 1) * y if unckecked and a is concrete
        | Concrete(a, at), _, _ when b = y ->
            let aPlusOne = simplifyConcreteAddition a 1 false (Types.ToDotNetType at) in
                simplifyMultiplication aPlusOne y state false t matched
        // (a * b) + (c * b) = (a + c) * b if unchecked and a and c are concrete
        | Concrete(a, _), _, Mul(Concrete(c, _), d, false, _) when d = b ->
            let aPlusC = simplifyConcreteAddition a c false t in
                simplifyMultiplication aPlusC b state false t matched
        | _ -> unmatched state

    and private simplifyAdditionToExpression x y state t matched unmatched =
        match x with
        | Add(a, b, false, _) -> simplifyAdditionToSum a b y state t matched unmatched
        | UnaryMinus(x, false, _) -> simplifyAdditionToUnaryMinus x y state t matched unmatched
        | Mul(a, b, false, _) -> simplifyAdditionToProduct a b y state t matched unmatched
        | _ -> unmatched state

    and private simplifyAdditionExt x y state isChecked t matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), Concrete(y, typeOfY) -> (simplifyConcreteAddition x y isChecked t, state) |> matched
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> matched (y, state)
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> matched (x, state)
        | Expression _, Expression _ when not isChecked ->
            simplifyAdditionToExpression x y state t matched (fun state ->
            simplifyAdditionToExpression y x state t matched unmatched)
        | Expression _, _ when not isChecked -> simplifyAdditionToExpression x y state t matched unmatched
        | _, Expression _ when not isChecked -> simplifyAdditionToExpression y x state t matched unmatched
        | _ -> unmatched state

    and private simplifyAddition x y state isChecked t k =
        let defaultCase state =
            let sorted = if (IsConcrete y) then (y, x) else (x, y) in
                (makeAddition isChecked t (fst sorted) (snd sorted), state) |> k
        simplifyGenericBinary "addition" x y state k
                              (fun x y _ _ -> simplifyConcreteAddition x y isChecked t)
                              (fun x y state k -> simplifyAdditionExt x y state isChecked t k defaultCase)

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus x isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.UnaryMinusChecked(x, t, success)
            if !success then MakeConcrete result t
            else Error (result :?> System.Exception)
        else
            MakeConcrete (Calculator.UnaryMinus(x, t)) t

    and private simplifyUnaryMinus x state isChecked t k =
        simplifyGenericUnary "unary minus" x state k (fun x _ -> simplifyConcreteUnaryMinus x isChecked t) (fun x state k ->
        match x with
        // -(-(x)) = x if both unchecked
        | UnaryMinus(x, false, t) when not isChecked -> (x, state) |> k
        // -(a + b) = (-a) + (-b) if all unchecked
        | Add(a, b, false, _) when not isChecked ->
            simplifyUnaryMinus a state false t (fun (minusA, state) ->
            simplifyUnaryMinus b state false t (fun (minusB, state) ->
            simplifyAddition minusA minusB state false t k))
         // -(a * x) = (-a) * x if both unchecked
        | Mul(Concrete(a, at), y, false, _) when not isChecked ->
            simplifyUnaryMinus (Concrete(a, at)) state isChecked (Types.ToDotNetType at) (fun (minusA, state) ->
            simplifyMultiplication minusA y state false t k)
        | _ -> (MakeUnary OperationType.UnaryMinus x isChecked (Types.FromPrimitiveDotNetType t), state) |> k)

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication x y isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.MulChecked(x, y, t, success)
            if !success then MakeConcrete result t
            else Error (result :?> System.Exception)
        else
            MakeConcrete (Calculator.Mul(x, y, t)) t

    and private simplifyMultiplicationOfProduct a b y state t matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) -> simplifyMultiplication (simplifyConcreteMultiplication a y false t) b state false t matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, false, _), b, _ when c = y -> simplifyMultiplication a b state false t matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, false, _), _ when c = y -> simplifyMultiplication a b state false t matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, false, _) when d = a -> simplifyMultiplication b c state false t matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, false, _) when d = b -> simplifyMultiplication a c state false t matched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-factors
            let factorsOfY =
                match y with
                | Mul(c, d, false, yt) -> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations [a; b] factorsOfY state t simplifyMultiplicationExt (makeProduct false t) matched unmatched

    and private simplifyMultiplicationOfDivision a b y state t matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / b) * y = (a * y) / b if a and y are concrete and unckecked
        | Concrete(a, at), b, Concrete(y, yt) ->
            let aMulY = simplifyConcreteMultiplication a y false t
            simplifyDivision aMulY b state false t matched
        // (a / y) * y = a if unchecked
        | a, b, y when b = y -> matched (a, state)
        // (a / (y * d)) * y = a/d if unchecked
        | a, Mul(c, d, false, _), y when c = y -> simplifyDivision a d state false t matched
        // (a / (c * y)) * y = a/c if unchecked
        | a, Mul(c, d, false, _), y when d = y -> simplifyDivision a c state false t matched
        | _ -> unmatched state

    and private simplifyMultiplicationOfExpression x y state t matched unmatched =
        match x with
        | Mul(a, b, false, _) -> simplifyMultiplicationOfProduct a b y state t matched unmatched
        | Div(a, b, false, _) -> simplifyMultiplicationOfDivision a b y state t matched unmatched
        | _ -> unmatched state

    and private simplifyMultiplicationExt x y state isChecked t matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> matched (MakeConcrete 0 t, state)
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> matched (MakeConcrete 0 t, state)
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, 1) -> matched (y, state)
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, 1) -> matched (x, state)
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, -1) -> simplifyUnaryMinus y state isChecked t matched
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus x state isChecked t matched
        | Expression _, Expression _ when not isChecked ->
            simplifyMultiplicationOfExpression x y state t matched (fun state ->
            simplifyMultiplicationOfExpression y x state t matched unmatched)
        | Expression _, _ when not isChecked -> simplifyMultiplicationOfExpression x y state t matched unmatched
        | _, Expression _ when not isChecked -> simplifyMultiplicationOfExpression y x state t matched unmatched
        | _ -> unmatched state

    and private simplifyMultiplication x y state isChecked t k =
        let defaultCase state =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            (makeProduct isChecked t (fst sorted) (snd sorted), state) |> k
        simplifyGenericBinary "product" x y state k
                              (fun x y _ _ -> simplifyConcreteMultiplication x y isChecked t)
                              (fun x y state k -> simplifyMultiplicationExt x y state isChecked t k defaultCase)

// ------------------------------- Simplification of "/" -------------------------------

    and simplifyConcreteDivision x y isChecked t =
        let success = ref true
        let result =
            if isChecked then Calculator.DivChecked(x, y, t, success)
            else Calculator.Div(x, y, t, success)
        if !success then MakeConcrete result t
        else Error (result :?> System.Exception)

    and private simplifyDivision x y state isChecked t k =
        let defaultCase state =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            (makeProduct isChecked t (fst sorted) (snd sorted), state) |> k
        simplifyGenericBinary "division" x y state k (fun x y _ _ -> simplifyConcreteDivision x y isChecked t) (fun x y state k ->
        match x, y with
        // 0 / y = 0
        | Concrete(x, _), _ when Calculator.IsZero(x) -> (MakeConcrete 0 t, state) |> k
        // x / 1 = x
        | x, Concrete(y, _) when Calculator.FuzzyEqual(y, 1) -> (x, state) |> k
        // x / -1 = -x
        | x, Concrete(y, _) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus x state isChecked t k
        // x / x = 1 if unchecked
        | x, y when not isChecked && x = y -> (MakeConcrete 1 t, state) |> k
        // x / -x = -1 if unchecked
        | x, UnaryMinus(y, false, _) when not isChecked && x = y -> (MakeConcrete -1 t, state) |> k
        // (a / b) / y = a / (b * y) if unchecked and b and y concrete
        | Div(a, Concrete(b, _), false, _), Concrete(y, _) when not isChecked ->
            let bMulY = simplifyConcreteMultiplication b y false t in
            simplifyDivision a bMulY state false t k
        | _ -> (MakeBinary OperationType.Divide x y isChecked (Types.FromPrimitiveDotNetType t), state) |> k)

    and private simplifyDivisionAndUpdateState x y state isChecked t k =
        simplifyNotEqual y (Concrete(0, TypeOf y)) state (fun (yIsNotZero, state) ->
        simplifyDivision x y state isChecked t (fun (d, state) ->
        k (d, State.addAssertion state yIsNotZero)))

// ------------------------------- Simplification of "%" -------------------------------

    and simplifyConcreteRemainder x y isChecked t =
        let success = ref true
        let result =
            if isChecked then Calculator.RemChecked(x, y, t, success)
            else Calculator.Rem(x, y, t, success)
        if !success then MakeConcrete result t
        else Error (result :?> System.Exception)

    and isRemainderZero x y t =
        let success = ref true
        Calculator.IsZero(Calculator.Rem(x, y, t, success)) && !success

    and private simplifyRemainder x y state isChecked t k =
        let defaultCase state =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            (makeProduct isChecked t (fst sorted) (snd sorted), state) |> k
        simplifyGenericBinary "remainder" x y state k (fun x y _ _ -> simplifyConcreteRemainder x y isChecked t) (fun x y state k ->
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
        | Mul(Concrete(a, _), b, false, _), Concrete(y, _) when not isChecked && isRemainderZero a y t ->
             (MakeConcrete 0 t, state) |> k
        // (if a then b else c) % y = (if a then (b%y) else (c%y)) if unchecked and b, c and y concrete
        | If(a, Concrete(b, _), Concrete(c, _), _), Concrete(y, _) when not isChecked ->
            let bModY = simplifyConcreteRemainder b y false t in
            let cModY = simplifyConcreteRemainder c y false t in
                // TODO: merge instead of Cond
                (Expression(Cond, [a; bModY; cModY], Types.FromPrimitiveDotNetType t), state) |> k
        // x % (if a then b else c) = (if a then (x%a) else (x%b)) if unchecked and x, b and c concrete
        | Concrete(x, _), If(a, Concrete(b, _), Concrete(c, _), _) when not isChecked ->
            let xModB = simplifyConcreteRemainder x b false t in
            let xModC = simplifyConcreteRemainder x c false t in
                // TODO: merge instead of Cond
                (Expression(Cond, [a; xModB; xModC], Types.FromPrimitiveDotNetType t), state) |> k
        | _ -> (MakeBinary OperationType.Remainder x y isChecked (Types.FromPrimitiveDotNetType t), state) |> k)

    and private simplifyRemainderAndUpdateState x y state isChecked t k =
        simplifyNotEqual y (Concrete(0, TypeOf y)) state (fun (yIsNotZero, state) ->
        simplifyRemainder x y state isChecked t (fun (d, state) ->
        k (d, State.addAssertion state yIsNotZero)))

// TODO: IMPLEMENT THE REST!

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and private simplifyConcreteComparison operator x y tx ty =
        MakeConcrete (Calculator.Compare(x, y) |> operator) typedefof<bool>

    and private simplifyComparison op x y state concrete sameIsTrue k =
        simplifyGenericBinary "comparison" x y state k concrete (fun x y state k ->
        match x, y with
        | x, y when x = y -> (MakeConcrete sameIsTrue typedefof<bool>, state) |> k
        | Add(Concrete(c, t), x, false, _), y when x = y -> simplifyComparison op (Concrete(c, t)) (Concrete(0, t)) state concrete sameIsTrue k
        | x, Add(Concrete(c, t), y, false, _) when x = y -> simplifyComparison op (Concrete(0, t)) (Concrete(c, t)) state concrete sameIsTrue k
        | _ -> (MakeBinary op x y false Bool, state) |> k)

    and private negate k (x, state) = k (!!x, state)

    and private simplifyEqual x y state k =
        simplifyComparison OperationType.Equal x y state (simplifyConcreteComparison ((=) 0)) true k
    and private simplifyNotEqual x y state k =
        simplifyComparison OperationType.Equal x y state (simplifyConcreteComparison ((=) 0)) true (negate k)
    and private simplifyLess x y state k =
        simplifyComparison OperationType.Less x y state (simplifyConcreteComparison ((>) 0)) false k
    and private simplifyLessOrEqual x y state k =
        simplifyComparison OperationType.LessOrEqual x y state (simplifyConcreteComparison ((>=) 0)) true k
    and private simplifyGreater x y state k =
        simplifyComparison OperationType.LessOrEqual x y state (simplifyConcreteComparison ((>=) 0)) true (negate k)
    and private simplifyGreaterOrEqual x y state k =
        simplifyComparison OperationType.Less x y state (simplifyConcreteComparison ((>) 0)) false (negate k)

// ------------------------------- General functions -------------------------------

    let private withState state x = (x, state)

    let internal simplifyBinaryOperation op x y state isChecked t k =
        match op with
        | OperationType.Add -> simplifyAddition x y state isChecked t k
        | OperationType.Subtract ->
            simplifyUnaryMinus y state false t (fun (minusY, state) ->
            simplifyAddition x minusY state isChecked t k)
        | OperationType.Multiply -> simplifyMultiplication x y state isChecked t k
        | OperationType.Divide -> simplifyDivisionAndUpdateState x y state isChecked t k
        | OperationType.Remainder -> simplifyRemainderAndUpdateState x y state isChecked t k
        | OperationType.ShiftLeft
        | OperationType.ShiftRight -> raise(new System.NotImplementedException())
        | OperationType.Equal -> simplifyEqual x y state k
        | OperationType.NotEqual -> simplifyNotEqual x y state k
        | OperationType.Greater -> simplifyGreater x y state k
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual x y state k
        | OperationType.Less -> simplifyLess x y state k
        | OperationType.LessOrEqual -> simplifyLessOrEqual x y state k
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.NullCoalescing -> raise(new System.NotImplementedException())
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary arithmetic operator"))

    let internal simplifyUnaryOperation op x state isChecked t k =
        match op with
        | OperationType.LogicalNeg -> raise(new System.NotImplementedException())
        | OperationType.UnaryMinus -> simplifyUnaryMinus x state isChecked t k
        | OperationType.UnaryPlus -> k (x, state)
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not an unary arithmetic operator"))

    let internal isArithmeticalOperation op t1 t2 =
        Types.IsNumeric t1 && Types.IsNumeric t2 &&
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
