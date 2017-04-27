namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Simplify
open VSharp.Terms

[<AutoOpen>]
module internal Arithmetics =

    let private makeAddition isChecked t x y k =
        MakeBinary OperationType.Add x y isChecked (Types.FromDotNetType t) |> k

    let private makeProduct isChecked t x y k =
        MakeBinary OperationType.Multiply x y isChecked (Types.FromDotNetType t) |> k

// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition isChecked t x y =
        if isChecked then
            let success = ref true
            let result = Calculator.AddChecked(x, y, t, success)
            if !success then MakeConcrete result t
            else Terms.MakeError result
        else
            MakeConcrete (Calculator.Add(x, y, t)) t

    let rec private simplifyAdditionToSum t a b y matched unmatched =
        // Simplifying (a + b) + y at this step
        match a, b, y with
        // (a + b) + y = (a + y) + b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) -> simplifyAddition false t (simplifyConcreteAddition false t a y) b matched
        // ((-y) + b) + y = b if unchecked
        | UnaryMinus(a, false, _), b, _ when a = y -> matched b
        // (a + (-y)) + y = a if unchecked
        | a, UnaryMinus(b, false, _), _ when b = y -> matched a
        // (a + b) + (-a) = b if unchecked
        | _, _, UnaryMinus(y, false, _) when a = y -> matched b
        // (a + b) + (-b) = a if unchecked
        | _, _, UnaryMinus(y, false, _) when b = y -> matched a
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-summands
            let summandsOfY =
                match y with
                | Add(c, d, false, yt) -> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations [a; b] summandsOfY t simplifyAdditionExt (simplifyAddition false t) matched unmatched

    and private simplifyAdditionToUnaryMinus t x y matched unmatched =
        // Simplifying (-x) + y at this step
        match x with
        // (-y) + y = 0
        | _ when x = y -> MakeConcrete 0 t |> matched
        // -(y + b) = -b
        | Add(a, b, false, _) when a = y -> matched b
        // -(a + y) = -a
        | Add(a, b, false, _) when b = y -> matched a
        | _ -> unmatched ()

    and private simplifyAdditionToProduct t a b y matched unmatched =
        // Simplifying (a * b) + y at this step
        match a, b, y with
        // (a * y) + y = (a + 1) * y if unckecked and a is concrete
        | Concrete(a, at), _, _ when b = y ->
            let aPlusOne = simplifyConcreteAddition false (Types.ToDotNetType at) a 1 in
            simplifyMultiplication false t aPlusOne y matched
        // (a * b) + (c * b) = (a + c) * b if unchecked and a and c are concrete
        | Concrete(a, _), _, Mul(Concrete(c, _), d, false, _) when d = b ->
            let aPlusC = simplifyConcreteAddition false t a c in
            simplifyMultiplication false t aPlusC b matched
        | _ -> unmatched ()

    and private simplifyAdditionToExpression x y t matched unmatched =
        match x with
        | Add(a, b, false, _) -> simplifyAdditionToSum t a b y matched unmatched
        | UnaryMinus(x, false, _) -> simplifyAdditionToUnaryMinus t x y matched unmatched
        | Mul(a, b, false, _) -> simplifyAdditionToProduct t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyAdditionExt isChecked t x y matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), Concrete(y, typeOfY) -> simplifyConcreteAddition isChecked t x y |> matched
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> matched y
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> matched x
        | Expression _, Expression _ when not isChecked ->
            simplifyAdditionToExpression x y t matched (fun () ->
            simplifyAdditionToExpression y x t matched unmatched)
        | Expression _, _ when not isChecked -> simplifyAdditionToExpression x y t matched unmatched
        | _, Expression _ when not isChecked -> simplifyAdditionToExpression y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyAddition isChecked t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y) in
                makeAddition isChecked t (fst sorted) (snd sorted) k
        simplifyGenericBinary "addition" x y k
                              (fun x y _ _ -> simplifyConcreteAddition isChecked t x y)
                              (fun x y k -> simplifyAdditionExt isChecked t x y k defaultCase)
                              (fun x y k -> simplifyAddition isChecked t x y k)

    and private simplifySubtraction isChecked t x y k =
        simplifyUnaryMinus isChecked t y (fun minusY ->
        simplifyAddition isChecked t x minusY k)

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus isChecked t x =
        if isChecked then
            let success = ref true
            let result = Calculator.UnaryMinusChecked(x, t, success)
            if !success then MakeConcrete result t
            else Terms.MakeError result
        else
            MakeConcrete (Calculator.UnaryMinus(x, t)) t

    and private simplifyUnaryMinus isChecked t x k =
        simplifyGenericUnary "unary minus" x k (fun x _ -> simplifyConcreteUnaryMinus isChecked t x) (fun x k ->
        match x with
        // -(-(x)) = x if both unchecked
        | UnaryMinus(x, false, t) when not isChecked -> k x
        // -(a + b) = (-a) + (-b) if all unchecked
        | Add(a, b, false, _) when not isChecked ->
            simplifyUnaryMinus false t a (fun minusA ->
            simplifyUnaryMinus false t b (fun minusB ->
            simplifyAddition false t minusA minusB k))
         // -(a * x) = (-a) * x if both unchecked
        | Mul(Concrete(a, at), y, false, _) when not isChecked ->
            simplifyUnaryMinus isChecked (Types.ToDotNetType at) (Concrete(a, at)) (fun minusA ->
            simplifyMultiplication false t minusA y k)
        | _ -> MakeUnary OperationType.UnaryMinus x isChecked (Types.FromDotNetType t) |> k)

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication isChecked t x y =
        if isChecked then
            let success = ref true
            let result = Calculator.MulChecked(x, y, t, success)
            if !success then MakeConcrete result t
            else Terms.MakeError result
        else
            MakeConcrete (Calculator.Mul(x, y, t)) t

    and private simplifyMultiplicationOfProduct t a b y matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) -> simplifyMultiplication false t (simplifyConcreteMultiplication false t a y) b matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, false, _), b, _ when c = y -> simplifyMultiplication false t a b matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, false, _), _ when c = y -> simplifyMultiplication false t a b matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, false, _) when d = a -> simplifyMultiplication false t b c matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, false, _) when d = b -> simplifyMultiplication false t a c matched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-factors
            let factorsOfY =
                match y with
                | Mul(c, d, false, yt) -> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations [a; b] factorsOfY t simplifyMultiplicationExt (simplifyMultiplication false t) matched unmatched

    and private simplifyMultiplicationOfDivision t a b y matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / b) * y = (a * y) / b if a and y are concrete and unckecked
        | Concrete(a, at), b, Concrete(y, yt) ->
            let aMulY = simplifyConcreteMultiplication false t a y
            simplifyDivision false t aMulY b matched
        // (a / y) * y = a if unchecked
        | a, b, y when b = y -> matched a
        // (a / (y * d)) * y = a/d if unchecked
        | a, Mul(c, d, false, _), y when c = y -> simplifyDivision false t a d matched
        // (a / (c * y)) * y = a/c if unchecked
        | a, Mul(c, d, false, _), y when d = y -> simplifyDivision false t a c matched
        | _ -> unmatched ()

    and private simplifyMultiplicationOfExpression t x y matched unmatched =
        match x with
        | Mul(a, b, false, _) -> simplifyMultiplicationOfProduct t a b y matched unmatched
        | Div(a, b, false, _) -> simplifyMultiplicationOfDivision t a b y matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt isChecked t x y matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> MakeConcrete 0 t |> matched
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> MakeConcrete 0 t |> matched
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, 1) -> matched y
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, 1) -> matched x
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, -1) -> simplifyUnaryMinus isChecked t y matched
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus isChecked t x matched
        | Expression _, Expression _ when not isChecked ->
            simplifyMultiplicationOfExpression t x y matched (fun () ->
            simplifyMultiplicationOfExpression t y x matched unmatched)
        | Expression _, _ when not isChecked -> simplifyMultiplicationOfExpression t x y matched unmatched
        | _, Expression _ when not isChecked -> simplifyMultiplicationOfExpression t y x matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplication isChecked t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct isChecked t (fst sorted) (snd sorted) k
        simplifyGenericBinary "product" x y k
                              (fun x y _ _ -> simplifyConcreteMultiplication isChecked t x y)
                              (fun x y k -> simplifyMultiplicationExt isChecked t x y k defaultCase)
                              (fun x y k -> simplifyMultiplication isChecked t x y k)

// ------------------------------- Simplification of "/" -------------------------------

    and simplifyConcreteDivision isChecked t x y =
        let success = ref true
        let result =
            if isChecked then Calculator.DivChecked(x, y, t, success)
            else Calculator.Div(x, y, t, success)
        if !success then MakeConcrete result t
        else Terms.MakeError result

    and private simplifyDivision isChecked t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y) in
            makeProduct isChecked t (fst sorted) (snd sorted) k
        in
        simplifyGenericBinary "division" x y k
            (fun x y _ _ -> simplifyConcreteDivision isChecked t x y)
            (fun x y k ->
                match x, y with
                // 0 / y = 0
                | Concrete(x, _), _ when Calculator.IsZero(x) -> MakeConcrete 0 t |> k
                // x / 1 = x
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, 1) -> x |> k
                // x / -1 = -x
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus isChecked t x k
                // x / x = 1 if unchecked
                | x, y when not isChecked && x = y -> MakeConcrete 1 t |> k
                // x / -x = -1 if unchecked
                | x, UnaryMinus(y, false, _) when not isChecked && x = y -> MakeConcrete -1 t |> k
                // (a / b) / y = a / (b * y) if unchecked and b and y concrete
                | Div(a, Concrete(b, _), false, _), Concrete(y, _) when not isChecked ->
                    let bMulY = simplifyConcreteMultiplication false t b y in
                    simplifyDivision false t a bMulY k
                | _ -> MakeBinary OperationType.Divide x y isChecked (Types.FromDotNetType t) |> k)
            (fun x y k -> simplifyDivision isChecked t x y k)

    and private simplifyDivisionAndCheckNotZero isChecked t x y k =
        simplifyEqual y (Concrete(0, TypeOf y)) (fun yIsZero ->
        simplifyDivision isChecked t x y (fun d ->
        Merging.merge2Terms yIsZero !!yIsZero (Terms.MakeError (new System.DivideByZeroException())) d |> k))

// ------------------------------- Simplification of "%" -------------------------------

    and simplifyConcreteRemainder isChecked t x y =
        let success = ref true
        let result =
            if isChecked then Calculator.RemChecked(x, y, t, success)
            else Calculator.Rem(x, y, t, success)
        if !success then MakeConcrete result t
        else Terms.MakeError result

    and isRemainderZero t x y =
        let success = ref true
        Calculator.IsZero(Calculator.Rem(x, y, t, success)) && !success

    and private simplifyRemainder isChecked t x y k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct isChecked t (fst sorted) (snd sorted) k
        simplifyGenericBinary "remainder" x y k
            (fun x y _ _ -> simplifyConcreteRemainder isChecked t x y)
            (fun x y k ->
                match x, y with
                // 0 % y = 0
                | Concrete(x, _), _ when Calculator.IsZero(x) -> MakeConcrete 0 t |> k
                // x % 1 = 0
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, 1) -> MakeConcrete 0 t |> k
                // x % -1 = 0
                | x, Concrete(y, _) when Calculator.FuzzyEqual(y, -1) -> MakeConcrete 0 t |> k
                // x % x = 0
                | x, y when not isChecked && x = y -> MakeConcrete 0 t |> k
                // x % -x = 0 if unchecked
                | x, UnaryMinus(y, false, _) when not isChecked && x = y -> MakeConcrete 0 t |> k
                // (a * b) % y = 0 if unchecked, b and y concrete and a % y = 0
                | Mul(Concrete(a, _), b, false, _), Concrete(y, _) when not isChecked && isRemainderZero t a y ->
                     MakeConcrete 0 t |> k
                | _ -> MakeBinary OperationType.Remainder x y isChecked (Types.FromDotNetType t) |> k)
            (fun x y k -> simplifyRemainder isChecked t x y k)

    and private simplifyRemainderAndCheckNotZero isChecked t x y k =
        simplifyEqual y (Concrete(0, TypeOf y)) (fun yIsZero ->
        simplifyRemainder isChecked t x y (fun d ->
        (Merging.merge2Terms yIsZero !!yIsZero (Terms.MakeError (new System.DivideByZeroException())) d) |> k))

// TODO: IMPLEMENT THE REST!

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and private simplifyConcreteComparison operator x y tx ty =
        MakeConcrete (Calculator.Compare(x, y) |> operator) typedefof<bool>

    and private simplifyComparison op x y concrete sameIsTrue k =
        simplifyGenericBinary "comparison" x y k concrete
            (fun x y k ->
                match x, y with
                | x, y when x = y -> MakeConcrete sameIsTrue typedefof<bool> |> k
                | Add(Concrete(c, t), x, false, _), y when x = y -> simplifyComparison op (Concrete(c, t)) (Concrete(0, t)) concrete sameIsTrue k
                | x, Add(Concrete(c, t), y, false, _) when x = y -> simplifyComparison op (Concrete(0, t)) (Concrete(c, t)) concrete sameIsTrue k
                | _ -> MakeBinary op x y false Bool |> k)
            (fun x y k -> simplifyComparison op x y concrete sameIsTrue k)

    and internal simplifyEqual x y k =
        simplifyComparison OperationType.Equal x y (simplifyConcreteComparison ((=) 0)) true k
    and internal simplifyNotEqual x y k =
        simplifyComparison OperationType.Equal x y (simplifyConcreteComparison ((=) 0)) true ((!!) >> k)
    and internal simplifyLess x y k =
        simplifyComparison OperationType.Less x y (simplifyConcreteComparison ((>) 0)) false k
    and internal simplifyLessOrEqual x y k =
        simplifyComparison OperationType.LessOrEqual x y (simplifyConcreteComparison ((>=) 0)) true k
    and internal simplifyGreater x y k =
        simplifyComparison OperationType.LessOrEqual x y (simplifyConcreteComparison ((>=) 0)) true ((!!) >> k)
    and internal simplifyGreaterOrEqual x y k =
        simplifyComparison OperationType.Less x y (simplifyConcreteComparison ((>) 0)) false ((!!) >> k)

// ------------------------------- General functions -------------------------------

    let (+++) x y =
        simplifyAddition false (Types.ToDotNetType (Terms.TypeOf x)) x y id

    let (---) x y =
        simplifySubtraction false (Types.ToDotNetType (Terms.TypeOf x)) x y id

    let ( *** ) x y =
        simplifyMultiplication false (Types.ToDotNetType (Terms.TypeOf x)) x y id

    let internal simplifyBinaryOperation op x y isChecked t k =
        match op with
        | OperationType.Add -> simplifyAddition isChecked t x y k
        | OperationType.Subtract -> simplifySubtraction isChecked t x y k
        | OperationType.Multiply -> simplifyMultiplication isChecked t x y k
        | OperationType.Divide -> simplifyDivisionAndCheckNotZero isChecked t x y k
        | OperationType.Remainder -> simplifyRemainderAndCheckNotZero isChecked t x y k
        | OperationType.ShiftLeft
        | OperationType.ShiftRight -> raise(new System.NotImplementedException())
        | OperationType.Equal -> simplifyEqual x y k
        | OperationType.NotEqual -> simplifyNotEqual x y k
        | OperationType.Greater -> simplifyGreater x y k
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual x y k
        | OperationType.Less -> simplifyLess x y k
        | OperationType.LessOrEqual -> simplifyLessOrEqual x y k
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.NullCoalescing -> __notImplemented__()
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary arithmetic operator"))

    let internal simplifyUnaryOperation op x isChecked t k =
        match op with
        | OperationType.LogicalNeg -> __notImplemented__()
        | OperationType.UnaryMinus -> simplifyUnaryMinus isChecked t x k
        | OperationType.UnaryPlus -> k x
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
