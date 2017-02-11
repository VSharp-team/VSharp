namespace VSharp.Core.Symbolic

open JetBrains.Decompiler.Ast
open VSharp.CSharpUtils
open VSharp.Core.Symbolic
open VSharp.Core.Symbolic.Simplify
open VSharp.Core.Symbolic.Terms
open VSharp.Core.Symbolic.Propositional

module internal Arithmetics =

    let private makeAddition isChecked t x y =
        MakeBinary OperationType.Add x y false (Types.FromPrimitiveDotNetType t)

    let private makeProduct isChecked t x y =
        MakeBinary OperationType.Multiply x y false (Types.FromPrimitiveDotNetType t)

    // Trying to simplify pairwise combinations of x- and y-operands.
    // For example, it tries to simplify (a + b) + (c + d) or (a * b) * (c * d)
    // by successively trying to combine (a * c), (a * d), (b * c) and (b * d).
    let private simplifyPairwiseCombinations xs ys t simplify reduce matched unmatched =
        let initialYs = ys

        let rec combineOne x ys failed cps =
            match ys with
            | [] -> cps x failed
            | h :: tl ->
                simplify x h false t
                    (fun x ->  combineOne x tl failed cps)
                    (fun () -> combineOne x tl (h :: failed) cps)

        let rec combine xs ys acc =
            match xs with
            | [] ->
                // Here we traversed all xs, checking for something matched...
                if List.length ys = List.length initialYs then unmatched () // Nothing matched, the whole process is failed
                else
                    // Something matched, the work is done, just combining results together...
                    let toReduce = List.append (List.rev acc) ys
                    // TODO: care about different types...
                    toReduce |> List.reduce reduce |> matched
            | x :: xs ->
                combineOne x ys [] (fun res ys -> combine xs ys (res :: acc))

        combine xs ys []

// ------------------------------- Simplification of "+" -------------------------------

    let private simplifyConcreteAddition x y isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.AddChecked(x, y, t, success)
            if !success then MakeConcrete result t
            else Error (result :?> System.Exception)
        else
            MakeConcrete (Calculator.Add(x, y, t)) t

    let rec private simplifyAdditionToSum a b y t matched unmatched =
        // Simplifying (a + b) + y at this step
        match a, b, y with
        // (a + b) + y = (a + y) + b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) -> simplifyAddition (simplifyConcreteAddition a y false t) b false t matched
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
            simplifyPairwiseCombinations [a; b] summandsOfY t simplifyAdditionExt (makeAddition false t) matched unmatched

    and private simplifyAdditionToUnaryMinus x y t matched unmatched =
        // Simplifying (-x) + y at this step
        match x with
        // (-y) + y = 0
        | _ when x = y -> MakeConcrete 0 t |> matched
        // -(y + b) = -b
        | Add(a, b, false, _) when a = y -> matched b
        // -(a + y) = -a
        | Add(a, b, false, _) when b = y -> matched a
        | _ -> unmatched ()

    and private simplifyAdditionToProduct a b y t matched unmatched =
        // Simplifying (a * b) + y at this step
        match a, b, y with
        // (a * y) + y = (a + 1) * y if unckecked and a is concrete
        | Concrete(a, at), _, _ when b = y ->
            let aPlusOne = simplifyConcreteAddition a 1 false (Types.ToDotNetType at) in
                simplifyMultiplication aPlusOne y false t matched
        // (a * b) + (c * b) = (a + c) * b if unchecked and a and c are concrete
        | Concrete(a, _), _, Mul(Concrete(c, _), d, false, _) when d = b ->
            let aPlusC = simplifyConcreteAddition a c false t in
                simplifyMultiplication (MakeConcrete aPlusC t) b false t matched
        | _ -> unmatched ()

//    and private simplifyAdditionToCondition a b c y t matched unmatched =
//        // Simplifying (if c then a else b) + y at this step
//        match a, b, y with
//        // (if c then a else b) + y = (if c then (a + y) else (b + y)) when a, b and y are concrete
//        | Concrete(a, at), Concrete(b, bt), Concrete(y, yt) ->
//            let aPlusY = simplifyConcreteAddition a y false (Types.ToDotNetType at)
//            let bPlusY = simplifyConcreteAddition b y false (Types.ToDotNetType bt)
//            // TODO: Merge it?
//            Expression(Cond, [c; aPlusY; bPlusY], Types.FromPrimitiveDotNetType t) |> matched
//        | _ -> unmatched ()

    and private simplifyAdditionToExpression x y t matched unmatched =
        match x with
        | Add(a, b, false, _) -> simplifyAdditionToSum a b y t matched unmatched
        | UnaryMinus(x, false, _) -> simplifyAdditionToUnaryMinus x y t matched unmatched
        | Mul(a, b, false, _) -> simplifyAdditionToProduct a b y t matched unmatched
//        | If(a, b, c, _) -> simplifyAdditionToCondition a b c y t matched unmatched
        | _ -> unmatched ()

    and private simplifyAdditionExt x y isChecked t matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), Concrete(y, typeOfY) -> simplifyConcreteAddition x y isChecked t |> matched
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> matched y
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> matched x
        | Expression _, Expression _ when not isChecked ->
            simplifyAdditionToExpression x y t matched (fun () ->
            simplifyAdditionToExpression y x t matched unmatched)
        | Expression _, _ when not isChecked -> simplifyAdditionToExpression x y t matched unmatched
        | _, Expression _ when not isChecked -> simplifyAdditionToExpression y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyAddition x y isChecked t k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y) in
                makeAddition isChecked t (fst sorted) (snd sorted) |> k
        simplifyGenericBinary "addition" x y k
                              (fun x y _ _ -> simplifyConcreteAddition x y isChecked t)
                              (fun x y k -> simplifyAdditionExt x y isChecked t k defaultCase)

// ------------------------------- Simplification of unary "-" -------------------------------

    and private simplifyConcreteUnaryMinus x isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.UnaryMinusChecked(x, t, success)
            if !success then MakeConcrete result t
            else Error (result :?> System.Exception)
        else
            MakeConcrete (Calculator.UnaryMinus(x, t)) t

    and private simplifyUnaryMinus x isChecked t k =
        simplifyGenericUnary "unary minus" x k (fun x _ -> simplifyConcreteUnaryMinus x isChecked t) (fun x k ->
        match x with
        // -(-(x)) = x if both unchecked
        | UnaryMinus(x, false, t) when not isChecked -> x |> k
        // -(a + b) = (-a) + (-b) if all unchecked
        | Add(a, b, false, _) when not isChecked ->
            simplifyUnaryMinus a false t (fun minusA ->
            simplifyUnaryMinus b false t (fun minusB ->
            simplifyAddition minusA minusB false t k))
         // -(a * x) = (-a) * x if both unchecked
        | Mul(Concrete(a, at), y, false, _) when not isChecked ->
            simplifyUnaryMinus (Concrete(a, at)) isChecked (Types.ToDotNetType at) (fun minusA ->
            simplifyMultiplication minusA y false t k)
        | _ -> MakeUnary OperationType.UnaryMinus x isChecked (Types.FromPrimitiveDotNetType t) |> k)

// ------------------------------- Simplification of "*" -------------------------------

    and private simplifyConcreteMultiplication x y isChecked t =
        if isChecked then
            let success = ref true
            let result = Calculator.MulChecked(x, y, t, success)
            if !success then MakeConcrete result t
            else Error (result :?> System.Exception)
        else
            MakeConcrete (Calculator.Mul(x, y, t)) t

    and private simplifyMultiplicationOfProduct a b y t matched unmatched =
        // Simplifying (a * b) * y at this step
        match a, b, y with
        // (a * b) * y = (a * y) * b if a and y concrete and unchecked
        | Concrete(a, at), _, Concrete(y, yt) -> simplifyMultiplication (simplifyConcreteMultiplication a y false t) b false t matched
        // ((a / y) * b) * y = a * b if unchecked
        | Div(a, c, false, _), b, _ when c = y -> simplifyMultiplication a b false t matched
        // (a * (b / y)) * y = a * b if unchecked
        | a, Div(b, c, false, _), _ when c = y -> simplifyMultiplication a b false t matched
        // (a * b) * (c / a) = b * c if unchecked
        | _, _, Div(c, d, false, _) when d = a -> simplifyMultiplication b c false t matched
        // (a * b) * (c / b) = a * c if unchecked
        | _, _, Div(c, d, false, _) when d = b -> simplifyMultiplication a c false t matched
        | _ ->
            // Trying to simplify pairwise combinations of x- and y-factors
            let factorsOfY =
                match y with
                | Mul(c, d, false, yt) -> [c; d]
                | _ -> [y]
            simplifyPairwiseCombinations [a; b] factorsOfY t simplifyMultiplicationExt (makeProduct false t) matched unmatched

    and private simplifyMultiplicationOfDivision a b y t matched unmatched =
        // Simplifying (a / b) * y at this step
        match a, b, y with
        // (a / b) * y = (a * y) / b if a and y are concrete and unckecked
        | Concrete(a, at), b, Concrete(y, yt) ->
            let aMulY = simplifyConcreteMultiplication a y false t
            simplifyDivision aMulY b false t matched
        // (a / y) * y = a if unchecked
        | a, b, y when b = y -> matched a
        // (a / (y * d)) * y = a/d if unchecked
        | a, Mul(c, d, false, _), y when c = y -> simplifyDivision a d false t matched
        // (a / (c * y)) * y = a/c if unchecked
        | a, Mul(c, d, false, _), y when d = y -> simplifyDivision a c false t matched
        | _ -> unmatched ()

//    and private simplifyMultiplicationOfCondition a b c y t matched unmatched =
//        // Simplifying (if c then a else b) * y at this step
//        match a, b, y with
//        // (if c then a else b) * y = (if c then (a * y) else (b * y)) when a, b and y are concrete
//        | Concrete(a, at), Concrete(b, bt), Concrete(y, yt) ->
//            let aMulY = simplifyConcreteMultiplication a y false (Types.ToDotNetType at) in
//            let bMulY = simplifyConcreteMultiplication b y false (Types.ToDotNetType bt) in
//            // TODO: Merge it?
//                Expression(Cond, [c; aMulY; bMulY], Types.FromPrimitiveDotNetType t) |> matched
//        | _ -> unmatched ()
//
    and private simplifyMultiplicationOfExpression x y t matched unmatched =
        match x with
        | Mul(a, b, false, _) -> simplifyMultiplicationOfProduct a b y t matched unmatched
        | Div(a, b, false, _) -> simplifyMultiplicationOfDivision a b y t matched unmatched
//        | If(a, b, c, _) -> simplifyMultiplicationOfCondition a b c y t matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplicationExt x y isChecked t matched unmatched =
        match x, y with
        | Concrete(x, typeOfX), _ when Calculator.IsZero(x) -> matched (MakeConcrete 0 t)
        | _, Concrete(y, typeOfY) when Calculator.IsZero(y) -> matched (MakeConcrete 0 t)
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, 1) -> matched y
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, 1) -> matched x
        | Concrete(x, typeOfX), _ when Calculator.FuzzyEqual(x, -1) -> simplifyUnaryMinus y isChecked t matched
        | _, Concrete(y, typeOfY) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus x isChecked t matched
        | Expression _, Expression _ when not isChecked ->
            simplifyMultiplicationOfExpression x y t matched (fun () ->
            simplifyMultiplicationOfExpression y x t matched unmatched)
        | Expression _, _ when not isChecked -> simplifyMultiplicationOfExpression x y t matched unmatched
        | _, Expression _ when not isChecked -> simplifyMultiplicationOfExpression y x t matched unmatched
        | _ -> unmatched ()

    and private simplifyMultiplication x y isChecked t k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct isChecked t (fst sorted) (snd sorted) |> k
        simplifyGenericBinary "product" x y k
                              (fun x y _ _ -> simplifyConcreteMultiplication x y isChecked t)
                              (fun x y k -> simplifyMultiplicationExt x y isChecked t k defaultCase)

// ------------------------------- Simplification of "/" -------------------------------

    and simplifyConcreteDivision x y isChecked t =
        let success = ref true
        let result =
            if isChecked then Calculator.DivChecked(x, y, t, success)
            else Calculator.Div(x, y, t, success)
        if !success then MakeConcrete result t
        else Error (result :?> System.Exception)

    and private simplifyDivision x y isChecked t k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct isChecked t (fst sorted) (snd sorted) |> k
        simplifyGenericBinary "division" x y k (fun x y _ _ -> simplifyConcreteDivision x y isChecked t) (fun x y k ->
        match x, y with
        // 0 / y = 0
        | Concrete(x, _), _ when Calculator.IsZero(x) -> MakeConcrete 0 t |> k
        // x / 1 = x
        | x, Concrete(y, _) when Calculator.FuzzyEqual(y, 1) -> x |> k
        // x / -1 = -x
        | x, Concrete(y, _) when Calculator.FuzzyEqual(y, -1) -> simplifyUnaryMinus x isChecked t k
        // x / x = 1 if unchecked
        | x, y when not isChecked && x = y -> MakeConcrete 1 t |> k
        // x / -x = -1 if unchecked
        | x, UnaryMinus(y, false, _) when not isChecked && x = y -> MakeConcrete -1 t |> k
        // (a / b) / y = a / (b * y) if unchecked and b and y concrete
        | Div(a, Concrete(b, _), false, _), Concrete(y, _) when not isChecked ->
            let bMulY = simplifyConcreteMultiplication b y false t
            simplifyDivision a bMulY false t k
        // (if a then b else c) / y = (if a then (b/y) else (c/y)) if unchecked and b, c and y concrete
//        | If(a, Concrete(b, _), Concrete(c, _), _), Concrete(y, _) when not isChecked ->
//            let bDivY = simplifyConcreteDivision b y false t in
//            let cDivY = simplifyConcreteDivision c y false t in
//                // TODO: merge instead of Cond
//                Expression(Cond, [a; bDivY; cDivY], Types.FromPrimitiveDotNetType t) |> k
        // x / (if a then b else c) = (if a then (x/a) else (x/b)) if unchecked and x, b and c concrete
//        | Concrete(x, _), If(a, Concrete(b, _), Concrete(c, _), _) when not isChecked ->
//            let xDivB = simplifyConcreteDivision x b false t in
//            let xDivC = simplifyConcreteDivision x c false t in
//                // TODO: merge instead of Cond
//                Expression(Cond, [a; xDivB; xDivC], Types.FromPrimitiveDotNetType t) |> k
        | _ -> MakeBinary OperationType.Divide x y isChecked (Types.FromPrimitiveDotNetType t) |> k)

    and private simplifyDivisionAndUpdateState x y isChecked t state k =
        simplifyNotEqual y (Concrete(0, TypeOf y)) (fun yIsNotZero ->
        simplifyDivision x y isChecked t (fun d ->
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

    and private simplifyRemainder x y isChecked t k =
        let defaultCase () =
            let sorted = if (IsConcrete y) then (y, x) else (x, y)
            makeProduct isChecked t (fst sorted) (snd sorted) |> k
        simplifyGenericBinary "remainder" x y k (fun x y _ _ -> simplifyConcreteRemainder x y isChecked t) (fun x y k ->
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
        | Mul(Concrete(a, _), b, false, _), Concrete(y, _) when not isChecked && isRemainderZero a y t ->
             MakeConcrete 0 t |> k
        // (if a then b else c) % y = (if a then (b%y) else (c%y)) if unchecked and b, c and y concrete
        | If(a, Concrete(b, _), Concrete(c, _), _), Concrete(y, _) when not isChecked ->
            let bModY = simplifyConcreteRemainder b y false t in
            let cModY = simplifyConcreteRemainder c y false t in
                // TODO: merge instead of Cond
                Expression(Cond, [a; bModY; cModY], Types.FromPrimitiveDotNetType t) |> k
        // x % (if a then b else c) = (if a then (x%a) else (x%b)) if unchecked and x, b and c concrete
        | Concrete(x, _), If(a, Concrete(b, _), Concrete(c, _), _) when not isChecked ->
            let xModB = simplifyConcreteRemainder x b false t in
            let xModC = simplifyConcreteRemainder x c false t in
                // TODO: merge instead of Cond
                Expression(Cond, [a; xModB; xModC], Types.FromPrimitiveDotNetType t) |> k
        | _ -> MakeBinary OperationType.Remainder x y isChecked (Types.FromPrimitiveDotNetType t) |> k)

    and private simplifyRemainderAndUpdateState x y isChecked t state k =
        simplifyNotEqual y (Concrete(0, TypeOf y)) (fun yIsNotZero ->
        simplifyRemainder x y isChecked t (fun d ->
        k (d, State.addAssertion state yIsNotZero)))

// TODO: IMPLEMENT THE REST!

// ------------------------------- Simplification of "=", "!=", "<", ">", ">=", "<=" -------------------------------

    and private simplifyConcreteComparison operator x y tx ty =
        MakeConcrete (Calculator.Compare(x, y) |> operator) typedefof<bool>

    and private simplifyComparison op x y concrete sameIsTrue k =
        simplifyGenericBinary "comparison" x y k concrete (fun x y k ->
        match x, y with
        | x, y when x = y -> MakeConcrete sameIsTrue typedefof<bool> |> k
        | Add(Concrete(c, t), x, false, _), y when x = y -> simplifyComparison op (Concrete(c, t)) (Concrete(0, t)) concrete sameIsTrue k
        | x, Add(Concrete(c, t), y, false, _) when x = y -> simplifyComparison op (Concrete(0, t)) (Concrete(c, t)) concrete sameIsTrue k
        | _ -> MakeBinary op x y false Bool |> k)

    and private simplifyEqual x y k =
        simplifyComparison OperationType.Equal x y (simplifyConcreteComparison ((=) 0)) true k
    and private simplifyNotEqual x y k =
        simplifyComparison OperationType.Equal x y (simplifyConcreteComparison ((=) 0)) true ((!!) >> k)
    and private simplifyLess x y k =
        simplifyComparison OperationType.Less x y (simplifyConcreteComparison ((<) 0)) false k
    and private simplifyLessOrEqual x y k =
        simplifyComparison OperationType.LessOrEqual x y (simplifyConcreteComparison ((<=) 0)) true k
    and private simplifyGreater x y k =
        simplifyComparison OperationType.LessOrEqual x y (simplifyConcreteComparison ((<=) 0)) true ((!!) >> k)
    and private simplifyGreaterOrEqual x y k =
        simplifyComparison OperationType.Less x y (simplifyConcreteComparison ((<=) 0)) false ((!!) >> k)

// ------------------------------- General functions -------------------------------

    let private withState state x = (x, state)

    let internal simplifyBinaryOperation op x y isChecked t state k =
        let kws = withState state >> k in
        match op with
        | OperationType.Add -> simplifyAddition x y isChecked t kws
        | OperationType.Subtract ->
            simplifyUnaryMinus y false t (fun minusY ->
            simplifyAddition x minusY isChecked t kws)
        | OperationType.Multiply -> simplifyMultiplication x y isChecked t kws
        | OperationType.Divide -> simplifyDivisionAndUpdateState x y isChecked t state k
        | OperationType.Remainder -> simplifyRemainderAndUpdateState x y isChecked t state k
        | OperationType.ShiftLeft
        | OperationType.ShiftRight -> raise(new System.NotImplementedException())
        | OperationType.Equal -> simplifyEqual x y kws
        | OperationType.NotEqual -> simplifyNotEqual x y kws
        | OperationType.Greater -> simplifyGreater x y kws
        | OperationType.GreaterOrEqual -> simplifyGreaterOrEqual x y kws
        | OperationType.Less -> simplifyLess x y kws
        | OperationType.LessOrEqual -> simplifyLessOrEqual x y kws
        | OperationType.LogicalAnd
        | OperationType.LogicalOr
        | OperationType.LogicalXor
        | OperationType.NullCoalescing -> raise(new System.NotImplementedException())
        | _ -> raise(new System.ArgumentException(op.ToString() + " is not a binary arithmetic operator"))

    let internal simplifyUnaryOperation op x isChecked t k =
        match op with
        | OperationType.LogicalNeg -> raise(new System.NotImplementedException())
        | OperationType.UnaryMinus -> simplifyUnaryMinus x isChecked t k
        | OperationType.UnaryPlus -> k x
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
