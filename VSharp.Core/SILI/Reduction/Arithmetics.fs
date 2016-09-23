namespace VSharp.Core.Symbolic.Reduction

open JetBrains.Decompiler.Ast
open VSharp.Core.Symbolic

module internal Arithmetics =

    let internal reduceAddition op x y isChecked psiModule =
        match (x, y) with
        //| (Concrete(x, typeOfX), Concrete(y, typeOfY)) -> calcAddition op x typeOfX y typeOfY isChecked psiModule
        | _ -> Terms.MakeBinary op x y
