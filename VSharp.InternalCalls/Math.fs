namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorlib.System.Math -------------------------------

module internal Math =
    let Acos (_ : state) args = Arithmetics.Acos (List.head args)
    let Asin (_ : state) args = Arithmetics.Asin (List.head args)
    let Atan (_ : state) args = Arithmetics.Atan (List.head args)
    let Atan2 (_ : state) args = Arithmetics.Atan2 (List.item 0 args) (List.item 1 args)
    let Ceiling (_ : state) args = Arithmetics.Ceiling (List.head args)
    let Cos (_ : state) args = Arithmetics.Cos (List.head args)
    let Cosh (_ : state) args = Arithmetics.Cosh (List.head args)
    let Floor (_ : state) args = Arithmetics.Floor (List.head args)
    let Sin (_ : state) args = Arithmetics.Sin (List.head args)
    let Tan (_ : state) args = Arithmetics.Tan (List.head args)
    let Sinh (_ : state) args = Arithmetics.Sinh (List.head args)
    let Tanh (_ : state) args = Arithmetics.Tanh (List.head args)
    let Round (_ : state) args = Arithmetics.Round (List.head args)
    let Sqrt (_ : state) args = Arithmetics.Sqrt (List.head args)
    let Log (_ : state) args = Arithmetics.Log (List.head args)
    let Log10 (_ : state) args = Arithmetics.Log10 (List.head args)
    let Exp (_ : state) args = Arithmetics.Exp (List.head args)
    let Pow (_ : state) args = Arithmetics.Pow (List.item 0 args) (List.item 1 args)
    let Abs (_ : state) args = Arithmetics.Abs (List.head args)
    let AbsS (_ : state) args = Arithmetics.AbsS (List.head args)
