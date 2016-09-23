namespace VSharp.Core.Symbolic.Reduction

open JetBrains.Decompiler.Ast
open VSharp.Core.Symbolic

module internal Calculator =
//
//    let private decompilerOpToPsiOpMap =
//        [(OperationType.Add, new );
////         (OperationType.ConditionalAnd, 1);
//        ]
//        |> Map.ofList
//
//    let private psiOp op = decompilerOpToPsiOpMap.[op]
//
//    let internal calc op x typeOfX y typeOfY isChecked psiModule =
//        let calculator = JetBrains.ReSharper.Psi.CSharp.ConstantValue.CSharpConstantCalculator.Instance
//        let xValue = new JetBrains.ReSharper.Psi.ConstantValue(x, psiModule)
//        let yValue = new JetBrains.ReSharper.Psi.ConstantValue(y, psiModule)
////        let result = calculator.CalculateBinaryOperator(psiOp op, xValue, yValue, isChecked)
//        let result = calculator.CalculateBinaryOperator("op_Addition", xValue, yValue, isChecked)
//        Concrete(result.Value, System.Type.GetType(result.Type.GetScalarType().GetClrName().FullName))
    let a = 1