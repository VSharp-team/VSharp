namespace VSharp

open JetBrains.Decompiler.Ast

module Transformations =

    let private copyTypeTo (src : INode) (dst : INode) =
        let srcType = Types.GetMetadataTypeOfNode src in
        dst.Data.SetValue(JetBrains.Decompiler.Utils.DataKey<JetBrains.Metadata.Reader.API.IMetadataType>("Type"), srcType)

    let transformOperationAssignment (ast : IBinaryOperationExpression) =
        let operator = Operations.getAssignmentOperation ast.OperationType in
        let target = ast.LeftArgument in
        let rightOperand = ast.RightArgument in
        let leftOperand = target.TypedClone<IExpression>() in
        ast.ReplaceChild(rightOperand, null)
        let binOp = AstFactory.CreateBinaryOperation(operator, leftOperand, rightOperand, null, ast.OverflowCheck) in
        copyTypeTo ast binOp
        ast.OperationType <- OperationType.Assignment
        ast.RightArgument <- binOp :> IExpression

    let transformPrefixCrement (unaryOperation : IUnaryOperationExpression) =
        let op =
            match unaryOperation.OperationType with
            | OperationType.PrefixIncrement -> OperationType.Add
            | OperationType.PrefixDecrement -> OperationType.Subtract
            | _ -> __notImplemented__()
        let leftArgument = unaryOperation.Argument in
        unaryOperation.ReplaceChild(leftArgument, null)
        let rightArgument = AstFactory.CreateLiteral(Constant.FromValueAndType(1, Types.GetMetadataTypeOfNode unaryOperation), null) in
        let target = leftArgument.TypedClone<IExpression>() in
        let sum = AstFactory.CreateBinaryOperation(op, leftArgument, rightArgument, null, unaryOperation.OverflowCheck) in
        let assignment = AstFactory.CreateBinaryOperation(OperationType.Assignment, target, sum, null, unaryOperation.OverflowCheck) in
        copyTypeTo unaryOperation sum
        copyTypeTo unaryOperation assignment
        if unaryOperation.Parent <> null then
            unaryOperation.Parent.ReplaceChild(unaryOperation, assignment)
        assignment

    let forStatementToRecursion (forStatement : IForStatement) =
        (*
        Here we transform
            for (type var = val; condition; iterator)
                body
        into
            void loop(type var = val) {  <--- head
                  if (condition) {        <--- ifStatement
                      body                <--- internalBody
                      iterator
                      loop(var)           <--- recursiveCallStatement
                  }                       <--- externalBody (endof)
             }
        *)
        printfn "Converting loop to recursion:\n%s" (forStatement.ToStringDebug())
        let indexer =
            match forStatement.Initializer with
            | :? ILocalVariableDeclarationStatement as decl -> decl
            | _ -> __notImplemented__()
        in
        let condition = forStatement.Condition in
        let iterator = forStatement.Iterator in
        let internalBody = forStatement.Body in
        forStatement.ReplaceChild(condition, null)
        forStatement.ReplaceChild(iterator, null)
        forStatement.ReplaceChild(internalBody, null)

        let signature = AstFactory.CreateFunctionSignature() in
        let body = AstFactory.CreateBlockStatement() in
        let lambdaBlock = AstFactory.CreateLambdaBlockExpression(signature, body, null) in
//        let recursiveCall = AstFactory.CreateFunctionPointerCall(lambdaBlock, [||], null) in
        let recursiveCall = AstFactory.CreateDelegateCall(lambdaBlock, null, [||], null) in
        let recursiveCallStatement = AstFactory.CreateExpressionStatement(recursiveCall, null) in
        let externalBody = AstFactory.CreateBlockStatement([internalBody; iterator(*; recursiveCallStatement*)]) in
        let ifStatement = AstFactory.CreateIf(condition, externalBody, null, null) in
        body.Statements.Add(ifStatement)
        System.Console.WriteLine("================Generated loop recursion:===================")
        System.Console.WriteLine(lambdaBlock.ToStringDebug())
        System.Console.WriteLine(recursiveCallStatement.ToStringDebug())
