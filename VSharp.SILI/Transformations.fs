namespace VSharp

open JetBrains.Decompiler.Ast
open System
open System.Reflection

module Transformations =

    let private setTypeOfNode (node : INode) (t : JetBrains.Metadata.Reader.API.IMetadataType) =
        node.Data.SetValue(JetBrains.Decompiler.Utils.DataKey<JetBrains.Metadata.Reader.API.IMetadataType>("Type"), t)

    let private copyTypeTo (src : INode) (dst : INode) =
        setTypeOfNode dst (Types.GetMetadataTypeOfNode src)

    let private createLocalVariable name typ (sibling : INode) =
        let rec findDeclarationScope (node : INode) =
            match node with
            | null -> null
            | :? ILocalVariableDeclarationScopeOwner as owner -> owner.DeclarationScope
            | _ -> findDeclarationScope node.Parent
        in
        let scope = findDeclarationScope sibling in
        // Hack: here we use reflection because all local variable instantiation code is internal
        let localVariableClass = Assembly.GetAssembly(typedefof<ILocalVariable>).GetType("JetBrains.Decompiler.Ast.Impl.LocalVariable") in
        let instance = Activator.CreateInstance(localVariableClass) in
        let (|||) = Microsoft.FSharp.Core.Operators.(|||) in
        localVariableClass.InvokeMember("DeclarationScope", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|scope|]) |> ignore
        localVariableClass.InvokeMember("Kind", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|LocalVariableKind.Regular|]) |> ignore
        localVariableClass.InvokeMember("Name", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|name|]) |> ignore
        localVariableClass.InvokeMember("Type", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|typ|]) |> ignore
        instance :?> ILocalVariable

    let private createMethodParameter name typ index =
        // Hack: here we use reflection because method parameter instantiation code is internal
        let methodParameterClass = Assembly.GetAssembly(typedefof<IMethodParameter>).GetType("JetBrains.Decompiler.Ast.Impl.MethodParameter") in
        let instance = Activator.CreateInstance(methodParameterClass) in
        let (|||) = Microsoft.FSharp.Core.Operators.(|||) in
        methodParameterClass.InvokeMember("Index", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|index|]) |> ignore
        methodParameterClass.InvokeMember("Kind", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|MethodParameterKind.Regular|]) |> ignore
        methodParameterClass.InvokeMember("Name", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|name|]) |> ignore
        methodParameterClass.InvokeMember("Type", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|typ|]) |> ignore
        instance :?> IMethodParameter

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
        unaryOperation.ReplaceWith(assignment)
        assignment

    let forStatementToRecursion (forStatement : IForStatement) =
        (*
        Here we transform
            for (type var = val; condition; iterator)
                body
        into
            Action<type> loop = null;     <--- declaration
            loop= var => {                <--- lambda, assignment
                  if (condition) {        <--- ifStatement
                      body                <--- internalBody
                      iterator
                      loop(var)           <--- recursiveCallStatement
                  }                       <--- externalBody (endof)
             };
             loop(val);
        *)
        let indexers =
            match forStatement.Initializer with
            | :? ILocalVariableDeclarationStatement as decl -> [decl]
            | :? IEmptyStatement -> []
            | _ -> __notImplemented__()
        in
        let typeOfIndexer (indexer : ILocalVariableDeclarationStatement) =
            indexer.VariableReference.Variable.Type |> Types.MetadataToDotNetType in
        let variableOfIndexer (indexer : ILocalVariableDeclarationStatement) =
            indexer.VariableReference.TypedClone<IExpression>()
        let initializerOfIndexer (indexer : ILocalVariableDeclarationStatement) =
            indexer.Initializer.TypedClone<IExpression>()
        let indexerTypes = List.map typeOfIndexer indexers in
        let indexerVariables = List.map variableOfIndexer indexers in
        let indexerInitializers = List.map initializerOfIndexer indexers in

        let lambdaDotNetType = typedefof<System.Action<_>>.MakeGenericType(Array.ofList indexerTypes) in
        let lambdaType = DecompilerServices.resolveType lambdaDotNetType in
        let loopId = IdGenerator.startingWith "loop" in
        let loopVariable = createLocalVariable loopId lambdaType forStatement in
        let lambdaReference = AstFactory.CreateLocalVariableReference(loopVariable, null) in
        let nullExpression = AstFactory.CreateLiteral(Constant.FromValueAndType(null, lambdaType), null) in
        let declaration = AstFactory.CreateLocalVariableDeclaration(lambdaReference, null, nullExpression) in

        let condition = forStatement.Condition in
        let iterator = forStatement.Iterator in
        let internalBody = forStatement.Body in
        forStatement.ReplaceChild(condition, null)
        forStatement.ReplaceChild(iterator, null)
        forStatement.ReplaceChild(internalBody, null)

        let recursiveCall = AstFactory.CreateDelegateCall(lambdaReference, null, Array.ofList indexerVariables, null) in
        let recursiveCallStatement = AstFactory.CreateExpressionStatement(recursiveCall, null) in
        let externalBody = AstFactory.CreateBlockStatement([internalBody; iterator; recursiveCallStatement]) in
        let ifStatement = AstFactory.CreateIf(condition, externalBody, null, null) in
        let body = AstFactory.CreateBlockStatement([ifStatement]) in

        let signature = AstFactory.CreateFunctionSignature() in
        let lambdaBlock = AstFactory.CreateLambdaBlockExpression(signature, body, null) in
        let lambdaBlockAssignment = AstFactory.CreateBinaryOperation(OperationType.Assignment, lambdaReference, lambdaBlock, null, OverflowCheckType.DontCare) in
        let lambdaBlockAssignmentStatement = AstFactory.CreateExpressionStatement(lambdaBlockAssignment, null) in
        let addParameterToSignature index (variable : ILocalVariableDeclarationStatement) =
            let parameter = createMethodParameter variable.VariableReference.Variable.Name variable.VariableReference.Variable.Type index in
            signature.Parameters.Add(parameter)
        List.iteri addParameterToSignature indexers
        setTypeOfNode lambdaBlockAssignment lambdaType

        let call = AstFactory.CreateDelegateCall(lambdaReference, null, Array.ofList indexerInitializers, null) in
        let callStatement = AstFactory.CreateExpressionStatement(call, null) in
        let block = AstFactory.CreateBlockStatement([declaration; lambdaBlockAssignmentStatement; callStatement]) in

        let parentDbg = forStatement.Parent
        match forStatement.Parent with
        | null -> ()
        | :? IBlockStatement as parentBlock ->
            let newParent = parentBlock.TypedClone<IBlockStatement>() in
            let forStatementIndex = Seq.findIndex ((=) (forStatement :> IStatement)) parentBlock.Statements in
            let newForStatement = Seq.item forStatementIndex newParent.Statements in
            let appendStatement statement = newParent.Statements.AddBefore(newForStatement, statement)
            block.Statements |> Seq.iter appendStatement
            newParent.Statements.Remove(newForStatement) |> ignore
            parentBlock.ReplaceWith(newParent)
        | parent -> parent.ReplaceChild(forStatement, block)
        block
