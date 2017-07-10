namespace VSharp

open JetBrains.Decompiler.Ast
open System
open System.Reflection

module Transformations =

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

    let transformPrefixCrement (unaryOperation : IUnaryOperationExpression) =
        let op =
            match unaryOperation.OperationType with
            | OperationType.PrefixIncrement -> OperationType.AssignmentAdd
            | OperationType.PrefixDecrement -> OperationType.AssignmentSubtract
            | _ -> __notImplemented__()
        let leftArgument = unaryOperation.Argument in
        unaryOperation.ReplaceChild(leftArgument, null)
        let rightArgument = AstFactory.CreateLiteral(Constant.FromValueAndType(1, Types.GetMetadataTypeOfNode unaryOperation), null) in
        let assignment = AstFactory.CreateBinaryOperation(op, leftArgument, rightArgument, null, unaryOperation.OverflowCheck) in
        DecompilerServices.copyTypeTo unaryOperation assignment
        unaryOperation.ReplaceWith(assignment)
        assignment

    let forStatementToRecursion (forStatement : IForStatement) =
        (*
        Here we transform
            for (type var = val; condition; iterator)
                body
        into
            var => {                      <--- lambda, assignment
                  if (condition) {        <--- ifStatement
                      body                <--- internalBody
                      iterator
                      currentLambda(var)  <--- recursiveCallStatement (hacked call)
                  }                       <--- externalBody (endof)
             }(val);
        Note that isn't a valid c# code: anonymous functions can't be recursively called
        without giving it a name. The interpreter must consider "CurrentLambdaExpression"
        property of delegate calls to invoke this!
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
        let condition = forStatement.Condition in
        let iterator = forStatement.Iterator in
        let internalBody = forStatement.Body in
        forStatement.ReplaceChild(condition, null)
        forStatement.ReplaceChild(iterator, null)
        forStatement.ReplaceChild(internalBody, null)

        let recursiveCall = AstFactory.CreateDelegateCall(null, null, Array.ofList indexerVariables, null) in
        let recursiveCallStatement = AstFactory.CreateExpressionStatement(recursiveCall, null) in

        let internalBodyContent =
            if internalBody :? IBlockStatement
            then (internalBody :?> IBlockStatement).Statements :> seq<_>
            else [internalBody] :> seq<_>
        let externalBody = AstFactory.CreateBlockStatement(Seq.append internalBodyContent [iterator; recursiveCallStatement]) in
        let ifStatement = AstFactory.CreateIf(condition, externalBody, null, null) in
        let body = AstFactory.CreateBlockStatement([ifStatement]) in
        // Continue consumers should only belong to block statements
        DecompilerServices.setPropertyOfNode recursiveCall "ContinueConsumer" true

        let signature = AstFactory.CreateFunctionSignature() in
        let lambdaBlock = AstFactory.CreateLambdaBlockExpression(signature, body, null) in
        let lambdaBlockStatement = AstFactory.CreateExpressionStatement(lambdaBlock, null) in
        let addParameterToSignature index (variable : ILocalVariableDeclarationStatement) =
            let parameter = createMethodParameter variable.VariableReference.Variable.Name variable.VariableReference.Variable.Type index in
            signature.Parameters.Add(parameter)
        List.iteri addParameterToSignature indexers
        DecompilerServices.setTypeOfNode lambdaBlockStatement lambdaType

        let call = AstFactory.CreateDelegateCall(lambdaBlock, null, Array.ofList indexerInitializers, null) in
        let callStatement = AstFactory.CreateExpressionStatement(call, null) in

        DecompilerServices.setPropertyOfNode recursiveCall "InlinedCallTarget" lambdaBlock
        DecompilerServices.setPropertyOfNode signature "InlinedCall" true
        DecompilerServices.setPropertyOfNode recursiveCallStatement "InlinedCall" true
        DecompilerServices.setPropertyOfNode callStatement "InlinedCall" true
        match forStatement.Parent with
        | null -> ()
        | :? IBlockStatement as parentBlock ->
            let newParent = parentBlock.TypedClone<IBlockStatement>() in
            let forStatementIndex = Seq.findIndex ((=) (forStatement :> IStatement)) parentBlock.Statements in
            let newForStatement = Seq.item forStatementIndex newParent.Statements in
            let appendStatement statement = newParent.Statements.AddBefore(newForStatement, statement)
            appendStatement callStatement
            newParent.Statements.Remove(newForStatement) |> ignore
            parentBlock.ReplaceWith(newParent)
        | parent -> parent.ReplaceChild(forStatement, callStatement)
        callStatement

    let isInlinedCall (statement : IExpressionStatement) =
        statement.Expression :? IDelegateCallExpression && DecompilerServices.getPropertyOfNode statement "InlinedCall" false :?> bool

    let isContinueConsumer (node : INode) =
        node <> null && DecompilerServices.getPropertyOfNode node "ContinueConsumer" false :?> bool

    let inlinedCallTarget (node : INode) =
        DecompilerServices.getPropertyOfNode node "InlinedCallTarget" null :?> IExpression |> function
        | null -> None
        | expression -> Some expression

    let isInlinedSignatureCall (signature : IFunctionSignature) =
        if signature <> null then DecompilerServices.getPropertyOfNode signature "InlinedCall" false :?> bool |> not
        else false

    let extractExceptionFilter (ast : IBlockStatement) =
        if ast.Statements.Count <> 1 then __notImplemented__()
        let filterStatement = ast.Statements.First in
        match filterStatement with
        | :? IIfStatement as cond
            when (cond.Then :? ISuccessfulFilteringStatement) && (cond.Else :? IRethrowStatement) -> cond.Condition
        | _ -> __notImplemented__()
