namespace VSharp.Interpreter

open VSharp
open JetBrains.Decompiler.Ast
open JetBrains.Metadata.Reader.API
open global.System
open System.Reflection
open System.Collections.Generic

module Transformations =

    let private createLocalVariable name typ (sibling : INode) =
        let rec findDeclarationScope (node : INode) =
            match node with
            | null -> null
            | :? ILocalVariableDeclarationScopeOwner as owner -> owner.DeclarationScope
            | _ -> findDeclarationScope node.Parent
        let scope = findDeclarationScope sibling
        // Hack: here we use reflection because all local variable instantiation code is internal
        let localVariableClass = Assembly.GetAssembly(typedefof<ILocalVariable>).GetType("JetBrains.Decompiler.Ast.Impl.LocalVariable")
        let instance = Activator.CreateInstance(localVariableClass)
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        localVariableClass.InvokeMember("DeclarationScope", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|scope|]) |> ignore
        localVariableClass.InvokeMember("Kind", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|LocalVariableKind.Regular|]) |> ignore
        localVariableClass.InvokeMember("Name", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|name|]) |> ignore
        localVariableClass.InvokeMember("Type", BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.SetProperty, Type.DefaultBinder, instance, [|typ|]) |> ignore
        instance :?> ILocalVariable

    let private createMethodParameter name typ index =
        // Hack: here we use reflection because method parameter instantiation code is internal
        let methodParameterClass = Assembly.GetAssembly(typedefof<IMethodParameter>).GetType("JetBrains.Decompiler.Ast.Impl.MethodParameter")
        let instance = Activator.CreateInstance(methodParameterClass)
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
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
        let leftArgument = unaryOperation.Argument
        unaryOperation.ReplaceChild(leftArgument, null)
        let rightArgument = AstFactory.CreateLiteral(Constant.FromValueAndType(1, MetadataTypes.getMetadataTypeOfNode unaryOperation), null)
        let assignment = AstFactory.CreateBinaryOperation(op, leftArgument, rightArgument, null, unaryOperation.OverflowCheck)
        DecompilerServices.copyTypeTo unaryOperation assignment
        unaryOperation.ReplaceWith(assignment)
        assignment

    let private replaceChildrenOfParentNode (child : IStatement) newChildren =
        match child.Parent with
        | null -> ()
        | :? IBlockStatement as parentBlock ->
            let newParent = parentBlock.TypedClone<IBlockStatement>()
            let childStatementIndex = Seq.findIndex ((=) child) parentBlock.Statements
            let newChildStatement = Seq.item childStatementIndex newParent.Statements
            let appendStatement statement = newParent.Statements.AddBefore(newChildStatement, statement)
            Seq.iter appendStatement newChildren
            newParent.Statements.Remove(newChildStatement) |> ignore
            parentBlock.ReplaceWith(newParent)
        | parent -> parent.ReplaceChild(child, AstFactory.CreateBlockStatement(newChildren))

    let private getReturnTypeOfLoopStatement (ast : INode) : Type option = // None == System.Void
        let hasReturn =
            try
                ast.VisitPreorder(fun (node : INode) ->
                    match node with
                    | :? IReturnStatement -> failwith "Got return"
                    | _ -> ())
                false
            with Failure "Got return" -> true
        let rec getReturnTypeFromMethod (node : INode) =
            match node with
            | null -> internalfail "Can't find enclosing method"
            | :? IDecompiledMethod as dm -> dm.MetadataMethod.ReturnValue.Type
            | node -> getReturnTypeFromMethod node.Parent

        if hasReturn
            then
                let returnType =
                    match MetadataTypes.getLambdaTypeSignature ast with
                    | Some signature -> Array.tryLast signature
                    | None -> Some <| getReturnTypeFromMethod ast
                returnType
                |> Option.filter (fun t -> t.FullName <> "System.Void")
                |> Option.map MetadataTypes.metadataToDotNetType
            else None

    let private abstractLoopStatementToRecursion (abstractLoopStatement : IAbstractLoopStatement) indexerTypes callArgumentsNames callArguments externalBody body fullBodyBlock traverseBody =
        (*
        Here we transform loops into anaphoric lambda recursive calls

        Note that isn't a valid c# code: anonymous functions can't be recursively called
        without giving it a name. The interpreter must consider "CurrentLambdaExpression"
        property of delegate calls to invoke this!
        *)

        let returnType = getReturnTypeOfLoopStatement abstractLoopStatement
        let internalBody = abstractLoopStatement.Body
        abstractLoopStatement.ReplaceChild(internalBody, null)
        let instructionReference = abstractLoopStatement.InstructionReference

        let recursiveCall = AstFactory.CreateDelegateCall(null, null, callArgumentsNames, instructionReference)
        // Continue consumers should only belong to block statements
        DecompilerServices.setPropertyOfNode recursiveCall "ContinueConsumer" true
        let recursiveCallStatement = AstFactory.CreateExpressionStatement(recursiveCall, instructionReference)

        let internalBodyContent =
            match internalBody with
            | :? IBlockStatement as internalBody -> internalBody.Statements :> seq<_>
            | _ -> Seq.singleton internalBody
        let externalBody = AstFactory.CreateBlockStatement(externalBody internalBodyContent (recursiveCallStatement :> IStatement))
        let body = body externalBody

        let signature = AstFactory.CreateFunctionSignature()
        let lambdaBlock = AstFactory.CreateLambdaBlockExpression(signature, body, instructionReference)
        let lambdaDotNetType =
            match returnType with
            | Some returnType ->
                Array.append indexerTypes [|returnType|]
                |> Array.map toString
                |> join ","
                |> sprintf "System.Func`%i[%s]" (indexerTypes.Length + 1)
            | None when indexerTypes.Length = 0 -> "System.Action"
            | None ->
                Array.map toString indexerTypes
                |> join ","
                |> sprintf "System.Action`%i[%s]" indexerTypes.Length
            |> Type.GetType
        let lambdaType = DecompilerServices.resolveType lambdaDotNetType
        DecompilerServices.setTypeOfNode lambdaBlock lambdaType

        let call = AstFactory.CreateDelegateCall(lambdaBlock, null, callArguments, instructionReference)
        let callStatement = AstFactory.CreateExpressionStatement(call, instructionReference)

        DecompilerServices.setPropertyOfNode recursiveCall "InlinedCallTarget" lambdaBlock
        DecompilerServices.setPropertyOfNode signature "InlinedCall" true
        DecompilerServices.setPropertyOfNode recursiveCallStatement "InlinedCall" true
        DecompilerServices.setPropertyOfNode callStatement "InlinedCall" true

        let fullBodyBlock = fullBodyBlock internalBodyContent (callStatement :> IStatement)

        traverseBody body signature

        replaceChildrenOfParentNode abstractLoopStatement fullBodyBlock
        fullBodyBlock

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
            | :? ILocalVariableDeclarationStatement as decl -> [|decl|]
            | :? IEmptyStatement -> [||]
            | _ -> __notImplemented__()
        let typeOfIndexer (indexer : ILocalVariableDeclarationStatement) =
            indexer.VariableReference.Variable.Type |> MetadataTypes.metadataToDotNetType
        let variableOfIndexer (indexer : ILocalVariableDeclarationStatement) =
            indexer.VariableReference.TypedClone<IExpression>()
        let initializerOfIndexer (indexer : ILocalVariableDeclarationStatement) =
            indexer.Initializer.TypedClone<IExpression>()
        let indexerTypes = Array.map typeOfIndexer indexers
        let indexerVariables = Array.map variableOfIndexer indexers
        let indexerInitializers = Array.map initializerOfIndexer indexers

        let condition = forStatement.Condition
        let iterator = forStatement.Iterator
        forStatement.ReplaceChild(condition, null)
        forStatement.ReplaceChild(iterator, null)

        let externalBody internalBodyContent recursiveCallStatement =
            Seq.append internalBodyContent [iterator; recursiveCallStatement]

        let body externalBody =
            let ifStatement = AstFactory.CreateIf(condition, externalBody, null, null)
            AstFactory.CreateBlockStatement([ifStatement])

        let replaceParameters (body : IBlockStatement) (signature : IFunctionSignature) =
            let mapIndexerNameToMethodParameter = new System.Collections.Generic.Dictionary<string, IMethodParameter>()
            let addParameterToSignature index (variable : ILocalVariableDeclarationStatement) =
                let parameter = createMethodParameter variable.VariableReference.Variable.Name variable.VariableReference.Variable.Type index
                signature.Parameters.Add(parameter)
                mapIndexerNameToMethodParameter.Add(variable.VariableReference.Variable.Name, parameter)
            Array.iteri addParameterToSignature indexers

            let indexerVariableNames = Array.map (fun (index: ILocalVariableDeclarationStatement) -> index.VariableReference.Variable.Name) indexers
            body.VisitPreorder(fun (node : INode) ->
                match node with
                | :? ILocalVariableReferenceExpression as localVar ->
                    if Array.contains localVar.Variable.Name indexerVariableNames
                    then
                        let methodParameter = mapIndexerNameToMethodParameter.[localVar.Variable.Name]
                        let parameterReference = AstFactory.CreateParameterReference(methodParameter, localVar.InstructionReference)
                        localVar.ReplaceWith(parameterReference)
                | _ -> ()
            )

        let fullBodyBlock _ callStatement = Seq.singleton callStatement

        abstractLoopStatementToRecursion forStatement indexerTypes indexerVariables indexerInitializers externalBody body fullBodyBlock replaceParameters
        |> Seq.exactlyOne :?> IExpressionStatement

    let loopStatementToRecursion (loopStatement : ILoopStatement) =
        (*
        Here we transform
            while (condition) body
        into
            var => {                      <--- lambda, assignment
                  if (condition) {        <--- ifStatement
                      body                <--- internalBody
                      currentLambda(var)  <--- recursiveCallStatement (hacked call)
                  }                       <--- externalBody (endof)
             }();

            while (true) body
        into
            var => {                      <--- lambda, assignment
                  body                    <--- internalBody
                  currentLambda(var)      <--- recursiveCallStatement (hacked call)
             }();

            do { body } while (condition)
        into
            {
                body
                var => {                      <--- lambda, assignment
                      if (condition) {        <--- ifStatement
                          body                <--- internalBody
                          currentLambda(var)  <--- recursiveCallStatement (hacked call)
                      }                       <--- externalBody (endof)
                 }();
             }

        Note that isn't a valid c# code: anonymous functions can't be recursively called
        without giving it a name. The interpreter must consider "CurrentLambdaExpression"
        property of delegate calls to invoke this!
        *)
        let condition = loopStatement.Condition
        let loopType = loopStatement.LoopType
        loopStatement.ReplaceChild(condition, null)

        let externalBody internalBodyContent recursiveCallStatement = Seq.append internalBodyContent [recursiveCallStatement]
        let body externalBody =
            match loopType with
            | LoopType.Unconditional -> externalBody
            | _ ->
                let ifStatement = AstFactory.CreateIf(condition, externalBody, null, null)
                AstFactory.CreateBlockStatement([ifStatement])

        let fullBodyBlock internalBodyContent callStatement =
            match loopType with
            | LoopType.Postconditional -> Seq.append internalBodyContent [callStatement]
            | _ -> Seq.singleton callStatement

        abstractLoopStatementToRecursion loopStatement [||] [||] [||] externalBody body fullBodyBlock (fun _ _ -> ())

    let private usingStatementWithAssignmentToBlock (usingStatement : IUsingStatement) =
        (*
        Here we transform
            using (ResourceType resource = expression) body
        into
            {
                ResourceType resource = expression;
                try {
                    body;
                }
                finally {
                    ((IDisposable)resource).Dispose();
                }
            }

        See: https://github.com/dotnet/csharplang/blame/82c1088104ead6f49db157c77d5acb0c9e3e3bc0/spec/statements.md#L1277
        *)
        let instructionReference = usingStatement.InstructionReference
        let varReference = usingStatement.VariableReference
        let initializerExpression = usingStatement.Expression
        let body = usingStatement.Body
        usingStatement.ReplaceChild(varReference, null)
        usingStatement.ReplaceChild(initializerExpression, null)
        usingStatement.ReplaceChild(body, null)

        // Assignment
        let varAssignmentStatement = AstFactory.CreateLocalVariableDeclaration(varReference, instructionReference, initializerExpression)

        // Try/Finally
        let metadataTypeInfoIDisposable = (DecompilerServices.resolveType(typedefof<IDisposable>) :?> IMetadataClassType).Type
        let metadataClassTypeIDisposable = MetadataTypeFactory.CreateClassType(metadataTypeInfoIDisposable)
        let castVarReferenceToDisposable = AstFactory.CreateTypeCast(varReference, metadataClassTypeIDisposable, OverflowCheckType.DontCare, instructionReference)

        let disposeMethodInstantiation =
            metadataTypeInfoIDisposable.GetMethods()
            |> Array.find (fun m -> m.Name = "Dispose" && m.Parameters.Length = 0)
            |> MethodSpecification
            |> MethodInstantiation

        let finallyBlock =
            let callDisposeExpression = AstFactory.CreateMethodCall(castVarReferenceToDisposable, disposeMethodInstantiation, true, [||], instructionReference, MemberAccessKind.Regular)
            let disposeStatement = AstFactory.CreateExpressionStatement(callDisposeExpression, instructionReference)
            AstFactory.CreateBlockStatement([disposeStatement])
        let bodyBlock =
            match body with
            | :? IBlockStatement as bodyBlock -> bodyBlock
            | _ -> AstFactory.CreateBlockStatement([body])
        let tryBlock = AstFactory.CreateTry(bodyBlock, [||], finallyBlock, null, instructionReference)

        let fullBody : IStatement seq = seq [varAssignmentStatement; tryBlock]
        let block = AstFactory.CreateBlockStatement(fullBody)

        replaceChildrenOfParentNode usingStatement fullBody
        block

    let private createLocalVariableReference prefix typ instructionReference sibling =
        let freshName = IdGenerator.startingWith prefix
        let freshVar = createLocalVariable freshName typ sibling
        let freshVarRef = AstFactory.CreateLocalVariableReference(freshVar, instructionReference)
        freshVarRef

    let usingStatementToBlock (usingStatement : IUsingStatement) =
        (*
        Here we transform
            using (expression) body
        into
            using (var __tmp__ = expression) body

        See: https://github.com/dotnet/csharplang/blame/82c1088104ead6f49db157c77d5acb0c9e3e3bc0/spec/statements.md#L1325
        *)
        if usingStatement.VariableReference = null
        then
            let tmpVarTyp = DecompilerServices.getTypeOfNode usingStatement.Expression
            let tmpVarRef =
                createLocalVariableReference "__tmpUsingVar__" tmpVarTyp usingStatement.InstructionReference usingStatement
            usingStatement.ReplaceChild(usingStatement.VariableReference, tmpVarRef)

        usingStatementWithAssignmentToBlock usingStatement

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
        let filterStatement = ast.Statements.First
        match filterStatement with
        | :? IIfStatement as cond
            when (cond.Then :? ISuccessfulFilteringStatement) && (cond.Else :? IRethrowStatement) -> cond.Condition
        | _ -> __notImplemented__()
