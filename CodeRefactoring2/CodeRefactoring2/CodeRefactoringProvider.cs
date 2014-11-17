using System.Collections.Generic;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeRefactorings;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using System;
using Microsoft.CodeAnalysis.Formatting;

namespace CodeRefactoring2
{
    [ExportCodeRefactoringProvider(CodeRefactoring2CodeRefactoringProvider.RefactoringId, LanguageNames.CSharp), Shared]
    internal class CodeRefactoring2CodeRefactoringProvider : CodeRefactoringProvider
    {
        public const string RefactoringId = "CodeRefactoring2";

        public sealed override async Task ComputeRefactoringsAsync(CodeRefactoringContext context)
        {
            // TODO: Replace the following code with your own analysis, generating a CodeAction for each refactoring to offer

            SyntaxNode root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Find the node at the selection.
            SyntaxNode node = root.FindNode(context.Span);

            // this covers both methods and ctors
            BaseMethodDeclarationSyntax methodDeclaration = node as BaseMethodDeclarationSyntax;
            if (methodDeclaration != null)
            {
                if (methodDeclaration.ParameterList.Parameters.Any())
                {
                    BlockSyntax blockSyntax = methodDeclaration.Body;
                    IEnumerable<IfStatementSyntax> ifStatements = blockSyntax.ChildNodes().OfType<IfStatementSyntax>();

                    foreach (ParameterSyntax parameter in methodDeclaration.ParameterList.Parameters)
                    {
                        TypeSyntax paramTypeName = parameter.Type;
                        SemanticModel semanticModel = await context.Document.GetSemanticModelAsync();
                        ITypeSymbol type = semanticModel.GetTypeInfo(paramTypeName).ConvertedType;

                        if (type.IsReferenceType)
                        {
                            // check if the null guard already exists.
                            bool isGuardAlreadyExists = ifStatements.Any(ifStatement =>
                            {
                                return ifStatement.ChildNodes()
                                    .OfType<BinaryExpressionSyntax>()
                                    .Where(x => x.IsKind(SyntaxKind.EqualsExpression))
                                    .Any(expression =>
                                    {
                                        bool result;
                                        bool isNullCheck = expression.Right.IsKind(SyntaxKind.NullLiteralExpression);
                                        if (isNullCheck)
                                        {
                                            IdentifierNameSyntax identifierSyntaxt = expression.ChildNodes().OfType<IdentifierNameSyntax>().FirstOrDefault();
                                            if (identifierSyntaxt != null)
                                            {
                                                string identifierText = identifierSyntaxt.Identifier.Text;
                                                string paramText = parameter.Identifier.Text;

                                                if (identifierText.Equals(paramText, StringComparison.Ordinal))
                                                {
                                                    // There is already a null guard for this parameter.
                                                    result = true;
                                                }
                                                else
                                                {
                                                    result = false;
                                                }
                                            }
                                            else
                                            {
                                                result = false;
                                            }
                                        }
                                        else
                                        {
                                            result = false;
                                        }

                                        return result;
                                    });
                            });

                            if (isGuardAlreadyExists == false)
                            {
                                CodeAction action = CodeAction.Create("Check paramater for null",
                                    c => AddGuardAsync(context.Document, parameter, methodDeclaration, c));

                                context.RegisterRefactoring(action);
                            }
                        }
                    }
                }
            }
        }

        private async Task<Document> AddGuardAsync(Document document, ParameterSyntax parameter, BaseMethodDeclarationSyntax methodDeclaration, CancellationToken cancellationToken)
        {
            BinaryExpressionSyntax binaryExpression = SyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression,
                SyntaxFactory.IdentifierName(parameter.Identifier),
                SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression));

            NameOfExpressionSyntax nameOfExp = SyntaxFactory.NameOfExpression(
                "nameof",
                SyntaxFactory.ParseTypeName(parameter.Identifier.Text));

            SeparatedSyntaxList<ArgumentSyntax> argsList = new SeparatedSyntaxList<ArgumentSyntax>()
                    .Add(SyntaxFactory.Argument(nameOfExp));

            ObjectCreationExpressionSyntax objectCreationEx = SyntaxFactory.ObjectCreationExpression(
                SyntaxFactory.ParseTypeName(nameof(ArgumentNullException)),
                SyntaxFactory.ArgumentList(argsList),
                null);

            ThrowStatementSyntax throwStatement = SyntaxFactory.ThrowStatement(objectCreationEx);

            BlockSyntax syntaxBlock = SyntaxFactory.Block(
                SyntaxFactory.Token(SyntaxKind.OpenBraceToken),
                new SyntaxList<StatementSyntax>().Add(throwStatement),
                SyntaxFactory.Token(SyntaxKind.CloseBraceToken)).WithAdditionalAnnotations(Formatter.Annotation);

            //SyntaxNodeOrTokenList syntaxList = new SyntaxNodeOrTokenList()
            //    .Add(SyntaxFactory.Token(SyntaxKind.IfKeyword))
            //    .Add(SyntaxFactory.Token(SyntaxKind.OpenParenToken))
            //    .Add(binaryExpression)
            //    .Add(SyntaxFactory.Token(SyntaxKind.CloseParenToken))
            //    .Add(syntaxBlock);

            IfStatementSyntax ifStatement = SyntaxFactory
                .IfStatement(SyntaxFactory.Token(SyntaxKind.IfKeyword),
                    SyntaxFactory.Token(SyntaxKind.OpenParenToken),
                    binaryExpression, SyntaxFactory.Token(SyntaxKind.CloseParenToken), syntaxBlock, null)
                .WithAdditionalAnnotations(Formatter.Annotation);

            SyntaxNode root = await document.GetSyntaxRootAsync(cancellationToken);
            SyntaxNode newRoot = root.InsertNodesBefore(methodDeclaration.Body.ChildNodes().First(), new SyntaxNode[] { ifStatement });

            return document.WithSyntaxRoot(newRoot);
        }
    }
}