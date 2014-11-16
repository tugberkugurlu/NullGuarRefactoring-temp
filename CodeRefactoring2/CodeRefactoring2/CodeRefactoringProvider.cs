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
            if(methodDeclaration != null)
            {
                if (methodDeclaration.ParameterList.Parameters.Any())
                {
                    foreach (ParameterSyntax parameter in methodDeclaration.ParameterList.Parameters)
                    {
                        TypeSyntax paramTypeName = parameter.Type;
                        SemanticModel semanticModel = await context.Document.GetSemanticModelAsync();
                        ITypeSymbol type = semanticModel.GetTypeInfo(paramTypeName).ConvertedType;

                        if (type.IsReferenceType)
                        {
                            // TODO: check if the null guard already exists.
                            // TODO: if not, instert provide code action to instert the guard.
                            // TODO: when inserting the guard, make sure it stays in order.

                            BlockSyntax blockSyntax = methodDeclaration.Body;
                            IEnumerable<IfStatementSyntax> ifStatements = blockSyntax.ChildNodes().OfType<IfStatementSyntax>();
                            if (ifStatements.Any())
                            {
                                foreach (IfStatementSyntax ifStatement in ifStatements)
                                {
                                }
                                // check if there is any if statement for null guard for this param.
                            }
                        }
                    }
                }
            }
        }
    }
}