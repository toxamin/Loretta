﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Loretta.CodeAnalysis.Text;

namespace Loretta.CodeAnalysis
{
    /// <summary>
    /// A reference to a syntax node.
    /// </summary>
    public abstract class SyntaxReference
    {
        /// <summary>
        /// The syntax tree that this references a node within.
        /// </summary>
        public abstract SyntaxTree SyntaxTree { get; }

        /// <summary>
        /// The span of the node referenced.
        /// </summary>
        public abstract TextSpan Span { get; }

        /// <summary>
        /// Retrieves the original referenced syntax node.  
        /// This action may cause a parse to happen to recover the syntax node.
        /// </summary>
        /// <returns>The original referenced syntax node.</returns>
        public abstract SyntaxNode GetSyntax(CancellationToken cancellationToken = default);

        /// <summary>
        /// Retrieves the original referenced syntax node.  
        /// This action may cause a parse to happen to recover the syntax node.
        /// </summary>
        /// <returns>The original referenced syntax node.</returns>
        public virtual Task<SyntaxNode> GetSyntaxAsync(CancellationToken cancellationToken = default) => Task.FromResult(GetSyntax(cancellationToken));

        /// <summary>
        /// The location of this syntax reference.
        /// </summary>
        /// <returns>The location of this syntax reference.</returns>
        /// <remarks>
        /// More performant than GetSyntax().GetLocation().
        /// </remarks>
        internal Location GetLocation() => SyntaxTree.GetLocation(Span);
    }
}
