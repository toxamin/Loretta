﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

namespace Loretta.CodeAnalysis.Syntax.InternalSyntax
{
    internal static class GreenNodeExtensions
    {
        internal static SyntaxList<T> ToGreenList<T>(this SyntaxNode? node) where T : GreenNode
        {
            return node != null ?
                ToGreenList<T>(node.Green) :
                default;
        }

        internal static SeparatedSyntaxList<T> ToGreenSeparatedList<T>(this SyntaxNode? node) where T : GreenNode
        {
            return node != null ?
                new SeparatedSyntaxList<T>(ToGreenList<T>(node.Green)) :
                default;
        }

        internal static SyntaxList<T> ToGreenList<T>(this GreenNode? node) where T : GreenNode =>
            new(node);
    }
}
