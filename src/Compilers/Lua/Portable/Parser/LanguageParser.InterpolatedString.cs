// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using System;
using System.Diagnostics;
using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;
using Loretta.CodeAnalysis.Lua.Utilities;
using Loretta.CodeAnalysis.PooledObjects;
using Loretta.CodeAnalysis.Text;

namespace Loretta.CodeAnalysis.Lua.Syntax.InternalSyntax
{
    internal partial class LanguageParser
    {
        private ExpressionSyntax ParseInterpolatedStringToken()
        {
            // We don't want to make the scanner stateful (between tokens) if we can possibly avoid it.
            // The approach implemented here is
            //
            // (1) Scan the whole interpolated string literal as a single token. Now the statefulness of
            // the scanner (to match { }'s) is limited to its behavior while scanning a single token.
            //
            // (2) When the parser gets such a token, here, it spins up another scanner / parser on each of
            // the holes and builds a tree for the whole thing (resulting in an InterpolatedStringExpressionSyntax).
            //
            // (3) The parser discards the original token and replaces it with this tree. (In other words,
            // it replaces one token with a different set of tokens that have already been parsed)
            //
            // (4) On an incremental change, we widen the invalidated region to include any enclosing interpolated
            // string nonterminal so that we never reuse tokens inside a changed interpolated string.
            //
            // This has the secondary advantage that it can reasonably be specified.
            // 
            // The substitution will end up being invisible to external APIs and clients such as the IDE, as
            // they have no way to ask for the stream of tokens before parsing.

            Debug.Assert(this.CurrentToken.Kind == SyntaxKind.InterpolatedStringToken);
            var originalToken = this.EatToken();

            var originalText = originalToken.ValueText!; // this is actually the source text
            var originalTextSpan = originalText.AsSpan();
            Debug.Assert(originalText[0] == '`');

            // compute the positions of the interpolations in the original string literal, if there was an error or not,
            // and where the open and close quotes can be found.
            var interpolations = ArrayBuilder<Lexer.Interpolation>.GetInstance();

            rescanInterpolation(out var error, out var openQuoteRange, interpolations, out var closeQuoteRange);

            // Only bother trying to do dedentation if we have a multiline literal without errors.  There's no point
            // trying in the presence of errors as we may not even be able to determine what the dedentation should be.

            var result = SyntaxFactory.InterpolatedStringExpression(getOpenQuote(), getContent(originalTextSpan), getCloseQuote());

            interpolations.Free();
            if (error != null)
            {
                // Errors are positioned relative to the start of the token that was lexed.  Specifically relative to
                // the starting `$` or `@`.  However, when placed on a node like this, it will be relative to the node's
                // full start.  So we have to adjust the diagnostics taking that into account.
                result = result.WithDiagnosticsGreen(MoveDiagnostics(new[] { error }, originalToken.GetLeadingTrivia()?.FullWidth ?? 0));
            }

            Debug.Assert(originalToken.ToFullString() == result.ToFullString()); // yield from text equals yield from node
            return result;

            void rescanInterpolation(out SyntaxDiagnosticInfo? error, out Range openQuoteRange, ArrayBuilder<Lexer.Interpolation> interpolations, out Range closeQuoteRange)
            {
                using var tempLexer = new Lexer(SourceText.From(originalText), this.Options);
                var info = default(Lexer.TokenInfo);
                tempLexer.ScanInterpolatedStringLiteralTop(ref info, out error, out openQuoteRange, interpolations, out closeQuoteRange);
            }

            SyntaxToken getOpenQuote()
            {
                return SyntaxFactory.Token(
                    originalToken.GetLeadingTrivia(),
                    SyntaxKind.InterpolatedStringStartToken,
                    originalText[openQuoteRange],
                    trailing: null);
            }

            CodeAnalysis.Syntax.InternalSyntax.SyntaxList<InterpolatedStringContentSyntax> getContent(ReadOnlySpan<char> originalTextSpan)
            {
                var content = PooledStringBuilder.GetInstance();
                var builder = _pool.Allocate<InterpolatedStringContentSyntax>();

                ReadOnlySpan<char> indentationWhitespace = default;

                var currentContentStart = openQuoteRange.End;
                for (var i = 0; i < interpolations.Count; i++)
                {
                    var interpolation = interpolations[i];

                    // Add a token for text preceding the interpolation
                    builder.Add(makeContent(originalTextSpan[currentContentStart..interpolation.OpenBraceRange.Start])!);

                    // Now parse the interpolation itself.
                    var interpolationNode = ParseInterpolation(this.Options, originalText, interpolation);

                    // Make sure the interpolation starts at the right location.

                    builder.Add(interpolationNode);
                    currentContentStart = interpolation.CloseBraceRange.End;
                }

                // Add a token for text following the last interpolation
                builder.Add(makeContent(originalTextSpan[currentContentStart..closeQuoteRange.Start])!);

                CodeAnalysis.Syntax.InternalSyntax.SyntaxList<InterpolatedStringContentSyntax> result = builder;
                _pool.Free(builder);
                content.Free();
                return result;
            }

            InterpolatedStringContentSyntax? makeContent(ReadOnlySpan<char> text)
            {
                if (text.Length == 0)
                    return null;
                return SyntaxFactory.InterpolatedStringText(MakeInterpolatedStringTextToken(text.ToString()));
            }

            SyntaxToken getCloseQuote()
            {
                // Make a token for the close quote " (even if it was missing)
                return TokenOrMissingToken(
                    leading: null,
                    SyntaxKind.InterpolatedStringEndToken,
                    originalText[closeQuoteRange],
                    originalToken.GetTrailingTrivia());
            }
            
        }

        private static bool CheckForSpaceDifference(
            ReadOnlySpan<char> currentLineWhitespace,
            ReadOnlySpan<char> indentationLineWhitespace,
            [NotNullWhen(true)] out string? currentLineMessage,
            [NotNullWhen(true)] out string? indentationLineMessage)
        {
            for (int i = 0, n = Math.Min(currentLineWhitespace.Length, indentationLineWhitespace.Length); i < n; i++)
            {
                var currentLineChar = currentLineWhitespace[i];
                var indentationLineChar = indentationLineWhitespace[i];

                if (currentLineChar != indentationLineChar &&
                    CharUtils.IsWhitespace(currentLineChar) &&
                    CharUtils.IsWhitespace(indentationLineChar))
                {
                    currentLineMessage = CharUtils.CharToString(currentLineChar);
                    indentationLineMessage = CharUtils.CharToString(indentationLineChar);
                    return true;
                }
            }

            currentLineMessage = null;
            indentationLineMessage = null;
            return false;
        }

        private static SyntaxToken TokenOrMissingToken(GreenNode? leading, SyntaxKind kind, string text, GreenNode? trailing)
            => text == ""
                ? SyntaxFactory.MissingToken(leading, kind, trailing)
                : SyntaxFactory.Token(leading, kind, text, trailing);

        private static InterpolationSyntax ParseInterpolation(
            LuaParseOptions options,
            string text,
            Lexer.Interpolation interpolation)
        {
            // Grab the text from after the { all the way to the start of the } (or the start of the : if present). This
            // will be used to parse out the expression of the interpolation.
            //
            // The parsing of the open brace, close brace and colon is specially handled in ParseInterpolation below.
            var followingRange = interpolation.CloseBraceRange;
            var expressionText = text[interpolation.OpenBraceRange.End..followingRange.Start];

            using var tempLexer = new Lexer(SourceText.From(expressionText), options);

            // First grab any trivia right after the {, it will be trailing trivia for the { token.
            var openTokenTrailingTrivia = tempLexer.LexSyntaxTrailingTrivia().Node;

            // Now create a parser to actually handle the expression portion of the interpolation
            using var tempParser = new LanguageParser(tempLexer, oldTree: null, changes: null);

            var result = tempParser.ParseInterpolation(
                text, interpolation,
                SyntaxFactory.Token(leading: null, SyntaxKind.OpenBraceToken, text[interpolation.OpenBraceRange], openTokenTrailingTrivia));

            Debug.Assert(text[interpolation.OpenBraceRange.Start..interpolation.CloseBraceRange.End] == result.ToFullString()); // yield from text equals yield from node
            return result;
        }

        private InterpolationSyntax ParseInterpolation(
            string text,
            Lexer.Interpolation interpolation,
            SyntaxToken openBraceToken)
        {
            var expression = getExpressionAndAlignment();
            var closeBraceToken = getFormatAndCloseBrace();

            var result = SyntaxFactory.Interpolation(openBraceToken, expression, closeBraceToken);
#if DEBUG
            Debug.Assert(text[interpolation.OpenBraceRange.Start..interpolation.CloseBraceRange.End] == result.ToFullString()); // yield from text equals yield from node
#endif
            return result;

            ExpressionSyntax getExpressionAndAlignment()
            {
                var expression = this.ParseExpression();

                return this.ConsumeUnexpectedTokens(expression);
            }

            SyntaxToken getFormatAndCloseBrace()
            {
                var leading = this.CurrentToken.GetLeadingTrivia();
                return getInterpolationCloseToken(leading);
            }

            SyntaxToken getInterpolationCloseToken(GreenNode? leading)
            {
                return TokenOrMissingToken(
                    leading,
                    SyntaxKind.CloseBraceToken,
                    text[interpolation.CloseBraceRange],
                    trailing: null);
            }
        }

        /// <summary>
        /// Interpret the given raw text from source as an InterpolatedStringTextToken.
        /// </summary>
        /// <param name="text">The text for the full string literal, including the quotes and contents</param>
        /// <param name="kind">The kind of the interpolated string we were processing</param>
        private SyntaxToken MakeInterpolatedStringTextToken(string text)
        {
            // For a normal/verbatim piece of content, process the inner content as if it was in a corresponding
            // *non*-interpolated string to get the correct meaning of all the escapes/diagnostics within.
            var prefix = "`";
            var fakeString = prefix + "`";
            using var tempLexer = new Lexer(SourceText.From(fakeString), this.Options, isInterpolatedReparsing: true);
            var token = tempLexer.Lex();
            Debug.Assert(token.Kind == SyntaxKind.StringLiteralToken);
            var result = SyntaxFactory.Literal(leading: null, text, SyntaxKind.InterpolatedStringTextToken, token.ValueText!, trailing: null);
            if (token.ContainsDiagnostics)
                result = result.WithDiagnosticsGreen(MoveDiagnostics(token.GetDiagnostics(), -prefix.Length));

            return result;
        }

        private static DiagnosticInfo[] MoveDiagnostics(DiagnosticInfo[] infos, int offset)
        {
            Debug.Assert(infos.Length > 0);
            var builder = ArrayBuilder<DiagnosticInfo>.GetInstance(infos.Length);
            foreach (var info in infos)
            {
                // This cast should always be safe.  We are only moving diagnostics produced on syntax nodes and tokens.
                var sd = (SyntaxDiagnosticInfo)info;
                builder.Add(sd.WithOffset(sd.Offset + offset));
            }

            return builder.ToArrayAndFree();
        }
    }
}
