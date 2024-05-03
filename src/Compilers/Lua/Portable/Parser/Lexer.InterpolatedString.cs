// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.
// See the LICENSE file in the project root for more information.

using Loretta.CodeAnalysis.Lua.Utilities;
using Loretta.CodeAnalysis.PooledObjects;

namespace Loretta.CodeAnalysis.Lua.Syntax.InternalSyntax
{
    internal partial class Lexer
    {
        private void ScanInterpolatedStringLiteral(ref TokenInfo info)
        {
            ScanInterpolatedStringLiteralTop(
                ref info,
                out var error,
                openQuoteRange: out _,
                interpolations: null,
                closeQuoteRange: out _);
            if (error != null)
                this.AddError(error);
        }

        internal void ScanInterpolatedStringLiteralTop(
            ref TokenInfo info,
            out SyntaxDiagnosticInfo? error,
            out Range openQuoteRange,
            ArrayBuilder<Interpolation>? interpolations,
            out Range closeQuoteRange)
        {
            var subScanner = new InterpolatedStringScanner(this);
            subScanner.ScanInterpolatedStringLiteralTop(out openQuoteRange, interpolations, out closeQuoteRange);
            error = subScanner.Error;
            info.Kind = SyntaxKind.InterpolatedStringToken;
            info.Text = TextWindow.GetText(intern: false);
        }

        /// <summary>
        /// Non-copyable ref-struct so that this will only live on the stack for the lifetime of the lexer/parser
        /// recursing to process interpolated strings.
        /// </summary>
        [NonCopyable]
        private ref struct InterpolatedStringScanner
        {
            private readonly Lexer _lexer;

            /// <summary>
            /// Error encountered while scanning.  If we run into an error, then we'll attempt to stop parsing at the
            /// next potential ending location to prevent compounding the issue.
            /// </summary>
            public SyntaxDiagnosticInfo? Error = null;

            public InterpolatedStringScanner(Lexer lexer)
            {
                _lexer = lexer;
            }

            private bool IsAtEnd(bool allowNewline)
            {
                char ch = _lexer.TextWindow.PeekChar();
                return
                    (!allowNewline && CharUtils.IsNewLine(ch)) ||
                    (ch == SlidingTextWindow.InvalidCharacter && _lexer.TextWindow.IsReallyAtEnd());
            }

            private void TrySetError(SyntaxDiagnosticInfo error)
            {
                // only need to record the first error we hit
                Error ??= error;
            }

            internal void ScanInterpolatedStringLiteralTop(
                out Range openQuoteRange,
                ArrayBuilder<Interpolation>? interpolations,
                out Range closeQuoteRange)
            {
                // Scan through the open-quote portion of this literal, determining important information the rest of
                // the scanning needs.
                var start = _lexer.TextWindow.Position;
                var succeeded = ScanOpenQuote();
                LorettaDebug.Assert(_lexer.TextWindow.Position != start);

                openQuoteRange = start.._lexer.TextWindow.Position;

                if (!succeeded)
                {
                    // Processing the start of this literal didn't give us enough information to proceed.  Stop now,
                    // terminating the string to the furthest point we reached.
                    closeQuoteRange = _lexer.TextWindow.Position.._lexer.TextWindow.Position;
                    return;
                }

                ScanInterpolatedStringLiteralContents(interpolations);
                ScanInterpolatedStringLiteralEnd(out closeQuoteRange);
            }

            
            private bool ScanOpenQuote()
            {
                // Handles reading the start of the interpolated string literal (up to where the content begins)
                var window = _lexer.TextWindow;
                window.AdvanceChar(1);
                return true;
            }

            private void ScanInterpolatedStringLiteralEnd(out Range closeQuoteRange)
            {
                // Handles reading the end of the interpolated string literal (after where the content ends)
                var closeQuotePosition = _lexer.TextWindow.Position;
                ScanNormalOrVerbatimInterpolatedStringLiteralEnd();
                // Note: this range may be empty.  For example, if we hit the end of a line for a single-line construct,
                // or we hit the end of a file for a multi-line construct.
                closeQuoteRange = closeQuotePosition.._lexer.TextWindow.Position;
            }

            private void ScanNormalOrVerbatimInterpolatedStringLiteralEnd()
            {
                if (_lexer.TextWindow.PeekChar() != '`')
                {
                    // Didn't find a closing quote.  We hit the end of a line (in the normal case) or the end of the
                    // file in the normal/verbatim case.
                    LorettaDebug.Assert(IsAtEnd(false));

                    TrySetError(_lexer.MakeError(
                        IsAtEnd(allowNewline: true) ? _lexer.TextWindow.Position - 1 : _lexer.TextWindow.Position,
                        width: 1, ErrorCode.ERR_UnfinishedString));
                }
                else
                {
                    // found the closing quote
                    _lexer.TextWindow.AdvanceChar(); // `
                }
            }
            private void ScanInterpolatedStringLiteralContents(ArrayBuilder<Interpolation>? interpolations)
            {

                while (true)
                {
                    if (IsAtEnd(false))
                    {
                        // error: end of line/file before end of string pop out. Error will be reported in
                        // ScanInterpolatedStringLiteralEnd
                        return;
                    }

                    switch (_lexer.TextWindow.PeekChar())
                    {
                        case '`':
                            return;
                        case '}':
                            HandleCloseBraceInContent();
                            continue;
                        case '{':
                            if (_lexer.TextWindow.PeekChar(1) == '{')
                            {
                                TrySetError(_lexer.MakeError(_lexer.TextWindow.Position, 2, ErrorCode.ERR_DoubleBracesNotAllowed));
                            }
                            HandleOpenBraceInContent(interpolations);
                            continue;
                        case '\\':
                            var _ = _lexer.ParseEscapeSequence(true, true)!;
                            continue;

                        default:
                            // found some other character in the string portion.  Just consume it as content and continue.
                            _lexer.TextWindow.AdvanceChar();
                            continue;
                    }
                }
            }
            
            private void HandleCloseBraceInContent() => _lexer.TextWindow.AdvanceChar();
            
            private void HandleOpenBraceInContent(ArrayBuilder<Interpolation>? interpolations)
            {
                var openBracePosition = _lexer.TextWindow.Position;
                _lexer.TextWindow.AdvanceChar();
                ScanInterpolatedStringLiteralHoleBalancedText('}');
                var closeBracePosition = _lexer.TextWindow.Position;
                if (_lexer.TextWindow.PeekChar() == '}')
                {
                    _lexer.TextWindow.AdvanceChar();
                }
                else
                {
                    TrySetError(_lexer.MakeError(openBracePosition - 1, 2, ErrorCode.ERR_UnclosedExpressionHole));
                }

                interpolations?.Add(new Interpolation(
                    new Range(openBracePosition, openBracePosition + 1),
                    new Range(closeBracePosition, _lexer.TextWindow.Position)));
            }
            
            /// <summary>
            /// Scan past the hole inside an interpolated string literal, leaving the current character on the '}' (if any)
            /// </summary>
            private void ScanInterpolatedStringLiteralHoleBalancedText(char endingChar)
            {
                while (true)
                {
                    var ch = _lexer.TextWindow.PeekChar();

                    // Note: within a hole newlines are always allowed.  The restriction on if newlines are allowed or not
                    // is only within a text-portion of the interpolated string.
                    if (IsAtEnd(allowNewline: true))
                    {
                        // the caller will complain
                        return;
                    }

                    switch (ch)
                    {
                        case '`':
                        {
                            var discarded = default(TokenInfo);
                            if (_lexer.TryScanInterpolatedString(ref discarded))
                            {
                                continue;
                            }
                            goto default;
                        }
                        case '}':
                            if (ch == endingChar)
                            {
                                return;
                            }

                            TrySetError(_lexer.MakeError(_lexer.TextWindow.Position, 1, ErrorCode.ERR_SyntaxError, endingChar.ToString()));
                            goto default;
                        case '"':
                        case '\'':
                            // handle string literal inside an expression hole.
                            _lexer.ParseShortString();
                            continue;
                        case '[':
                            if (_lexer.ConsumeLongString(true, out _, out _))
                            {
                                continue;
                            }
                            _lexer.TextWindow.AdvanceChar();
                            continue;
                        
                        case '-':
                            if (_lexer.TextWindow.PeekChar(1) == '-')
                            {
                                _lexer.TextWindow.AdvanceChar(2);
                                if (!_lexer.ConsumeLongString(false, out _, out _))
                                    _lexer.ScanToEndOfLine();
                                continue;
                            }
                            _lexer.TextWindow.AdvanceChar();
                            continue;

                        case '/' when _lexer._options.SyntaxOptions.AcceptCCommentSyntax:
                            if ((ch = _lexer.TextWindow.PeekChar(1)) == '/')
                            {
                                _lexer.ScanToEndOfLine();
                                continue;
                            }
                            else if (ch == '*')
                            {
                                _lexer.ScanMultiLineCComment(out _);
                                continue;
                            }
                            
                            _lexer.TextWindow.AdvanceChar();
                            continue;
                        case '{':
                            ScanInterpolatedStringLiteralHoleBracketed('{', '}');
                            continue;
                        default:
                            // part of code in the expression hole
                            _lexer.TextWindow.AdvanceChar();
                            continue;
                    }
                }
            }

            private void ScanInterpolatedStringLiteralHoleBracketed(char start, char end)
            {
                LorettaDebug.Assert(start == _lexer.TextWindow.PeekChar());
                _lexer.TextWindow.AdvanceChar();
                ScanInterpolatedStringLiteralHoleBalancedText(end);
                if (_lexer.TextWindow.PeekChar() == end)
                {
                    _lexer.TextWindow.AdvanceChar();
                }
                else
                {
                    // an error was given by the caller
                }
            }
        }
    }
}
