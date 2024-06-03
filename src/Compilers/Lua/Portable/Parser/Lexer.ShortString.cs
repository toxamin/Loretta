using System.Text;
using Loretta.CodeAnalysis.Lua.Utilities;

namespace Loretta.CodeAnalysis.Lua.Syntax.InternalSyntax
{
    internal sealed partial class Lexer
    {
        private string? ParseEscapeSequence(bool needsReturn = false, bool allowEscapingOpenBrace = false)
        {
            StringBuilder? temporaryBuilder = null;
            if (needsReturn)
                temporaryBuilder = new StringBuilder();
            LorettaDebug.Assert(TextWindow.PeekChar() == '\\');
            var escapeStart = TextWindow.Position;
            TextWindow.AdvanceChar();
            char ch;
            switch (ch = TextWindow.PeekChar())
            {
                case '\n':
                case '\r':
                {
                    AppendChar(TextWindow.NextChar());
                    char ch2;
                    if (CharUtils.IsNewLine(ch2 = TextWindow.PeekChar())
                        && ch != ch2)
                    {
                        AppendChar(TextWindow.NextChar());
                    }

                    break;
                }

                case 'a':
                    TextWindow.AdvanceChar();
                    AppendChar('\a');
                    break;

                case 'b':
                    TextWindow.AdvanceChar();
                    AppendChar('\b');
                    break;

                case 'f':
                    TextWindow.AdvanceChar();
                    AppendChar('\f');
                    break;

                case 'n':
                    TextWindow.AdvanceChar();
                    AppendChar('\n');
                    break;

                case 'r':
                    TextWindow.AdvanceChar();
                    AppendChar('\r');
                    break;

                case 't':
                    TextWindow.AdvanceChar();
                    AppendChar('\t');
                    break;

                case 'v':
                    TextWindow.AdvanceChar();
                    AppendChar('\v');
                    break;

                case '\\':
                    TextWindow.AdvanceChar();
                    AppendChar('\\');
                    break;

                case '\'':
                    TextWindow.AdvanceChar();
                    AppendChar('\'');
                    break;

                case '"':
                    TextWindow.AdvanceChar();
                    AppendChar('"');
                    break;
                
                case '`' when _options.SyntaxOptions.AcceptInterpolatedStrings:
                    TextWindow.AdvanceChar();
                    AppendChar('`');
                    break;
                
                case '{' when allowEscapingOpenBrace:
                    TextWindow.AdvanceChar();
                    AppendChar('{');
                    break;

                case 'z':
                    if (_options.SyntaxOptions.AcceptInvalidEscapes && !_options.SyntaxOptions.AcceptWhitespaceEscape)
                        goto default;

                    TextWindow.AdvanceChar();

                    while (CharUtils.IsWhitespace(TextWindow.PeekChar()))
                        TextWindow.AdvanceChar();

                    if (!_options.SyntaxOptions.AcceptWhitespaceEscape)
                        AddError(escapeStart, TextWindow.Position - escapeStart,
                            ErrorCode.ERR_WhitespaceEscapeNotSupportedInVersion);
                    break;

                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                {
                    var parsedCharInteger = parseDecimalInteger(escapeStart);
                    if (parsedCharInteger != char.MaxValue)
                        AppendChar(parsedCharInteger);
                    break;
                }

                case 'x':
                {
                    if (_options.SyntaxOptions.AcceptInvalidEscapes &&
                        !_options.SyntaxOptions.AcceptHexEscapesInStrings)
                        goto default;

                    TextWindow.AdvanceChar();
                    var parsedCharInteger = parseHexadecimalEscapeInteger(escapeStart);
                    if (parsedCharInteger != char.MaxValue)
                        AppendChar(parsedCharInteger);

                    if (!_options.SyntaxOptions.AcceptHexEscapesInStrings)
                        AddError(escapeStart, TextWindow.Position - escapeStart,
                            ErrorCode.ERR_HexStringEscapesNotSupportedInVersion);
                }
                    break;

                case 'u':
                {
                    if (_options.SyntaxOptions.AcceptInvalidEscapes && !_options.SyntaxOptions.AcceptUnicodeEscape)
                        goto default;

                    TextWindow.AdvanceChar();
                    var parsed = parseUnicodeEscape(escapeStart);
                    Append(parsed);

                    if (!_options.SyntaxOptions.AcceptUnicodeEscape)
                        AddError(escapeStart, TextWindow.Position - escapeStart,
                            ErrorCode.ERR_UnicodeEscapesNotSupportedLuaInVersion);
                }
                    break;

                default:
                    if (!_options.SyntaxOptions.AcceptInvalidEscapes)
                    {
                        // Skip the character after the escape.
                        TextWindow.AdvanceChar();
                        AddError(escapeStart, TextWindow.Position - escapeStart, ErrorCode.ERR_InvalidStringEscape);
                    }

                    break;
            }

            return needsReturn ? temporaryBuilder!.ToString() : null;

            void Append(string s)
            {
                if (needsReturn)
                    temporaryBuilder!.Append(s);
                else
                    _builder.Append(s);
            }
            void AppendChar(char c)
            {
                if (needsReturn)
                    temporaryBuilder!.Append(c);
                else
                    _builder.Append(c);
            }
        }

        private string ParseShortString()
        {
            _builder.Clear();
            var delim = TextWindow.NextChar();
            LorettaDebug.Assert(delim is '"' or '\'' or '`');

            char ch;
            while (!IsAtEnd(ch = TextWindow.PeekChar()) && ch != delim)
            {
                var charStart = TextWindow.Position;
                switch (ch)
                {
                    #region Escapes

                    case '\\':
                    {
                        ParseEscapeSequence(false, delim == '`');
                    }
                        break;

                    #endregion Escapes

                    case '\n':
                    case '\r':
                    {
                        _builder.Append(TextWindow.NextChar());
                        char ch2;
                        if (CharUtils.IsNewLine(ch2 = TextWindow.PeekChar())
                            && ch != ch2)
                        {
                            _builder.Append(TextWindow.NextChar());
                        }

                        AddError(charStart, TextWindow.Position - charStart, ErrorCode.ERR_UnescapedLineBreakInString);
                    }
                        break;

                    default:
                        _builder.Append(TextWindow.NextChar());
                        break;
                }
            }

            if (TextWindow.PeekChar() == delim)
            {
                TextWindow.AdvanceChar();
            }
            else
            {
                AddError(ErrorCode.ERR_UnfinishedString);
            }

            return TextWindow.Intern(_builder);
        }

        private char parseDecimalInteger(int start)
        {
            var readChars = 0;
            var num = 0;
            char ch;
            while (readChars < 3 && CharUtils.IsDecimal(ch = TextWindow.PeekChar()))
            {
                TextWindow.AdvanceChar();
                num = (num * 10) + (ch - '0');
                readChars++;
            }

            if (readChars < 1 || num > 255)
            {
                AddError(start, TextWindow.Position - start, ErrorCode.ERR_InvalidStringEscape);
                return char.MaxValue;
            }

            return (char) num;
        }

        private ulong parseHexadecimalNumber(int start, int maxDigits, ErrorCode lessThanZeroErrorCode)
        {
            var readChars = 0;
            var num = 0L;
            while (readChars < maxDigits)
            {
                var peek = TextWindow.PeekChar();
                if (CharUtils.IsDecimal(peek))
                {
                    TextWindow.AdvanceChar();
                    num = (num << 4) | (uint) (peek - '0');
                }
                else if (CharUtils.IsHexadecimal(peek))
                {
                    TextWindow.AdvanceChar();
                    num = (num << 4) | (uint) (10 + CharUtils.AsciiLowerCase(peek) - 'a');
                }
                else
                {
                    break;
                }

                readChars++;
            }

            if (readChars < 1)
            {
                AddError(start, TextWindow.Position - start, lessThanZeroErrorCode);
                return 0UL;
            }

            return (ulong) num;
        }

        private char parseHexadecimalEscapeInteger(int start) =>
            (char) parseHexadecimalNumber(start, 2, ErrorCode.ERR_InvalidStringEscape);

        private string parseUnicodeEscape(int start)
        {
            var missingOpeningBrace = TextWindow.PeekChar() is not '{';
            if (!missingOpeningBrace)
                TextWindow.AdvanceChar();

            var codepoint = parseHexadecimalNumber(start, 16, ErrorCode.ERR_HexDigitExpected);

            var missingClosingBrace = TextWindow.PeekChar() is not '}';
            if (!missingClosingBrace)
                TextWindow.AdvanceChar();

            if (missingOpeningBrace)
                AddError(start, TextWindow.Position - start, ErrorCode.ERR_UnicodeEscapeMissingOpenBrace);
            if (missingClosingBrace)
                AddError(start, TextWindow.Position - start, ErrorCode.ERR_UnicodeEscapeMissingCloseBrace);
            if (codepoint > 0x10FFFF)
            {
                AddError(start, TextWindow.Position - start, ErrorCode.ERR_EscapeTooLarge, "10FFFF");
                codepoint = 0x10FFFF;
            }

            // Return the codepoint itself if it's in the BMP.
            // NOTE: It *is* technically incorrect to consider a surrogate
            // an Unicode codepoint but Lua accepts it so we do it as well.
            
            var builder = new StringBuilder();
            foreach (var b in Utf8esc((uint) codepoint))
            {
                builder.Append((char) b);
            }
            return builder.ToString();
        }
        
        private const int UTF8BUFFSZ = 8;
        
        private static int LuaO_utf8esc(byte[] buff, uint x)
        {
            int n = 1; // number of bytes put in buffer (backwards)
            if (x < 0x80) // ascii?
                buff[UTF8BUFFSZ - 1] = (byte)x;
            else
            { // need continuation bytes
                uint mfb = 0x3f; // maximum that fits in first byte
                do
                { // add continuation bytes
                    buff[UTF8BUFFSZ - (n++)] = (byte)(0x80 | (x & 0x3f));
                    x >>= 6; // remove added bits
                    mfb >>= 1; // now there is one less bit available in the first byte
                } while (x > mfb); // still needs continuation byte?
                buff[UTF8BUFFSZ - n] = (byte)((~mfb << 1) | x); // add first byte
            }
            return n;
        }
        
        private static byte[] Utf8esc(uint escape)
        {
            var buff = new byte[UTF8BUFFSZ];
            var bytes = new List<byte>();
            var n = LuaO_utf8esc(buff, escape);
            for (; n > 0; n--) // add 'buff' to string
                bytes.Add(buff[UTF8BUFFSZ - n]);
            return bytes.ToArray();
        }
    }
}
