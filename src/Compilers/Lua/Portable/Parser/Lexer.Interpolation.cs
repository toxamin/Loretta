using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Loretta.CodeAnalysis.Lua.Syntax.InternalSyntax
{
    internal partial class Lexer
    {
        internal readonly struct Interpolation
        {
            public readonly Range OpenBraceRange;

            /// <summary>
            /// Range of the format colon in the interpolation.  Empty if there is no colon.
            /// </summary>
            public readonly Range ColonRange;

            /// <summary>
            /// Range of the close brace.  Empty if there was no close brace (an error condition).
            /// </summary>
            public readonly Range CloseBraceRange;

            public bool HasColon => ColonRange.Start.Value != ColonRange.End.Value;

            public Interpolation(Range openBraceRange, Range colonRange, Range closeBraceRange)
            {
                OpenBraceRange = openBraceRange;
                ColonRange = colonRange;
                CloseBraceRange = closeBraceRange;
            }
        }
    }
}
