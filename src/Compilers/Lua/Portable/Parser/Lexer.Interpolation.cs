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
            public readonly Range CloseBraceRange;
            public Interpolation(Range openBraceRange, Range closeBraceRange)
            {
                OpenBraceRange = openBraceRange;
                CloseBraceRange = closeBraceRange;
            }
        }
    }
}
