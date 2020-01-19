﻿using System;
using System.Collections.Generic;
using GParse;
using GParse.Lexing;
using GParse.Parsing;
using GParse.Parsing.Parselets;
using Loretta.Lexing;
using Loretta.Parsing.AST;
using LuaToken = GParse.Lexing.Token<Loretta.Lexing.LuaTokenType>;

namespace Loretta.Parsing.Modules
{
    internal class AnonymousFunctionExpressionParserModule : IPrefixParselet<LuaTokenType, Expression>
    {
        public static AnonymousFunctionExpressionParserModule Instance { get; } = new AnonymousFunctionExpressionParserModule ( );

        public static void Register ( IPrattParserBuilder<LuaTokenType, Expression> builder ) =>
            builder.Register ( LuaTokenType.Keyword, "function", Instance );

        public Boolean TryParse ( IPrattParser<LuaTokenType, Expression> genParser, IProgress<Diagnostic> diagnosticReporter, out Expression expression )
        {
            ITokenReader<LuaTokenType> reader = genParser.TokenReader;
            var parser = genParser as LuaParser;
            if ( !reader.Accept ( LuaTokenType.Keyword, "function", out LuaToken functionKw ) )
            {
                expression = default;
                return false;
            }

            Scope scope = parser.EnterScope ( true );
            var toks = new List<LuaToken>
            {
                functionKw,
            };

            if ( !reader.Accept ( LuaTokenType.LParen, out LuaToken lparen ) )
            {
                diagnosticReporter.Report ( LuaDiagnostics.SyntaxError.ThingExpectedFor ( reader.Location, '(', $"function keyword at line {functionKw.Range.Start.Line} column {functionKw.Range.Start.Column}" ) );
                lparen = TokenFactory.Token ( "(", LuaTokenType.LParen, "(" );
            }
            toks.Add ( lparen );

            LuaToken rparen;
            var argList = new List<Expression> ( );
            while ( !reader.Accept ( LuaTokenType.RParen, out rparen ) )
            {
                if ( reader.Accept ( LuaTokenType.VarArg, out LuaToken vararg ) )
                {
                    argList.Add ( new VarArgExpression ( vararg ) );
                    rparen = reader.FatalExpect ( LuaTokenType.RParen );
                    break;
                }
                else
                {
                    if ( !reader.Accept ( LuaTokenType.Identifier, out LuaToken arg ) )
                    {
                        rparen = TokenFactory.Token ( ")", LuaTokenType.RParen, ")" );
                        diagnosticReporter.Report ( LuaDiagnostics.SyntaxError.ThingExpectedFor ( reader.Location, "Identifier", "function" ) );
                        break;
                    }

                    argList.Add ( new IdentifierExpression ( arg, parser.GetOrCreateVariable ( arg, Scope.FindMode.CheckSelf ) ) );

                    if ( reader.Accept ( LuaTokenType.Comma, out LuaToken comma ) )
                    {
                        toks.Add ( comma );
                    }
                    else if ( !reader.IsAhead ( LuaTokenType.RParen ) )
                    {
                        rparen = TokenFactory.Token ( ")", LuaTokenType.RParen, ")" );
                        diagnosticReporter.Report ( LuaDiagnostics.SyntaxError.ThingExpected ( reader.Location, "')'" ) );
                        break;
                    }
                }
            }
            toks.Add ( rparen );

            StatementList body = parser.ParseStatementList ( scope );

            if ( !reader.Accept ( LuaTokenType.Keyword, "end", out LuaToken end ) )
            {
                end = TokenFactory.Token ( "end", LuaTokenType.Keyword, "end" );
                diagnosticReporter.Report ( LuaDiagnostics.SyntaxError.ThingExpectedFor ( reader.Location, "'end'", $"function starting at line {functionKw.Range.Start.Line} column {functionKw.Range.Start.Column}" ) );
            }
            toks.Add ( end );
            parser.LeaveScope ( );
            expression = new AnonymousFunctionExpression ( toks, argList, body );
            return true;
        }
    }
}