﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using Loretta.CodeAnalysis.Text;

namespace Loretta.CodeAnalysis.Lua
{
    /// <summary>
    /// An enumerator for diagnostic lists.
    /// </summary>
    internal struct SyntaxTreeDiagnosticEnumerator
    {
        private readonly SyntaxTree? _syntaxTree;
        private NodeIterationStack _stack;
        private Diagnostic? _current;
        private int _position;
        private const int DefaultStackCapacity = 8;

        internal SyntaxTreeDiagnosticEnumerator(SyntaxTree syntaxTree, GreenNode? node, int position)
        {
            _syntaxTree = null;
            _current = null;
            _position = position;
            if (node != null && node.ContainsDiagnostics)
            {
                _syntaxTree = syntaxTree;
                _stack = new NodeIterationStack(DefaultStackCapacity);
                _stack.PushNodeOrToken(node);
            }
            else
            {
                _stack = new NodeIterationStack();
            }
        }

        /// <summary>
        /// Moves the enumerator to the next diagnostic instance in the diagnostic list.
        /// </summary>
        /// <returns>Returns true if enumerator moved to the next diagnostic, false if the
        /// enumerator was at the end of the diagnostic list.</returns>
        public bool MoveNext()
        {
            while (_stack.Any())
            {
                var diagIndex = _stack.Top.DiagnosticIndex;
                var node = _stack.Top.Node;
                var diags = node.GetDiagnostics();
                if (diagIndex < diags.Length - 1)
                {
                    diagIndex++;
                    var sdi = (SyntaxDiagnosticInfo) diags[diagIndex];

                    //for tokens, we've already seen leading trivia on the stack, so we have to roll back
                    //for nodes, we have yet to see the leading trivia
                    var leadingWidthAlreadyCounted = node.IsToken ? node.GetLeadingTriviaWidth() : 0;

                    // don't produce locations outside of tree span
                    LorettaDebug.Assert(_syntaxTree is not null);
                    var length = _syntaxTree.GetRoot().FullSpan.Length;
                    var spanStart = Math.Max(0, Math.Min(_position - leadingWidthAlreadyCounted + sdi.Offset, length));
                    var spanWidth = Math.Min(spanStart + sdi.Width, length) - spanStart;

                    _current = new LuaDiagnostic(sdi, new SourceLocation(_syntaxTree, new TextSpan(spanStart, spanWidth)));

                    _stack.UpdateDiagnosticIndexForStackTop(diagIndex);
                    return true;
                }

                var slotIndex = _stack.Top.SlotIndex;
            tryAgain:
                if (slotIndex < node.SlotCount - 1)
                {
                    slotIndex++;
                    var child = node.GetSlot(slotIndex);
                    if (child == null)
                    {
                        goto tryAgain;
                    }

                    if (!child.ContainsDiagnostics)
                    {
                        _position += child.FullWidth;
                        goto tryAgain;
                    }

                    _stack.UpdateSlotIndexForStackTop(slotIndex);
                    _stack.PushNodeOrToken(child);
                }
                else
                {
                    if (node.SlotCount == 0)
                    {
                        _position += node.Width;
                    }

                    _stack.Pop();
                }
            }

            return false;
        }

        /// <summary>
        /// The current diagnostic that the enumerator is pointing at.
        /// </summary>
        public Diagnostic Current
        {
            get { LorettaDebug.Assert(_current is not null); return _current; }
        }

        private struct NodeIteration
        {
            internal readonly GreenNode Node;
            internal int DiagnosticIndex;
            internal int SlotIndex;

            internal NodeIteration(GreenNode node)
            {
                Node = node;
                SlotIndex = -1;
                DiagnosticIndex = -1;
            }
        }

        private struct NodeIterationStack
        {
            private NodeIteration[] _stack;
            private int _count;

            internal NodeIterationStack(int capacity)
            {
                LorettaDebug.Assert(capacity > 0);
                _stack = new NodeIteration[capacity];
                _count = 0;
            }

            internal void PushNodeOrToken(GreenNode node)
            {
                if (node is Syntax.InternalSyntax.SyntaxToken token)
                {
                    PushToken(token);
                }
                else
                {
                    Push(node);
                }
            }

            private void PushToken(Syntax.InternalSyntax.SyntaxToken token)
            {
                var trailing = token.GetTrailingTrivia();
                if (trailing != null)
                {
                    Push(trailing);
                }

                Push(token);
                var leading = token.GetLeadingTrivia();
                if (leading != null)
                {
                    Push(leading);
                }
            }

            private void Push(GreenNode node)
            {
                if (_count >= _stack.Length)
                {
                    var tmp = new NodeIteration[_stack.Length * 2];
                    Array.Copy(_stack, tmp, _stack.Length);
                    _stack = tmp;
                }

                _stack[_count] = new NodeIteration(node);
                _count++;
            }

            internal void Pop() => _count--;

            internal bool Any() => _count > 0;

            internal NodeIteration Top => this[_count - 1];

            internal NodeIteration this[int index]
            {
                get
                {
                    LorettaDebug.Assert(_stack != null);
                    LorettaDebug.Assert(index >= 0 && index < _count);
                    return _stack[index];
                }
            }

            internal void UpdateSlotIndexForStackTop(int slotIndex)
            {
                LorettaDebug.Assert(_stack != null);
                LorettaDebug.Assert(_count > 0);
                _stack[_count - 1].SlotIndex = slotIndex;
            }

            internal void UpdateDiagnosticIndexForStackTop(int diagnosticIndex)
            {
                LorettaDebug.Assert(_stack != null);
                LorettaDebug.Assert(_count > 0);
                _stack[_count - 1].DiagnosticIndex = diagnosticIndex;
            }
        }
    }
}
