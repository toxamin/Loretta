﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

using System.Text;

namespace Loretta.CodeAnalysis.Text
{
    /// <summary>
    /// A <see cref="SourceText"/> that represents a subrange of another <see cref="SourceText"/>.
    /// </summary>
    internal sealed class SubText : SourceText
    {
        public SubText(SourceText text, TextSpan span)
            : base(checksumAlgorithm: text.ChecksumAlgorithm)
        {
            if (span.Start < 0
                || span.Start >= text.Length
                || span.End < 0
                || span.End > text.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(span));
            }

            UnderlyingText = text ?? throw new ArgumentNullException(nameof(text));
            UnderlyingSpan = span;
        }

        public override Encoding? Encoding => UnderlyingText.Encoding;

        public SourceText UnderlyingText { get; }

        public TextSpan UnderlyingSpan { get; }

        public override int Length => UnderlyingSpan.Length;

        internal override int StorageSize => UnderlyingText.StorageSize;

        internal override SourceText StorageKey => UnderlyingText.StorageKey;

        public override char this[int position]
        {
            get
            {
                if (position < 0 || position > Length)
                {
                    throw new ArgumentOutOfRangeException(nameof(position));
                }

                return UnderlyingText[UnderlyingSpan.Start + position];
            }
        }

        public override string ToString(TextSpan span)
        {
            CheckSubSpan(span);

            return UnderlyingText.ToString(GetCompositeSpan(span.Start, span.Length));
        }

        public override SourceText GetSubText(TextSpan span)
        {
            CheckSubSpan(span);

            return new SubText(UnderlyingText, GetCompositeSpan(span.Start, span.Length));
        }

        public override void CopyTo(int sourceIndex, char[] destination, int destinationIndex, int count)
        {
            var span = GetCompositeSpan(sourceIndex, count);
            UnderlyingText.CopyTo(span.Start, destination, destinationIndex, span.Length);
        }

        private TextSpan GetCompositeSpan(int start, int length)
        {
            int compositeStart = Math.Min(UnderlyingText.Length, UnderlyingSpan.Start + start);
            int compositeEnd = Math.Min(UnderlyingText.Length, compositeStart + length);
            return new TextSpan(compositeStart, compositeEnd - compositeStart);
        }
    }
}
