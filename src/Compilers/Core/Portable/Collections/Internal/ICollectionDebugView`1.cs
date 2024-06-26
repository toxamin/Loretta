﻿// Licensed to the .NET Foundation under one or more agreements.
// The .NET Foundation licenses this file to you under the MIT license.

// NOTE: This code is derived from an implementation originally in dotnet/runtime:
// https://github.com/dotnet/runtime/blob/v5.0.2/src/libraries/System.Private.CoreLib/src/System/Collections/Generic/ICollectionDebugView.cs
//
// See the commentary in https://github.com/dotnet/roslyn/pull/50156 for notes on incorporating changes made to the
// reference implementation.

using System.Diagnostics;

namespace Loretta.CodeAnalysis.Collections.Internal
{
    internal sealed class ICollectionDebugView<T>
    {
        private readonly ICollection<T> _collection;

        public ICollectionDebugView(ICollection<T> collection)
        {
            _collection = collection ?? throw new ArgumentNullException(nameof(collection));
        }

        [DebuggerBrowsable(DebuggerBrowsableState.RootHidden)]
        public T[] Items
        {
            get
            {
                var items = new T[_collection.Count];
                _collection.CopyTo(items, 0);
                return items;
            }
        }
    }
}
