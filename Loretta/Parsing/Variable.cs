﻿using System;

namespace Loretta.Parsing
{
    public class Variable
    {
        public String Identifier { get; private set; }
        public readonly Scope ParentScope;

        public Variable ( String name, Scope scope )
        {
            this.Identifier = name;
            this.ParentScope = scope;
        }

        public void Rename ( String newIdentifier ) => this.Identifier = newIdentifier;
    }
}