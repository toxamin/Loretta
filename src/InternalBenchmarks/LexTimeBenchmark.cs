﻿using BenchmarkDotNet.Attributes;
using BenchmarkDotNet.Jobs;
using Loretta.CodeAnalysis;
using Loretta.CodeAnalysis.Lua;
using Loretta.InternalBenchmarks.Attributes;

#nullable disable

namespace Loretta.InternalBenchmarks
{
    [SimpleJob(RuntimeMoniker.Net60)]
    [MeanThroughputColumn("File"), MedianColumn, MedianThroughputColumn("File")]
    [MemoryDiagnoser]
    public class LexTimeBenchmark
    {
        private static readonly LuaParseOptions s_parseOptions = new(LuaSyntaxOptions.All);

        [ParamsSource(nameof(Files))]
        public TestFile File { get; set; }

        public static IEnumerable<TestFile> Files
        {
            get
            {
                yield return TestFile.Load("samples/benchies/anim.lua");
                yield return TestFile.Load("samples/benchies/rustic.lua");
                yield return TestFile.Load("samples/benchies/rustic-24mb.lua");
            }
        }

        [Benchmark]
        public ImmutableArray<SyntaxToken> Lex() =>
            SyntaxFactory.ParseTokens(File.Text, options: s_parseOptions).ToImmutableArray();
    }
}
