using System;

namespace TypeShape.CSharp.Tests
{
    public record SimpleCSharpRecord(int x, string y, bool z);

    public record DerivedCSharpRecord(int x, string y, bool z, byte w) : SimpleCSharpRecord(x, y, z);

    public record CSharpRecordWithProps
    {
        public int X { get; set; }
        public string Y { get; set; }
        public bool Z { get; }
    }
}