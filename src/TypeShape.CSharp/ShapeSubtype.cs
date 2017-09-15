using System;

// Binding type arguments in subtype constraint currently not possible in C#,
// defining those shape types in C#.

namespace TypeShape
{
    public interface ISubtypeVisitor<TBase, TResult>
    {
        TResult Visit<TSubtype>() where TSubtype : TBase;
    }

    public interface IShapeSubtype<TBase>
    {
        Type Subtype { get; }
        TResult Accept<TResult>(ISubtypeVisitor<TBase, TResult> visitor);
    }

    public class ShapeSubtype<TSubtype, TBase> : IShapeSubtype<TBase> where TSubtype : TBase
    {
        public Type Subtype => typeof(TSubtype);

        public TResult Accept<TResult>(ISubtypeVisitor<TBase, TResult> visitor)
        {
            return visitor.Visit<TSubtype>();
        }
    }
}
