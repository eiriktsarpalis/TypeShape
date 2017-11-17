using System;

// Binding type arguments in subtype constraint currently not possible in F#,
// defining those shape types in C#.

namespace TypeShape
{
    /// Witness type that encapsulates subtype relationship of two generic types,
    /// used to assist F# type checker does not explicitly permit such configurations
    public class SubtypeWitness<TSubtype, TBase> where TSubtype : TBase
    {
        /// <summary>
        ///     Upcasts supplied value
        /// </summary>
        /// <param name="value"></param>
        /// <returns>The upcast value</returns>
        public TBase Upcast(TSubtype value) { return (TBase)value; }

        /// <summary>
        ///     Returns true if value matches the subtype type.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsSubtype(TBase value)
        {
            return value is TSubtype;
        }

        /// <summary>
        ///     Safe downcast
        /// </summary>
        /// <param name="value"></param>
        /// <param name="result"></param>
        /// <returns></returns>
        public bool TryDowncast(TBase value, out TSubtype result)
        {
            if (value is TSubtype)
            {
                result = (TSubtype)value;
                return true;
            }
            else
            {
                result = default(TSubtype);
                return false;
            }
        }
    }

    public interface ISubtypeVisitor<TBase, TResult>
    {
        TResult Visit<TSubtype>() where TSubtype : TBase;
    }

    public interface ISubtypeWitnessVisitor<TBase, TResult>
    {
        TResult Visit<TSubtype>(SubtypeWitness<TSubtype, TBase> container) where TSubtype : TBase;
    }

    public interface IShapeSubtype<TBase>
    {
        Type Subtype { get; }
        TResult Accept<TResult>(ISubtypeVisitor<TBase, TResult> visitor);
        TResult Accept<TResult>(ISubtypeWitnessVisitor<TBase, TResult> visitor);
    }

    public sealed class ShapeSubtype<TSubtype, TBase> : IShapeSubtype<TBase> where TSubtype : TBase
    {
        public Type Subtype => typeof(TSubtype);

        public TResult Accept<TResult>(ISubtypeVisitor<TBase, TResult> visitor)
        {
            return visitor.Visit<TSubtype>();
        }

        TResult IShapeSubtype<TBase>.Accept<TResult>(ISubtypeWitnessVisitor<TBase, TResult> visitor)
        {
            return visitor.Visit<TSubtype>(new SubtypeWitness<TSubtype,TBase>());
        }
    }
}
