using System;

// Binding type arguments in subtype constraint currently not possible in F#,
// defining those shape types in C#.

namespace TypeShape.Core
{
    /// Witness type that encapsulates subtype relationship of two generic types,
    /// used to assist F# type checker does not explicitly permit such configurations
    public class SubtypeWitness<TSubtype, TBase> where TSubtype : TBase
    {
        /// <summary>
        ///     Returns true if value matches the subtype type.
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public bool IsSubtype(TBase value) => value is TSubtype;

        /// <summary>
        ///     Upcasts supplied value
        /// </summary>
        /// <param name="value"></param>
        /// <returns>The upcast value</returns>
        public TBase Upcast(TSubtype value) => value;

        /// <summary>
        ///     Unsafe downcast
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        public TSubtype Downcast(TBase value) => (TSubtype)value;

        /// <summary>
        ///     Safe downcast
        /// </summary>
        /// <param name="value"></param>
        /// <param name="result"></param>
        /// <returns></returns>
        public bool TryDowncast(TBase value, out TSubtype result)
        {
            if (value is TSubtype subtypeValue)
            {
                result = subtypeValue;
                return true;
            }
            else
            {
                result = default;
                return false;
            }
        }
    }

    /// <summary>
    ///     Subtype Visitor interface
    /// </summary>
    /// <typeparam name="TBase">Base type being visited.</typeparam>
    /// <typeparam name="TResult">Result type of the visitor.</typeparam>
    public interface ISubtypeVisitor<TBase, TResult>
    {
        /// <summary>
        ///     Performs visitation by bringing the encapsulated subtype into scope.
        /// </summary>
        /// <typeparam name="TSubtype"></typeparam>
        /// <returns></returns>
        TResult Visit<TSubtype>() where TSubtype : TBase;
    }

    /// <summary>
    ///     Subtype relationship visitor interface
    /// </summary>
    /// <typeparam name="TBase"></typeparam>
    /// <typeparam name="TResult"></typeparam>
    public interface ISubtypeWitnessVisitor<TBase, TResult>
    {
        /// <summary>
        ///     Visits the subtype witness by bringing the subtype type into scope.
        /// </summary>
        /// <typeparam name="TSubtype"></typeparam>
        /// <param name="container"></param>
        /// <returns></returns>
        TResult Visit<TSubtype>(SubtypeWitness<TSubtype, TBase> container) where TSubtype : TBase;
    }

    /// <summary>
    ///     Interface that encapsulates a subtype relationship between an
    ///     exposed TBase type and an encapsulated TSubtype type.
    /// </summary>
    /// <typeparam name="TBase"></typeparam>
    public interface IShapeSubtype<TBase>
    {
        /// <summary>
        ///     System.Type representation of the encapsulated subtype.
        /// </summary>
        Type Subtype { get; }

        /// <summary>
        ///     Visits the subtype using a provided subtype visitor.
        /// </summary>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="visitor"></param>
        /// <returns></returns>
        TResult Accept<TResult>(ISubtypeVisitor<TBase, TResult> visitor);

        /// <summary>
        ///     Visits the subtype using a provided subtype witness visitor.
        /// </summary>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="visitor"></param>
        /// <returns></returns>
        TResult Accept<TResult>(ISubtypeWitnessVisitor<TBase, TResult> visitor);
    }

    /// <summary>
    ///     Concrete subtype shape implementation
    /// </summary>
    /// <typeparam name="TSubtype"></typeparam>
    /// <typeparam name="TBase"></typeparam>
    public sealed class ShapeSubtype<TSubtype, TBase> : IShapeSubtype<TBase> where TSubtype : TBase
    {
        /// <summary>
        ///     System.Type representation of the encapsulated subtype
        /// </summary>
        public Type Subtype => typeof(TSubtype);

        /// <summary>
        ///     Subtype visitor accept implementation
        /// </summary>
        /// <typeparam name="TResult"></typeparam>
        /// <param name="visitor"></param>
        /// <returns></returns>
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
