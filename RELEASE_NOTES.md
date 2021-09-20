### 10.0.0-alpha1
* Revert to using Reflection.Emit when generating member accessors.
* Add ShapeMember.GetByRef and SetByRef methods for updating objects by reference.
* Apply performance optimizations to all bundled generic programs.

### 9.0.0
* Only target netstandard2.0.
* Remove TYPESHAPE_EMIT build conditional.
* Remove System.Reflection.Emit dependency.

### 8.0.1
* Surface correct target frameworks in nuget package.

### 8.0.0
* Add higher-kinded type DSLs for type-safe generic programming.
* Make shapeof<> operator return typed results and deprecate tshapeof.
* Make TypeShape<T> instances singletons.

### 7.1.0
* Expose IsAnonymousRecord property for F# record shapes.

### 7.0.0
* Fix Nullable related bugs.
* Deprecate fixed arity Tuple and Choice shapes.
* Simplify API for array, list, option and ref shapes.
* Rename `ITypeShapeVisitor` to `ITypeVisitor`.
* Rename `IShapeMember` to `IShapeReadOnlyMember`.
* Rename `IShapeWriteMember` to `IShapeMember`.
* Rename `IShapeMember.Project` to `IShapeMember.Get`.
* Rename `IShapeMember.Inject` to `IShapeMember.Set`.

### 6.1.2
* Fix Nullable related bugs.

### 6.1.1
* Remove Sourcelink nuget dependency.

### 6.1
* Expose generic clone function in main library.
* Add cancellable generic iter combinator.

### 6.0.1
* Use Microsoft Github SourceLink.

### 6.0.0
* Revise UnionContract design.

### 5.1.0
* Fix API Issues.

### 5.0.0
* Generalize "EventSum" implementation to "UnionEncoder".
* Provide "gsizeof" operator.

### 4.0.0
* Namespace refactorings.
* Provide "empty", "EventSum" and combinator implementations.

### 3.1.1
* Tuple Shape Bugfix

### 3.1
* Minor Bugfix

### 3.0
* Support netstandard2.0

### 2.20
* Refine subtype shapes

### 2.19
* Refine exception shapes

### 2.18
* Add Subtype shapes.

### 2.17
* Bugfixes and misc improvements.

### 2.16
* Support struct unions, records and tuples.

### 2.15
* Add element fields to Choice shapes.

### 2.14
* Add element fields to Tuple shapes.

### 2.13
* Fix code generation issue.

### 2.12
* Add elemenent shape properties to collection shapes.

### 2.11
* Add TypeCache.GetOrAdd method.

### 2.10
* Add binary search utility.

### 2.9
* Bugfix in ShapeUnion.GetTag.

### 2.8
* Refactor TypeCache implementation.

### 2.7
* Rename TypeIndex to TypeCache.

### 2.6
* Refactor ISerializable shape to include constructor and staging support.
* Refactor TypedIndex API.

### 2.5
* Staging bugfix.

### 2.4
* Add RecType utility.
* ShapePoco bugfixes.

### 2.3
* Add Shape.Primitive active pattern.

### 2.2
* Update staged samples.

### 2.1
* Add Expr.unlet utility.

### 2.0
* Add support for ShapeMember, ShapeConstructor, ShapeCliMutable, ShapePoco.
* Add support for staged expressions.

### 1.2
* Fix Equality shape implementation.
* Add Comparison shape.

### 1.1
* Add Struct, NotStruct and Equality shapes.

### 1.0 - Initial Release
* Initial release
